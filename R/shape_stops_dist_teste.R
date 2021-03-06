#' Create linestrings from shapes
#'
#' Returns the most common service (the sequence of stops on the `stop_times` file) for each shape
#'
#' @param gtfs A GTFS object.
#' @param service_id A character specifying a service (from the `calendar`) to subset, if desired. This is specially
#' useful for large GTFS file where one is interested only on weekdays trips, for example.
#' @param route_id A character specifying a route (from the `routes`) to subset, if desired
#'
#' @return A `data.table`.with the scheduled service (sequence of stops) for each `route` and `shape_id`
#'
#' @section Details:
#' Details here.
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' trip_geometry <- get_trip_geometry(gtfs)
#' head(trip_geometry)
#'
#' # the above is identical to
#' trip_geometry <- get_trip_geometry(gtfs, file = c("shapes", "stop_times"))
#' head(trip_geometry)
#'
#' trip_ids <- c("CPTM L07-0", "2002-10-0")
#' trip_geometry <- get_trip_geometry(gtfs, trip_id = trip_ids)
#' trip_geometry
#' plot(trip_geometry["origin_file"])
#'

# options(scipen = 9999)
# library(gtfstools)
# library(data.table)
# library(sf)
# library(kauetools)
# gtfs <- read_gtfs("../../data-raw/dissertacao/gtfs/2018/GTFS_fortaleza_20180907.zip")
# gtfs <- read_gtfs("/media/kaue/Data/data-raw/gtfs/brazil/cur/2019/gtfs_cur_urbs_2019-05.zip")



# Em qual ponto do arquivo shapes encontra-se a parada daquela linha e sentido?
shapes_stops_dists <- function(gtfs) {


  # calcular stops_linha
  stop_linhas <- extract_scheduled_stops(gtfs)

  # transform to the stop format (each obs is one stop) to the segment format
  stop_linhas_segments <- stop_linhas[, ':='(stop_sequence_depois = shift(stop_sequence, type = "lead"),
                                             stop_id_depois =       shift(stop_id,       type = "lead"),
                                             stop_lon_depois =      shift(stop_lon,      type = "lead"),
                                             stop_lat_depois =      shift(stop_lat,      type = "lead")),
                                      by = shape_id]
  stop_linhas_segments <- stop_linhas_segments[, .SD[-.N], by = shape_id]
  stop_linhas_segments[, segment_id := paste0(stop_id, "-", stop_id_depois), by = shape_id]
  stop_linhas_segments_unique <- unique(stop_linhas_segments, by = "segment_id")


  b1 <- stop_linhas_segments_unique[, .(shape_id, route_id, stop_id, stop_sequence, stop_lon, stop_lat)]
  b2 <- stop_linhas_segments_unique[, .(shape_id, route_id, stop_id = stop_id_depois, stop_sequence = stop_sequence_depois, stop_lon = stop_lon_depois, stop_lat = stop_lat_depois)]

  ## Add on a 'sequence' variable so we know which one comes first
  b1[, seq := 1L ]
  b2[, seq := 2L ]

  ## put back together
  dt <- rbindlist(list(b1, b2), use.names = TRUE)
  setorder(dt, shape_id, stop_sequence, seq)
  # remove duplicates
  stop_linhas_segments_unique <- unique(dt, by = c("shape_id", "route_id", "stop_id", "stop_sequence"))


  # # we need to duplicate the last observation of each shape to get the full stops format
  # a <-stop_linhas_segments_unique[, .SD[.N], by = shape_id]
  # a <-a[, .(shape_id, route_id, stop_id = stop_id_depois, stop_sequence = stop_sequence_depois, stop_lon = stop_lon_depois, stop_lat = stop_lat_depois)]
  # # duplicate so we get back to the 'stops' format
  # stop_linhas_segments_unique <- stop_linhas_segments_unique[, .(shape_id, route_id, stop_id, stop_sequence, stop_lon, stop_lat)]
  # # bind
  # stop_linhas_segments_unique <- rbind(stop_linhas_segments_unique, a)
  # # order
  # setorder(stop_linhas_segments_unique, shape_id, stop_sequence)
  #
  # # shapes_linhas <- setDT(shapes_to_sf(gtfs))
  # shapes_linhas <- gtfs$shapes
  #
  # stop_linhas_segments_unique[, .N, by = shape_id] %>% View()


  # function for each shape_id
  # shape_id1 <- "shape075-I"
  # shape_id1 <- "shape026-I"
  # shape_id1 <- "shape004-V"
  # shape_id1 <- shapes_linhas$shape_id[10]
  # shape_id1 <- shapes_linhas$shape_id[541]

  diff <- setdiff(stop_linhas$shape_id, shapes_linhas$shape_id)
  if (length(diff) >= 1)  message("The following shape_id are in the stop_times file but not on shapes: \n", diff)

  # make sure every shape on the stop files are on the shapes
  shapes_linhas <- shapes_linhas[shape_id %in% stop_linhas_segments_unique$shape_id]



  shapes_stops_dists_shape <- function(shape_id1) {

    # filter shape_id
    shapes_linhas_filter <- setDT(shapes_linhas)[shape_id == shape_id1]
    stop_linhas_filter <- stop_linhas_segments_unique[shape_id == shape_id1]
    # make shure shape_pt_sequence is from 1:n()
    shapes_linhas_filter[, shape_pt_sequence := 1:.N]

    # calculate shape dist traveled
    get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1),head(cbind(lon,lat),-1))
    shapes_linhas_filter[, shape_dist_traveled := c(0,cumsum(get.dist(shape_pt_lon,shape_pt_lat)))]


    # Se a linha tiver so duas paradas, considerar aquelas duas paradas
    if (nrow(stop_linhas_filter) == 2) {

      stop_linhas_filter <- stop_linhas_filter[, dist := 0]


    } else {



      # # Separar a primeira e ultima parada
      # stops_linhas_ok <- stops_linhas_df %>%
      #   # slice(-1, -n()) %>%
      #   slice(-1) %>%
      #   mutate(stop_sequence_id = 1:n())
      #
      # # stops_linhas_ultimas <- slice(stops_linhas_df, 1, n()) %>% mutate(dist = 0) %>% select(-stop_name)
      # stops_linhas_ultimas <- slice(stops_linhas_df, 1) %>% mutate(dist = 0) %>% select(-stop_name)

      uui <- RANN::nn2(shapes_linhas_filter[, .(shape_pt_lon, shape_pt_lat)], stop_linhas_filter[,. (stop_lon, stop_lat)], 10)
      uui_df <- as.data.frame( uui$nn.idx)
      # uui_df <- cbind(as.data.frame( uui$nn.idx), as.data.frame(uui$nn.dists * 111320))

      # create tidy df with points an distances

      uui_df_tidy <- as.data.table(uui$nn.idx)
      uui_df_tidy[, stop_sequence := 1:.N]
      colnames(uui_df_tidy) <- c(paste0(rep("option_", 10), c(1:10)), "stop_sequence")
      uui_df_tidy <- tidyr::pivot_longer(uui_df_tidy, cols = option_1:option_10, names_to = "option", values_to = "shape_pt_sequence") %>% setDT()

      uui_df_tidy2 <- as.data.table(uui$nn.dists * 111320)
      uui_df_tidy2 <- tidyr::pivot_longer(uui_df_tidy2, cols = V1:V10, names_to = "option", values_to = "dist_point") %>% setDT()

      uui_df_tidy[, dist_from_shape := uui_df_tidy2$dist_point]




      # teste
      # uui_df[11,10] <- 900


      # make sure that the first and the last stops are located in the beginning
      # of the shape
      # in this case, in the first 20 points of the shape
      if (stop_linhas_filter[[1, 4]] == 1) {

        first_stop_row <- uui_df[1, ][uui_df[1,] %between% c(1, 20)]
        uui_df$ordered[1] <- first(first_stop_row)

      } else uui_df$ordered[1] <- uui_df[1,1]

      # search_results[nrow(search_results),c("shape_pt_sequence")] <-
      #   ifelse(search_results[nrow(search_results),c("shape_pt_sequence")] %between% c(nrow(shapes_df_sf_break) - 20, nrow(shapes_df_sf_break)),
      #          search_results[nrow(search_results),c("shape_pt_sequence")], nrow(shapes_df_sf_break))

      # i <- 2
      # i <- 10
      # i <- 11
      # i <- 17
      # i <- 69
      # i <- 38
      # i <- 40
      # i <- 41
      # i <- 43

      # iterate from the stop_sequence 2 and after, because we already locked to stop_sequence 1
      for (i in 2:nrow(uui_df)) {

        # extract the values related to the stop_sequence in question
        x <- uui_df[i, 1:10]

        if (is.na(uui_df$ordered[i-1])) {

          # get first non NA
          target <- last(uui_df$ordered[1:i-1][!is.na(uui_df$ordered[1:i-1])])

          # the target will be the snapped point from the previous interation
        } else {target <- uui_df$ordered[i-1]}

        # make sure that all possible snap values are after the previous snapped point
        # this ensure that we will not have negative distances - the most problematic situations
        x <- x[x > target]

        # sanity check:
        if (length(x) == 0) {

          go <- NA

          # make sure that the closest point is not too far away from the previous point.
          # one situation that can happen is when the snapped point is after the previous snap
          # (it's ok), but it was snapped far down the road, and not very close to the previous point
        } else if (x[1] - target <= 40) {

          go <- x[1]

          # if the above situation occur, we will atribute 'weights' to each point, based on its difference to the previous snapped
          # so, if the closest point is 400 (wrong snap) and the second is 20 (correct snap),
          # the frist point will be assigned a new value of (400 - preivous_snapped (2)) * 1 = 398
          # the second point will be assigned a new value of (20 - preivous_snapped(2)) * 2 = 38
          # and we select the minimium of these weighted values, so the second points would be choosen
          # because 38 < 398
        } else {

          go <- x[which.min((x - target) * seq(1, by = 1, length.out = length(x)))] %>% as.numeric()
        }


        if(length(go) == 0) go <- NA

        uui_df$ordered[i] <- go

      }

      search_results <- setDT(uui_df)
      uui_df[, stop_sequence := 1:.N]
      # rename columns
      uui_df <- uui_df[, .(shape_pt_sequence = ordered, stop_sequence)]
      uui_df[uui_df_tidy, on = c("shape_pt_sequence", "stop_sequence"),
             c("dist_from_shape") := list(i.dist_from_shape)]



      dplyr::left_join(select(uui_df_tidy, stop_sequence, shape_pt_sequence, dist_from_shape),
                       by = c("shape_pt_sequence", "stop_sequence"))



      # uui_df_tidy_ai <- uui_df_tidy %>%
      #   left_join(search_results %>% select(shape_pt_sequence, stop_sequence), by = "stop_sequence")
      # # group_by(stop_sequence) %>%
      # # filter(between(shape_pt_sequence, ordered - 10, ordered + 10))


      # jogar no arquivo de shapes
      shapes_linhas_filter <- shapes_linhas_filter %>%
        left_join(search_results, by = "shape_pt_sequence") %>%
        mutate(stop_sequence = as.numeric(stop_sequence))
      # mutate(dist = c(0,cumsum(get.dist(lon,lat)))) %>%
      # fill(stop_sequence)

      shapes_linhas_filter_acc <- shapes_linhas_filter %>% filter(!is.na(stop_sequence)) %>%
        # discount the distance from the first point
        mutate(shape_dist_traveled = shape_dist_traveled - shape_dist_traveled[1]) %>%
        select(stop_sequence, shape_dist_traveled, dist_from_shape)

      stop_linhas_filter <- stop_linhas_filter %>%
        left_join(shapes_linhas_filter_acc, by = "stop_sequence")



    }

  }

  # run function for every shape
  trechos <- parallel::mclapply(shapes_linhas$shape_id,
                                FUN = purrr::possibly(shapes_stops_dists_shape, otherwise = NA_real_),
                                mc.cores = 10)
  names(trechos) <- shapes_linhas$shape_id
  trechos_NA <- trechos[is.na(trechos)]
  if (length(trechos_NA >= 1)) message("It was not possible to estimate stop distances for shape id: \n", names(trechos_NA))

  # bind them
  trechos <- trechos[!is.na(trechos)]
  trechos <- rbindlist(trechos)

}



