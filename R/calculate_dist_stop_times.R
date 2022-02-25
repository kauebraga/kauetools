#' Calculate distances and speeds for stop_times segments
#'
#' Returns the GTFS with the stop_time file with columns of distance and speed
#'
#' @param gtfs A GTFS object.
#'
#' @return A `gtfs` object with updated stop_times
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
#'@export

# options(scipen = 9999)
# library(gtfstools)
# library(data.table)
# library(sf)
# library(kauetools)



# Em qual ponto do arquivo shapes encontra-se a parada daquela linha e sentido?
calculate_dist_stop_times <- function(gtfs) {

  # gtfs <- read_gtfs("../../data-raw/dissertacao/gtfs/2018/GTFS_fortaleza_20180907.zip")
  # gtfs <- read_gtfs("/media/kaue/Data/data-raw/gtfs/brazil/cur/2019/gtfs_cur_urbs_2019-05.zip")
  # gtfs <- read_gtfs("/media/kaue/Data/data-raw/gtfs/brazil/spo/2019/gtfs_spo_sptrans_2019-04.zip")
  # gtfs <- read_gtfs("/media/kaue/Data/data-raw/gtfs/brazil/cam/2019/gtfs_cam_emdec_2019-10_fixed.zip")
  # gtfs <- read_gtfs("/media/kaue/Data/data-raw/gtfs/brazil/rio/2019/gtfs_rio_fetranspor_2019-10_mod.zip")
  # gtfs <- read_gtfs("/media/kaue/Data/data-raw/gtfs/brazil/rio/intersul_v1.zip")
  # gtfs <- read_gtfs("../../repos/curso_r_transportes/data-raw/gtfs_brt_08-10-2021.zip")

  # calcular stops_linha
  stop_linhas <- extract_scheduled_stops(gtfs)

  # convert to sf
  shapes_linhas <- gtfstools::convert_shapes_to_sf(gtfs)


  # function for each shape_id
  # shape_id1 <- "shape075-I"
  # shape_id1 <- "shape026-I"
  # shape_id1 <- "shape004-V"
  # shape_id1 <- shapes_linhas$shape_id[10]
  # shape_id1 <- shapes_linhas$shape_id[541]
  # shape_id1 <- "3234" # cur
  # shape_id1 <- unique(shapes_linhas$shape_id)[89] # cam
  # shape_id1 <- "31408_V" # cam
  # shape_id1 <- "shape011-I" # for
  # shape_id1 <- "shape012-V" # for
  # shape_id1 <- "2000042111110101" # rio brt
  # shape_id1 <- "2200006102030101" # rio novo bernardo
  # shape_id1 <- "2200010101030101" # rio novo bernardo


  diff <- setdiff(stop_linhas$shape_id, shapes_linhas$shape_id)
  if (length(diff) >= 1)  message("The following shape_id are in the stop_times file but not on shapes: \n", diff)

  message("Total shapes: ", length(unique(shapes_linhas$shape_id)))

  # make sure every shape on the stop files are on the shapes
  shapes_linhas <- subset(shapes_linhas, shape_id %in% stop_linhas$shape_id)

  # shape_id1 <- shapes_linhas$shape_id[20]

  shapes_stops_dists_shape <- function(shape_id1) {

    # filter shape_id
    shapes_linhas_filter <- subset(shapes_linhas, shape_id == shape_id1)
    stop_linhas_filter   <- subset(stop_linhas,   shape_id == shape_id1)

    # standardize shape resolution - at least every 50 meters
    shapes_linhas_filter <- st_segmentize(shapes_linhas_filter, 50)
    # shapes_linhas_filter <- sfheaders::sf_cast(shapes_linhas_filter, "POINT")
    # transform to lon lat
    shapes_linhas_filter <- sfheaders::sf_to_df(shapes_linhas_filter) %>% setDT()
    # rename
    shapes_linhas_filter <- shapes_linhas_filter[, .(shape_pt_lon = x, shape_pt_lat = y)]

    # create shape_pt_sequence
    shapes_linhas_filter[, shape_pt_sequence := 1:.N]
    # calculate shape dist traveled
    get.dist <- function(lon, lat) geosphere::distGeo(tail(cbind(lon,lat),-1),head(cbind(lon,lat),-1))
    shapes_linhas_filter[, shape_dist_traveled := c(0,cumsum(get.dist(shape_pt_lon,shape_pt_lat)))]





    # # Separar a primeira e ultima parada
    # stops_linhas_ok <- stops_linhas_df %>%
    #   # slice(-1, -n()) %>%
    #   slice(-1) %>%
    #   mutate(stop_sequence_id = 1:n())
    #
    # # stops_linhas_ultimas <- slice(stops_linhas_df, 1, n()) %>% mutate(dist = 0) %>% select(-stop_name)
    # stops_linhas_ultimas <- slice(stops_linhas_df, 1) %>% mutate(dist = 0) %>% select(-stop_name)

    uui <- RANN::nn2(shapes_linhas_filter[, .(shape_pt_lon, shape_pt_lat)], stop_linhas_filter[,. (stop_lon, stop_lat)], 25)
    # uui <- RANN::nn2(shapes_linhas_filter[, .(shape_pt_lon, shape_pt_lat)], stop_linhas_filter[,. (stop_lon, stop_lat)], searchtype = "radius", radius = 0.004491556)
    uui_df <- as.data.frame( uui$nn.idx)
    # uui_df <- cbind(as.data.frame( uui$nn.idx), as.data.frame(uui$nn.dists * 111320))

    # create tidy df with points an distances

    uui_df_tidy <- as.data.table(uui$nn.idx)
    uui_df_tidy[, stop_sequence := 1:.N]
    colnames(uui_df_tidy) <- c(paste0(rep("option_", 25), c(1:25)), "stop_sequence")
    uui_df_tidy <- tidyr::pivot_longer(uui_df_tidy, cols = option_1:option_25, names_to = "option", values_to = "shape_pt_sequence") %>% setDT()

    uui_df_tidy2 <- as.data.table(uui$nn.dists * 111320)
    uui_df_tidy2 <- tidyr::pivot_longer(uui_df_tidy2, cols = V1:V25, names_to = "option", values_to = "dist_point") %>% setDT()

    uui_df_tidy[, dist_from_shape := uui_df_tidy2$dist_point]




    # teste
    # uui_df[11,10] <- 900


    # make sure that the first and the last stops are located in the beginning
    # of the shape
    # in this case, in the first points of the shape that sum a max of 200m of distance
    # if (stop_linhas_filter[[1, 4]] == 1) {

    # shape_start_point <- max(shapes_linhas_filter[shape_dist_traveled < 500]$shape_pt_sequence)
    first_stop_row <- uui_df[1, ][uui_df[1,] %between% c(1, 50)]
    uui_df$ordered[1] <- first(first_stop_row)

    # a <- shapes_linhas_filter %>% st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
    # b <- stop_linhas_filter %>% st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
    # mapview::mapview(a[1:100,], zcol = "shape_pt_sequence") + mapview::mapview(b)

    # } else uui_df$ordered[1] <- uui_df[1,1]

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
      x <- uui_df[i, 1:25]

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



    # uui_df_tidy_ai <- uui_df_tidy %>%
    #   left_join(search_results %>% select(shape_pt_sequence, stop_sequence), by = "stop_sequence")
    # # group_by(stop_sequence) %>%
    # # filter(between(shape_pt_sequence, ordered - 10, ordered + 10))


    # jogar no arquivo de shapes
    shapes_linhas_filter[uui_df, on = "shape_pt_sequence",
                         c("stop_sequence", "dist_from_shape") := list(i.stop_sequence, i.dist_from_shape)]
    shapes_linhas_filter[, stop_sequence := as.numeric(stop_sequence)]

    # keep only the shapes points where there was the best match between shape and stop
    shapes_linhas_filter <- shapes_linhas_filter[!is.na(stop_sequence)]
    # discount the distance from the first point
    shapes_linhas_filter[, shape_dist_traveled := shape_dist_traveled - shape_dist_traveled[1]]
    # select vars
    shapes_linhas_filter <- shapes_linhas_filter[, .(stop_sequence, shape_dist_traveled, dist_from_shape)]

    # bring the results (cumulative distance) to the scheduled service
    stop_linhas_filter[shapes_linhas_filter, on = "stop_sequence",
                       c("dist_acc", "dist_error") := list(i.shape_dist_traveled, i.dist_from_shape)]

    # save
    # readr::write_rds(stop_linhas_filter, sprintf("teste/data/snap_shape_stop/snap_shape_stop_%s.rds", shape_id1))




  }

  # run function for every shape
  trechos <- parallel::mclapply(unique(shapes_linhas$shape_id),
                                FUN = purrr::possibly(shapes_stops_dists_shape, otherwise = NA_real_),
                                mc.cores = 5)
  names(trechos) <- unique(shapes_linhas$shape_id)
  trechos_NA <- trechos[is.na(trechos)]
  if (length(trechos_NA >= 1)) message("It was not possible to estimate stop distances for shape id: \n", paste0(names(trechos_NA), collapse = ", "))

  # bind them
  trechos <- trechos[!is.na(trechos)]
  trechos <- rbindlist(trechos)

  # bring shape_id to stop_times
  gtfs$stop_times[gtfs$trips, on = "trip_id",
             c("shape_id") := list(i.shape_id)]
  gtfs$stop_times[trechos, on = c("shape_id", "stop_id", "stop_sequence"),
             c("dist_acc") := list(i.dist_acc)]
  # calculate speeds
  gtfs$stop_times[, arrival_time1 := hms_to_sec(arrival_time)]
  gtfs$stop_times[, ':='(ttime = arrival_time1 - shift(arrival_time1, 1, type = "lag"),
                    dist = dist_acc - shift(dist_acc, 1, type = "lag")), by = "trip_id"]
  gtfs$stop_times[, speed := (dist/as.integer(ttime)) *3.6 ]

  return(gtfs)



}



