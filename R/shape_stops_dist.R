
# options(scipen = 9999)
# library(gtfstools)
# library(data.table)
# library(sf)
# library(kauetools)
# gtfs <- read_gtfs("../../data-raw/dissertacao/gtfs/2018/GTFS_fortaleza_20180907.zip")


# Em qual ponto do arquivo shapes encontra-se a parada daquela linha e sentido?
shapes_stops_dists <- function(gtfs) {

  # calcular stops_linha
  stop_linhas <- extract_scheduled_stops(gtfs)
  shapes_linhas <- setDT(shapes_to_sf(gtfs))

  # function for each shape_id
  # shape_id1 <- "shape075-I"
  # shape_id1 <- shapes_linhas$shape_id[10]
  # shape_id1 <- shapes_linhas$shape_id[541]

  diff <- setdiff(stop_linhas$shape_id, shapes_linhas$shape_id)
  if (length(diff) >= 1)  message("The following shape_id are in the stop_times file but not on shapes: \n", diff)

  # make sure every shape on the stop files are on the shapes
  shapes_linhas <- shapes_linhas[shape_id %in% stop_linhas$shape_id]

  shapes_stops_dists_shape <- function(shape_id1) {

    # filter shape_id
    shapes_linhas_filter <- setDT(shapes_linhas)[shape_id == shape_id1]
    stop_linhas_filter <- stop_linhas[shape_id == shape_id1]

    # Se a linha tiver so duas paradas, considerar aquelas duas paradas
    if (nrow(stop_linhas_filter) == 2) {

      stop_linhas_filter <- stop_linhas_filter[, dist := 0]


    } else {

      # # Separar a primeira parada
      # stops_linhas_ok <- stop_linhas_filter %>%
      #   # slice(-1, -n()) %>%
      #   slice(-1) %>%
      #   mutate(stop_sequence_id = 1:n())
      #
      # # stops_linhas_ultimas <- slice(stops_linhas_df, 1, n()) %>% mutate(dist = 0) %>% select(-stop_name)
      # stops_linhas_ultimas <- slice(stop_linhas_filter, 1) %>% mutate(dist = 0) %>% select(-stop_name)

      # ou..
      shapes_sp <- shapes_linhas_filter %>% st_sf() %>% st_transform(29194) %>%
        as_Spatial()

      stops_linhas_sp <- stop_linhas_filter %>% st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% st_transform(29194) %>% as_Spatial()

      # Aplicar geosphere
      dist <- rgeos::gProject(shapes_sp, stops_linhas_sp)

      # Juntar
      stop_linhas_filter[, dist := dist]

      # outpout
      fwrite(stop_linhas_filter, sprintf("teste/data/%s.csv", shape_id1))

      return(stop_linhas_filter)

    }

  }

  # run function for every shape
  trechos <- lapply(shapes_linhas$shape_id,
                    FUN = purrr::possibly(shapes_stops_dists_shape, otherwise = NA_real_))
  names(trechos) <- shapes_linhas$shape_id
  trechos_NA <- trechos[is.na(trechos)]
  if (length(trechos_NA >= 1)) message("It was not possible to estimate stop distances for shape id: \n", names(trechos_NA))

  # bind them
  trechos <- trechos[!is.na(trechos)]
  trechos <- rbindlist(trechos)

}
