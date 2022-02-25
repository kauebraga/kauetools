#' Extract GTFS segments
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
#'@export

# options(scipen = 9999)
# library(gtfstools)
# library(data.table)
# library(sf)
# library(kauetools)
# gtfs <- read_gtfs("../../data-raw/dissertacao/gtfs/2018/GTFS_fortaleza_20180907.zip")
# gtfs <- read_gtfs("/media/kaue/Data/data-raw/gtfs/brazil/cur/2019/gtfs_cur_urbs_2019-05.zip")

extract_segments <- function(gtfs) {

  # get stops
  stops <- gtfs$stops[, .(stop_id, stop_lon, stop_lat)]

  # get stoptimes
  stop_times <- gtfs$stop_times[, .(trip_id, stop_id, arrival_time, departure_time, stop_sequence)]

  # trasnform stop times to a segment format
  # transform to the stop format (each obs is one stop) to the segment format
  stop_time_segments <- stop_times[, ':='(stop_sequence_depois = shift(stop_sequence, type = "lead"),
                                          stop_id_depois = shift(stop_id, type = "lead")),
                                   by = trip_id]
  stop_time_segments <- stop_time_segments[, .SD[-.N], by = trip_id]
  stop_time_segments[, segment_id := paste0(stop_id, "-", stop_id_depois)]
  stop_linhas_segments_distinct <- unique(stop_time_segments, by = c("segment_id"))
  stop_linhas_segments_distinct <- stop_linhas_segments_distinct[, .(segment_id, stop_id_start = stop_id, stop_id_end = stop_id_depois)]

  # bring stops coordinates
  stop_linhas_segments_distinct[stops, on = c("stop_id_start" = "stop_id"),
                                c("stop_lon_start", "stop_lat_start") :=
                                  list(i.stop_lon, i.stop_lat)]

  stop_linhas_segments_distinct[stops, on = c("stop_id_end" = "stop_id"),
                                c("stop_lon_end", "stop_lat_end") :=
                                  list(i.stop_lon, i.stop_lat)]

  # from https://stackoverflow.com/questions/51918536/r-create-linestring-from-two-points-in-same-row-in-dataframe
  dt1 <- stop_linhas_segments_distinct[, .(segment_id, lon = stop_lon_start, lat = stop_lat_start)]
  dt2 <- stop_linhas_segments_distinct[, .(segment_id, lon = stop_lon_end,   lat = stop_lat_end)]

  ## Add on a 'sequence' variable so we know which one comes first
  dt1[, seq := 1L ]
  dt2[, seq := 2L ]

  ## put back together
  dt <- rbindlist(list(dt1, dt2), use.names = TRUE)
  setorder(dt, segment_id, seq)

  stop_linhas_segments_sf <- sfheaders::sf_linestring(
    obj = dt
    , x = "lon"
    , y = "lat"
    , linestring_id = "segment_id"
  )

  return(stop_linhas_segments_sf)

}

