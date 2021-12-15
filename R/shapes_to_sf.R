#' Ecreate linestrings from shapes
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
shapes_to_sf <- function(gtfs, shape_id = NULL) {

  # generate geometry; the condition for nrow == 0 prevents an sfheaders error
  shapes <- if (!is.null(shape_id)) gtfs$shapes[shape_id %chin% shape_id] else gtfs$shapes
  shapes <- shapes[order(shape_id, shape_pt_sequence)]

  if (nrow(shapes) == 0) {

    empty_linestring <- sf::st_sfc()
    class(empty_linestring)[1] <- "sfc_LINESTRING"

    shapes_sf <- sf::st_sf(
      shape_id = character(),
      geometry = empty_linestring,
      stringsAsFactors = FALSE
    )

  } else {

    shapes_sf <- sfheaders::sf_linestring(
      shapes,
      x = "shape_pt_lon",
      y = "shape_pt_lat",
      linestring_id = "shape_id"
    )

  }

  shapes_sf <- sf::st_set_crs(shapes_sf, 4326)

}
