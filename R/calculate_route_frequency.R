#' Calculate route frequency
#'
#' Calculate headway between departures for route and direction
#'
#' @param gtfs A GTFS object.
#' @param route_id  A character specifying a route (from `routes`) to subset, if desired
#' @param service_id1 A character specifying a service_id (from `calendar`) to subset, if desired
#' @param start_time A character specifying the start of the window
#' @param end_time A character specifying the end of the window
#' @param mean_headway TRUE/FASE - Should return the mean headway in the entire window (TRUE) or the hourly mean headway (FALSE)?
#'
#' @return A `data.table` with the headway for each `route` and `shape_id`
#'
#' @export

# function to calculate route frequency on gtfs
# gtfs_path <- "/home/kaue/Documents/Downloads_linux_notebook/gtfs_for_etufor_2019-10.zip"
# gtfs_path <- "/home/kaue/Documents/Downloads_linux_notebook/gtfs_spo_bernardo_2020-11.zip"
# gtfs_path <- "/home/kaue/Documents/Downloads_linux_notebook/gtfs_cur_urbs_2019-10.zip"
# lines to be changed frequency
# gtfs <- read_gtfs(gtfs_path, encoding = "UTF-8")

# route_id <- "026"; service_id <- c("U"); start_time <- "06:00:00"; end_time <- "08:00:00"
# route_id1 <- "240"; service_id1 <- c("159-155", "166-156"); start_time <- "06:00:00"; end_time <- "08:00:00"
# route_id <- "CPTM L11"; service_id <- c("USD", "U__"); start_time <- "06:00:00"; end_time <- "08:00:00"

calculate_route_frequency <- function(gtfs, route_id = NULL, service_id = NULL,
                                      start_time = "06:00:00", end_time = "08:00:00",
                                      mean_headway = FALSE) {

  env <- environment()

  # identify type of gtfs
  type_gtfs <- if (any(grepl(pattern = "frequencies", x = names(gtfs)))) {

    if(nrow(gtfs$frequencies) > 0) "frequencies" else "stop_times"

  } else "stop_times"

  # get trips
  trips <- gtfs$trips

  # filter trips by route
  trips <- if (!is.null(get("route_id", envir = env))) trips[route_id %in% get("route_id", envir = env)] else trips

  # filter trips by service
  trips <- if (!is.null(get("service_id", envir = env))) trips[service_id %in% get("service_id", envir = env)] else trips

  # add shape_id to trips if shapes is not available
  if (is.null(trips$shape_id)) trips[, shape_id := NA] else trips

  # get route info
  routes <- gtfs$routes[, .(route_id, route_long_name)]
  routes <- if (!is.null(get("route_id", envir = env))) routes[route_id %in% get("route_id", envir = env)] else routes


  if (type_gtfs == "frequencies") {

    frequencies <- gtfs$frequencies[trip_id %in% trips$trip_id]

    frequencies[, start_time := as.ITime(start_time)]
    frequencies[, end_time := as.ITime(end_time)]

    # filter only peak hours
    frequencies_filter <- frequencies[start_time >= as.ITime(get("start_time", envir = env))]
    frequencies_filter <- frequencies_filter[end_time < as.ITime(get("end_time", envir = env))]

    # bring route id and direction_id
    frequencies_filter <- merge(frequencies_filter, trips[, .(trip_id, route_id, direction_id, shape_id)],
                                sort = FALSE)
    # bring route info
    frequencies_filter <- merge(frequencies_filter, routes, by = "route_id", sort = FALSE)



    if (mean_headway) {
      # calculate mean headways
      headways <- frequencies_filter[, .(headway_mean = as.integer(mean(headway_secs/60, na.rm = TRUE))),
                                    by = .(route_id, direction_id, shape_id, route_long_name)]
    } else {

      frequencies_filter[, hour := data.table::hour(start_time)]
      headways <- frequencies_filter[, .(headway_mean = headway_secs/60),
                                           by = .(route_id, direction_id, shape_id, route_long_name, hour)]

    }

  } else {

    # get stoptimes
    stop_times <- gtfs$stop_times[trip_id %in% trips$trip_id]

    # bring route_id to stop_times
    stop_times <- merge(stop_times,
                        trips[, .(trip_id, route_id, direction_id, shape_id, service_id, trip_headsign)],
                        by = "trip_id",
                        sort = FALSE)


    stop_times[, arrival_time := as.ITime(arrival_time)]

    # get start of each trip
    stop_times_starts <- stop_times[, .(arrival_time = as.ITime(data.table::first(arrival_time)),
                                        n_stops = .N,
                                        ttime_trip = (last(arrival_time) - first(arrival_time))/60),
                                    by = .(service_id, route_id, trip_id, direction_id, shape_id, trip_headsign)]

    setorder(stop_times_starts, route_id, direction_id, shape_id, arrival_time)

    stop_times_starts_filter <- stop_times_starts[between(arrival_time, as.ITime(get("start_time", envir = env)), as.ITime(get("end_time", envir = env)))]

    stop_times_starts_filter[, headway := arrival_time - shift(arrival_time, type = "lag"),
                             by = .(service_id, route_id, direction_id, shape_id, trip_headsign)]

    stop_times_starts_filter[, headway := as.integer(headway) / 60]

    # bring route info
    stop_times_starts_filter <- merge(stop_times_starts_filter, routes, by = "route_id", sort = FALSE)

    if (mean_headway) {
      # calculate mean headway by direction
      headways <- stop_times_starts_filter[, .(headway_mean = as.integer(mean(headway, na.rm = TRUE))),
                                           by = .(route_id, direction_id, shape_id, route_long_name, trip_headsign)]
    } else {

      stop_times_starts_filter[, hour := data.table::hour(arrival_time)]
      headways <- stop_times_starts_filter[, .(headway_mean = as.integer(mean(headway, na.rm = TRUE))),
                                           by = .(route_id, direction_id, shape_id, route_long_name, hour, trip_headsign)]

    }

  }

  return(headways)

}
