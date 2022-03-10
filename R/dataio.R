#' Create data folder structure
#'
#' @export

create_data_structure <- function() {

  # get project name
  project_name <- basename(rstudioapi::getActiveProject())

  # create data-raw
  dir.create(sprintf("../../data-raw/%s", project_name))

  # create data
  dir.create(sprintf("../../data/%s", project_name))

}




#' Read data based on my structure
#'
#' @param file Filename
#' @param type Type. data or data-raw?
#'
#' @export

read_data <- function(file, folder = "data", ...) {

  # get project name
  project_name <- basename(rstudioapi::getActiveProject())

  # make path
  path <- sprintf("../../%s/%s/%s", type, project_name, file)

  # open
  if (tools::file_ext(path) == "csv") {

    a <- fread(path)

  } else if (tools::file_ext(path) == "rds") {

    a <- readRDS(path)

  } else if (tools::file_ext(path) %in% c("gpkg", "shp")) {

    a <- st_read(path, ...)

  }

  return(a)

}




#' Write data based on my structure
#'
#' @param data Data to be written to disk
#' @param file Filename
#'
#' @export
write_data <- function(data, file) {

  # get project name
  project_name <- basename(rstudioapi::getActiveProject())

  # make path
  path <- sprintf("../../data/%s/%s", project_name, file)

  # se a pasta nao existir, cria-la
  if (!dir.exists(dirname(path))) dir.create(dirname(path))

  # open
  if (tools::file_ext(path) == "csv") {

    fwrite(data, path)

  } else if (tools::file_ext(path) == "rds") {

    saveRDS(data, path, compress = FALSE)

  } else if (tools::file_ext(path) %in% c("gpkg", "shp")) {

    st_write(data, path)

  }

  return(path)

}




