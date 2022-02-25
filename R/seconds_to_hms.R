seconds_to_hms <- function(seconds) {

  # extract hours
  hours_int <- as.integer(seconds/3600)

  # extract minutes
  minutes_int <- as.integer((seconds - (hours_int * 3600))/60)

  # extract seconds
  seconds_int <- as.integer(seconds - (hours_int * 3600 + minutes_int * 60))

  # format
  hours_format <- ifelse(nchar(hours_int) == 1, sprintf("0%s", hours_int), as.character(hours_int))
  minutes_format <- ifelse(nchar(minutes_int) == 1, sprintf("0%s", minutes_int), as.character(minutes_int))
  seconds_format <- ifelse(nchar(seconds_int) == 1, sprintf("0%s", seconds_int), as.character(seconds_int))

  # out
  final <- paste0(hours_format, ":", minutes_format, ":", seconds_format)

}

#' Convert from HH:MM:SS to seconds
#'@export
hms_to_sec <- function(x) {

  if (grepl(pattern = "\\d{2}:\\d{2}:\\d{2}", x = x)) {

    xlist <- strsplit(x,split=":")
    h <- as.numeric(sapply(xlist,"[",1))
    m <- as.numeric(sapply(xlist,"[",2))
    s <- as.numeric(sapply(xlist,"[",3))
    xdec <- h*3600+m*60+s
  } else xdec <- NA

  return(xdec)
}
