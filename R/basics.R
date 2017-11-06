#' @importFrom httr GET
#' @importFrom RProtoBuf readProtoFiles
#' @importFrom dplyr mutate group_by summarize arrange
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
NULL



#' Register the api key.
#'
#' @param key
#'
#' @export
#'
register_nsw <- function(key) {
  # current options
  O <- getOption("tRainspotting")
  O$opendata_nsw <- key
  options(tRainspotting = O)
  invisible(NULL)
}

get_nsw_apikey <- function() {
  O <- getOption("tRainspotting")
  if (is.null(O$opendata_nsw)) {
    stop("No API key registered for nsw transport")
  }
  return(O$opendata_nsw)
}
#' Vehicle positions for NSW.
#'
#' @param vehicle_type
#' @param apikey
#' @description This function fetches position data from the NSW public transport
#' vehicle position API. An API key must be obtained by registering
#' at \url{https://opendata.transport.nsw.gov.au/}. The result is parsed with the RProtoBuf package using the
#' Google gtfs-realtime proto. The prototype compiler stuff is initialised at package
#' load. The result can be used to query trip updates as well as extract positions
#' @return RProtoBuf message
#' @export
nswVehicles <- function(vehicle_type = c("ferries",
                                         "sydneytrains",
                                         "buses",
                                         "lightrail",
                                         "nswtrains")) {
  apikey <- get_nsw_apikey()
  vehicle_type <- match.arg(vehicle_type)
  URL <- "https://api.transport.nsw.gov.au/v1/gtfs/vehiclepos/"
  URL <- paste0(URL, vehicle_type)

  data <- httr::GET(URL,
                    httr::add_headers(
                      Authorization = paste("apikey", apikey)))
  check_url_status(data)
  ## do the protobuf stuff
  buffer <- transit_realtime.FeedMessage$read(data$content)
  return(buffer)
}


check_url_status <- function(data) {
  if (data$status_code != 200) {
    msg <- paste("Failed GET - status", data$status_code)
    stop(msg)
  }
}


#' Get a data frame of vehicle positions from a RProtoBuf FeedMessage.
#'
#' @param M a FeedMessage, from nswVehicles, or equivalent
#'
#' @return a data frame, possibly with empty columns
#' @export
#'
getPosition <- function(M) {
  if (M@type !=  "transit_realtime.FeedMessage") {
    stop("Must be a FeedMessage\n")
  }
  m <- M$entity
  # check the size of returned objects and keep nonzero ones
  vlen <- sapply(m, function(X)length(X$vehicle))
  m <- m[vlen > 0]

  extracted <- lapply(m, function(G) {
    row <- c(as.list(G$vehicle$trip),
             as.list(G$vehicle$vehicle),
             as.list(G$vehicle$position))
    row <- as.data.frame(row, stringsAsFactors = FALSE)
    return(row)
  })
  extracted <- do.call(rbind, extracted)
  return(extracted)
}

#' get TripUpdates from a FeedMessage
#'
#' @param M
#'
#' @return dataframe containing messages
#' @export
#'
#' @examples
getTripUpdates <- function(M){
  if (M@type !=  "transit_realtime.FeedMessage") {
    stop("Must be a FeedMessage\n")
  }
  m <- M$entity
  # check the size of returned objects and keep nonzero ones
  vlen <- sapply(m, function(X)length(X$trip_update))
  m <- m[vlen > 0]
  return(m)
}

#' extract alerts from a FeedMessage
#'
#' @param M
#'
#' @return dataframe containing alerts
#' @export
#'
#' @examples
getAlerts <- function(M){
  if (M@type !=  "transit_realtime.FeedMessage") {
    stop("Must be a FeedMessage\n")
  }
  m <- M$entity
  # check the size of returned objects and keep nonzero ones
  vlen <- sapply(m, function(X)length(X$alert))
  m <- m[vlen > 0]
return(m)
}

#' Retrieve latest ferry position
#' @description Ferries have multiple entries in the live position feed. There is
#' a note about this on the FAQ. This function filters out the most recent entry
#' by pulling apart the trip_id column.
#' @param pos_DF a data frame created by getPositions
#'
#' @return a data frame with one row per vehicle
#' @export
#'
#' @examples
cleanFerries <- function(pos_DF) {
  pos_DF <- mutate(
    pos_DF,
    tripidLab = gsub("^([[:alpha:]]+)[[:digit:]]+-[[:digit:]]+$",
                     "\\1", trip_id),
    tripidA = as.numeric(gsub("^[[:alpha:]]+([[:digit:]]+)-[[:digit:]]+$",
                              "\\1", trip_id)),
    tripidB = as.numeric(gsub("^[[:alpha:]]+[[:digit:]]+-([[:digit:]]+)$",
                              "\\1", trip_id))
  )
  posbyvehicle <- nest(pos_DF, -id) # nolint
  posbyvehicle <- mutate(posbyvehicle,
                         data = purrr::map(data, ~dplyr::arrange(.x, dplyr::desc(tripidA))), # nolint
                         data = purrr::map(data, ~.x[1, ]))
  posbyvehicle <- unnest(posbyvehicle)
  return(posbyvehicle)
}


#' Fetch static gtfs route data from NSW transport.
#' @description GTFS data comes as a large zip file and can
#' be used to display all sorts of routes and stops. This function
#' downloads this data from the NSW transport site. The folder can
#' then be passed to the functions from the gtfsr package. The gtfsr
#' package can't be used directly because the transitfeed site doesn't
#' carry the NSW data anymore.
#' @return folder name containing the download
#' @export
#'
#' @examples
fetchNSWRoutes <- function() {
  apikey <- get_nsw_apikey()
  URL <- "https://api.transport.nsw.gov.au/v1/publictransport/timetables/complete/gtfs"
  td <- tempfile(fileext=".zip")

  data <- httr::GET(URL,
                    httr::add_headers(
                      Accept = "application/zip",
                      Authorization = paste("apikey", apikey)), httr::write_disk(td), httr::progress())
  check_url_status(data)
  zipoutdir <- file.path(tempdir(), "nsw_gtfs")
  unzip(td, exdir=zipoutdir)
  return(zipoutdir)
}
