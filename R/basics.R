#' @importFrom httr GET
#' @importFrom RProtoBuf readProtoFiles
#' @export
nswVehicles <- function(vehicle_type=c("ferries", "sydneytrains"), apikey)
{
  vehicle_type <- match.arg(vehicle_type)
  URL <- "https://api.transport.nsw.gov.au/v1/gtfs/vehiclepos/"
  URL <- paste0(URL, vehicle_type)

  data <- httr::GET(url, add_headers( Authorization=paste("apikey", apikey)))
  check_url_status(data)
  ## do the protobuf stuff
  buffer <- transit_realtime.FeedMessage$read(data$content)
  return(buffer)
}


check_url_status <- function(data)
{
  if (!data$status_code != 200) {
    msg <- paste("GET status", data$status_code)
    stop(msg)
  }
}
