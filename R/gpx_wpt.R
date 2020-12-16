#' Writes a gpx-file of waypoints with symbols from OpenCPN vocabulary.
#'
#' @param file Name of gpx output file
#' @param lat,lon Latitude and longitude of waypoints.
#' @param name Name of waypoint, e.g 1 thru number of waypoints.
#' @param symbol Symbols e.g. from OpenCPN vocabulary (could be worked in).
#' @keywords gpx, lat, lon, waypoint
#' @keywords Writes a gpx-file of waypoints.
#' @export
#' @examples
#'   \dontrun{gpx_wpt(tibble(
#'      file="tmp_wpt.gpx",lat=c(65,66,67),lon=c(-20,-19,-18),name=1:3,
#'      symbol=c("Symbol-Glow-Small-Red","Symbol-Glow-Small-Blue",
#'	   "Symbol-Glow-Small-Orange")
#'   } 

gpx_wpt <- function(file,lat,lon,name,symbol){
  pre <- '<?xml version="1.0" encoding="utf-8" standalone="yes"?> <gpx version="1.1" creator="Birkir Bardarson" xmlns="http://www.topografix.com/GPX/1/1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">'
  post <- '</gpx>'
  sink(file=paste0(file))
  cat(paste(
    c(pre,
      mapply(function(a, b, c, d) {
        sprintf('<wpt lat="%f" lon="%f">
          <name>%s</name>
          <sym>%s</sym>
        </wpt>', a, b, c, d)
      }, lat, lon, name, symbol),
      post, "\n"),
    collapse="\n"))
  sink()
}
