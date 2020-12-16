#' Writes a gpx-file of a single, simple route.
#'
#' @param file Name of gpx output file
#' @param lat,lon Latitude and longitude of route-points.
#' @param name Name of route-point, e.g 1 thru number of rte-points.
#' @keywords gpx, lat, lon, route
#' @return Writes a gpx-file of a single route.
#' @export
#' @examples
#'   \dontrun{gpx_rte(tibble(
#'      file="tmp_rte.gpx",lat=c(65,66,67),lon=c(-20,-19,-18),name=1:3)
#'   } 

gpx_rte <- function(file,lat,lon,name){
  pre <- '<?xml version="1.0" encoding="utf-8" standalone="yes"?> <gpx version="1.1" creator="Siggi" xmlns="http://www.topografix.com/GPX/1/1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">\n  <rte>'
  post <- '  </rte>\n</gpx>'
  sink(file=paste0(file))
  cat(paste(
    c(pre,
      mapply(function(a, b, c) {
        sprintf('<rtept lat="%f" lon="%f">
          <name>%s</name>
        </rtept>', a, b, c)
      }, lat, lon, name),
      post, "\n"),
    collapse="\n"))
  sink()
}

