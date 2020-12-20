#' Splits transects by strata and writes gpx-files to a given directory.
#'
#' @param file Name of gpx output file
#' @param lat,lon Latitude and longitude of route-points.
#' @param name Name of route-point, e.g 1 thru number of rte-points.
#' @keywords gpx, lat, lon, route
#' @return Writes a gpx-file of a single route.
#' @export
#' @examples
#'   strata <- gpx2sf(
#'     paste(system.file("extdata",package="hafrostox"),"gpx",sep="/"))
#'   fn <- tempfile()
#'   sf2wkt(strata,fn)
#'   zz <- surveyPlanner(fn,type="RectEnclZZ", bearing=c("NE","W","N","E"),
#'     nmi=1000,knots=10,seed=1,equalEffort=TRUE,retour=TRUE)
#'   dn <- tempdir()
#'   dn
#'   trans2gpx(zz, dn)

trans2gpx <- function(surv,dir_name) {
  trans <- surv$Transect[,c("stratum","lat_start","lon_start","lat_stop","lon_stop")]
  trans <- split.data.frame(trans, trans$stratum)
  trans <- lapply(trans, function(x) {
    x %>% dplyr::select(lat=lat_start,lon=lon_start)
    })

  for (i in seq(along=trans)) {
    df <- trans[[i]]
    df$name <- 1:nrow(df)
    gpx_rte(file = paste(dir_name,paste0("trans_strata",i,".gpx"),sep="/"),
      lat=df$lat,lon=df$lon,name=df$name)
  }
}
