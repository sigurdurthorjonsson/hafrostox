#' Converts a tibble with lat lon to sf geometry object.
#'
#' Returns simple geometries only, of type 'LINESTRING','MULTIPOINT' or 'POLYGON'.
#' @param tbl Tibble with columns 'lat' and 'lon'
#' @param geom Geometry type
#' @keywords geometry, tibble, lat, lon
#' @return sf-object of requested type.
#' @export

tbl2sf <- function(tbl,geom="LINESTRING") {
  if(!(geom %in% c("LINESTRING","MULTIPOINT","POLYGON"))) 
    stop("'geom' not a 'LINESTRING', 'MULTIPOINT' or a 'POLYGON'.")
  tbl %>% 
    dplyr::select(X=lon,Y=lat) %>%
    as.matrix() %>%
    sf::st_linestring() -> sf
    if(geom!="LINESTRING") sf::st_cast(sf,geom) -> sf
    sf::st_sfc(sf,crs=4326)
}
