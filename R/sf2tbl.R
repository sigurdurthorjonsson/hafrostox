#' Converts a sf geometry object to a tibble with columns 'lat' and 'lon'.
#'
#' Deals with simple/single geometries only.
#'
#' @param sf sf geometry object.
#' @keywords geometry, tibble, lat, lon
#' @return A tibble with columns 'lat' and 'lon'.
#' @export
sf2tbl <- function(sf)
  sf %>% 
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    dplyr::select(lat=Y,lon=X)

