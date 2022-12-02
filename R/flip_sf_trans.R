#' Flips a transect layout given as a simple 'sf' linestring.
#'
#' Transect layout flipped by changing the order of the nodes.
#'
#' @param tr_sf 'sf' linestring containing a transect layout.
#'
#' @details Note that number of nodes must be even for the flip to work.
#'
#' @keywords transect 
#' @return An flipped transect layout as an sf-linestring.
#' @export
#' @examples
#' \dontrun{
#'   library(geo)
#'   tibble::tribble(~y,~x,
#'     67, -23,
#'     68, -23, 68, -22,
#'     67, -22, 67, -21,
#'     68, -21, 68, -20,
#'     67, -20, 67, -19,
#'     68, -19, 68, -18,
#'     67, -18, 67, -17,
#'     68, -17, 68, -16,
#'     67, -16, 67, -15,
#'     68, -15, 68, -14,
#'     67, -14) %>%
#'  select(x,y) -> l
#'  l <- st_linestring(as.matrix(l))
#'  l %>% st_sfc(crs=4326) -> l
#'  plot(l,axes=TRUE,lwd=5,col="blue",lty=2)
#'  plot(flip_sf_trans(l),col="magenta",lwd=3,add=TRUE)
#' }

flip_sf_trans <- function(tr_sf) {
  mat <- sf::st_coordinates(tr_sf)
  n <- nrow(mat)
  if(n%%2!=0) stop("# of nodes must be even")
  id_diff <- c(1,-1,1,-1)
  ids <- 1:n
  ids <- ids + rep(id_diff,length.out=n)
  mat <- mat[ids,1:2] %>%
    sf::st_linestring() %>%
    sf::st_sfc(crs=4326)
}

