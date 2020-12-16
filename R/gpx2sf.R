#' Reads strata from StoX project files.
#'
#' With some XML-magic from Arne Johannes Holmin we get at a StoX-project's
#' strata boundaries and read them in as an sf-data.frame.
#' @param gpx_strata_dir Path to a directory with gpx-files of stratum boundaries
#' @details It is assumed a simple route demarcating an open boundary of a polygon
#'   is supplied, the function reads in the GPXs, closes the boundaries and
#'   converts to 'simple features'.
#' @keywords strata, stratum, XML, StoX project
#' @return An sf-dataframe.
#' @export
#' @examples
#' \dontrun{
#' 
#' 
#' }

gpx2sf <- function(gpx_strata_dir) {
  list.files(gpx_strata_dir,pattern="gpx",full.names=TRUE) %>%
    map(readGPX) %>%
    map(~.x$routes[[1]]) %>%
    lapply(., function(x) st_multipolygon(
      list(list(as.matrix(bind_rows(
        x[,c("lon","lat")],x[1,c("lon","lat")])))))) %>%
    st_as_sfc() %>%
    st_sf(ID = 1:length(.), crs = 4326) %>%
    rename(geom = ".")

}


