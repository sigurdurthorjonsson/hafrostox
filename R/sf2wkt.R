#' Writes e.g. stratum boundaries as simple feature geometries 
#' out to well know text files.
#'
#' Takes a sf-data.frame and writes out a text representation 
#' of the geometries.
#' @param sfdf Simple features dataframe
#' @param fn Filename to write out to
#' @keywords sf-data.frame, well known text 
#' @return A text file representation of the geometries
#' @export
#' @examples
#' \dontrun{
#' strata <- gpx2sf(
#'   paste(system.file("extdata", package="hafrostox"), "gpx", sep="/"))
#' sf2wkt(strata, "strata_wkt.txt")
#' }

sf2wkt <- function(sfdf, fn="tmp_wkt.txt")
  write(paste(sfdf$ID, sf::st_as_text(sfdf$geom),
    sep="\t"), ncolumns=1, file=fn)
