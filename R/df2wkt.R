#' Writes a data.frame as 'well-known-text' or 'wkt'. 
#'
#' Takes a data.frame/tibble with coordinates / stratum boundary and
#' writes them out as 'well-know-text' or wkt-file. Assumes open polygon
#' i.e. first line in df is appended before writing out, as the result is
#' to be used as input to 'Rstox::surveyPlanner' or as step in 
#' the wrapper 'hafrostox::srv_plnr'.
#'
#' @param df Stratum boundary for a single stratum as a tibble/data.frame with cols 'lat' and 'lon'
#' @param fn Filename to store wkt representation in.
#' @keywords surveyPlanner, stratum, parameters
#' @return A wkt-text-file written out.
#' @export
#' @examples
#' \dontrun{
#' 
#' tibble::tribble(~name, ~lat, ~lon,
#'   "NW1",  63.56, -20.57, 	  	  #1
#'   "NE", 63.44, -20.11,		  #2
#'   "SE", 63.31, -20.27,		  #3
#'   "SW", 63.28, -20.65,		  #4
#'   "NW2", 63.49,-20.64) -> stratum 	  #5
#' 
#' df2wkt(stratum, "stratum_wkt.txt")
#' }

df2wkt <- function(df, fn="tmp_wkt.txt")
  write(
    paste0("1\tMULTIPOLYGON (((",
      df[,c("lon","lat")] |>
      dplyr::bind_rows(df[1,c("lon","lat")]) |>
      tidyr::unite(col=coord,sep=" ") |>
      dplyr::pull(coord) |>
      paste(collapse=", "),
      ")))"),
    sep="\t", ncolumns=1,file=fn)
