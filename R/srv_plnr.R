#' For a given data.frame of stratum boundaries and
#' a set of parameters in a named list set up a survey
#' plan with 'Rstox::surveyPlanner'.
#'
#' @param stratum_df Stratum boundary for a single stratum a tibble/data.frame with cols 'lat' and 'lon'
#' @param ... Further arguments to 'Rstox::surveyPlanner'.
#' @keywords surveyPlanner, stratum, parameters
#' @return A survey plan created with 'Rstox::surveyPlanner'.
#' @export
#' @examples
#' \dontrun{
#' strata <- gpx2sf(
#'   paste(system.file("extdata", package="hafrostox"), "gpx", sep="/"))
#' sf2wkt(strata, "strata_wkt.txt")
#' }

srv_plnr <- function(stratum_df, ...) {
  file_name<- tempfile("wkt",fileext=".txt")
  df2wkt(stratum_df,fn=file_name)

  plan <- Rstox::surveyPlanner(project=file_name,
    ...)
  unlink(file_name)
  plan
}
