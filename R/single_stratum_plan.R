#' For a given data.frame of stratum boundaries and
#' a set of parameters in a named list set up a survey
#' plan with 'Rstox::surveyPlanner'.
#'
#' @param stratum Stratum boundary for a single stratum a tibble/data.frame with cols 'lat' and 'lon'
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

single_stratum_plan <- function(stratum, ...) {
  fn <- tempfile("st",fileext=".gpx")
  geo::frame2gpx(stratum,fn)
  strata  <- gpx2sf(dirname(fn))
  unlink(fn)

  fn <- tempfile("wkt",fileext=".txt")
  sf2wkt(strata,fn)

  plan <- Rstox::surveyPlanner(project=fn,
    ...)
  unlink(fn)
  plan
}
