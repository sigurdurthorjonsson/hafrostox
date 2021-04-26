#' Reads strata from StoX project json file.
#'
#' Simple approach of searching for strings before and after
#' geojson strata definition in a StoX (>3) project json file,
#' read directly with 'geojson_sf' into an simple features,'sf', object.
#' @param project Full path to a StoX project
#' @keywords strata, stratum, XML, StoX project
#' @return An sf-dataframe.
#' read_strata_from_project_json(
#'   paste(system.file("extdata", package="hafrostox"),
#'     "project.json", sep="/"))
read_strata_from_project_json_old <-
function (project)
{
  p <- scan(project, what="", sep="\n", strip.white=TRUE)
  p <- p[seq(stringr::str_which(p, '\\"StratumPolygon\\": \\{'),
    stringr::str_which(p, '\\"processName\\": \\"StratumArea\\",') - 4)]
  p[1] <- '{'
  geojsonsf::geojson_sf(paste0(p, collapse=" "))
}
