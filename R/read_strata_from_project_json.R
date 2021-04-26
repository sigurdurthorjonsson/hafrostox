#' Reads strata from StoX json project files.
#'
#' Uses 'rjson' to read project.json (StoX > 3) and extract
#' geojson strata definition, then convert with 'geojsonsf::geojson_sf'
#' to a simple features,'sf', object.
#' @param proj_fn Path to and filname of a StoX project json-file
#' @keywords strata, stratum, XML, StoX project
#' @return An sf-dataframe.
#' @export
#' @examples
#' read_strata_from_project_json(
#'   paste(system.file("extdata", package="hafrostox"),
#'     "project.json", sep="/"))
read_strata_from_project_json <-
function (proj_fn)
{
  p <- rjson::fromJSON(file=proj_fn)
  st <- p$project$models$baseline[[1]]$processData$StratumPolygon
  st <- rjson::toJSON(st)
  geojsonsf::geojson_sf(st)
}
