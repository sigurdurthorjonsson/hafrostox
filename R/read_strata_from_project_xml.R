#' Reads strata from StoX project XML file.
#'
#' With some XML-magic from Arne Johannes Holmin we get at a StoX-project's
#' strata boundaries and read them in as an sf-data.frame.
#' @param project Full path to and filename of a StoX project
#' @param names Optional list of names for the individual strata
#' @keywords strata, stratum, XML, StoX project
#' @return An sf-dataframe.
#' @export
#' @examples
#' \dontrun{
#' read_strata_from_project_xml(
#'   "~/workspace/stox/project/Test_Rstox/process/project.xml"  
#' }

read_strata_from_project_xml <- function(project, names = NULL) {
  XML::xmlParse(project) %>%
    XML::xmlToList() -> l
  l$processdata$stratumpolygon %>%
    lapply("[[", "text") -> l
  l[sapply(l, startsWith, "MULTIPOLYG")] %>%
    unlist() %>%
    sf::st_as_sfc() %>%
    sf::st_sf(ID = if(is.null(names)) 
        1:length(.) 
      else 
        names, crs = 4326) %>%
    dplyr::rename(geom = ".")
}
    
