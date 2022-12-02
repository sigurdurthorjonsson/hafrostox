#' Flips a transect layout given as a dataframe.
#'
#' Transect layout flipped by changing the order of the nodes.
#'
#' @param tr_df Dataframe containing a transect layout.
#'
#' @details Note that number of nodes must be even for the flip to work.
#'
#' @keywords transect
#' @return An flipped transect layout as a dataframe.
#' @export
#' @examples
#' \dontrun{
#'   library(geo)
#'   tibble::tribble(~lat,~lon,
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
#'     67, -14) -> df
#'  geo::geoplot(df,type="n",grid=F,xlim=list(lat=c(66,68.25),lon=c(-24,-13)))
#'  geo::gbplot(c(500,1000),lty=2)
#'  geo::geolines(df,col="blue",lwd=5,lty=2)
#'  geo::geolines(flip_df_trans(df),col="magenta",lwd=3)
#' }

flip_df_trans <- function(tr_df) {
  n <- nrow(tr_df)
  if(n%%2!=0) stop("# of nodes must be even")
  id_diff <- c(1,-1,1,-1)
  ids <- 1:n
  ids <- ids + rep(id_diff,length.out=n)
  tr_df[ids,]
}

