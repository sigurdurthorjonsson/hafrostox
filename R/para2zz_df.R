#' Changes a parallel transect layout to a zigzag layout.
#'
#' Parallel layout changed to zigzag by averaging the position 
#' of transverse transect endpoints, which become the endpoints
#' of the sawtooths in the zigzag.
#'
#' @param tr_df Dataframe containing a parallel transect layout.
#'
#' @details The first and last nodes in the original layout are kept, 
#'   number of nodes must be even. Assuming a regular parallel layout 
#'   as input, the angle of the resulting sawtooths will be halved 
#'   at the start end end of the output zigzag.
#'
#' @keywords transect, parallel, zigzag
#' @return A zigzag transect layout as a dataframe.
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
#'  geo::geolines(df,col="blue",lwd=3)
#'  geo::geolines(para2zz_df(df),col="magenta",lwd=3)
#' }

para2zz_df <- function(tr_df) {
  n <- nrow(tr_df)
  if(n%%2!=0) stop("# of nodes must be even")
  h <-utils::head(tr_df,1)
  t <- utils::tail(tr_df,1)
  b <- tr_df[2:(n-1),]
  mat <- matrix(t(b),ncol=4,byrow=T)
  b <- data.frame(lat=apply(mat[,c(1,3)],1,mean),
    lon=apply(mat[,c(2,4)],1,mean))
  dplyr::bind_rows(h,b,t)
}

