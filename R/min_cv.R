####find which IS gives min RSD#####
#' Title
#'
#' @param x :list of IS names with RSD
#'
#' @return name of IS name which gives min RSD for normalization
#' @export
#'
#' @examples
min_cv<-function(x){
  names(which.min(x))
}
