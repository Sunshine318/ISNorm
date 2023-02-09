######cut bins function used in calc.mRSD####
#cut object on bins
#' Title: split object on bins
#'
#' @param obj :a vector
#' @param bins :a sequence
#'
#' @return list of listings
#' @export
#' @importFrom plyr ddply colwise .
#'
#' @examples
cut.bins<-function(obj,bins=seq(10,100,10)){
  library(plyr)
  interval<-cut(obj,bins,include.lowest = TRUE)
  tmp<-data.frame(count=obj,interval=interval)
  tmp<-ddply(tmp,.(interval),colwise(length))
  #need a mechanism to group values over largest bin####
  int<-flc(tmp$interval)
  int[is.na(int)]<-paste0(">",max(bins))
  tmp$interval<-int
  tmp
}
