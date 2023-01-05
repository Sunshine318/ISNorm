######split bins function used in calc.mRSD####
#split object on bins
#' Title
#'
#' @param obj
#' @param bins
#'
#' @return
#' @export
#' @importFrom plyr ddply colwise
#'
#' @examples
split.bins<-function(obj,bins=seq(10,100,10)){
  library(plyr)
  interval<-cut(obj,bins,include.lowest = TRUE)
  tmp<-data.frame(count=obj,interval=interval)
  tmp<-ddply(tmp,.(interval),colwise(length))
  #need a mechanism to group values over largest bin####
  int<-fixlc(tmp$interval)
  int[is.na(int)]<-paste0(">",max(bins))
  tmp$interval<-int
  tmp
}
