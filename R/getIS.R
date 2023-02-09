####function to get IS for full data set####
####y is classnames to be used########data is the data frame for data to be normalized
#' Title
#'
#' @param y IS name
#' @param data  dataframe to be normalized and has IS data in it for all samples
#'
#' @return extract IS data from data.frame
#' @export
#'
#' @examples
getIS<-function(y,data){
  ####removed log #######
  tmp<-t(data)
  classP_PC0<-tmp[,which(colnames(tmp)==y)]
  #classP_PC0<-tmp[,grep(y,colnames(tmp))]

# Insert roxygen ----------------------------------------------------------


}
