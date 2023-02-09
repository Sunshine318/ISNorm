#' Function for normalizing all data with given IS
#'
#' @param class :  IS name
#' @param data : dataframe of all data
#' @param listIS_vector :list for IS to be used with each lipid
#' @param ALLIS :dataframe containing all IS only
#'
#' @return: IS normalized data
#' @export
#'
#' @examples
NormalizedToIS<-function(class,data,listIS_vector,ALLIS){
  if(sum(listIS_vector==class)>=1)
  {
    b1b2Pos<-t(data)
    input<-b1b2Pos[,-c(grep("IS",colnames(b1b2Pos)))]
    #######returns normalized
    norm_isPCp<-input[,listIS_vector==class]*mean(as.numeric(ALLIS[[class]]))/as.numeric(ALLIS[[class]])
    if(sum(listIS_vector==class)==1){
      norm_isPCp<-data.frame(norm_isPCp,check.names=F)
      names(norm_isPCp)<-colnames(input)[listIS_vector==class]
    }
    return(norm_isPCp)
  }

}
