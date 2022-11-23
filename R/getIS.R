####function to get IS for full data set####
####y is classnames to be used########data is the data frame for data to be normalized
getIS<-function(y,data){
  ####removed log #######
  tmp<-t(data)
  classP_PC0<-tmp[,which(colnames(tmp)==y)]
  #classP_PC0<-tmp[,grep(y,colnames(tmp))]

}
