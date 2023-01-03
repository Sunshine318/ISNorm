
#' Title : function to perform IS normalization on a data frame. It should contain IS in dataframe itself . The IS names start with word "IS"
#'
#' @param batch_file
#' @param className
#' @param Control_samples
#' @param sample_names
#' @param Batch
#'
#' @return matrix
#' @export (qdap::mgsub)
#'
#' @examples
ISnorm<-function(batch_file,className,Control_samples,sample_names,Batch){
  Pt.pool<-batch_file[,sample_names[grep(Control_samples,sample_names)]]
  test.poolp<-t(Pt.pool)
  colnames(test.poolp)<-rownames(Pt.pool)

  Bp<-Batch[grep(Control_samples,sample_names)]
  #Bp<-rep("B1",7)

  rsd<-sapply(className,listlipid,Pt.pool,as.factor(Bp))
  rownames(rsd)<-row.names(Pt.pool[-c(grep("IS",rownames(Pt.pool))),])
  ######list of IS to be used for each Lipid
  listIS<-apply(rsd,1,min_cv)
  ###separate all internal standards for all samples including test.pools####
  ALL_is<-sapply(className,getIS,batch_file,simplify=F)

  listIS1<-as.vector(listIS)
  all_norm<-sapply(className,NormalizedToIS,batch_file,listIS1,ALL_is)
  all_norm1<-all_norm[lengths(all_norm) != 0]


  #all_norm<-sapply(className,NormalizedToIS,batch_file,listIS1,ALL_is)
  #all_norm<-all_norm[-c(2,4,6,15,17,18,20,21)]
  classdf<-do.call(cbind.data.frame,all_norm1)
  cl<-className
  cl1<-paste0(cl,".")
  cl2<-rep("",length(cl1))
  library(qdap)
  colnames(classdf) <- qdap::mgsub(cl1,cl2,colnames(classdf),fixed=TRUE)
  return(classdf)
}

#colnames(Pos_norm)<-colnames(Pos_norm)




