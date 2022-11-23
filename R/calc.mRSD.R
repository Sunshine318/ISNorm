###########function tol calculate RSD within and between batch
calc.mRSD<-function(data,batch=data.frame(1:nrow(data)),summary.range=seq(0,100,10),use="mean"){
  library(plyr)
  #bin summaries into range
  if(!is.factor(batch)){batch<-tryCatch(factor(batch[,],levels=unique(batch[,])),error=function(e){factor(batch,levels=unique(batch))}); message("Batch converted to factor.")}
  #main object
  tmp<-data.frame(batch=batch,data)
  # summary
  b.m<-ddply(tmp,.(batch),colwise(mean))
  b.s<-ddply(tmp,.(batch),colwise(sd))
  b.rsd<-abs(b.s/b.m*100)
  b.rsd[,1]<-b.m[,1]
  #generate summary objects for analytes between all batches
  analyte.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],2,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],2,sd,na.rm=T))
  colnames(analyte.RSD)[1]<-use# RSD for variables over all batches
  analyte.RSD.summary<-split.bins(obj=analyte.RSD[,1],bins=seq(0,100,10)) # summary for variables over all batches
  analyte.RSD.summary$percent<-round(analyte.RSD.summary$count/sum(analyte.RSD.summary$count)*100,1)
  #generate summary objects for batches based on all analytes
  within.batch.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],1,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],1,sd,na.rm=T))
  rownames(within.batch.RSD)<-b.rsd[,1]
  colnames(within.batch.RSD)[1]<-use
  within.batch.RSD.summary<-split.bins(na.omit(within.batch.RSD[,1]),bins=seq(0,100,10)) # ,max(within.batch.RSD[,1]
  within.batch.RSD.summary$percent<-round(within.batch.RSD.summary$count/sum(within.batch.RSD.summary$count)*100,1)
  #return summary
  list(batch.means=b.m,batch.sd=b.s,all.batch.RSD=b.rsd,variable.RSD=analyte.RSD,batch.RSD=within.batch.RSD,variable.RSD.summary=analyte.RSD.summary,batch.RSD.summary=within.batch.RSD.summary)
}
