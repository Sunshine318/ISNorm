#######batch values for test pools#######
listlipid<-function(classname,pool,Batch){
  temp.p<-t(pool)
  indata<-pool[-c(grep("IS",rownames(pool))),]
  #####separating the internal standard block####
  tmp.class<-temp.p[,which(colnames(temp.p)==classname)]
  #tmp.class<-temp.p[,grep(classname,colnames(temp.p))]
  tmp.norm<-t(apply(indata, 1, "/", tmp.class))
  RSD<-calc.mRSD(data.frame(t(tmp.norm)),as.factor(Batch))
  RSD$variable.RSD[,1]
}


