#' Title Calculates missing proportion in columns for each row
#'
#' @param the.matrix rows as features , columns as samples
#'
#' @return matrix with row count and proportion missing
#' @export
#'
#' @examples
count.NA<-function(the.matrix){
  result<-t(apply(the.matrix,1,function(x,n){
    m<-sum(is.na(x))
    c(m,m/n)
  },n=dim(the.matrix)[2]))
  dimnames(result)<-list(dimnames(the.matrix)[[1]],c("row count","proportion"))
  result
}
