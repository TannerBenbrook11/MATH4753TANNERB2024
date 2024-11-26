#' Histogram of density sample mean
#'
#' @param n sample size
#' @param iter the iterator
#' @param a lower bound
#' @param b upper bound
#'
#' @return a graph that is a histogram
#' @export
#'
#' @importFrom graphics hist lines
#' @importFrom stats density dunif runif
#'
#' @examples mycltu(100, 1000, 0, 10)
mycltu=function(n,iter,a=0,b=10){
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)

  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")

  x <- seq(min(w), max(w), length.out = 100)
  lines(density(w),col="Blue",lwd=3)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
