#' Bootstrap function
#'
#' @param iter iterator
#' @param x sample length
#' @param fun the function you're using
#' @param alpha confidence interval bound
#' @param cx text size
#' @param ... allows for extra arguments to be passed to the hist function.
#' @importFrom graphics segments text
#' @importFrom stats quantile
#'
#' @return a list with xstat contained in it along with all other existing quantities
#' @export
#'
#' @examples myboot2(x=c(1,1,1,2,2,2,2,3,3,3,4,4), col="Green")
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  xstat = xstat[!is.na(xstat) & !is.nan(xstat)] ## remove NaN's from vector
  ci=quantile(xstat,c(alpha/2,1-alpha/2), na.rm = TRUE)# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
