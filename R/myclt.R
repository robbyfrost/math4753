#' myclt
#'
#' @param n sample size n
#' @param iter number of iterations
#' @param a
#' @param b
#'
#' @return Prints a histogram of the distribution of the sum of uniforms
#' @export
#'
#' @examples
#' \dontrun{myclt(n = 10, iter = 10000)}

myclt = function(n,iter,a=0,b=5){
  y = runif(n*iter,a,b)
  data = matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm = apply(data,2,sum)
  h = hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
}
