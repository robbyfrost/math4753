#' @title myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a upper bound pf the lower tail
#'
#' @return Displays the curve, shaded area between the curve and x axis from -∞ to x=a, and calculates the area (probability, P(X<=a)) which is released to the command-line in a list.
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), col = "#840206", ylab = "Density")
  xcurve = seq(qnorm(0.00000001, mu, sigma), a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(qnorm(0.000001, mu ,sigma), xcurve, a), c(0, ycurve, 0), col = "#5EA7F3")
  area = pnorm(a, mu, sigma)
  arear = round(area, 4)
  text(a, dnorm(a, mu, sigma)/2, paste0("Area = ", arear))
  return(area)
}

