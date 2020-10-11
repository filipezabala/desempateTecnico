#' Confidence interval in a pool with two candidates.
#' @param \code{x} Number of votes to candidate 1.
#' @param \code{n} Sample size.
#' @param \code{alpha} Significance level. Default: \code{alpha=0.05}.
#' @examples ic(217, 398, .05)
#' ic(181, 398, .05)
#' ic(170, 568, .05)
#' @export
ci_prop <- function(x,n,alpha=0.05){
  p <- x/n
  li <- round(p - qnorm(1-alpha/2)*sqrt(p*(1-p)/n), 3)
  ls <- round(p + qnorm(1-alpha/2)*sqrt(p*(1-p)/n), 3)
  cat('p_hat = ',round(p,4),', IC[ ', li,' ; ', ls,' ]',sep = '')
}