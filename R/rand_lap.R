
#' Random Laplace
#'
#' @param n number of observations returned
#' @param bpar scale parameter of Laplace
#'
#' @return n random laplace variable
#' @export
#'
#' @examples rand_lap(20, 10)
#'

rand_lap <- function(n, bpar){
  rexp(n = n, rate = 1/bpar) * sample(c(-1, 1), n, TRUE)
}
