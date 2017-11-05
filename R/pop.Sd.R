#' A function to calculate the population standard deviation of a vector
#' 
#' @param x vector for which to calculate the population standard deviation
#' @return population standard deviation
#' @examples 
#' set.seed(1)
#' pop.SD(rnorm(100))
#' #compare to sample standard deviation
#' set.seed(1)
#' sd(rnorm(100))
#' @export

# calculates population standard deviation
pop.SD <- function(x) {
  sqrt(sum((x-mean(x))^2) / length(x))
}