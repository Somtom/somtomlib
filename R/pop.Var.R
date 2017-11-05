#' A function to calculate the population variance of a vector
#' 
#' @param x vector for which to calculate the population variance
#' @return population variance
#' @examples 
#' set.seed(1)
#' pop.Var(rnorm(100))
#' #compared to sample variance:
#' set.seed(1)
#' var(rnorm(100))
#' @export

# calculates population varience
pop.Var <- function(x) {
  sum((x-mean(x))^2) / length(x)
}
