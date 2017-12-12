#' A function to calculate a cumulative varianz for a given Vector. It calculates the
#' standard deviation from beginning of the vector to current position in vector
#'
#' @param v numeric or integer vector to calculate the cumulative varianz
#' @return Returns vector with length of input vector which contains cumulativ varianz to given point
#' @examples
#'  v <- c(1,4,5,7,7,1,2,NA,34,NA,NA,NA,2,4,76,8,9,2,2)
#'  y <- cumVar(v)
#'  y
#'  y[5]
#'  var(v[1:5])
#'  y[14]
#'  var(v[1:14])
#'
#' @export

cumVar <- function(v) {
  NAs <- 0
  M <- 0
  S <- 0
  N <- length(v)
  VAR <- NA
  res <- numeric()

  for (i in 1:N) {
    x <- v[i]
    k <- i - NAs
    if (!is.na(x)) {
      oldM <- M
      M <- M + (x - M)/k
      S <- S + (x - M)*(x - oldM)
      VAR <- S/(k - 1)
    }
    else {NAs <- NAs + 1}
    res[i] <- VAR
  }
  return(res)
}

