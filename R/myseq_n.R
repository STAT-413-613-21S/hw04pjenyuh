#' Get the nth element of a certain recursive sequence
#'
#' Element n is the sum of element n-1 and the difference between elements n-2 and n-3 divided by n
#' @param x a vector containing the first three numeric elements of this sequence
#' @param n a positive integer
#'
#' @return element n
#' @export myseq_n
#'
#' @examples myseq_n(x = c(2, 4, 3), n = 7)
myseq_n <- function(x, n){
# Error checking
  if (length(x) != 3) {
    stop('x must be of length 3') # Don't know when to Use stop(), message() or warning()
  }
  else if (!is.numeric(x) | !is.vector(x)) {
    stop('x must be a numeric vector')
  }
  else if (!is.numeric(n) | n != as.integer(n)) {
    warning('n must be an integer')
  }
  else if (n <= 0) {
    warning('n must be positive (> 0)')
  }
  myseq_vec <- vector("double", length = n)

  for(n in seq_along(myseq_vec)){
    if (n <= 3) {
      myseq_vec[n] <- x[n]
    }
    else {
      myseq_vec[n] <- myseq_vec[n-1] + (myseq_vec[n-3] - myseq_vec[n-2]) / n
    }
  }
  return(myseq_vec[n])
}

# Testing

myseq_n(x = c(2, 4, 3), n = 7)
