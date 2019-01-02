#' A pair distance for binary/ categorical variables.
#'
#' @description This function computes and returns the distance matrix computed by using
#' the simple matrching distance.
#'
#' @param x A data frame/ matrix.
#' @param y A second data frame/ matrix.
#'
#' @details This is a function to compute simple matching distance.
#' It returns a matrix of distance objects, i.e n x n.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @examples
#' set.seed(1)
#' a <- matrix(sample(1:2, 7*3, replace = TRUE), 7, 3)
#' matching(a, a)
#'
#' @export
matching <- function(x, y) {

  if(ncol(x)!=ncol(y))
    stop(sQuote("x")," and ",sQuote("y"),
         " must have the same number of columns")

  x <- data.matrix(x)
  y <- data.matrix(y)
  z <- matrix(0, nrow=nrow(x), ncol=nrow(y))
  for(i in 1:nrow(y)){
    z[,i] <- colSums(1/ ((t(x) - y[i,])/(t(x) - y[i,])), na.rm = TRUE)/ncol(x)
  }
  rownames(z) <- rownames(x)
  colnames(z) <- rownames(y)
  return(z)
}
