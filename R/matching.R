#' A pair distance for binary/ categorical variables
#'
#' @description This function computes the simple matching distance from
#' two data frames/ matrices.
#'
#' @param x A first data frame or matrix (\emph{see} \strong{Details}).
#' @param y A second data frame or matrix (\emph{see} \strong{Details}).
#'
#' @details The \code{x} and \code{y} arguments have to be data frames/
#' matrices with the same number of columns where the row indicates the object
#' and the column is the variable. This function calculates all pairwise
#' distance between rows in the \code{x} and \code{y} data frames/ matrices.
#' If the \code{x} data frame/ matrix is equal to the \code{y} data frame/
#' matrix, it explicitly calculates all distances in the \code{x} data frame/
#' matrix.
#'
#' The simple matching distance between objects \emph{i} and \emph{j} is
#' calculated by
#' \deqn{d_{ij} = \frac{\sum_{s=1}^{P}(x_{is}-x_{js})}{P}}
#' where  \eqn{P} is the number of variables, and \eqn{ x_{is}-x_{js} \in}
#' \{0, 1\}. \eqn{ x_{is}-x_{js} = 0}, if \eqn{ x_{is}=x_{js}} and
#' \eqn{x_{is}-x_{js} = 1}, when \eqn{x_{is} \neq x_{js}}.
#'
#' As an example, the distance between objects 1 and 2 is presented.
#'
#' \tabular{lrrr}{
#' object \tab x \tab y \tab z \cr
#' 1 \tab 1 \tab 2 \tab 2 \cr
#' 2 \tab 1 \tab 2 \tab 1
#' }
#'
#' The distance between objects 1 and 2 is
#' \deqn{d_{12} = \frac{\sum_{s=1}^{3}(x_{is}-x_{js})}{3} = \frac{0 + 0 + 1}{3} =
#' \frac{1}{3} = 0.33}
#'
#' @return Function returns a distance matrix with the number of rows equal to
#' the number of objects in the \code{x} data frame/ matrix (\eqn{n_x}) and
#' the number of columns equals to the number of objects in the \code{y}
#' data frame/ matrix (\eqn{n_y}).
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
