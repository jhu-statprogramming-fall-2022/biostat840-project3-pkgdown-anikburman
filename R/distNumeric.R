#' A pair distance for numerical variables
#'
#' @description This function computes a pairwise numerical distance between
#' two numerical data sets.
#'
#' @param x A first data matrix (\emph{see} \strong{Details}).
#' @param y A second data matrix (\emph{see} \strong{Details}).
#' @param method A method to calculate the pairwise numerical distance
#' (\emph{see} \strong{Details}).
#' @param xyequal A logical if \code{x} is equal to \code{y} (\emph{see}
#' \strong{Details}).
#'
#' @details The \code{x} and \code{y} arguments have to be matrices with
#' the same number of columns where the row indicates the object and the
#' column is the variable. This function calculate all pairwise distance
#' between rows in the \code{x} and \code{y} matrices. Although it calculates
#' a pairwise distance between two data sets, the default function computes all
#' distances in the \code{x} matrix. If the \code{x} matrix is not equal to
#' the \code{y} matrix, the \code{xyequal} has to be set \code{FALSE}.
#'
#' The \code{method} available are \code{mrw} (Manhattan weighted by range),
#' \code{sev} (squared Euclidean weighted by variance), \code{ser}
#' (squared Euclidean weighted by range), \code{ser.2} (squared Euclidean
#' weighted by squared range) and \code{se} (squared Euclidean).
#' Their formulas are:
#' \deqn{mrw_{ij} = \sum_{r=1}^{p_n} \frac{|x_{ir} - x_{jr}|}{R_r}}
#' \deqn{sev_{ij} = \sum_{r=1}^{p_n} \frac{(x_{ir} - x_{jr})^2}{s_r^2}}
#' \deqn{ser_{ij} = \sum_{r=1}^{p_n} \frac{(x_{ir} - x_{jr})^2}{ R_r }}
#' \deqn{ser.2_{ij} = \sum_{r=1}^{p_n} \frac{(x_{ir} - x_{jr})^2}{ R_r^2 }}
#' \deqn{se_{ij} = \sum_{r=1}^{p_n} (x_{ir} - x_{jr})^2}
#' where \eqn{p_n} is the number of numerical variables, \eqn{R_r} is the range
#' of the \emph{r}-th variables, \eqn{s_r^2} is the variance of the \emph{r}-th
#' variables.
#'
#' @return Function returns a distance matrix with the number of rows equal to
#' the number of objects in the \code{x} matrix (\eqn{n_x}) and the number of
#' columns equals to the number of objects in the \code{y} matrix (\eqn{n_y}).
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @importFrom stats var
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' mrwdist[1:6,1:6]
#'
#' @export
distNumeric <- function(x, y, method = "mrw", xyequal = TRUE) {

  if((is.matrix(x)&&is.matrix(x))==FALSE)
    stop("x and y must be a matrix object!")

  if(ncol(x)!=ncol(y))
    stop(sQuote("x")," and ",sQuote("y"),
         " must have the same number of columns")

  if (xyequal == TRUE) {
    span <- apply(x, 2, function(x) max(x)-min(x))
    variance <- apply(x, 2, var)
  } else {
    span <- apply(rbind(x,y), 2, function(x) max(x)-min(x))
    variance <- apply(rbind(x,y), 2, var)
  }
  span.2 <- span^2

  num_distance <- c("mrw", "sev", "ser", "ser.2", "se")
  method <- match.arg(method, num_distance)
  result <- switch(method,
                mrw = weightedNum(x, y, p = 1, alpha = span),
                ser = weightedNum(x, y, p = 2, alpha = span),
                ser.2 = weightedNum(x, y, p = 2, alpha = span.2),
                sev = weightedNum(x, y, p = 2, alpha = variance),
                se = weightedNum(x, y, p = 2, alpha = 1))
  return(result)
}

