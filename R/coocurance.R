#' A co-occurrence distance for binary/ categorical variables data.
#'
#' @description This function is deprecated, use cooccur function instead.
#'
#' @param data A matrix or data frame of binary/ categorical variables. The values of matrix are
#' integer, i.e 1, 2, 3, ....
#'
#' @details This is a function to compute a co-occurence distance.
#' It returns a matrix of distance objects , i.e n x n.
#'
#' @return A matrix of distance from binary/ categorical variable.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Harikumar, S., PV, S., 2015. K-medoid clustering for heterogeneous data sets.
#' JProcedia Computer Science 70, 226-237.
#'
#' @examples
#' set.seed(1)
#' a <- matrix(sample(1:2, 7*3, replace = TRUE), 7, 3)
#' cooccur(a)
#'
#' @export
coocurance <- function(data) {
  .Deprecated("cooccur")
  cooccur(data)
}


