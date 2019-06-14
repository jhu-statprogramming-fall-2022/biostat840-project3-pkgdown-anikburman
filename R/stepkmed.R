#' Step k-medoid algorithm from Yu et al.
#'
#' @description This function is deprecated, use the \code{\link{inckmed}}
#' function instead.
#'
#' @param distdata A matrix of distance objects (n x n) or a diss class.
#' @param ncluster A number of clusters.
#' @param iterate A number of iterations for the clustering algorithm.
#' @param alpha A numeric number to determine the range of initial medoids
#' selection.
#'
#' @export

stepkmed <- function(distdata, ncluster, iterate = 10, alpha = 1) {

  .Deprecated("stepkmed")
  inckmed(distdata, ncluster, iterate, alpha)

}
