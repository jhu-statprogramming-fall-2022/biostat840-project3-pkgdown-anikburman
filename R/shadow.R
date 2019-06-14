#' Centroid shadow value (CSV) index of each cluster based on medoid
#'
#' @description This function is deprecated, use the \code{\link{csv}}
#' function instead.
#'
#' @param distdata A distance object/ a n x n distance matrix.
#' @param idmedoid A vector of id medoids.
#' @param idcluster A vector of cluster membership.
#'
#' @export

shadow <- function(distdata, idmedoid, idcluster) {

  .Deprecated("csv")
  csv(distdata, idmedoid, idcluster)

}
