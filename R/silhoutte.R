#' Silhoutte index of each cluster
#'
#' @description This function is deprecated, use the \code{\link{sil}}
#' function instead.
#'
#' @param distdata A distance object/ a n x n distance matrix.
#' @param idmedoid A vector of id medoids.
#' @param idcluster A vector of cluster membership.
#'
#' @export

silhoutte <- function(distdata, idmedoid, idcluster) {

  .Deprecated("sil")
  sil(distdata, idmedoid, idcluster)

}
