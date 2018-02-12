#' A Bootstrap replications for clustering alorithm
#'
#' @description This function do bootstrap replications for a cluster algorithm.
#'
#'
#' @param distdata A matrix of distance objects (n x n) or a diss class.
#' @param nclust The number of clusters
#' @param algorithm Any clustering algorithm function with the end result being only membership.
#' @param nboot The number of bootstrap replicates
#' @param diss A logical if the distdata is a dist or matrix object.
#'
#' @details This is a function to obtain bootstrap evaluation for a cluster. The cluster matrix
#' can be further analyzed. In the algorithm function, the input arguments are only a distance/ matrix
#' and a number of cluster.The output is only the membership.
#'
#' @return Function returns a bootstrap cluster matrix (n x number of bootstrap replicates).
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' parkboot <- function(x, nclust) {
#' res <- fastkmed(x, nclust, iterate = 50)
#' return(res$cluster)
#' }
#' irisboot <- clustboot(mrwdist, nclust=3, parkboot, nboot=7)
#' head(irisboot)
#'
#'
#' @export

clustboot <- function(distdata, nclust=2, algorithm, nboot=25, diss = TRUE) {

  if(any(is.na(distdata))) stop("Cannot handle missing values!")

  if((is.matrix(distdata)||class(distdata)=="dist")==FALSE)
    stop("The distdata must be a matrix or a dist object!")

  matboot <- matrix(0, nrow = nrow(distdata), ncol = nboot)
  algorithm <- match.fun(algorithm)
  for (i in 1:nboot) {
    idx <- sample(1:nrow(distdata), nrow(distdata), replace = TRUE)
    idx <- sort(unique(idx))
    if(diss == TRUE) {
      bootclust <- algorithm(distdata[idx,idx], nclust = nclust)
    } else {
      bootclust <- algorithm(distdata[idx,], nclust = nclust)
    }
    matboot[idx,i] <- bootclust
  }
  return(matboot)
}
