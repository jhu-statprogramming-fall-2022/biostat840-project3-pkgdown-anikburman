#' Step k-medoid algorithm from Yu et al.
#'
#' @description This function computes and returns the clustering result
#' computed by using a specified distance via Yu et al. algorithm.
#'
#' @param distdata A matrix of distance objects (n x n) or a diss class.
#' @param ncluster A number of cluster.
#' @param iterate A number of iteration for clustering algorithm.
#' @param alpha A numeric number to determine the range of initial medoids
#' selection.
#'
#' @details This is a k-medoids algorithm that has been proposed by Yu et al. The algorihm
#' has been claimed to be a remedy of simple and fast k-medoid. The medoids updating in this algorihm is similar to
#' simple and fast k-medoid.
#'
#' @return Function returns a partitioning clustering algorithm result consists of cluster
#' membership, cluster medoid, the minimum distance to the cluster medoid.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Yu, D., Liu, G., Guo, M., Liu, X., 2018. An improved K-medoids algorithm based on step increasing and optimizing medoids.
#' Expert Systems with Applications 92, 464-473.
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' result <- stepkmed(mrwdist, ncluster = 3, iterate = 50, alpha = 1.5)
#' table(result$cluster, iris[,5])
#'
#'
#' @export

stepkmed <- function(distdata, ncluster, iterate = 10, alpha = 1) {

  if(any(is.na(distdata))) stop("Cannot handle missing values!")

  if((is.matrix(distdata)||class(distdata)=="dist")==FALSE)
    stop("The distdata must be a matrix or a dist object!")

  if(is.matrix(distdata)==TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc!=nr) stop("The distdata is not an x n distance matrix!")
    }

  if(class(distdata)=="dist") distdata <- as.matrix(distdata)

  if(is.null(rownames(distdata))==TRUE) rownames(distdata) <- 1:nrow(distdata)

  medinit <- stepinit(distdata, ncluster, alpha = alpha)
  result <- fastkmed(distdata, ncluster, iterate = iterate, init = medinit)

  return(result)
}
