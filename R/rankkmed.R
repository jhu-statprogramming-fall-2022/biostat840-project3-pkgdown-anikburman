#' Rank k-medoid algorithm from Zadegan et. al.
#'
#' @description This function computes and returns the clustering result
#' computed by using a specified distance via rank k-medoids algorithm.
#'
#' @param distdata A matrix of distance objects (n x n) or a diss class.
#' @param ncluster A number of cluster.
#' @param m A number of objects to compute hostility.
#' @param iterate A number of iteration for clustering algorithm.
#' @param initial A vector of initial objects as the cluster medoids.
#'
#' @details This is a k-medoids algorithm that has been proposed by Zadegan et. al. The algorihm
#' has been claimed to be suitable for large dataset. The medoids updating in this algorihm is
#' similar to kmeans centroid updating.
#'
#' @return Function returns a partitioning clustering algorithm result consists of cluster
#' membership, cluster medoid, the minimum distance to the cluster medoid.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Zadegan, S.M.R, Mirzaie M, and Sadoughi, F. 2013. Ranked k-medoids: A fast and
#' accurate rank-based partitioning algorithm for clustering large datasets. Knowledge-Based
#' Systems 39, 133-143.
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' result <- fastkmed(mrwdist, ncluster = 3, iterate = 50)
#' table(result$cluster, iris[,5])
#'
#'
#' @export

rankkmed <- function (distdata, ncluster, m = 3, iterate = 10, initial = NULL) {

  if (any(is.na(distdata)))
    stop("Cannot handle missing values!")
  if ((is.matrix(distdata) || class(distdata) == "dist") ==
      FALSE)
    stop("The distdata must be a matrix or a dist object!")
  if (is.matrix(distdata) == TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc != nr)
      stop("The distdata is not an x n distance matrix!")
  }
  if (class(distdata) == "dist")
    distdata <- as.matrix(distdata)
  n <- nrow(distdata)
  if (m < 2 || m > n) stop(paste("Give paramater m between 2 and", n))

  originrow <- rownames(distdata)
  rownames(distdata) <- 1:n

  R <- distdata
  for (i in 1:n) {
    R[i,] <- rank(distdata[i,])
  }
  sortedindex <- distdata
  for (i in 1:n) {
    sortedindex[i,] <- order(distdata[i,])
  }
  if(is.null(initial)) {
    medoid_init <- sort(sample(1:n, ncluster))
  } else {
    if(length(unique(initial)) < ncluster) stop(paste("Initial medoids must be", ncluster,
                                                      "unique objects."))
    medoid_init <- initial
  }

  iter <- 1
  medsave <- groupsave <- vector("list", iterate)
  group <- hostile <- lmedoid <- vector("list", ncluster)
  evalmed <- numeric(ncluster)

  repeat {
    for (i in 1:ncluster) {
      group[[i]] <- sortedindex[medoid_init[i], 1:m]
      hostile[[i]] <- apply(R[group[[i]],group[[i]], drop = FALSE], 1, sum)
      lmedoid[[i]] <- as.numeric(names(which.max(hostile[[i]])))
    }
    for (i in 1:ncluster) {
      evalmed[i] <- sum(c(sapply(group, function(x) x==lmedoid[[i]])))
    }
    medoid_0 <- c(unlist(lmedoid)[evalmed==1], unique(unlist(lmedoid)[evalmed>1]))
    groupsave[[iter]] <- unlist(group)
    lsample <- unlist(groupsave)
    if (length(medoid_0)!=ncluster) {
      id2 <- c(1:n)[-lsample]
      medoid_1 <- c(medoid_0, sample(id2, ncluster-length(medoid_0)))
      medoid_1 <- sort(medoid_1)
    } else {
      medoid_1 <- sort(medoid_0)
    }
    medsave[[iter]] <- medoid_1
    if (iter == iterate) { break }
    iter <- iter + 1
    medoid_init <- medoid_1

  }
  finmedoid <- medoid_1
  member <- apply(R[,medoid_1], 1, which.min)
  dist_2 <- distdata[,finmedoid]
  E <- sum(apply(dist_2, 1, min))

  if (is.null(originrow)) {
    id.med <- finmedoid
  } else {
    id.med <- originrow[finmedoid]
  }

  result <- list(cluster = member, medoid = id.med, minimum_distance = E)
  return(result)
}
