#' Rank k-medoid algorithm
#'
#' @description This function runs the rank k-medoids algorithm proposed by
#' Zadegan et. al. (2013).
#'
#' @param distdata A distance matrix (n x n) or dist object.
#' @param ncluster A number of clusters.
#' @param m A number of objects to compute hostility (see
#' \strong{Details}).
#' @param iterate A number of iterations for the clustering algorithm.
#' @param init A vector of initial objects as the cluster medoids
#' (see \strong{Details}).
#'
#' @details This algorithm is claimed to cope with the local optima problem
#' of the simple and fast-kmedoids algorithm (\code{\link{fastkmed}}). The
#' \code{m} argument is defined by the user and has to be \eqn{1 < m \leq n}.
#' The \code{m} is a hostility measure computed by
#' \deqn{m_i = \sum_{X_j \in Y} r_{ij}}
#' where \eqn{x_j} is the object j, Y is the set of objects
#' as many as m, and \eqn{r_{ij}} is the rank distance, i.e. sorted
#' distance, between object i and j.
#'
#' \code{init} can be provided with a vector of id objects. The length of
#' the vector has to be equal to the number of clusters. However, assigning
#' a vector in the \code{init} argument, the algorithm is no longer the rank
#' k-medoids algorithm.
#'
#' @return Function returns a list of components:
#'
#' \code{cluster} is the clustering memberships result.
#'
#' \code{medoid} is the id medoids.
#'
#' \code{minimum_distance} is the distance of all objects to their cluster
#' medoid.
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
#' result <- rankkmed(mrwdist, ncluster = 3, iterate = 50)
#' table(result$cluster, iris[,5])
#'
#'
#' @export

rankkmed <- function (distdata, ncluster, m = 3, iterate = 10, init = NULL) {

  if (any(is.na(distdata)))
    stop("Cannot handle missing values!")
  if ((is.matrix(distdata) || inherits(distdata, "dist")) ==
      FALSE)
    stop("The distdata must be a matrix or a dist object!")
  if (is.matrix(distdata) == TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc != nr)
      stop("The distdata is not an x n distance matrix!")
  }
  if (inherits(distdata, "dist"))
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
  if(is.null(init)) {
    medoid_init <- sort(sample(1:n, ncluster))
  } else {
    if(length(unique(init)) < ncluster) stop(paste("Initial medoids must be", ncluster,
                                                      "unique objects."))
    medoid_init <- init
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
    lsample <- unlist(groupsave[[iter]])
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
  dist_2 <- distdata[,finmedoid, drop = FALSE]
  E <- sum(apply(dist_2, 1, min))

  if (is.null(originrow)) {
    id.med <- finmedoid
  } else {
    id.med <- originrow[finmedoid]
  }

  result <- list(cluster = member, medoid = id.med, minimum_distance = E)
  return(result)
}
