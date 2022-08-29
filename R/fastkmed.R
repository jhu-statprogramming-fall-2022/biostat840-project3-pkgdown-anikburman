#' Simple and fast k-medoid algorithm
#'
#' @description This function runs the simple and fast k-medoid algorithm
#' proposed by Park and Jun (2009).
#'
#' @param distdata A distance matrix (n x n) or dist object.
#' @param ncluster A number of clusters.
#' @param iterate A number of iterations for the clustering algorithm.
#' @param init A vector of initial objects as the cluster medoids
#' (see \strong{Details}).
#'
#' @details The simple and fast k-medoids, which sets a set of medoids as the
#' cluster centers, adapts the k-means algorithm for medoid up-dating.
#' The new medoids of each iteration are calculated in the within cluster
#' only such that it gains speed.
#'
#' \code{init = NULL} is required because the Park and Jun (2009) has
#' a particular method to select the initial medoids.  The initial medoids
#' are selected by
#' \deqn{ v_j = \sum_{i=1}^n \frac{d_{ij}}{\sum_{l=1}^n d_{il}},
#' \quad j = 1, 2, 3, \ldots, n }
#' where the first k of the \eqn{v_j} is selected if the number of
#' clusters is k.
#'
#' \code{init} can be provided with a vector of id objects. The length of
#' the vector has to be equal to the number of clusters. However, assigning
#' a vector in the \code{init} argument, the algorithm is no longer the simple
#' and fast k-medoids algorithm. The \code{\link{inckmed}} function,
#' for example, defines a different method to select the initial medoid
#' though it applies the \code{\link{fastkmed}} function.
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
#' @references Park, H., Jun, C., 2009. A simple and fast algorithm for
#' k-medoids clustering. Expert Systems with Applications 36, pp. 3336-3341.
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' result <- fastkmed(mrwdist, ncluster = 3, iterate = 50)
#' table(result$cluster, iris[,5])
#'
#'
#' @export

fastkmed <- function(distdata, ncluster, iterate = 10, init = NULL) {

  if(any(is.na(distdata))) stop("Cannot handle missing values!")

  if((is.matrix(distdata)||inherits(distdata, "dist"))==FALSE)
    stop("The distdata must be a matrix or a dist object!")

  if(is.matrix(distdata)==TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc!=nr) stop("The distdata is not an x n distance matrix!")
  }

  if(inherits(distdata, "dist")) distdata <- as.matrix(distdata)

  if(is.null(rownames(distdata))==TRUE) rownames(distdata) <- 1:nrow(distdata)

  n <- nrow(distdata)
  index <- 1:n

  if (is.null(init) == TRUE) {
    distsum <- colSums(distdata/sum(distdata))
    names(distsum) <- index
    iduniq <- as.numeric(names(distsum [!duplicated(distsum)]))
    distuniq <- unique(distsum)
    names(distuniq) <- iduniq
    sorted_object <- as.numeric(names(sort(distuniq)))
    medoid_init <- sorted_object[1:ncluster]
    if (sum(!is.na(medoid_init)) < ncluster)
      stop("An empty cluster is present, change the number of clusters or initial medoids")

  } else {
    if (ncluster != length(init))
      stop("The initial medoids must equal to the number of clusters")
    if (sum(is.na(init)) > 0)
      stop("The initial medoids consist of NA")

    medoid_init <- init
  }

  dist_0 <- distdata[,medoid_init, drop = FALSE]
  for (i in 1:ncluster) {
    dist_0[names(dist_0[,i]) == colnames(dist_0)[i], i] <- -1
  }
  member_0 <- apply(dist_0, 1, which.min)
  E0 <- sum(apply(dist_0, 1, min))

  if (length(unique(member_0)) != ncluster)
    stop("An empty cluster is present, change the number of clusters or initial medoids")

  row_0 <- row_1 <- medoid_0 <- medoid_1 <- numeric(ncluster)

  for (i in 1:ncluster) {
    row_0[i] <- which.min(rowSums(distdata[member_0==i,member_0==i, drop = FALSE]))
    medoid_0[i] <- index[member_0==i][row_0[i]]
  }

  dist_1 <- distdata[,medoid_0, drop = FALSE]
  for (i in 1:ncluster) {
    dist_1[names(dist_1[,i]) == colnames(dist_1)[i], i] <- -1
  }
  member_1 <- apply(dist_1, 1, which.min)
  E1 <- sum(apply(dist_1, 1, min))

  for (i in 1:ncluster) {
    row_1[i] <- which.min(rowSums(distdata[member_1==i,member_1==i, drop = FALSE]))
    medoid_1[i] <- index[member_1==i][row_1[i]]
  }

  if (sum(sort(medoid_0) != sort(medoid_1))!=0) {

    x <- 1
    medoid <- vector("list", iterate)
    E <- numeric(iterate)
    repeat {
      dist_0 <- dist_1
      member_0 <- member_1
      E0 <- E1
      row_0 <- row_1
      medoid_0 <- medoid_1

      dist_1 <- distdata[,medoid_0, drop = FALSE]
      for (i in 1:ncluster) {
        dist_1[names(dist_1[,i]) == colnames(dist_1)[i], i] <- -1
      }
      member_1 <- apply(dist_1, 1, which.min)

      for (i in 1:ncluster) {
        row_1[i] <- which.min(rowSums(distdata[member_1==i,member_1==i, drop = FALSE]))
        medoid_1[i] <- index[member_1==i][row_1[i]]
      }
      E1 <- sum(apply(dist_1, 1, min))

      medoid[[x]] <- medoid_1
      E[x] <- E1
      if (x == iterate || sum(sort(medoid_0) != sort(medoid_1))==0) { break }
      x <- x + 1

    }
    medoid <- do.call(rbind, medoid)
    E <- E[1:x]
    index_E <- which.min(E)
    medoid_2 <- medoid[index_E,]
    dist_2 <- distdata[,medoid_2, drop = FALSE]
    for (i in 1:ncluster) {
      dist_2[names(dist_2[,i]) == colnames(dist_2)[i], i] <- -1
    }
    member_2 <- apply(dist_2, 1, which.min)
    E2 <- sum(apply(dist_2, 1, min))

    names(member_2) <- rownames(distdata)

    result <- list(cluster = member_2, medoid = medoid_2, minimum_distance = E2)#,

  }
  else {
    dist_2 <- distdata[,medoid_1, drop = FALSE]
    member_1 <- apply(dist_2, 1, which.min)
    E2 <- sum(apply(dist_2, 1, min))
    result <- list(cluster = member_1, medoid = medoid_1, minimum_distance = E2)
  }
  return(result)
}
