#' Simple k-medoid algorithm
#'
#' @description This function runs the simple k-medoid algorithm
#' proposed by Budiaji and Leisch (2019).
#'
#' @param distdata A distance matrix (\emph{n x n}) or \emph{dist} object.
#' @param ncluster A number of clusters.
#' @param seeding A number of seedings to run the algorithm
#' (\emph{see} \strong{Details}).
#' @param iterate A number of iterations for each seeding
#' (\emph{see} \strong{Details}).
#'
#' @details The simple k-medoids, which sets a set of medoids as the
#' cluster centers, adapts the simple and fast k-medoid algoritm.
#' The best practice to run the simple and fast k-medoid is by running
#' the algorithm several times with different random seeding options.
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
#' @references W. Budiaji, and F. Leisch. 2019. Simple K-Medoids Partitioning
#' Algorithm for Mixed Variable Data. Algorithms Vol 12(9) 177
#'
#' @examples
#' num <- as.matrix(iris[,1:4])
#' mrwdist <- distNumeric(num, num, method = "mrw")
#' result <- skm(mrwdist, ncluster = 3, seeding = 50)
#' table(result$cluster, iris[,5])
#'
#'
#' @export

skm <- function(distdata, ncluster, seeding = 20, iterate = 10) {

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
  colnames(distdata) <- rownames(distdata) <- 1:n

  distsum <- colSums(distdata/sum(distdata))
  names(distsum) <- index
  iduniq <- as.numeric(names(distsum [!duplicated(distsum)]))
  distuniq <- unique(distsum)
  names(distuniq) <- iduniq
  sorted_object <- as.numeric(names(sort(distuniq)))
  init1 <- sorted_object[1]
  noninit1 <- sorted_object[-1]

  z  <- vector("list", seeding)
  for (i in 1:seeding) {
    medinit <- c(init1, sample(noninit1, ncluster-1))
    z[[i]] <- fastkmed(distdata, ncluster, iterate, init = medinit)
  }

  min_dist <- numeric(seeding)
  for (i in 1:seeding) {
    min_dist[i] <- z[[i]]$minimum_distance
  }
  result <- z[[which.min(min_dist)]]
  return(result)

}
