#' Simple and fast k-medoid algorithm from Park and Jun.
#'
#' @description This function computes and returns the clustering result
#' computed by using a specified distance via Park and Jun's algorithm.
#'
#' @param distdata A matrix of distance objects (n x n) or a diss class.
#' @param ncluster A number of cluster.
#' @param iterate A number of iteration for clustering algorithm.
#' @param init An index of the initial medoids.
#'
#' @details This is a k-medoids algorithm that has been proposed by Park and Jun. The algorihm
#' has been claimed to be fast and simple. The medoids updating in this algorihm is similar to
#' kmeans centroid updating.
#'
#' @return Function returns a partitioning clustering algorithm result consists of cluster
#' membership, cluster medoid, the minimum distance to the cluster medoid.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Park, H., Jun, C., 2009. A simple and fast algorithm for k-medoids clustering.
#' Expert Systems with Applications 36, 3336-3341.
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

  if((is.matrix(distdata)||class(distdata)=="dist")==FALSE)
    stop("The distdata must be a matrix or a dist object!")

  if(is.matrix(distdata)==TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc!=nr) stop("The distdata is not an x n distance matrix!")
    }

  if(class(distdata)=="dist") distdata <- as.matrix(distdata)

  if(is.null(rownames(distdata))==TRUE) rownames(distdata) <- 1:nrow(distdata)

  n <- nrow(distdata)
  index <- 1:n
  if (is.null(init) == TRUE) {
    sorted_object <- order(unique(colSums(distdata/sum(distdata))))
    medoid_init <- sorted_object[1:ncluster]
  } else {
    if (ncluster != length(init)) stop("The initial medoids must equal to the number of clusters")
    medoid_init <- init
  }


  dist_0 <- distdata[,medoid_init]
  member_0 <- apply(dist_0, 1, which.min)
  E0 <- sum(apply(dist_0, 1, min))

  row_0 <- row_1 <- medoid_0 <- medoid_1 <- numeric(ncluster)

  for (i in 1:ncluster) {
    row_0[i] <- which.min(rowSums(distdata[member_0==i,member_0==i, drop = FALSE]))
    medoid_0[i] <- index[member_0==i][row_0[i]]
  }

  dist_1 <- distdata[,medoid_0]
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

    dist_1 <- distdata[,medoid_0]
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
  dist_2 <- distdata[,medoid_2]
  member_2 <- apply(dist_2, 1, which.min)
  E2 <- sum(apply(dist_2, 1, min))

  #totss <- sum(distdata[sorted_object[1],])
  #betweenss <- totss-E2
  names(member_2) <- rownames(distdata)
  #medoid <- distdata[medoid_2,]

  result <- list(cluster = member_2, medoid = medoid_2, minimum_distance = E2)#,
  #total_var = paste(round(betweenss/totss, 4)*100, "%" ))
  }
  else {
    dist_2 <- distdata[,medoid_1]
    member_1 <- apply(dist_2, 1, which.min)
    E2 <- sum(apply(dist_2, 1, min))
    result <- list(cluster = member_1, medoid = medoid_1, minimum_distance = E2)
  }
  return(result)
}
