#' Centroid shadow value (CSV) index of each cluster based on medoid.
#'
#' @description This function create a centroid shadow value index using medoid
#' (instead of centroid) and its plot.
#'
#' @param distdata A distance object/ a n x n distance matrix.
#' @param idmedoid A vector of id medoids.
#' @param idcluster A vector of cluster membership.
#'
#'
#' @details This is a function to produce a centroid shadow value index
#' (using medoid) and its plot
#' for each cluster. The id medoids must match with the cluster
#' membership, for example, if the id medoids are 2, 25, and 57 (3 medoids),
#' the idcluster must have 3 unique memberships.
#'
#' @return Function returns a shadow value index and plot.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references F. Leisch. 2010 Neighborhood graphs, stripes and shadow plots
#' for cluster visualization. Statistics and Computing. vol. 20, pp. 457-469
#'
#' @examples
#' distiris <- as.matrix(dist(iris[,1:4]))
#' res <- fastkmed(distiris, 3)
#' sha <- shadow(distiris, res$medoid, res$cluster)
#' sha$result[c(1:3,70:75,101:103),]
#' sha$plot
#'
#' @export

shadow <- function(distdata, idmedoid, idcluster) {

  if((is.matrix(distdata)||class(distdata)=="dist")==FALSE)
    stop("The distdata must be a matrix or a dist object!")
  if(is.matrix(distdata)==TRUE) {
    nr <- nrow(distdata)
    nc <- ncol(distdata)
    if (nc!=nr) stop("The distdata is not an x n distance matrix!")
  }
  if(class(distdata)=="dist") distdata <- as.matrix(distdata)

  nclust <- length(unique(idcluster))

  if (length(idmedoid) != nclust)
    stop("The idmedoid and idcluster do not match, revised them!")
  if (length(idcluster) != nrow(distdata))
    stop("The vector of membership must have the same length with the number of objects!")

  shad <- vector("list", nclust)
  for (i in 1:nclust) {
    dista1 <- distdata[idcluster==i, idmedoid[i], drop = FALSE]
    distmedo <- distdata[idcluster==i, idmedoid, drop = FALSE]
    clclose <- apply(distmedo, 1, secondorder)
    dista2 <- diag(distmedo[,clclose, drop = FALSE])
    shad[[i]] <- 2*dista1/(dista1+dista2)
  }
  vecsha <- do.call(rbind, shad)
  ord <- order(as.numeric(rownames(vecsha)))
  result <- data.frame(shadval = vecsha[ord], cluster = idcluster)
  orderesult <- orderindex(result$shadval, result$cluster)
  plot1 <- plotsil(orderesult)
  return(list(result = result, plot = plot1))
}
