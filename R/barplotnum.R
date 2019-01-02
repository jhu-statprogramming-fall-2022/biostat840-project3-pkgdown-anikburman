#' Barplot of each cluster for numerical variables data set.
#'
#' @description This function create a barplot from a cluster result.
#'
#' @param dataori An original data set.
#' @param clust A vector of cluster membership.
#' @param nc A number of column of the plot.
#'
#'
#' @details This is a function to produce a barplot for each cluster.
#'
#' @return Function returns a barplot.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @importFrom stats t.test
#'
#' @examples
#' dat <- iris[,1:4]
#' memb <- cutree(hclust(dist(dat)),3)
#' barplotnum(dat, memb)
#' barplotnum(dat, memb, 2)
#'
#' @export

barplotnum <- function(dataori, clust, nc = 1) {

  if (length(clust) != nrow(dataori))
    stop("The vector of membership must have the same length with the number of objects!")


  nvar <- ncol(dataori)
  popmean <- apply(dataori, 2, mean)
  clustmem <- table(clust)
  propclust <- round(clustmem/sum(clustmem),2)*100
  nclust <- length(propclust)
  matmean <- ttest <- matrix(0, nclust, nvar)
  colnames(matmean) <- colnames(ttest) <- colnames(dataori)
  rownames(matmean) <- rownames(ttest) <-
    paste("Cluster ", 1:nclust, ": ", clustmem, " points (", propclust,"%)",
          sep = "")

  for (i in 1:nclust) {
    matmean[i,] <- apply(dataori[clust==i,], 2, mean)
    for (j in 1:nvar) {
      ttest[i,j] <- t.test(dataori[clust==i,j], mu = popmean[j])$p.value
    }
  }

  datalong <- expand.grid(1:nrow(matmean), 1:ncol(matmean))
  datalong$mean <- c(matmean)
  datalong$variable <- rep(colnames(matmean), each = nclust)
  datalong$cluster <- rep(rownames(matmean), nvar)
  datalong$mu <- rep(popmean, each = nclust)
  datalong$pval <- c(ttest)
  datalong$t.test <- rep("Not Significant", nvar*nclust)
  datalong$t.test[datalong$pval < 0.05] <- "Significant"
  datalong$Dot <- rep("Mean population", nvar*nclust)
  wt <- 12/nvar + 0.5
  plot1 <- ggplot(data = datalong, aes_string(x = "variable", y = "mean")) +
    geom_bar(stat = "identity", aes(fill = t.test), width = 0.7,
             position = position_stack(reverse = TRUE)) +
    geom_point(aes_string(y = "mu", colour = "Dot"), size = wt, na.rm = TRUE) +
    coord_flip() +
    guides(fill = guide_legend(title="Alpha 5%:"), colour=guide_legend(title="")) +
    scale_color_manual(values=c("black")) +
    scale_fill_manual(values=c("gray66", "gray25"))
  if (nc == 1) {
    plot2 <- plot1 + facet_grid(cluster~., scales="free_y", space="free_y")
  } else {
    plot2 <- plot1 +  facet_wrap( ~ cluster, ncol = nc)
  }
  return(plot2)
}
