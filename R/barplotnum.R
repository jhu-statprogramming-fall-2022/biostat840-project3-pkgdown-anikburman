#' Barplot of each cluster for numerical variables data set
#'
#' @description This function creates a barplot from a cluster result. A barplot
#' indicates the location and dispersion of each cluster. The x-axis
#' of the barplot is variable's mean, while the y-axis is the variable's name.
#'
#' @param dataori An original data set.
#' @param clust A vector of cluster membership (see \strong{Details}).
#' @param nc A number of columns for the plot of all cluster
#' (see \strong{Details}).
#' @param alpha A numeric number to set the significant level (between 0 and 0.2).
#'
#'
#' @details This is a marked barplot because some markers are added, i.e.
#' a significant test, a population mean for each (numerical) variable.
#' The significance test applies t-test between the population's mean and
#' cluster's mean in every variable. The alpha is set in between 0 to 20\%.
#' If the population mean differs to the cluster's mean, the bar shade in the
#' barplot also differs.
#'
#' \code{clust} is a vector with the length equal to the number of objects
#' (n), or the function will be an error otherwise. \code{nc}
#' controls the layout (grid) of the plot. If \code{nc = 1}, the plot of each
#' cluster is placed in a column. When the number of clusters is 6 and
#' \code{nc = 2}, for example, the plot has a layout of 3-row and 2-column grids.
#'
#' @return Function returns a barplot.
#'
#' @author Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
#'
#' @references Leisch, F. (2008). Handbook of Data Visualization, Chapter
#' Visualizing cluster analysis and finite mixture models, pp. 561-587.
#' Springer Handbooks of Computational Statistics. Springer Verlag.
#' @references Dolnicar, S. and F. Leisch (2014). Using graphical statistics to
#' better understand market segmentation solutions. International Journal of
#' Market Research 56, 207-230.
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

barplotnum <- function(dataori, clust, nc = 1, alpha = 0.05) {

  if (length(clust) != nrow(dataori))
    stop("The vector of membership must have the same length with the number of objects!")

  if (alpha <= 0 | alpha > 0.2)
    stop("alpha has to be in between 0 and 0.2")

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
  datalong$t.test[datalong$pval < alpha] <- "Significant"
  datalong$Dot <- rep("Mean population", nvar*nclust)
  wt <- 12/nvar + 0.5
  plot1 <- ggplot(data = datalong, aes_string(x = "variable", y = "mean")) +
    geom_bar(stat = "identity", aes(fill = t.test), width = 0.7,
             position = position_stack(reverse = TRUE)) +
    geom_point(aes_string(y = "mu", colour = "Dot"), size = wt, na.rm = TRUE) +
    coord_flip() +
    guides(fill = guide_legend(title=paste("Alpha ", alpha*100, "%:", sep = "")),
           colour=guide_legend(title="")) +
    scale_color_manual(values=c("black")) +
    scale_fill_manual(values=c("gray66", "gray25"))
  if (nc == 1) {
    plot2 <- plot1 + facet_grid(cluster~., scales="free_y", space="free_y")
  } else {
    plot2 <- plot1 +  facet_wrap( ~ cluster, ncol = nc)
  }
  return(plot2)
}
