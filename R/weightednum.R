
weightedNum <- function(x, y, p = 2, alpha = 1) {
  if(ncol(x)!=ncol(y))
    stop(sQuote("x")," and ",sQuote("y"),
         " must have the same number of columns")

  x <- data.matrix(x)
  y <- data.matrix(y)
  z <- matrix(0, nrow=nrow(x), ncol=nrow(y))
  for(i in 1:nrow(y)){
    z[,i] <- colSums(1/ alpha * abs(t(x) - y[i,])^p, na.rm = TRUE)
  }
  rownames(z) <- rownames(x)
  colnames(z) <- rownames(y)
  return(z)
}

secondorder <- function(x) {
  idx <- order(x)
  z <- idx[2]
  return(z)
}
orderindex <- function(value, member) {
  tabk <- table(member)
  k <- length(tabk)
  divdata <- vector("list", k)
  for (i in 1:k) {
    divdata[[i]] <- matrix(0, tabk[i], 4)
    val <- sort(subset(value, member == i))
    divdata[[i]][,1] <- val
    divdata[[i]][,2] <- 1:tabk[i]
    me <- mean(val)
    divdata[[i]][,3] <- rep(me, tabk[i])
    divdata[[i]][,4] <- rep(i, tabk[i])
  }
  final <- do.call(rbind, divdata)
  colnames(final) <- c("value", "object", "mean", "cluster")
  final <- as.data.frame(final)
  return(final)
}
plotsil <- function(ord, tit = "") {
  ggplot(data = ord, aes_string(x = "object", y = "value")) +
    geom_line(aes(y = mean), colour = "red", alpha = 0.5) +
    geom_area(fill = "red", alpha = 0.5) +
    scale_x_reverse() +
    ggtitle(tit) +
    facet_grid( ~ cluster, scales = "free_x", space = "free") +
    ylim(min(ord$value, 0),1) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(face="bold", size=16, hjust=0.5)
    )
}
fastclust <- function(x, nclust) {
  res <- fastkmed(x, nclust, iterate = 50)
  return(res$cluster)
}
