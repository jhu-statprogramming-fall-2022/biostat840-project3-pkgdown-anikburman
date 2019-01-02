
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

stepinit <- function(distdata, ncluster, alpha = 1.1) {

  n <- nrow(distdata)
  if (ncluster > n)
    stop ("The number of cluster is bigger than the number of objects, reduce it!")
  index <- 1:n

  sorted_object <- order(unique(colSums(distdata/sum(distdata))))
  o1 <- sorted_object[1]
  sigma <- sum(distdata[,o1]-distdata[o1,o1])/(n-1)
  sigmai <- apply(distdata, 1, function (x) sum(x)/(n-1))
  sm <- index[sigmai <= sigma*alpha]
  distsm <- distdata[sm,sm,drop=FALSE]

  first1 <- sm[which.min(apply(distsm, 2, sum))]
  first2 <- sm[which.max(apply(distsm, 2, sum))]
  med1 <- c(first1, first2)
  med1uni <- unique(med1)
  if (length(med1uni) != 2)
    stop ("Increase the value of alpha!")

  begin <- 1
  canmedindex <- 2^(1:14)
  regindex <- 1:14
  regindex <- regindex[canmedindex >= n & canmedindex >= ncluster]
  last <- regindex[1]
  canmed <- vector("list", last)
  repeat {
    canmed[[begin]] <- med1
    member <- apply(distdata[ , canmed[[begin]], drop = FALSE], 1, which.min)
    sm1 <- medsave <- vector("list", length(canmed[[begin]]))
    for (i in 1:length(canmed[[begin]])) {
      sm1[[i]] <- index[member==i]
      medsave[[i]] <- sm1[[i]][which.max(distdata[sm1[[i]], canmed[[begin]][i]])]
    }
    med2 <- unlist(medsave)
    med1 <- c(canmed[[begin]], med2)
    if (begin == last) { break } else {
      begin <- begin + 1
    }
  }
  canmed <- canmed[[last]]
  ii <- length(canmed)/2
  canmed1 <- canmed[1:ii]
  canmed2 <- canmed[-c(1:ii)]
  distorder <- numeric(ii)
  for (i in 1:ii) {
    distorder[i] <- distdata[canmed1[i],canmed2[i]]
  }
  ordercanmed2 <- order(distorder, decreasing = TRUE)
  canmed2 <- canmed2[ordercanmed2]

  medoid_init <- c(canmed1, canmed2)[1:ncluster]
  return(medoid_init)
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
