
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
  index <- 1:n

  sorted_object <- order(unique(colSums(distdata/sum(distdata))))
  o1 <- sorted_object[1]
  sigma <- sum(distdata[,o1]-distdata[o1,o1])/(n-1)
  sigmai <- apply(distdata, 1, function (x) sum(x)/(n-1))
  sm <- index[sigmai < sigma*alpha]
  distsm <- distdata[sm,sm,drop=FALSE]

  first1 <- sm[which.min(apply(distsm, 2, sum))]
  first2 <- sm[which.max(apply(distsm, 2, sum))]
  med1 <- c(first1, first2)

  begin <- 1
  canmedindex <- 2^(1:14)
  regindex <- 1:14
  regindex <- regindex[canmedindex <= n & canmedindex >= ncluster]
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

