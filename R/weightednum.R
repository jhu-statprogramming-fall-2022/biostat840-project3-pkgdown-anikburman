
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

