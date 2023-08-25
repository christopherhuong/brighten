edgeInclude <- function(x,mat, transpose = FALSE){
  n <- length(x)
  res <- 1/n * Reduce("+",lapply(x,function(x)getmatrix(x,mat)!=0))
  if (transpose){
    res <- t(res)
  }
  1000 * res
}


signInclude <- function(x,mat, transpose = FALSE, lowertri = FALSE){
  Pos <- lapply(x,function(x)getmatrix(x,mat)>0)
  inclPos <- 1/length(x) * Reduce("+",Pos)
  dfPos <- data.frame(
    from = c(row(inclPos)),
    to = c(col(inclPos)),
    incl = c(inclPos),
    type = "pos"
  )
  
  Neg <- lapply(x,function(x)getmatrix(x,mat)<0)
  inclNeg <- 1/length(x) * Reduce("+",Neg)
  dfNeg <- data.frame(
    from = c(row(inclNeg)),
    to = c(col(inclNeg)),
    incl = c(inclNeg),
    type = "neg"
  )
  
  tab <- rbind(dfPos,dfNeg)
  if (transpose){
    tab[,1:2] <- tab[,2:1]
  }
  
  if (lowertri){
    tab <- tab[tab$from >= tab$to,]
  }
  
  tab
}



compareResults <- function(est,real){
  cor0 <- function(x,y,...){
    if (sum(!is.na(x)) < 2 || sum(!is.na(y)) < 2 || sd(x,na.rm=TRUE)==0 | sd(y,na.rm=TRUE) == 0){
      return(0)
    } else {
      return(cor(x,y,...))
    }
  }
  
  bias <- function(x,y) mean(abs(x-y),na.rm=TRUE)
  
  # True positives:
  TruePos <- sum(est != 0 &  real != 0)
  
  # False pos:
  FalsePos <- sum(est != 0 & real == 0)
  
  # True Neg:
  TrueNeg <- sum(est == 0 & real == 0)
  
  # False Neg:
  FalseNeg <- sum(est == 0 & real != 0)
  
  out <- list()
  
  ### Sensitivity:
  out$sensitivity <- TruePos / (TruePos + FalseNeg)
  
  # Specificity:
  out$specificity <- TrueNeg / (TrueNeg + FalsePos)
  
  # Correlation:
  out$Correlation <- cor0(est,real)
  
  out$Bias <- bias(est,real)
  
  out$TruePos <- TruePos
  out$FalsePos <- FalsePos
  out$TrueNeg <- TrueNeg
  out$FalseNeg <- FalseNeg
  
  return(out)
}
