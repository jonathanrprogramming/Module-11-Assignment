tukey_multiple <- function(x) { 
  outliers <- array(TRUE,dim=dim(x)) 
  for (j in 1:ncol(x)) 
  { 
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j]) 
  } 
  outlier.vec <- vector(length=nrow(x)) 
  for (i in 1:nrow(x)) 
  { outlier.vec[i] <- all(outliers[i,]) } return(outlier.vec) }

debug(tukey_multiple <- function(x) { 
  outliers <- array(TRUE,dim=dim(x)) 
  for (j in 1:ncol(x)) 
  { 
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j]) 
  } 
  outlier.vec <- vector(length=nrow(x)) 
  for (i in 1:nrow(x)) 
  { outlier.vec[i] <- all(outliers[i,]) } return(outlier.vec) })

debug(for (i in 1:nrow(x))
debug({ outlier.vec[i] <- all(outliers[i]) })
debug(return(outlier.vec) }))
debug(outlier.vec))
debug(outliers(vector))

tukey_multiple <- function(x) { 
  outliers <- array(TRUE,dim=dim(x)) 
  for (j in 1:ncol(x)) 
  { 
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j]) 
  } 
  outlier.vec <- vector(length=nrow(x)) 
  for (i in 1:nrow(x)) 
  { outliers(vectors=i) <- all(outliers(vector=i) } return(outlier.vec) }
