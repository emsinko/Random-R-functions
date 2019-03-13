mocnina <- function(mat,power){
  A <- mat
  if(power == 0) return(diag(dim(A)[1]))
  else if(power == 1) return(mat) else
    for(i in 1:(power-1)){ 
      A <- A %*% mat
    }
  return(A)
}
