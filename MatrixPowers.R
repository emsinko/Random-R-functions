mocnina <- function(mat,x){
  A <- mat
  if(x == 0) return(diag(dim(A)[1]))
  else if(x == 1) return(mat) else
    for(i in 1:(x-1)){ 
      A <- A%*%mat
    }
  return(A)
}
