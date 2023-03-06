
inverse_function <- function(x){
  X_I = cbind(x,diag(1,ncol(x)))
  n= dim(x)[1]
  for( j in 1:n){ 
    if (j == 1){
      X_I[j,] = X_I[j,]/X_I[j,j]
      for (k in 1:n){
        if (k !=j){
          X_I[k,]=X_I[k,] - (X_I[k,j]/X_I[j,j])*X_I[j,]
        }
        else{
          next
        }
      }
      
    }else if(j!=n) {
      X_I_J = X_I[-c(1:j-1),]
      X_I_J=X_I_J[order(X_I_J[,j],decreasing = T),]
      X_I = rbind(X_I[c(1:j-1),],X_I_J)
      X_I[j,] = X_I[j,]/X_I[j,j]
      for (k in 1:n){
        if (k !=j){
          X_I[k,]=X_I[k,] - (X_I[k,j]/X_I[j,j])*X_I[j,]
        }
        else{
          next
        }
      }
      
    }else {
      X_I[j,] = X_I[j,]/X_I[j,j]
      for (k in 1:n){
        if (k !=j){
          X_I[k,]=X_I[k,] - (X_I[k,j]/X_I[j,j])*X_I[j,]
        }
        else{
          next
        }
      }
    }
  }
  
  return(X_I[,(n+1):(2*n)])
}

a <- matrix(rnorm(25,0,2),ncol=5);a
round(solve(a),5)==round(inverse_function(a),5)
