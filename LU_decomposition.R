
a <- matrix(rnorm(25,0,5),ncol=5);a
library(matrixcalc)
LUD1 <- lu.decomposition(a)
LU_function <- function(x){

 
  n = dim(x)[1]
  L_inverse = diag(1,n)
  for (i in 1:(n-1)){
    for(j in (i+1):n){
      I = diag(1,n)
      I[j,]=I[j,] -x[j,i]/x[i,i]*I[i,]
      x[j,]=x[j,] -x[j,i]/x[i,i]*x[i,]
      L_inverse = L_inverse%*%I
      
    }
    
  }
  L = 2*diag(1,n) - L_inverse
  U = x
  return(list(L=L,U=U,A = L%*%U))
}
LUD2<-LU_function(a)
identical(LUD1$L,LUD2$L)
identical(LUD1$U,LUD2$U)
