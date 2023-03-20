##Newton's method
Newton <- function(f,x,eps = 10^(-5)){
  h= 1e-08
  
  while(abs(f(x))>eps){
    x=x-f(x)/((f(x+h)-f(x))/h)
  }
  return(x)
}
Newton2 <- function(f,x,eps = 10^(-5)){
  h= 1e-08
  N = 0 err = 10; err_set = NULL
  while(err>eps & N < 1000){
    x0 = x
    x=x-f(x)/((f(x+h)-f(x))/h)
    err = abs(x-x0)
    err_set = c(err_set,err)
    N = N+1
  }
  return(list(solution= x,num_repeat =N, errors = round(err_set,4))
}
Newton(my_fun1,-3)
my_fun1 <- function(x){x^6 -6.7*x^5+8.8*x^4-6.7*x^3+8.8*x^2-6.7*x+7.8}
