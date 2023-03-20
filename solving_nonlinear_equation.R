#Bisection method
bi_method <- function(f,x0,x1,eps=10^(-5)){
  err_set = NULL
  if (f(x0) * f(x1) > 0){
    print('No root in the interval')
    return(NULL)
  }
  else{
    x2 = (x0 + x1)/2
    while(abs(f(x2))>eps){
      if(f(x0) * f(x2) < 0){
        x1 = x2
      }
      else{
        x0 = x2
      }
      x2 = (x0+x1)/2
      err = abs(f(x2))
      err_set = c(err_set,err)
   
    }
  }
  return(list(solution=x2,num_repeat = length(err_set) ,errors = round(err_set,4)))
}
error_Bisection = bi_method(my_fun1,4,6)$errors

Bisection <- function(f,x0,x1,eps = 10^(-5)){
  if(f(x0) * f(x1) >0 ){
    print('wrong initial interval!')
    return(NULL)
  }
  N = 0; err = x1 - x0;errors = NULL
  while(err>eps & N <= 1000){
    x2 = (x0+x1)/2
    if(f(x0)*f(x2) <0 ){
      x1=x2
    }
    else{
      x0=x2
    }
    err = x1 - x0
    N = N + 1
    errors = c(errors, err)
  }
  return(list(solution= x2,num_repeat= N,errors = round(errors,4)))
}


#linear interpolation method
interpolation <- function(f,x0,x1,eps = 10^(-5)){
  f_x0 = f(x0)
  f_x1 = f(x1)
  if(f_x0*f_x1>0){
    print('wrong interval')
  }
  err = x1 - x0; n=0;err_set=NULL
  while (err>eps & n <1000) {
    x2 = (f_x1*x0 - f_x0*x1)/(f_x1-f_x0)
    f_x2 = f(x2)
    if (f(x0) * f(x2) <0){
      x1 = x2
    }
    else{
      x0 = x2
    }
    n = n + 1
    err = abs(x1 - x0)
    err_set = c(err_set,err)
    
  }
  return(list(solution= x2,num_repeat = n, errors = round(err_set,4)))
}
error_interpolation = interpolation(my_fun1,4,6)$errors

interpolation2 <- function(f,x0,x1,eps = 10^(-5)){
  f_x0 = f(x0)
  f_x1 = f(x1)
  if(f_x0*f_x1>0){
    print('wrong interval')
  }
  x2 = (f_x1*x0 - f_x0*x1)/(f_x1-f_x0)
  err_set = NULL
  while (abs(f(x2))>eps) {
    if (f(x0) * f(x2) <0){
      x1 = x2
    }
    else{
      x0 = x2
    }
    x2 = (f_x1*x0 - f_x0*x1)/(f_x1-f_x0)
    err = abs(f(x2))
    err_set = c(err_set,err)
  }
  return(list(solution=x2, num_repeat = length(err_set), errors = round(err_set,4)))
}


##Newton's method
Newton <- function(f,x,eps = 10^(-5)){
  h= 1e-08
  err_set = NULL
  while(abs(f(x))>eps){
    x0 = x
    x=x-f(x)/((f(x+h)-f(x))/h)
    err = abs(x-x0)
    err_set = c(err_set,err)
  }
  return(list(solution= x, num_repeat = length(err_set),errors = round(err_set,4)))
}
error_Newton<-Newton(my_fun1,10)$errors
Newton2 <- function(f,x,eps = 10^(-5)){
  h= 1e-08
  N = 0; err = 10; err_set = NULL
  while(err>eps & N < 1000){
    x0 = x
    x=x-f(x)/((f(x+h)-f(x))/h)
    err = abs(x-x0)
    err_set = c(err_set,err)
    N = N+1
  }
  return(list(solution= x,num_repeat =N, errors = round(err_set,4)))
}
error_Newton = Newton(my_fun1,3)$errors

my_fun1 <- function(x){x^6 -6.7*x^5+8.8*x^4-6.7*x^3+8.8*x^2-6.7*x+7.8}

#plot
x_val = seq(-1,6,by =0.001)
y_val = my_fun1(x_val)

plot(x_val,y_val,type = 'l')
abline(h=0,col='blue',lty =2)
points(1.5,0,pch=19)
points(5.2,0,pch=19)

#error 비교
plot(error_interpolation,type='b', cex =1.5 ,cex.lab =2,xlim = c(1,20))
lines(error_Bisetion,type='b', col= 'red',lty = 2)
lines(error_Newton, type = 'b', col ='blue',lty =3)


legend("topright", legend = c("Bisection", "Linear Interpolation", "Newton Method"),
       col = c("black", "red", "blue"), lty = 1:3, cex = 1.5)
