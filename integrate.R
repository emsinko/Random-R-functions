# Vypocet integralu pomocou trapezium metody

integral_trapez <- function(a,b,n,f){
  
  #######################################################################################
  # <a,b> je interval na ktorom integrujeme                                             #
  #  n    je pocet lichobeznikov, ktorych obsah spocitavame                             #
  #  f    funkcia, ktoru integrujeme. Piseme napr. v tvare f = function(x) x^2 - sin(x) #
  #######################################################################################
  
  delta_x <- (b-a)/n
  x <- seq(from = a, to = b, by = delta_x)
  if(length(f(x)) == 1) return(delta_x / 2  * (2*f(x)*(n+1) - f(a) - f(b)))  #
  else return(delta_x / 2  * (2*sum(f(x)) - f(a) - f(b)))
}

integral_trapez(3, 4, 100, f = function(x) 2)
  

