P <- 55000       # principal
r <- 1.2 / 100 / 12   # rate p.m.
n <-  20*12         # number of payments

A <- P*( (r*(1+r)^n)/((1+r)^n-1))  
print(A)
library(tidyverse)

splatka <- function(P = 55000,r = 1.2,n = 20){
  r <- r/100/12
  n <- 12*n
  A <- P*( (r*(1+r)^n)/((1+r)^n-1))
  return(A)
}

priebeh_istiny <- function(P = 55000,r = 1.2,n = 20){
  r <- r/100/12
  n <- 12*n
  
  A <- P*( (r*(1+r)^n)/((1+r)^n-1))
  
  istina <- c(P)
  for(i in 1:n)  istina[i+1] <- istina[i]*(1+r) - A
  
  print(glue::glue("Úver vo vıške: {P}€ \n Mesaèná splátka je: {round(A,2)}€ \n Poèet splátok: {n} \n Preplatok: {round(n*A-P,2)}€"))
  data.frame(splatka = 0:n,istina) %>% 
    ggplot(aes(x = splatka, y = istina)) + geom_col() + theme_bw()
  
}

priebeh_istiny(P = 55000,r = 1.2,n = 20)



priebeh_istiny_mimoriadna <- function(P = 55000, r = 1.2, n = 30, A_mimoriadna = 75*12){ 
  r <- r/100/12
  n <- 12*n
  A <- P*( (r*(1+r)^n)/((1+r)^n-1))
  
  istina <- c(P)
  i <- 1
  pocet_mimoriadnych <<- 0 
  while(min(istina) > 10){
    if(i %% 12 == 0 & i > 0){
      istina[i+1] <- istina[i]*(1+r) - A - A_mimoriadna
      pocet_mimoriadnych <<- pocet_mimoriadnych + 1 
    }
    else{
      istina[i+1] <- istina[i]*(1+r) - A
    }
    i <- i + 1
    #print(pocet_mimoriadnych)
  }
  #print(i)
  print(glue::glue("Úver vo vıške: {P}€ \n Mesaèná splátka je: {round(A,2)}€ \n Poèet splátok: {i-1} \n Preplatok: {round((i-1)*A + pocet_mimoriadnych*A_mimoriadna-P,2)}€"))
  data.frame(splatka = 0:(i-1),istina) %>% 
    ggplot(aes(x = splatka, y = istina)) + geom_area() + theme_bw()
}

priebeh_istiny_mimoriadna(P = 55000, r = 1.2, n = 30, A_mimoriadna = 85.88*12)


splatka(P = 55000, r = 1.2, n = 20) - splatka(P = 55000, r = 1.2, n = 30)

priebeh_istiny(P = 55000,r = 1.2,n = 20)
priebeh_istiny(P = 55000,r = 1.2,n = 30)
