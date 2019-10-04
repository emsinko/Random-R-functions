### Vzorec:
'https://en.wikipedia.org/wiki/Equated_monthly_installment'

#Nainstaluj si balik dplyr:
install.packages("dplyr")
library(dplyr)

# Predpoklady pre interval (a,b) --> riesenie lezi v tomto intervale
bisection_method <- function(f,a,b){
  if( f(a)*f(b) < 0){
    while(abs(a-b) > 0.00001){
      c <- (a+b)/2
      if(f(c) == 0) return(c)
      if(f(a)*f(c) < 0){
        b <- c
      }
      else{
        a <- c 
      }
    }
    return((a+b)/2)
  } 
  else{
    return("Koren je mimo intervalu (a,b)")
  }
}

# Testovanie funkcie:
bisection_method(f = function(x) x^2+x-6, a= 1, b= 5)  # tato funkcia ma koren = -3 a 2 


##########
### Vypocet urokovej sadzby p.a. 
##########

find_rate <- function(n,istina,splatka){
  f <- function(r) return(istina * ( ((r/12)*(1+r/12)^n)/((1+r/12)^n - 1) ) - splatka)
  if(splatka*n == istina) return(0)
  round(bisection_method(f = f, a = 0.00000000001, b = 1),5)
}


# Testovanie na kalkulacke
find_rate(n = 240 ,istina = 50000 , splatka = 288.77)
find_rate(n = 24 ,istina = 1000 , splatka = 42.76)
find_rate(n = 5 ,istina = 5 , splatka = 1)



# Pre mapovanie tejto funkcie na jednotlive riadky je potrebna vektorova verzia tejto funkcie
find_rate_vec <- Vectorize(find_rate)

# Aplikacia na data - minimalisticka tabulecka
plab <- data.frame(principal = c(50000,1000,5), instalment = c(288.77,42.76, 1), months = c(240,24,5))

plab %>% mutate(urok = find_rate_vec(n = months, istina = principal, splatka = instalment),
                urok_percent = paste(urok*100,"%"))
                 
                 
#######
## Ulozenie na server 

library(odbc)
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "sqlserver_nazov", Database = "Nazov_schemy", Trusted_Connection = "True")
#odbc::dbWriteTable(con, "ZL_urokova_sadzba", output)    ## ZL_urokova_sadzba = nazov tabulky              

