#############
## Ramon 
#############

# Uloha 4 



cuts <-  c(0,5,10,20,30,40,70)
middle <- NULL
for(i in 2:length(temp)){
  middle <- c(middle, (cuts[i] + cuts[i-1])/2)
}
middle


library(glue)

ramon_report <- function(x,freq){ 
    
  data <- rep(x,freq)
  n <- length(data)
  var_data <- (n-1)/n*var(data)
  sd_data <- sqrt(var_data)
  
  #print(sum(data^2)/n - mean(data)^2)
  
  print(glue("n: {round(length(data),2)}"))
  print(glue("Mean: {round(mean(data),2)}"))
  print(glue("Variance: {round(var_data,2)}"))
  print(glue("Standard deviation: {round(sd_data,2)}"))
  
}

ramon_report(c(5.5,15.5,23,28, 40.5, 60.5), freq = c(2,4,3,5,4,1)*15)

ramon_report(c(2.5,7.5,15,25,35,55), freq = c(11,20,32,18,10,6))

freq_density  =  freq/sirka


