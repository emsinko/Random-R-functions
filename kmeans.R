# Vymazanie vsetkych premennych
# rm(list = ls())

k_means <- function(x, k, trace = TRUE, maxiter = 10, change = 0.001){ 
  
  data <- as.data.frame(x)        # ulozenie datovej vstupnej premennej x do premennej data a zabezbecenie aby bola formatu data.frame
  is_duplicated <- TRUE           # inicializacia logickej premenne, aby zbehla prva iteracia vo while-cykle
  
  ### Zabezpecenie, aby centre neboli rovnake
  #  Pouzijeme na to funkciu duplicated, ktora priradi pre kazdy riadok hodnotu FALSE ak presne taky riadok zatial v tabulke nebol. 
  #  T.j. ak sa nachadza vo vystupe aspon jedno TRUE, tak sa v tabulke nachadzaju duplicitne riadky
  
  while(is_duplicated){
    index <- sample(nrow(data), k)                   # vyber zaciatocnych k riadkov (nahodny vyber k riadkov z dat)
    center_points <- data[index,]                    # ulozenie centier do tabulky center_points
    is_duplicated <- any(duplicated(center_points))  # kontrola duplicit: ak tam nebude ziadna duplicita, tak funkcia any() vrati FALSE. Funkcia any() vrati TRUE iba ak aspon jedna hodnota bude vo funkcii TRUE (t.j. aspon jedna duplicita)
    center_points$group <- 1:k                       # ocislovanie k-skupin (pridanie stlpca do tabulky)
  }
  
  
  # Inicializacia premennych 
  iter_number <- 1              
  small_change <- FALSE
  dist_all <- Inf
  
  
  while(small_change == FALSE & iter_number < (maxiter + 1)){ 
    for(i in 1:k){ 
      data$x1 <- center_points[i, 1]
      data$x2 <- center_points[i, 2]
      data[ , paste0("group", i)]   <- sqrt((data[,1]-data$x1)^2 + (data[,2]-data$x2)^2)  # vypocet euklidovskych distancii od vsetkych centier 
    }
  
  data$new_group <- apply(data[,paste0("group", 1:k)], MARGIN = 1, FUN  = which.min)  # urcenie centra, ktory ma najmensiu euklidovsku vzdialenost
  data$min_distance <- apply(data[,paste0("group", 1:k)], MARGIN = 1, FUN  = min)     # urcenie minimalnej vzdialenosti
  
  dist_all_before <- dist_all          # inicializacia suctu vsetkych vzdialenosti
  dist_all <-  sum(data$min_distance)  # vypocet noveho suctu vsetkych vzdialenosti
  
  
  # Vizualizacia
    if(trace){
      plot(data[,1],data[,2], col = data$new_group, pch = 20, cex = 0.8, xlab = colnames(data)[1], ylab = colnames(data)[2], main = paste0("Iteration n.",iter_number))
      points(center_points[,-3],   col = center_points$group, pch = 4,  cex = 2,  lwd = 2)
      Sys.sleep(1.5)  # toto sposobuje spomalenie procesu, aby to potom vyzeralo, ako animacia (pouzivam RStudio)
    }
    
  # Vypocet novych centier (priemerna hodnota suradnic po skupinach)
  center_points <- aggregate(data[, 1:2], list(data$new_group), mean)[,2:3]
  center_points$group <- 1:k   
  
  iter_number <- iter_number + 1     # navysenie poctu iteracii o 1
  small_change <- abs((dist_all_before - dist_all))/dist_all < change  # kontrola podmienky na dalsiu iteraciu
  }
  
  output <- list(iterations = iter_number, centres=center_points, index=data$new_group, distance=data$min_distance, data = data)
  return(output)
}

# Testovanie na datach faithful
k_means(x = faithful, k = 2, trace = TRUE, maxiter = 10, change = 0.001)


samp1 <- data.frame(x_var = rnorm(25, mean = -1, sd = 1), y_var = rnorm(25, mean = 1, sd = 1))
samp2 <- data.frame(x_var = rnorm(25, mean = -1, sd = 1), y_var = rnorm(25, mean = -1, sd = 1))
samp3 <- data.frame(x_var = rnorm(25, mean = 1, sd = 1), y_var = rnorm(25, mean = 1, sd = 1))
samp4 <- data.frame(x_var = rnorm(25, mean = 1, sd = 1), y_var = rnorm(25, mean = -1, sd = 1))

colMeans(samp1)
colMeans(samp2)
colMeans(samp3)
colMeans(samp4)

k_means(samp1, k=2, trace=TRUE, change=0.01)
k_means(samp2, k=3, trace=TRUE, change=10)
k_means(samp3, k=4, trace=TRUE, change=0.00006)


# https://www.springboard.com/blog/data-mining-python-tutorial/


