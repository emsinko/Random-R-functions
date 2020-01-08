Skip to content
Search or jump to…

Pull requests
Issues
Marketplace
Explore

@emsinko 
Learn Git and GitHub without any code!
  Using the Hello World guide, you’ll start a branch, write comments, and open a pull request.


emsinko
/
  Random-R-functions
0
00
Code Issues 0 Pull requests 0 Actions Projects 0 Wiki Security Insights Settings
Random-R-functions/Sudoku solver.R
@emsinko emsinko Add files via upload
03aba22 8 hours ago
369 lines (288 sloc)  14.1 KB

#-------------------------
#  SUDOKU SOLVER  --------
# ------------------------

### Functions for row, column and sector search:

get_sector <- function(row_index, col_index){
  return(3 * ( (row_index - 1) %/% 3) + (col_index - 1) %/% 3 + 1)
}


get_numbers_from_row <- function(sudoku_grid, row_index, col_index){
  return(unique(sudoku_grid[row_index,]))
}

get_numbers_from_column <- function(sudoku_grid, row_index, col_index){
  return(unique(sudoku_grid[ , col_index]))
}

get_numbers_from_sector <- function(sudoku_grid, row_index, col_index){
  return(unique(sudoku_grid[sector_lookuptab == get_sector(row_index, col_index)]))
}

get_numbers_from_column_row_sector <- function(sudoku_grid, row_index, col_index){
  return(unique(c(
    get_numbers_from_row(sudoku_grid, row_index, col_index),
    get_numbers_from_column(sudoku_grid, row_index, col_index),
    get_numbers_from_sector(sudoku_grid, row_index, col_index)
  )))
}

get_left_upper_index_of_sector <- function(row_index, column_index){
  if(get_sector(row_index, column_index) == 1) return(list(column = 1, row = 1))
  if(get_sector(row_index, column_index) == 2) return(list(column = 4, row = 1))
  if(get_sector(row_index, column_index) == 3) return(list(column = 7, row = 1))
  if(get_sector(row_index, column_index) == 4) return(list(column = 1, row = 4))
  if(get_sector(row_index, column_index) == 5) return(list(column = 4, row = 4))
  if(get_sector(row_index, column_index) == 6) return(list(column = 7, row = 4))
  if(get_sector(row_index, column_index) == 7) return(list(column = 1, row = 7))
  if(get_sector(row_index, column_index) == 8) return(list(column = 4, row = 7))
  if(get_sector(row_index, column_index) == 9) return(list(column = 7, row = 7))
}

sector_lookuptab <- 
  matrix(c( 
    rep(c(rep(1,3), rep(4,3), rep(7,3)),3), 
    rep(c(rep(2,3), rep(5,3), rep(8,3)),3),
    rep(c(rep(3,3), rep(6,3), rep(9,3)),3)
  ), nrow = 9, ncol = 9, byrow = FALSE)


check_solution <- function(solved_sudoku){
  if(
    all
    (
      all(rowSums(solved_sudoku) == 45),
      all(colSums(solved_sudoku) == 45),
      sum(get_numbers_from_sector(solved_sudoku,1,1)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,1,4)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,1,7)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,4,1)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,4,4)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,4,7)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,7,1)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,7,4)) == 45,
      sum(get_numbers_from_sector(solved_sudoku,7,7)) == 45
    )
  ) return(TRUE) #return("Solution is correct")
  else return(FALSE) #("Solution is incorrect")
}

library(tidyverse)
library(dplyr)
library(magrittr)


grids <- read.delim("https://projecteuler.net/project/resources/p096_sudoku.txt", header = FALSE, stringsAsFactors = FALSE)

sudoku <- 
  grids[2:10, ] %>%
  strsplit("") %>%
  unlist() %>%
  as.integer() %>%
  matrix(ncol = 9, nrow = 9, byrow = T)




# get_numbers_from_column(sudoku_grid = sudoku, row_index = 1, col_index = 3) 
# get_numbers_from_row(sudoku_grid = sudoku, row_index = 1, col_index = 3) 
# get_numbers_from_sector(sudoku_grid = sudoku, row_index = 1, col_index = 3) 
# get_numbers_from_column_row_sector(sudoku_grid = sudoku, row_index = 1, col_index = 3) 

solve_sudoku <- function(sudoku, max_iter = 20){
  
  candidates <- matrix(rep(list(1:9), times = 81), nrow = 9, ncol = 9)
  
  time <- Sys.time()
  for(i in 1:max_iter){
    for(row in 1:9){
      for(column in 1:9){
        
        if(sum(colSums(sudoku == 0)) == 0){
          time <- Sys.time() - time
          print(paste("Sudoku solved in", round(as.numeric(time),2), "seconds"))
          ifelse(check_solution(sudoku), "Solution checked - correct", "Solution checked - incorrect")
          print("Solution:")
          print(sudoku)
          return(sudoku)
        }
        
        if(sudoku[row,column] != 0){
          candidates[row,column][[1]] <- sudoku[row,column]
        }
        
        if(sudoku[row,column] == 0){
          taken_numbers <- NULL       #Initialize numeric vector of taken numbers
          taken_numbers <- get_numbers_from_column_row_sector(sudoku_grid = sudoku, row_index = row, col_index = column) # get filled numbers from row, column and sector for unsolved cell
          # print(paste("candidate", row, column))
          # print(candidates[row,column])
          # print(paste("taken_numbers:", taken_numbers))
          candidates[row,column][[1]] <- setdiff(unlist(candidates[row,column]), taken_numbers) # delete numbers, which can not be candidates 
          # print(candidates[row,column])
          
          # If there is only one remaining candidate, fill it into curently cell
          if(length(unlist(candidates[row,column])) == 1){
            print(paste0("Filling number ", unlist(candidates[row,column]), " to position(",row,",",column,") - LAST IN ROW/COLUMN/SECTOR METHOD"))
            sudoku[row, column] <- unlist(candidates[row,column])
          }
          
          ### ROWS:
          
          # Pre riadky: * Najde ktore cisla vieme doplnit len na jedno miesto v riadku a ktore zaroven uz nie je vyplnene
          unique_candidate_in_row <- setdiff(as.numeric(which(table(unlist(candidates[row,1:9])) == 1)),  sudoku[row,sudoku[row,] != 0])
          
          if(length(unique_candidate_in_row) > 0){
            for(new_to_fill in unique_candidate_in_row){
              new_number_column_index <- which(lapply(candidates[row,1:9], function(x) new_to_fill %in% x) == TRUE)
              sudoku[row, new_number_column_index] <- new_to_fill
              candidates[row, new_number_column_index][[1]] <- new_to_fill
              print(paste0("Filling number ", new_to_fill, " to position(",row,",", new_number_column_index,") - ROW METHOD"))
            } 
          }
          
          ### COLUMNS:
          
          # Pre stlpce: * Najde ktore cisla vieme doplnit len na jedno miesto v stlpci a ktore zaroven uz nie je vyplnene
          unique_candidate_in_column<- setdiff(as.numeric(which(table(unlist(candidates[1:9,column])) == 1)),  sudoku[sudoku[,column] != 0, column])
          
          if(length(unique_candidate_in_column) > 0){
            for(new_to_fill in unique_candidate_in_column){
              new_number_row_index <- which(lapply(candidates[1:9, column], function(x) new_to_fill %in% x) == TRUE)
              sudoku[new_number_row_index, column] <- new_to_fill
              candidates[new_number_row_index, column][[1]] <- new_to_fill
              print(paste0("Filling number ", new_to_fill, " to position(",new_number_row_index,",", column,") - COLUMN METHOD"))
            } 
          }
          
          ### SECTORS
          
          # Pre sektory * Najde ktore cisla vieme doplnit len na jedno miesto v sektore a ktore zaroven uz nie je vyplnene
          unique_candidate_in_sector <- setdiff(as.numeric(which(table(unlist(candidates[sector_lookuptab == get_sector(row, column)])) == 1)), get_numbers_from_sector(sudoku, row,column))
          if(length(unique_candidate_in_sector) > 0){
            # print("som v sektore")
            # print(unique_candidate_in_sector)
            # print(unique_candidate_in_sector)
            for(new_to_fill in unique_candidate_in_sector){
              movement_in_sector <- which(lapply(candidates[sector_lookuptab == get_sector(row, column)], function(x) new_to_fill %in% x) == TRUE)
              x_index_of_new_to_fill <- get_left_upper_index_of_sector(row,column)$column + (movement_in_sector - 1) %/% 3 
              y_index_of_new_to_fill <- get_left_upper_index_of_sector(row,column)$row + (movement_in_sector - 1) %% 3
              #print(paste("row, column:",row, column))
              #print(paste("x:",x_index_of_new_to_fill))
              #print(paste("y:",y_index_of_new_to_fill))
              sudoku[y_index_of_new_to_fill, x_index_of_new_to_fill] <- new_to_fill
              candidates[y_index_of_new_to_fill,x_index_of_new_to_fill][[1]] <- new_to_fill
              print(paste0("Filling number ", new_to_fill, " to position(",y_index_of_new_to_fill,",", x_index_of_new_to_fill,") - SECTOR METHOD"))
            }
          }
        }
      }
    }
  }
  print(paste("Can't solve in", max_iter,"iterations"))
  return(sudoku)
}

# Pojdeme postupne cez nevyplnene hodnoty v mriezke (if value != 0/NA)
# Pozbierame pre danu bunku vsetky cisla nachadzajuce sa v riadku, stlpci a v "sektore".  Vymazeme v helperi pre dane policko mozne hodnoty, ktore by sme vedeli doplnit

sudoku <- 
  grids[462:470, ] %>%
  strsplit("") %>%
  unlist() %>%
  as.integer() %>%
  matrix(ncol = 9, nrow = 9, byrow = T)

solve_sudoku(sudoku, max_iter = 30)
solve_sudoku_backpropagation(sudoku, num_iter = 100000) # nefunguje okraj, neohranicilo mi to 9 na spodku


#################################
### BACKPROPAGATION ALGORITHM ###
#################################

library(tidyverse)


grids <- read.delim("https://projecteuler.net/project/resources/p096_sudoku.txt", header = FALSE, stringsAsFactors = FALSE)

sudoku <- 
  grids[2:10, ] %>%
  strsplit("") %>%
  unlist() %>%
  as.integer() %>%
  matrix(ncol = 9, nrow = 9, byrow = T)


get_sector <- function(row_index, col_index){
  return(3 * ( (row_index - 1) %/% 3) + (col_index - 1) %/% 3 + 1)
}


get_row_column_index <- function(index){
  return(list(row = (index-1) %% 9 + 1, column = floor((index -1) / 9) + 1))
}

get_non_unique_numbers_from_sector <- function(sudoku_grid, row_index, col_index){
  return(sudoku_grid[sector_lookuptab == get_sector(row_index, col_index)])
}


check_rules <- function(sudoku_grid, row_index, column_index){
  return(
    all(
      sum(sudoku_grid[row_index , ] == sudoku_grid[row_index,column_index]) == 1,
      sum(sudoku_grid[,column_index] == sudoku_grid[row_index,column_index]) == 1,
      sum(get_non_unique_numbers_from_sector(sudoku_grid, row_index,column_index) == sudoku_grid[row_index,column_index]) == 1 
    )
  )
}

sector_lookuptab <- 
  matrix(c( 
    rep(c(rep(1,3), rep(4,3), rep(7,3)),3), 
    rep(c(rep(2,3), rep(5,3), rep(8,3)),3),
    rep(c(rep(3,3), rep(6,3), rep(9,3)),3)
  ), nrow = 9, ncol = 9, byrow = FALSE)

solve_sudoku_backpropagation <- function(sudoku, num_iter = 1000){
  
  time <- Sys.time()
  filled_matrix <- sudoku != 0
  sudoku_copy <- sudoku
  
  filled_indexes <- which(filled_matrix == TRUE)
  unfilled_indexes <- which(filled_matrix == FALSE)
  
  index <- min(unfilled_indexes)
  
  sudoku_copy[index] <- 1
  
  #if(sudoku_copy[index] == 9){
  #  return("CHYBA !! NEDA SA VYPLNIT POLICKO")
  #}
  
  ###############
  ### FOR CYKLUS / WHILE CYKLUS ! 
  
  for(i in 1:num_iter){
    #print(paste("Iteration:",i,"           Position of sudoku:",index,"/ 81"))
    
    # Ak cislo v policku splna pravidla:
    if(check_rules(sudoku_copy,get_row_column_index(index)$row, get_row_column_index(index)$column)){
      if(index == max(unfilled_indexes)){
        print(paste("Sudoku solved in", Sys.time() - time, "secs. with",i,"iterations"))
        return(sudoku_copy)
      }
      index <- index + 1 # posunieme sa o 1 policko dopredu
      helper <- 1  # zapamatame si smer 
      #print(paste("index",index))
      # Ak este mozeme zvysovat cislo:
    } else if(sudoku_copy[index] != 9){  
      sudoku_copy[index] <- sudoku_copy[index] + 1  # zvysime hodnotu policka o 1
      helper <- 0
      #print(sudoku_copy[index])
      # Ak uz je aktualne cislo = 9      
    } else {                          
      sudoku_copy[index] <- 0 # vynuluj mi aktualne policko
      index <- index - 1      # vrat sa o jedno policko spät
      helper <- -1            # zapamatanie smeru (ak by bolo predvyplnene policko vzadu, musime ho preskocit) 
    }
    
    #print(index)
    while(index %in% filled_indexes){
      index <- index + 1 * helper
    }
    
    # Ak sme sa posuvali dopredu: 
    if(helper == 1){ 
      sudoku_copy[index] <- 1  # inicializujeme policko 
      
      # Ak sme sa posunuli dozadu:
    } else if( (helper == -1) & (sudoku_copy[index] != 9) ){
      sudoku_copy[index] <- sudoku_copy[index] + 1 
    } else if( (helper == -1) & (sudoku_copy[index] == 9) ){
      ## CHYBA: ak je helper -1 a presiahli sme  # mozu byt 3x 9 v rade - problem
      sudoku_copy[index] <- 0
      index <- index - 1 # A ZASE MUSIME CHCECKNUT CI NEJDE O CISLO ZO ZADANIA .. rekurzia
      
      while(index %in% filled_indexes){
        index <- index + 1 * helper
      }
      
      if(sudoku_copy[index] == 9){
        sudoku_copy[index] <- 0
        index <- index - 1 # A ZASE MUSIME CHCECKNUT CI NEJDE O CISLO ZO ZADANIA .. rekurzia
        
        while(index %in% filled_indexes){
          index <- index + 1 * helper
        }
      }
      sudoku_copy[index] <- sudoku_copy[index] + 1 
    }
    
    # if(helper == 0){
    #  next()
    # }
    
    #} else if(sudoku_copy[index] != 9){     ## NAVYS AK SA DA, ALEBO SA VRAT 
    #  sudoku_copy[index] <- sudoku_copy[index] + 1  # zvysime hodnotu policka o 1
    
    # Ak uz je aktualne cislo = 9      
    #} else {                          
    #  sudoku_copy[index] <- 0 # vynuluj mi aktualne policko
    #  index <- index - 1      # vrat sa o jedno policko spät
    #  helper <- -1            # zapamatanie smeru (ak by bolo predvyplnene policko vzadu, musime ho preskocit) 
  }
  #############
  #############
  print("Unsolved sudoku:")
  return(sudoku_copy)
}

solve_sudoku_backpropagation(sudoku, num_iter = 100000)

sudoku

sudoku_trans <- t(sudoku)

# riesenie prvy stlpec: 483921657
solve_sudoku_backpropagation(sudoku_trans, num_iter = 111100) # chyba pri 141-> 142. Prvy stlpec done, spodne cislo ok, ale nejde dalej v indexe 
check_rules(solve_sudoku_backpropagation(sudoku_trans, num_iter = 9),4,1)

test <- solve_sudoku_backpropagation(sudoku_trans, num_iter = 141)
check_rules(test,9,1)
test

sum(test[9 , ] == test[9,1]) == 1,
sum(test[,1] == test[9,1]) == 1,
sum(get_non_unique_numbers_from_sector(test, 9,1) == test[9,1]) == 1 


check_rules(test,get_row_column_index(9)$row, get_row_column_index(9)$column)

get_sector(row_index, column_index)


### Project Euler - Problem 96:
  
  
grids <- read.delim("https://projecteuler.net/project/resources/p096_sudoku.txt", header = FALSE, stringsAsFactors = FALSE)

euler_sum <- 0 

for(i in seq(from = 2, to = 492, by = 10)){
  
  sudoku <- 
    grids[i:(i+8), ] %>%
    strsplit("") %>%
    unlist() %>%
    as.integer() %>%
    matrix(ncol = 9, nrow = 9, byrow = T)
  
  #solve_sudoku(sudoku, max_iter = 1000)
  solved_sudoku <- solve_sudoku_backpropagation(sudoku, num_iter = 10000000)
  euler_sum <- euler_sum + as.numeric(paste(solved_sudoku[1,1:3], collapse = ""))
  print(paste("Solution of",(i+8)/10,"/ 50","is",ifelse(check_solution(solved_sudoku),"good", "WRONG !!!!!!"),"Increase sum by:",sum(solved_sudoku[1,1:3]),"to :",euler_sum ))
}  

