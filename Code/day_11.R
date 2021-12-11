# Adventofcode Day 11
octo_mat <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_11.txt"
    ),
    sep = "",
    quote = "",
    comment.char = "",
    colClasses = 'character'
  )


split <- strsplit(octo_mat[, 1], "")



equal_matrix <-
  matrix(as.numeric(unlist(split)),
         ncol = length(split[[1]]),
         byrow = TRUE)
equal_df <- as.data.frame(equal_matrix)
#add boundary
boundmatrix <- matrix(rep(1, 144), ncol = 12)
boundmatrix[2:11, 2:11] <- equal_matrix
boundmatrix<-as.data.frame(boundmatrix)

flashes <- function(df, steps) {
  flash_count <- 0
  for (i in seq(steps)) {
    seen_matrix <- matrix(rep(FALSE, 144), ncol = 12)
    #add 1 energy level
    df <- df + 1
    #check matrix couple of times
    for (i in 1:10) {
    
      #check every element for flash
      for (row in 2:(length(df[1,]) - 1)) {
        for (col in 2:(length(df[1,]) - 1)) {
          #check element
          if (df[row, col] >= 10) {
            #Check if element is already seen
            if (seen_matrix[row, col] == FALSE) {
              #add 1 energy level to all surrounding octos
              df[row - 1, col - 1] <- df[row - 1, col - 1] + 1
              df[row - 1, col] <- df[row - 1, col] + 1
              df[row - 1, col + 1] <- df[row - 1, col + 1] + 1
              df[row, col - 1] <- df[row, col - 1] + 1
              df[row, col + 1] <- df[row, col + 1] + 1
              df[row + 1, col - 1] <- df[row + 1, col - 1] + 1
              df[row + 1, col] <- df[row + 1, col] + 1
              df[row + 1, col + 1] <- df[row + 1, col + 1] + 1
              seen_matrix[row, col] <- TRUE
            }
            
          }
          
        }
        
     }
    }

    flash_count <- flash_count + length(which(df[2:11, 2:11] >= 10))
    df[df>=10]<-0
  }
  return(flash_count)
}
flashes(boundmatrix, 100)

#Part 2
all_flashes <- function(df) {
  flash_count <- 0
  steps<-0
  all_flashed<-FALSE
  while(all_flashed==FALSE) {
    steps<-steps+1
    seen_matrix <- matrix(rep(FALSE, 144), ncol = 12)
    #add 1 energy level
    df <- df + 1
    #check matrix couple of times
    for (i in 1:20) {
      
      #check every element for flash
      for (row in 2:(length(df[1,]) - 1)) {
        for (col in 2:(length(df[1,]) - 1)) {
          #check element
          if (df[row, col] >= 10) {
            #Check if element is already seen
            if (seen_matrix[row, col] == FALSE) {
              #add 1 energy level to all surrounding octos
              df[row - 1, col - 1] <- df[row - 1, col - 1] + 1
              df[row - 1, col] <- df[row - 1, col] + 1
              df[row - 1, col + 1] <- df[row - 1, col + 1] + 1
              df[row, col - 1] <- df[row, col - 1] + 1
              df[row, col + 1] <- df[row, col + 1] + 1
              df[row + 1, col - 1] <- df[row + 1, col - 1] + 1
              df[row + 1, col] <- df[row + 1, col] + 1
              df[row + 1, col + 1] <- df[row + 1, col + 1] + 1
              seen_matrix[row, col] <- TRUE
            }
            
          }
          
        }
        
      }
    }
    
    if(sum(seen_matrix[2:11,2:11])==100){
      all_flashed<-TRUE
    }
    df[df>=10]<-0
  }
  return(steps)
}

all_flashes(boundmatrix)

