# Adventofcode Day 13
origami <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_13.txt"
    ),
    sep = ",",
    quote = "",
    comment.char = "",
    #colClasses = 'character'
    nrows = 1125
  )
folds <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_13.txt"
    ),
    sep = "=",
    quote = "",
    comment.char = "",
    #colClasses = 'character'
    skip = 1125
  )
origami <- origami + 1
folds[, 2] <- folds[, 2] + 1


mat <- matrix(rep(0), ncol = 1311, nrow = 895)
fillmatrix <- function(ori) {
  for (row in 1:1125) {
    mat[origami[row, 2], origami[row, 1]] <<- 1
  }
}
fillmatrix(origami)

folding <- function(m, folding_line) {
  folding_dir <- if (sum(grepl("x", folding_line)) > 0) {
    "x"
  } else{
    "y"
  }
  folding_point <- (folding_line[[2]])
  print(sum(m))
  if (folding_dir == "x") {
    for (row in 1:length(m[, 1])) {
      for (col in folding_point:length(m[1, ])) {
        if (m[row, col] != 0) {
          dis_to_fp<-col-folding_point
          
          m[row, (folding_point - dis_to_fp)] <- 1
         
        }
      }
    }
    m<-m[,-(folding_point:length(m[1, ]))]
  } else{
    for (row in folding_point:length(m[, 1])) {
      for (col in 1:length(m[1, ])) {
        if (m[row, col] > 0) {
          dis_to_fp<-row-folding_point
          m[(folding_point- dis_to_fp), col] <- 1
          
        }
      }
    }
    m<-m[-(folding_point:length(m[, 1])),]
  }
  
  print(dim(m))
  return(m)
}


folding(mat, folds[1, ])
#Part2
newmat<-folding(mat, folds[1, ])
folding(newmat,folds[2,])

fold_all<-function(m,df){
  row<-0
  for(line in df[,1]){
    row<-row+1
    m<-folding(m,df[row,])
    
    
  }
  return(m)
}
result2<-fold_all(mat,folds)



