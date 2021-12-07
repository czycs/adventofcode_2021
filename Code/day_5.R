### Adventofcode
### Day 5: Hydrothermal Venture
Hydrothermal <-
  read.table(
    url(    "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_5.txt"),
    sep = ",",
    quote = "",
    comment.char = "",
    #colClasses = 'character'
  )
colnames(Hydrothermal) <- c("x1","y1","x2","y2")


testhydro<-matrix(c(0,9 , 5,9,
                    8,0 , 0,8,
                    9,4 , 3,4,
                    2,2 , 2,1,
                    7,0 , 7,4,
                    6,4 , 2,0,
                    0,9 , 2,9,
                    3,4 , 1,4,
                    0,0 , 8,8,
                    5,5 , 8,2), ncol=4, byrow = TRUE)+1

hor_vert_lines <- function(mat) {
  result_mat <- matrix(c(0), ncol = 1000, nrow = 1000)
  #print(result_mat)
  i = 1
  for (col_ind in mat[, 2]) {
    if (mat[i, 2] == mat[i, 4]) {
      result_mat[mat[i, 2], mat[i, 1]:mat[i, 3]] <- result_mat[mat[i, 2], mat[i, 1]:mat[i, 3]] + 1
    }
    if (mat[i, 1] == mat[i, 3]) {
      result_mat[mat[i, 2]:mat[i, 4], mat[i, 1]] <- result_mat[mat[i, 2]:mat[i, 4], mat[i, 1]] + 1
    }
    i = i + 1
  }
  return(result_mat)
}

sum(hor_vert_lines(testhydro)>=2)
result_1<-sum(hor_vert_lines(Hydrothermal)>=2)


#Part2


hor_vert_diag_lines <- function(mat) {
  result_mat <- matrix(c(0), ncol = max(mat), nrow = max(mat))
  #print(result_mat)
  
  for (i in 1:length(mat[,1])) {
    
    if (mat[i, 2] == mat[i, 4]) {
      result_mat[mat[i, 2], mat[i, 1]:mat[i, 3]] <- result_mat[mat[i, 2], mat[i, 1]:mat[i, 3]] + 1
    }else if (mat[i, 1] == mat[i, 3]) {
      result_mat[mat[i, 2]:mat[i, 4], mat[i, 1]] <- result_mat[mat[i, 2]:mat[i, 4], mat[i, 1]] + 1
    } else if (abs((mat[i, 3] - mat[i, 1]) / (mat[i, 4] - mat[i, 2])) == 1) {
      seq_x<-c(mat[i,1]:mat[i,3])
      seq_y<-c(mat[i,2]:mat[i,4])
      for(k in 1:length(seq_x)){
        result_mat[seq_x[k], seq_y[k]] <- result_mat[seq_x[k], seq_y[k]] +1
      }
    }
  #print(result_mat)
  }
  #print(result_mat)
  return(result_mat)
}

sum(hor_vert_diag_lines(testhydro)>1)
sum(hor_vert_diag_lines(testhydro2)>1)

result_2 <- sum(hor_vert_diag_lines(Hydrothermal)>1)

