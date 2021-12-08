### Adventofcode
### Day 4:
bingo_draw <-
  read.table(
    url(    "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_4.txt"),
    sep = ",",
    quote = "",
    comment.char = "",
    nrows = 1
    
  )

bingo_draw<-as.numeric(bingo_draw)
bingo_sheets<-read.table(
  url(    "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_4.txt"),
  sep = "",
  quote = "",
  comment.char = "",
  skip=1

)
# Transform mat to array with mat size r c
matsplitter<-function(M, r, c) {
  rg <- (row(M)-1)%/%r+1
  cg <- (col(M)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(M))/r/c
  cv <- unlist(lapply(1:N, function(x) M[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
} 

bingo_sheet_array<-matsplitter(bingo_sheets,5,5)

test_vec<-c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,6,13,15,25,12,22,18,20,8,19,3,26,1)

testmat<-matrix(c(22,13,17,11,0,
                  8,  2, 23,  4, 24,
                  21,  9, 14, 16,  7,
                  6, 10,  3, 18,  5,
                  1, 12, 20, 15, 19),ncol = 5,byrow = TRUE)
testmat2<-matrix(c(3, 15,  0,  2, 22,
                   9 ,18, 13, 17,  5,
                   19 , 8,  7, 25, 23,
                   20 ,11 ,10, 24,  4,
                   14 ,21, 16, 12,  6),ncol = 5,byrow = TRUE)

testmat3<-matrix(c(14, 21, 17, 24,  4,
                   10, 16, 15,  9, 19,
                   18,  8, 23, 26, 20,
                   22, 11, 13,  6,  5,
                   2,  0, 12,  3,  7),ncol = 5,byrow = TRUE)

new_array<-(array(c(testmat,testmat2,testmat3),c(5,5,3)))





first_win<-function(arr,draw){
  for (i in draw) {
    arr[arr==i]<- -1
    #check all matrices
    for(j in 1:(length(arr)/25)){
      #check rows
      for (row_i in 1:5) {
        if(sum(arr[row_i,,j])==-5){
          print(arr[,,j])
          print(i)
          return(i)
          break
        }
        
      }  
      #check col
      for (col_i in 1:5) {
        if(sum(arr[,col_i,j])==-5){
          print(arr[,,j])
          print(i)
          return(i)
          break
        }
        
      }
    }
    
  }
}

first_win(bingo_sheet_array ,bingo_draw)


win_single_mat<-function(mat,draw){
  for (i in draw) {
    mat[mat==i]<- -1
      #check rows
      for (row_i in 1:5) {
        if(sum(mat[row_i,])==-5){
          print(mat[,])
          print(i)
          return(i)
          break
        }
        
      }  
      #check col
      for (col_i in 1:5) {
        if(sum(mat[,col_i])==-5){
          print(mat[,])
          print(i)
          return(i)
          break
        }
        
      }
    }
      

}


#part2
last_win <- function(arr, draw) {
  z=1
  for (i in draw) {
  z=z+1
    
    arr[arr == i] <- -1
    #check all matrices
    del_vec <- c()
    
    for (j in 1:(length(arr[1,1,]))) {
      #currentmat <- arr[, , j]
      #check rows
      for (row_i in 1:5) {
        if (sum(arr[row_i, , j]) == -5) {
          del_vec<-append(del_vec,j)
          
        }
        
      }
      #check col
      for (col_i in 1:5) {
        if (sum(arr[, col_i, j]) == -5) {
          if(length(which(del_vec==j))==0){
            del_vec<-append(del_vec,j)
          }
        }
        
      }
    }
    if (length(del_vec) > 0) {
      arr <- arr[, , -del_vec]
      if (length(arr) <= 25) {
        win_single_mat(arr,draw)
        break
      }
    }
    
  }
}


testresult<-last_win(new_array,test_vec)
result<-last_win(bingo_sheet_array , bingo_draw)



