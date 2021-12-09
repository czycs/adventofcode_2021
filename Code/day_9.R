#advenofcode day 8
#Part1
#advenofcode day 8
cave_table <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_9.txt"
    ),
    sep = "",
    quote = "",
    comment.char = "",
    colClasses = 'character'
  )

split_list <- strsplit(cave_table[, 1], "")
split_df <- do.call(rbind.data.frame, split_list)
colnames(split_df) <- c(1:100)
mat_num<-matrix(as.numeric(unlist(split_df)),ncol = ncol(split_df))

testmatrix<-matrix(c(2,1,9,9,9,4,3,2,1,0,
                     3,9,8,7,8,9,4,9,2,1,
                     9,8,5,6,7,8,9,8,9,2,
                     8,7,6,7,8,9,6,7,8,9,
                     9,8,9,9,9,6,5,6,7,8),ncol = 10,byrow = TRUE)

find_low_points <- function(df) {
  output_vec <- c()

  for (row_ind  in c(1:100)) {

    for(col_ind in c(1:100)){
    
    if (row_ind == 1) {
      if (col_ind == 1) {
        if (df[row_ind, col_ind] < df[row_ind + 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind + 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      } else if (col_ind == 100) {
        if (df[row_ind, col_ind] < df[row_ind + 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind - 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      } else{
        if (df[row_ind, col_ind] < df[row_ind + 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind + 1] &
            df[row_ind, col_ind] < df[row_ind, col_ind - 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      }
      
    } else if (row_ind == 100) {
      if (col_ind == 1) {
        if (df[row_ind, col_ind] < df[row_ind - 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind + 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      } else if (col_ind == 100) {
        if (df[row_ind, col_ind] < df[row_ind - 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind - 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      } else{
        if (df[row_ind, col_ind] < df[row_ind - 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind + 1] &
            df[row_ind, col_ind] < df[row_ind, col_ind - 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      }
      
    } else {
      if (col_ind == 1) {
        if (df[row_ind, col_ind] < df[row_ind + 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind - 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind + 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      } else if (col_ind == 100) {
        if (df[row_ind, col_ind] < df[row_ind - 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind + 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind - 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      } else{
        if (df[row_ind, col_ind] < df[row_ind - 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind + 1, col_ind] &
            df[row_ind, col_ind] < df[row_ind, col_ind - 1] &
            df[row_ind, col_ind] < df[row_ind, col_ind + 1]) {
          output_vec <- append(output_vec, df[row_ind, col_ind])
          
        }
      }
    }
    }
    
  }
  return(output_vec)
}
find_low_points(testmatrix)

result1<-sum(find_low_points(mat_num)+1)

#Part2

