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
mat_num <- matrix(as.numeric(unlist(split_df)), ncol = ncol(split_df))

find_low_points <- function(df) {
  output_vec <- c()
  
  for (row_ind  in c(1:100)) {
    for (col_ind in c(1:100)) {
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


result1 <- sum(find_low_points(mat_num) + 1)

#Part2


nine_mat <- matrix(rep(9, 102 * 102), ncol = 102, nrow = 102)


nine_mat[2:101, 2:101] <- mat_num

seen_mat <- `dim<-`(nine_mat %in% c(9), dim(nine_mat))

basin_mat <- nine_mat
basin_mat[nine_mat == 9] <- 0

basin_search <- function(df, row_ind, col_ind, basin_id) {
  seen_mat[row_ind, col_ind] <<- TRUE
  basin_mat[row_ind, col_ind] <<- basin_id
  if (!seen_mat[row_ind - 1, col_ind]) {
    basin_mat <- basin_search(df, row_ind - 1, col_ind, basin_id)
  }
  if (!seen_mat[row_ind + 1, col_ind]) {
    basin_mat <- basin_search(df, row_ind + 1, col_ind, basin_id)
  }
  if (!seen_mat[row_ind, col_ind - 1]) {
    basin_mat <- basin_search(df, row_ind, col_ind - 1, basin_id)
  }
  if (!seen_mat[row_ind, col_ind + 1]) {
    basin_mat <- basin_search(df, row_ind, col_ind + 1, basin_id)
  }
  basin_mat
}

basin_lowpoint <- function(df) {
  #Find low point
  basin_id = 0
  for (row_ind in 2:101) {
    for (col_ind in 2:101) {
      if (df[row_ind, col_ind] < df[row_ind - 1, col_ind]) {
        if (df[row_ind, col_ind] < df[row_ind + 1, col_ind]) {
          if (df[row_ind, col_ind] < df[row_ind, col_ind - 1]) {
            if (df[row_ind, col_ind] < df[row_ind, col_ind + 1]) {
              #df[row_ind,col_ind] is low point
              basin_id = basin_id + 1
              basin_search(df, row_ind, col_ind, basin_id)
            }
          }
        }
      }
    }
  }
}
basin_lowpoint(nine_mat)

result2 <-prod(sort(as.vector(table(basin_mat)), decreasing = TRUE)[2:4])
