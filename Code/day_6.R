lanternfish_df <-
  read.table(
    url("https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_7.txt"),
    sep = ",",
    quote = "\"",
    comment.char = ""
  )
lanternfish_vec <- as.numeric(c(lanternfish_df[1, ]))
testinput <- c(3, 4, 3, 1, 2)

growth <- function(input_vec, days) {

  for (i in 1:days) {
    input_vec <- input_vec - 1

    if (sum(input_vec == 0) > 0) {
      newlantern <- length(which(input_vec == 0))
      
      elem_zero <- which(input_vec == 0)
      for (ind in elem_zero) {
        input_vec[ind] <- input_vec[ind] + 7
        
      }
      input_vec <- append(input_vec, rep(c(9), newlantern))
      
    }
    #print(i)
    #print(object.size(input_vec))
  }
  return(length(input_vec))
  
}

#for bigger exponential growth
growth_2 <- function(input, days) {
  counts <- c(0, tabulate(input, nbins = 8))
  for (i in seq_len(days)) {
    new_lantern <- counts[1]
    counts[1:8] <- counts[2:9]
    counts[7] <- counts[7] + new_lantern
    counts[9] <- new_lantern
  }
  
  (sum(counts))
  
}

growth_2(testinput, 80)
growth_2(testinput, 256)


testresult1 <- growth(testinput, 17)
testresult1_2 <- growth(testinput, 79)
#testresult2 <- growth(testinput, 255) #Crash vector >8gb 

#Result for Part 1 & 2
result1 <- growth(lanternfish_vec, 79)
result2<- growth_2(lanternfish_vec,256)

