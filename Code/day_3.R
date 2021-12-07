#Adventofcode day3
library("tidyverse")

Binary_Diagnostic <-read.table(
  url(    "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_3.txt"),
  quote = "\"",
  comment.char = "",
  colClasses = 'character'
)

Binary_Diagnostic$splits <- strsplit(Binary_Diagnostic$V1, "")

newdf<-Binary_Diagnostic$splits
equal<-lapply(newdf, `length<-`, max(lengths(newdf)))
equal_matrix<-matrix(unlist(equal),ncol = length(equal[[1]]),byrow = TRUE)
equal_df<-as.data.frame(equal_matrix)

more_common <- function(df) {
  out<-c()
  for (i in 1:length(df[1,])) {
    if(sum(as.numeric(equal_df[,i]))>=(length(df[,1])/2)){
      out<-append(out,1)
    } else{
      out<-append(out,0)
    }
      
  }
  return(out)
}
binar1<-more_common(equal_df)
binar2<-+(!binar1)
#Binary to numeric
bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

result1<-bitsToInt(binar1)*bitsToInt(binar2)

#Part2

common_finder <- function(x, fun, even) {
  tab <- table(x)
  if (length(tab) == 2 & tab[1] == tab[2]){
    return(even)
    }
  names(which(fun(tab) == tab))
}

rate_calculator <- function(mat, fun, even) {
  considered <- !logical(nrow(mat))
  
  res <- c()
  
  for (i in seq_len(ncol(mat))) {
    top <- common_finder(mat[considered, i], fun, even)
    res <- c(res, top)
    considered <- considered & (mat[, i] == top)
    
    if (sum(considered) == 1) break
  }
  mat[considered, ] |>
    paste0(collapse = "") |>
    strtoi(base = 2)
}

oxygen <- rate_calculator(equal_matrix, max, "1")
co2 <- rate_calculator(equal_matrix, min, "0")

life_support_rating<-oxygen * co2
