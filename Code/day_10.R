# Adventofcode Day 10 
syntax_errror_input <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_10.txt"
    ),
    sep = "",
    quote = "",
    comment.char = "",
    colClasses = 'character'
  )
library("tidyverse")
library(stringi)

#nchar(syntax_errror_input[2,])
#newdf<-Binary_Diagnostic$splits



check_line <- function(string) {
  #remove good pairs(goodpairs==(),[],{},<>)
  for (i in 1:100) {
    string <- str_replace_all(string, "Rr", "")
    string <- str_replace_all(string, "Ee", "")
    string <- str_replace_all(string, "Gg", "")
    string <- str_replace_all(string, "Dd", "")
    
  }
  return(string)
}
check_line("ERGRDRRrrEedEEGEeGDRrDdd")
check_input<-function(df){
  points<-0
  for(row in df[,1]){
    
    klein<-str_extract(check_line(row), "[a-z]") 
    if(!is.na(klein)){
      if(klein == "r"){
        points <- points + 3
      }
      if(klein == "e"){
        points <- points + 57
      }
      if(klein == "g"){
        points <- points + 1197
      }
      if(klein == "d"){
        points <- points + 25137
      }
    }
  }
  return(points)
}



result1<-check_input(syntax_errror_input)

#Part 2

check_incomplete<-function(df){
  score_vec<-c()
  for(row in df[,1]){
    klein<-str_extract(check_line(row), "[a-z]") 
    if(is.na(klein)){
      
      if(nchar(row)!=0){
        
        cur_string<-check_line(row)
        reversed_string<-unlist(strsplit(stri_reverse(cur_string),""))
        score<-0 
        for(z in 1:length(reversed_string)){
          pos<-reversed_string[z]
          if(pos=="R"){
            score<-(score*5)+1
          }
          if(pos=="E"){
            score<-(score*5)+2
          }
          if(pos=="G"){
            score<-(score*5)+3
          }
          if(pos=="D"){
            score<-(score*5)+4
          }
        }
        
        score_vec<-append(score_vec,score)
         
      }else{
        next
      }
    }
  }
  sortedscore<-sort(score_vec)
  len<-ceiling(length(score_vec)/2)
  return(sortedscore[len])
}


result2<-check_incomplete(syntax_errror_input)

