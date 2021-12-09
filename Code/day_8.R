#advenofcode day 8
seven_segment <-
  read.table(
    url("https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_8.txt"),
    sep = "|",
    quote = "",
    comment.char = "",
    
  )

split_list<-strsplit(seven_segment[,2], " ")
split_df<-do.call(rbind.data.frame, split_list)

df_char_length<-do.call(rbind.data.frame, lapply(split_list, nchar))

counter<-function(x){
  count<-0
  for (i in c(2,3,4,7)) {
    count=count+length(which(x==i))
  }
  return(count)
}
result1<-counter(df_char_length)


#Part2

#Create Map from first part of input
decoder <- function(vec) {
  #solved==FALSE
  count<-c()
    for (i in vec) {
      
      current <- strsplit(i, "")

      if (length(current[[1]]) == 2) {
        one<-current[[1]]
      
      }
      
      if (length(current[[1]]) == 3) {
        seven<-current[[1]]
      }
      if (length(current[[1]]) == 4) {
        four<-current[[1]]
     
      }
      if (length(current[[1]]) == 7) {
        eight<-current[[1]]
      }
      count<-append(count,current[[1]])
    }

  pos1<-setdiff(seven,one)
  pos2<-as.character(names(which(table(count)==6)))
  
  
  pos5<-as.character(names(which(table(count)==4)))
  pos6<-as.character(names(which(table(count)==9)))
  
  pos3<-setdiff(one,pos6)
  pos4<-setdiff(setdiff(four,one),pos2)
  
  pos7<-setdiff(setdiff(setdiff(eight,four),pos5),pos1)
  

  map<-data.frame(c(pos1,pos2,pos3,pos4,pos5,pos6,pos7),c(1:7))
 return(map)
}


#tranlate the the 4 digits
to_number<-function(map,vec){
  
  output<-c()
  for (i in vec) {
    split<-strsplit(i, "")
    out<-c()
    for (h in split[[1]]) {
      out<-append(out,which(map==h))
      
    }
    if(prod(out)==1260){
      output<-append(output,0)
    }
    if(sum(out)==9){
      output<-append(output,1)
    }
    if(prod(out)==420){
      output<-append(output,2)
    }
    if(prod(out)==504){
      output<-append(output,3)
    }
    if(prod(out)==144){
      output<-append(output,4)
    }
    if(prod(out)==336){
      output<-append(output,5)
    }
    if(prod(out)==1680){
      output<-append(output,6)
    }
    if(sum(out)==10){
      output<-append(output,7)
    }
    if(prod(out)==5040){
      output<-append(output,8)
    }
    if(prod(out)==1008){
      output<-append(output,9)
    }
    
    
    
  }

  return(sum(output * 10^((length(output)-1):0)))
  
}



#solve for every line in the input
solver<-function(df){
  out_vector<-c()
  scrambled<-df[,1]
  outvec<-df[,2]
  list1<-strsplit(scrambled, " ")
  #print(list1[2:5])

  list2<-strsplit(outvec, " ")
  
  for (p in 1:length(list1)) {
    out_vector<-append(out_vector,to_number(decoder(list1[[p]]),list2[[p]]))
    
  }
  return(out_vector)
}

  
result2<-sum(solver(seven_segment))

  
  
  
  
