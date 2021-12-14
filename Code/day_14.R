# Adventofcode Day 14 Extended Polymerization
polymerization_rules <-
  read.table(
    url(
      #"https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/testinput14.txt"
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_14.txt"
    ),
    sep = "",
    quote = "",
    comment.char = "",
    colClasses = 'character',
    skip = 1
  )
polymerization_line <-
  read.table(
    url(
      #"https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/testinput14.txt"
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_14.txt"
    ),
    sep = "",
    quote = "",
    comment.char = "",
    colClasses = 'character',
    nrows = 1
  )

polymerization_line <- unlist(strsplit(polymerization_line[1, 1], ""))

poly_func <- function(rules, line, times) {
  current_line <- line
  check <- c()
  next_line <- line
  for (i in 1:times) {
    pos <- c(1, 2)
    insert_pos <- 0
    while (min(pos) != (length(current_line))) {
      check <-paste(c(current_line[pos[1]], current_line[pos[2]]), collapse = "")
      character_to_insert <- rules[which(rules == check), 3]
      next_line <-append(next_line, character_to_insert, after = (min(pos) + insert_pos))
      pos <- pos + 1
      insert_pos <- insert_pos + 1
    }
    current_line <- next_line
  }
  print(length(current_line))
  return(table(current_line))
}

result<-poly_func(polymerization_rules, polymerization_line, 10)


#Part2
#Idea:pair is not unique ex:NN->NC,CN
#create table with allpairs
table_df<-cbind(polymerization_rules[,1])
table_df<-as.data.frame(table_df)
rownames(table_df)<-polymerization_rules[,1]
table_df<-cbind(table_df,rep(0))
table_df<-cbind(table_df,rep(0))

load_starting_vec<-function(df,line){
  current_line <- line
  check <- c()
  pos <- c(1, 2)
  insert_pos <- 0
  while (min(pos) != (length(current_line))) {
    check <-paste(c(current_line[pos[1]], current_line[pos[2]]), collapse = "")
    row<-which(df[,1]==check)
    df[row,2]<-df[row,2]+1
    pos <- pos + 1
    insert_pos <- insert_pos + 1
  }
  table_df<<-df
  return(df)
}
load_starting_vec(table_df,polymerization_line)

transform_df_x<-function(dataf,rules,x){
  df<-dataf
  for (i in 1:x) {
    
  for(row in df[,1]){
    current_pair<-df[row,1]
    if (df[row,2]>0) {

      number_of_new_pairs<-df[row,2]

      character_to_insert <- rules[which(rules == current_pair), 3]

      new_pair_1<-paste0(substr(current_pair, 1, 1),character_to_insert,collapse = "")
      
      new_pair_2<-paste0(character_to_insert, substr(current_pair, 2, 2),collapse = "") 
      
      df[which(df==new_pair_1),3]<-number_of_new_pairs+df[which(df==new_pair_1),3]
      df[which(df==new_pair_2),3]<-number_of_new_pairs+df[which(df==new_pair_2),3] 

      }
    
  }
  df[,2]<-df[,3]
  df[,3]<-rep(0)

  }
  print(sum(df[,2]))
  return(df)
}
#testdf<-transform_df_once(table_df,polymerization_rules)

pairs_in<-transform_df_x(table_df,polymerization_rules,40)

get_leter_count<-function(df){
  newdf<-data.frame(c(rep(0,2)))
  for(i in LETTERS){
    double<-paste0(i,i,collapse = "")
    df[grepl(double,df[,1]),2]<-df[grepl(double,df[,1]),2]*2
  }
  print(df)
  for (k in LETTERS) {
   newdf<-cbind(newdf,c(k,as.numeric(sum(df[grepl(k,df[,1]),2]))))
  }
  return(newdf)
}
num<-as.data.frame(get_leter_count(pairs_in)  )
num<-num[,colSums(num!=0)>1]  

newvec<-as.numeric(unlist(num[2,]))
result2<-ceiling((max(newvec)-min(newvec))/2)-1

