# Adventofcode Day 15 Chiton
chiton <-
  read.table(
    url(
      #"https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/testinput_15.txt"
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_15.txt"
    ),
    sep = "",
    quote = "",
    comment.char = "",
    colClasses = 'character',
  )
library(tidyverse)
library(igraph)
split_list<-strsplit(chiton[,1], "")
split_vec<-as.numeric(unlist(split_list))
split_df<-matrix(split_vec,ncol=sqrt(length(split_vec)),byrow=TRUE)

#create boundaries
df_size<-length(split_df[1,])
bound<-matrix(rep(9), nrow = length(split_df[1,])+2, ncol = length(split_df[1,])+2)
bound[2:(df_size+1),2:(df_size+1)]<-split_df

#Part1 
#find path with lowest sum
#create adjacency matrix
create_adjac <- function(df) {
  size_mat<-length(df[1,])
  print(size_mat)
  newdf <- matrix(rep(100), ncol = size_mat^2, nrow = size_mat^2)
  for (col in 2:(length(df[1, ]) - 1)) {
    for (row in 2:(length(df[1, ]) - 1)) {
      print(df[[(((col-1)*12)+row)]])
      #get index of object
      index<-(((col-1)*12)+row)
      #up
      newdf[index,(((col-1)*12)+(row-1))]<-df[row-1,col]
      #down
      newdf[index,(((col-1)*12)+(row+1))]<-df[row+1,col]
      #left
      newdf[index,(((col-2)*12)+(row))]<-df[row,col-1]
      #right
      newdf[index,(((col)*12)+(row))]<-df[row,col+1]
      
    }
  }
  return(newdf)
}


#dijkstra

testinput<-matrix(c(1,1,9,9,9,
                  9,1,9,9,9,
                  1,1,9,9,9,
                  1,9,9,9,9,
                  1,1,1,1,1),ncol = 5,byrow = TRUE)

#dij<-testinput
dij<-split_df


dijkstra<-function(mat,start){
  size<-ncol(mat)
  new_m<-matrix(rep(Inf,length(mat)),ncol = ncol(mat))
  new_m[start]<-0
  que<-2:length(mat)
  for(i in que){
    cur<-mat[i]
    #create vec from neighbours
    vec<-c()
    #last element
    if(i==length(mat)){
      print(mat[i])
      new_m[i]<-mat[i]
      break
    }
    #checkup
    if (i%%size!=1) {
      vec1<-new_m[i-1]+cur
      vec<-append(vec,vec1)
    }
    #checklow
    if (i%%size!=0) {
      vec2<-new_m[i+1]+cur
      vec<-append(vec,vec2)
    }
    #checkleft
    if (floor(i/size)!=0) {
      vec3<-new_m[(i%%size),(floor(i/size)-1)+1]+cur
      vec<-append(vec,vec3)
    }
    #checkright
    if (floor(i/size)!=(ncol(mat)-1)) {
      vec4<-new_m[(i%%size),(floor(i/size)+1)+1]+cur
      vec<-append(vec,vec4)
    }
    
    if (min(vec)<new_m[i]) {
      new_m[i]<-min(vec)
    }
    #recheck col
    if(i%%size==0){
      
      for(g in 1:(size-1)){
        vect<-c()
        j<-i-g
        
        curg<-mat[j]

        #checkup
        if (j%%size!=1) {
          vect1<-new_m[j-1]+curg
          vect<-append(vect,vect1)
        }
        #checklow
        if (j%%size!=0) {

          vect2<-new_m[j+1]+curg
          vect<-append(vect,vect2)
        }
        #checkleft
        if (floor(j/size)!=0) {
          vect3<-new_m[(j%%size),(floor(j/size)-1)+1]+curg
          vect<-append(vect,vect3)
        }
        #checkright
        if (floor(j/size)!=(ncol(mat)-1)) {
          vect4<-new_m[(j%%size),(floor(j/size)+1)+1]+curg
          vect<-append(vect,vect4)
        }
        

        if (min(vect)<new_m[j]) {

          new_m[j]<-min(vect)
        }

      }
      
    }
    #recheck row
    if(i%%size==0){
      for(h in 0:(size-1)){
        for(f in 0:(size-1)){
        vecto<-c()
        k<-(i-f)-5*h
        
        curgk<-mat[k]
        
        #checkup
        if (k%%size!=1) {
          vecto1<-new_m[k-1]+curgk
          vecto<-append(vecto,vecto1)
        }
        #checklow
        if (k%%size!=0) {
          
          vecto2<-new_m[k+1]+curgk
          vecto<-append(vecto,vecto2)
        }
        #checkleft
        if (floor(k/size)!=0) {
          vecto3<-new_m[(k%%size),(floor(k/size)-1)+1]+curgk
          vecto<-append(vecto,vecto3)
        }
        #checkright
        if (floor(k/size)!=(ncol(mat)-1)) {
          vecto4<-new_m[(k%%size),(floor(k/size)+1)+1]+curgk
          vecto<-append(vecto,vecto4)
        }
        print(paste(vecto))
       
        
        
      }
    }
    }
  }
  return(new_m)
}

result1<-dijkstra(dij,1)

ans <- function(grid) {
  g <- make_lattice(c(nrow(grid), ncol(grid))) %>% 
    as.directed(mode = 'mutual')
  E(g)$weight <- grid[get.edgelist(g)[, 2]]
  
  distances(g, 1, nrow(grid) * ncol(grid), 'out')
}
ans(dij)
