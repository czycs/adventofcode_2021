# Adventofcode Day 11
octo_mat <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_11.txt"
    ),
    sep = "",
    quote = "",
    comment.char = "",
    colClasses = 'character'
  )
library(ggplot2)
library(gganimate)
library(purrr)
library(reshape2)

split <- strsplit(octo_mat[, 1], "")



equal_matrix <-
  matrix(as.numeric(unlist(split)),
         ncol = length(split[[1]]),
         byrow = TRUE)
equal_df <- as.data.frame(equal_matrix)
#add boundary
boundmatrix <- matrix(rep(1, 144), ncol = 12)
boundmatrix[2:11, 2:11] <- equal_matrix
boundmatrix<-as.data.frame(boundmatrix)

flashes <- function(df, steps) {
  flash_count <- 0
  for (i in seq(steps)) {
    seen_matrix <- matrix(rep(FALSE, 144), ncol = 12)
    #add 1 energy level
    df <- df + 1
    #check matrix couple of times
    for (i in 1:10) {
    
      #check every element for flash
      for (row in 2:(length(df[1,]) - 1)) {
        for (col in 2:(length(df[1,]) - 1)) {
          #check element
          if (df[row, col] >= 10) {
            #Check if element is already seen
            if (seen_matrix[row, col] == FALSE) {
              #add 1 energy level to all surrounding octos
              df[row - 1, col - 1] <- df[row - 1, col - 1] + 1
              df[row - 1, col] <- df[row - 1, col] + 1
              df[row - 1, col + 1] <- df[row - 1, col + 1] + 1
              df[row, col - 1] <- df[row, col - 1] + 1
              df[row, col + 1] <- df[row, col + 1] + 1
              df[row + 1, col - 1] <- df[row + 1, col - 1] + 1
              df[row + 1, col] <- df[row + 1, col] + 1
              df[row + 1, col + 1] <- df[row + 1, col + 1] + 1
              seen_matrix[row, col] <- TRUE
            }
            
          }
          
        }
        
     }
    }

    flash_count <- flash_count + length(which(df[2:11, 2:11] >= 10))
    df[df>=10]<-0
  }
  return(flash_count)
}
flashes(boundmatrix, 100)

#Part 2
all_flashes <- function(df) {
  flash_count <- 0
  steps<-0
  all_flashed<-FALSE
  newlist<-list()
  while(all_flashed==FALSE) {
    steps<-steps+1
    seen_matrix <- matrix(rep(FALSE, 144), ncol = 12)
    #add 1 energy level
    df <- df + 1
    #check matrix couple of times
    for (i in 1:20) {
      
      #check every element for flash
      for (row in 2:(length(df[1,]) - 1)) {
        for (col in 2:(length(df[1,]) - 1)) {
          #check element
          if (df[row, col] >= 10) {
            #Check if element is already seen
            if (seen_matrix[row, col] == FALSE) {
              #add 1 energy level to all surrounding octos
              df[row - 1, col - 1] <- df[row - 1, col - 1] + 1
              df[row - 1, col] <- df[row - 1, col] + 1
              df[row - 1, col + 1] <- df[row - 1, col + 1] + 1
              df[row, col - 1] <- df[row, col - 1] + 1
              df[row, col + 1] <- df[row, col + 1] + 1
              df[row + 1, col - 1] <- df[row + 1, col - 1] + 1
              df[row + 1, col] <- df[row + 1, col] + 1
              df[row + 1, col + 1] <- df[row + 1, col + 1] + 1
              seen_matrix[row, col] <- TRUE
            }
            
          }
          
        }
        
      }
    }
    tru_false_mat<-df[2:11,2:11]
    tru_false_mat<-(tru_false_mat>9)
    newlist[[steps]]<-tru_false_mat
    if(sum(seen_matrix[2:11,2:11])==100){
      all_flashed<-TRUE
    }
    
    df[df>=10]<-0
    
  }
  print(steps)
  return(newlist)
}

output<-all_flashes(boundmatrix)

melted<-melt(output)
melted_list<-lapply(output, melt)

melted_df<-do.call(rbind.data.frame, melted_list)
melted_df$step<-rep(c(1:382),each=100)

graph1<-ggplot(melted_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = c( "black","white")) +
  theme_bw() +
  transition_states(step,
                    transition_length = 2,
                    state_length = 2)+
  theme(legend.position = "none")
  
graph1.animation= graph1+
  transition_time(step)+
  labs(subtitle = "Step: {frame_time}") +
  shadow_wake(wake_length = 0.1)+
  enter_fade() + 
  exit_shrink()

animate(graph1.animation, height = 500, width = 800, fps = 25, duration = 25,
        end_pause = 50, res = 100)
anim_save("graph.gif")

