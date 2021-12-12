# Adventofcode Day 12 Passage Pathing
passage_pathing <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_12.txt"
    ),
    sep = "-",
    quote = "",
    comment.char = "",
    colClasses = 'character'
  )

library(tidyverse)
require(igraph)
require(sp)

df<-as.data.frame(passage_pathing)

graph<-simplify(graph.data.frame(df,directed=F))
simplepaths<- all_simple_paths(graph,"start","end")
short<-all_shortest_paths(graph,"start","end")


map <- cbind(c(passage_pathing[,1], passage_pathing[,2]), c(passage_pathing[,2], passage_pathing[,1]))
map <- map[map[,2] != "start",]
sc <- setdiff(map[grepl("[a-z]", map[,1]), 1], c("start", "end"))

count_paths <- function(p, sc_vec, part1 = TRUE) {
  
  if (p == "end") return(1)
  if (p %in% sc) {

    sc_vec[p] <- sc_vec[p] + 1L
  }
  id <- paste(p, paste0(sc_vec, collapse = ""), sep = "_")
  if (id %in% names(envir$res)) return(envir$res[id])
  if (any(sc_vec > 1) & part1) {
    res <- 0
  } else if (!part1 & (any(sc_vec > 2) | sum(sc_vec > 1) > 1)) {
    res <- 0
  } else{
    ne <- setdiff(map[map[,1] == p, 2], names(sc_vec[sc_vec > 1]))
    res <- if (length(ne) == 0) 0L else sum(sapply(ne,  count_paths, sc_vec = sc_vec, part1 = part1))
  }
  
  envir$res <- c(envir$res, setNames(res, id))
  return(res)
  
}

envir <- environment()
envir$res <- NULL
sc_vec <- setNames(rep(0, length(sc)), sc)
count_paths("start", sc_vec, TRUE)


#Part2
envir$res <- NULL
count_paths("start", sc_vec, FALSE)
