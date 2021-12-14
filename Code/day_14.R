# Adventofcode Day 14 Extended Polymerization
polymerization_rules <-
  read.table(
    url(
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
      check <-paste(paste(c(current_line[pos[1]], current_line[pos[2]]), collapse = ""))
      character_to_insert <- rules[which(rules == check), 3]
      next_line <-append(next_line, character_to_insert, after = (min(pos) + insert_pos))
      pos <- pos + 1
      insert_pos <- insert_pos + 1
    }
    current_line <- next_line
  }
  return(table(current_line))
}

poly_func(polymerization_rules, polymerization_line, 10)

#Part2
#Idea:pair is not unique ex:NN->NC,CN

