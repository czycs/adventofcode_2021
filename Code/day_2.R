#Adventofcode day2

position_instruction <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_2.txt"
    ),
    quote = "\"",
    comment.char = ""
  )

#Part1
pos1 <- function(x, y) {
  z = 1
  forw = 0
  depth = 0
  for (i in x) {
    if (i == "forward") {
      forw = forw + y[z]
    } else if (i == "down") {
      depth = depth + y[z]
    } else if (i == "up") {
      depth = depth - y[z]
    }
    z = z + 1
  }
  return(depth * forw)
}
#Result Part1
pos1(position_instruction[,1], position_instruction[,2])

#Part2

pos_aim <- function(x, y) {
  z = 1
  forw = 0
  depth = 0
  aim = 0
  for (i in x) {
    if (i == "forward") {
      if (aim == 0) {
        forw = forw + y[z]
      } else {
        depth = y[z] * aim + depth
        forw = forw + y[z]
      }
    } else if (i == "down") {
      aim = aim + y[z]
    } else if (i == "up") {
      aim = aim - y[z]
    }
    z = z + 1
  }

  return(depth * forw)
}

pos_aim(position_instruction[,1], position_instruction[,2])
