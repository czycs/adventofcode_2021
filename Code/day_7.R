#Adventofcode Day 7

crab_subs <-
  read.table(
    "C:/Users/lagij/Desktop/adventofcode/input_7.txt",
    sep = ",",
    quote = "\"",
    comment.char = ""
  )
crab_subs_vec <- as.numeric(crab_subs[1, ])
#plot(density(crab_subs_vec))

testinput <- c(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

#Part1
median(testinput)
median(crab_subs_vec)


#Part2
fuel_usage <- function(vec) {
  least_fuel = 1000000000
  for (z in min(vec):max(vec)) {
    fuel = 0
    
    for (i in vec) {
      #Part1:
      #fuel <- fuel + abs(z - i)
      #Part2:
      fuel <- fuel + sum(1:abs(z - i))
      if (fuel > least_fuel) {
        break
      }
    }
    if (fuel < least_fuel) {
      least_fuel <- fuel
      #print(z)
    }
  }
  return(least_fuel)
  
}


fuel_usage(testinput)
fuel_usage(crab_subs_vec)
