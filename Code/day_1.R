#Adventofcode Day 1
sonar_readings <-
  read.table(
    url(
      "https://raw.githubusercontent.com/czycs/adventofcode_2021/main/Input/input_1.txt"
    ),
    quote = "\"",
    comment.char = ""
  )
#Part1
count_higher <-
  sum(sonar_readings$V1[1:length(sonar_readings$V1) - 1] > sonar_readings$V1[2:length(sonar_readings$V1)])
#Part2
three_count <-  sum(sonar_readings$V1[4:length(sonar_readings$V1)] > sonar_readings$V1[1:(length(sonar_readings$V1)-3)])
#Theory sum(vec[1]+vec[2]+vec[3])>sum(vec[0]+vec[1]+vec[2])==sum(vec[3])>sum(vec[0])
