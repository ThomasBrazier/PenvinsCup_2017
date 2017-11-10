ind <- read.table("data/b30indCORR.txt", header = TRUE)
quad <- read.table("data/b30quadCORR.txt", header = TRUE)
head(quad)
head(ind)
str(ind)
str(quad)

source("checkPenvins.R")

checkDataFrame(ind)

checkRatio(ind)

checkPenvins(quad)

file.show("tests_summary.txt")
