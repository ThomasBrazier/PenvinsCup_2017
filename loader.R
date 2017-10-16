ind <- read.table("b30ind.txt", header = TRUE, sep=",")
quad <- read.table("b30quad.txt", header = TRUE)
head(quad)

source("checkPenvins.R")

checkPenvins(quad)

file.show("tests_summary.txt")
