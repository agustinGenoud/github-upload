#####################
# Clean
rm(list=ls())
gc()

#####################
# Libraries & usr-functions
library(tidyverse)
library(readr)

etl <- dget("etl.R")
match <- dget("match.R")
merge <- dget("merge.R")


#####################
# Load Data

mock_db <- read_csv("MOCK_DATA.csv", col_names = FALSE)

df <- data.frame(
  name=character(),
  lname=character(),
  tel=character(),
  mail=character(),
  fecha_nac=character(),
  stringsAsFactors = F
)

df[1, ] <-  c("John", "Doe", "235-2635", "jdoe@yahoo.com", "17/12/1984")
df[2, ] <-  c("J.", "Doe", "234-4358", "jondoe@gmail.com", "17/12/1984")
df[3, ] <-  c("John", "D.", "235-2635", "jdoe@itba.edu.ar", "17/12/1984")
df[4, ] <-  c("Agustin", "Doe", "432-9973", "agenoud@itba.edu.ar", "17/12/2001")

df
typeof(df)


#####################
# ETL

data <- etl(df)
data


#####################
# SWOOSH

col <- length(data)
reg <- nrow(data)

for (i in 1:reg){
  
}



match(data[1,], data[2,])
match(data[1,], data[3,])
match(data[1,], data[4,])

match(data[2,], data[1,])
match(data[2,], data[3,])
match(data[2,], data[4,])

match(data[3,], data[1,])
match(data[3,], data[2,])
match(data[3,], data[4,])

match(data[4,], data[1,])
match(data[4,], data[2,])
match(data[4,], data[3,])


data[5,] <- merge(data[1,], data[3,])
data

match(data[1,], data[5,])
match(data[5,], data[3,])

data







