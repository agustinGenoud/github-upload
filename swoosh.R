rm(list=ls())
gc()

library(data.table)
library(tidyverse)

# ETL
etl <- function(dbl){
  
  dbl <- as.data.table(dbl)
  dbl <- dbl[ ,.(name=tolower(name),
                 lname=tolower(lname),
                 tel=tel,
                 mail=tolower(mail),
                 fecha_nac=fecha_nac), ]
  dbl[, name := gsub(" ", "", name), ]
  dbl[, name := gsub("\\.", "", name), ]
  dbl[, lname := gsub("\\.", "", lname), ]
  dbl[, tel := gsub("-","", tel), ]
  
  for (row in 1:nrow(dbl)){
    dbl[row, mail_a := strsplit(mail, "@")[[1]][1], ]
    dbl[row, mail_b := strsplit(strsplit(mail, "@")[[1]][2], "\\.")[[1]][1], ]
    dbl[row, mail_c := strsplit(strsplit(mail, "@")[[1]][2], "\\.")[[1]][2], ]
  }
  
  dbl[, mail := NULL, ]
  
  dbl[, fecha_nac := gsub("/", "", fecha_nac), ]
  
  return(dbl)
  
}




# MATCH
match <- function(r1, r2){
  nombre <- as.numeric(setequal(r1[, "name"], r2[, "name"]))
  apellido <- as.numeric(setequal(r1[, "lname"], r2[, "lname"]))
  
  tel <- as.numeric(setequal(r1[, "tel"], r2[, "tel"]))
  
  mail_a <- as.numeric(setequal(r1[, "mail_a"], r2[, "mail_a"]))
  mail_b <- as.numeric(setequal(r1[, "mail_b"], r2[, "mail_b"]))
  mail_c <- as.numeric(setequal(r1[, "mail_c"], r2[, "mail_c"]))
  
  fecha <- as.numeric(setequal(r1[, "fecha_nac"], r2[, "fecha_nac"])) * 2
  
  com <- sum(sum(mail_a, mail_b, mail_c) / 3, tel) / 2
  total <- sum(nombre, apellido, fecha, com) / 4
  
  out <- isTRUE(total > 0.5)
  return(out)
}

#as.numeric(setequal(t1[, "tel"], t2[, "tel"])) * 2


# MERGE
merge <- function(t1, t2){
  
  if (is.data.table(t1) == TRUE){
    n1 <- t1[, name]
    a1 <- t1[, lname]
    tl1 <- t1[, tel]
    m1 <- t1[, mail_a]
    f1 <- t1[, fecha_nac]
  }
  
  if (is.data.table(t2) == TRUE){
    n2 <- t2[, name]
    a2 <- t2[, name]
    tl2 <- t2[, tel]
    m2 <- t2[, mail_a]
    f2 <- t2[, fecha_nac]
  }
  
  t12 <- NULL
    
  if (n1 != n2){
    t12['name'] <- list(name=c(n1, n2))
  }
  
  if (a1 != a2){
    t12['lname'] <- list(lname=c(a1, a2))
  }
  if (tl1 != tl2) {
    t12['tel'] <- list(tel=c(tl1, tl2))
  }

  if (m1 != m2){
    t12['mail_a'] <- list(tel=c(m1, m2))
  }

  if (f1 != f2){
    t12['fecha_nac'] <- list(tel=c(f1, f1))
  }
  
  return(t12)

}


# Data
library(readr)
mock_db <- read_csv("MOCK_DATA.csv", col_names = FALSE)

l1 <-  c("John", "Doe", "235-2635", "jdoe@yahoo.com", "17/12/1984")
l2 <-  c("J.", "Doe", "234-4358", "jondoe@gmail.com", "17/12/1984")
l3 <-  c("John", "D.", "235-2635", "jdoe@itba.edu.ar", "17/12/1984")
l4 <-  c("Agustin", "Doe", "432-9973", "agenoud@itba.edu.ar", "17/12/2001")
db <-  rbind(l1, l2, l3, l4)
db <- as.data.table(db)
colnames(db) <- c("name", "lname", "tel", "mail", "fecha_nac")
typeof(db)
db

data <- etl(db)
data


# SWOOSH
t1 <- data[1,]
t2 <- data[2,]

data
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

x <- merge(merge(data[2,], data[1,]), data[3,])
c <- merge(data[2,], data[1,])

merge(data[1,], data[2,])

x <- merge(data[2,], data[1,])

merge(x, data[3,])
