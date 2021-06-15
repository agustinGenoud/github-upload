# ETL
etl <- function(dbl){
  dbl <- df
  
  dbl <- map_df(dbl, tolower) 
  dbl <- map_df(dbl, gsub, pattern=" ", replacement="",) 
  
  tmp <- unlist(strsplit(dbl$mail, split="@"))
  cols <- c("mail_a", "mail_b")
  nC <- length(cols)
  ind <- seq(from=1, by=nC, length=nrow(dbl))
  for(i in 1:nC) {
    dbl[, cols[i]] <- tmp[ind + i - 1]
  }
  
  tmp <- unlist(strsplit(dbl$mail_b, split="\\."))
  cols <- c("mail_b", "mail_c")
  nC <- length(cols)
  ind <- seq(from=1, by=nC, length=nrow(dbl))
  for(i in 1:nC) {
    dbl[, cols[i]] <- tmp[ind + i - 1]
  }
  
  dbl$mail <- NULL
  
  dbl <- map_df(dbl, gsub, pattern="\\.", replacement="",) 
  dbl <- map_df(dbl, gsub, pattern="-", replacement="",) 
  dbl <- map_df(dbl, gsub, pattern="/", replacement="",) 
  
  return(dbl)
  
}