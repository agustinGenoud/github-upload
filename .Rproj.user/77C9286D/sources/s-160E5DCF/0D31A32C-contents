# MATCH
match <- function(r1, r2){

  tmp1 <- NULL
  tmp2 <- NULL
  res <- list(name=NULL, lname=NULL, tel=NULL)
  
  for(a in 1:length(r1)){
    tmp1[a] <- strsplit(r1[[a]], " ")
    tmp2[a] <- strsplit(r2[[a]], " ")
    
    i_len <- length(tmp1[[a]])
    j_len <- length(tmp2[[a]])
    
     for(i in 1:i_len){
      for(j in 1:j_len){
        res[[a]] <- 0
        val_tmp <- setequal(tmp1[[a]][[i]], tmp2[[a]][[j]])
        
          if(val_tmp == TRUE){
            res[[a]] <- 1
          }
      }
    }
  }
  
  com <- sum(sum(res[[5]], res[[6]], res[[7]]) / 3, res[[3]]) / 2
  total <- sum(res[[1]], res[[2]], res[[4]], com) / 5
  
  out <- isTRUE(total > 0.5)
  return(out)
}