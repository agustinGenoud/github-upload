# MERGE
merge <- function(t1, t2){

  t12 <- list(
    name=character(),
    lname=character(),
    tel=character(),
    fecha_nac=character(),
    ma=character(),
    mb=character(),
    mc=character()
  )
  
for(i in 1:length(t2)){
  if(t1[[i]] != t2[[i]]){
    t12[i] <- paste(t1[[i]], t2[[i]])
    } else{
      t12[i] <- t1[[i]]
      }
}
  
  df12 <- data.frame(
    name=t12[[1]],
    lname=t12[[2]],
    tel=t12[[3]],
    ma=t12[[4]],
    mb=t12[[5]],
    mc=t12[[6]],
    fecha_nac=t12[[7]],
    stringsAsFactors = FALSE
  )
  
  return(df12)
}
