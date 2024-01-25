dlt2 <- function(pi){
  if (length(pi) == 1){
    pbinom(1,3,pi, lower.tail = F) + (dbinom(1,3,pi)*pbinom(0,3,pi, lower.tail = F))
  } else {
    success = 1
    for (i in 1:length(pi)){
      if (i != length(pi)){
        success = success * (dbinom(0,3,pi[i]) + (dbinom(1,3,pi[i])*dbinom(0,3,pi[i])))
      } else {
        success = success * (pbinom(1,3,pi[i], lower.tail = F) + (dbinom(1,3,pi[i])*pbinom(0,3,pi[i], lower.tail = F)))
        
      }
      
    }
    success
  }
}



dlt2(0.15)
dlt2(c(0.15,0.2))
dlt2(c(0.15,0.2,0.25))
dlt2(c(0.15,0.2,0.25,0.3))
dlt2(c(0.15,0.2,0.25,0.3,0.33))
