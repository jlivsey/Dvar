library(microbenchmark)

ericMethod <- function(){
Atmp1 = array(runif(60),c(3,4,5))
Atmp1[,,5] = NA; Atmp1[,4,] = NA; Atmp1[3,,]=NA

Anew = Atmp1
for(j in 1:3) Anew = Afill.Step(Anew,j)$Anew
}

jimMethod <- function(){
  Atmp1 = array(runif(60),c(3,4,5))
  Atmp1[,,5] = NA; Atmp1[,4,] = NA; Atmp1[3,,]=NA

  flag <- TRUE
  while(flag){
    out <- fill_na(Atmp1)
    Atmp1 <- out$Aa
    # na.list <- out$na.list
    flag <- out$flag
  }
}

m <- microbenchmark(ericMethod,
                    jimMethod,
                    times = 1000)
m
