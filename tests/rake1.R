# Example of method A from Erics write-up
# Fully specified probability array
# Want to map to parameters of log-linear model

library(survey)

# ficticious setup: (sex, race, geo) categories
#                      2,    3,  20  total levels in each category

I <- c(2, 3, 20)
P <- array(runif(prod(I)), dim = I)

# 1-tuples only
J <- list()
J[[1]] <- 1
J[[2]] <- 2
J[[3]] <- 3

A <- list()
A[[1]] <- NA
A[[2]] <- c(NA, NA)
A[[3]] <- rep(NA, 19)


nc <- 1 + (1 + 2 + 19) + (1*2 + 1*19 + 2*19) + (1*2*19)
demoFram <- matrix(NA, sum(I), nc)

frameVector <- function(x1, x2, x3){
  v1 <- rep(0, 2); if(x1 != 0) v1[x1] <- 1
  v2 <- rep(0, 3); if(x2 != 0) v2[x2] <- 1
  v3 <- rep(0, 20);if(x3 != 0) v3[x3] <- 1
  return(c(v1, v2, v3))
}

idx <- 0
for(i in 0:1){
  for(j in 0:2){
    for(k in 0:19){
      idx <- idx + 1
      demoFram[, idx] <- frameVector(i, j, k)
    }
  }
}

demoFram <- as.data.frame(demoFram)
Baswt <- rep(1, nrow(demoFram))


tmpdsgn = svydesign(~1, data=demoFram, weights=~Baswt)

column_names <- colnames(demoFram)
variable_string <- as.character(cat(column_names, sep="+"))

rakwt3 = calibrate(tmpdsgn,
                   formula(~paste("~", colnames,collapse="+")),
                   coltots,
                   calfun="raking")










