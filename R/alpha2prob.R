I <- c(2, 3, 4)
J
Jcheck(J)
lenJ <- sapply(J, length)
P <- array(dim = I)

# all indicies of P array (by construction all values are NA)
Pidx <- which(is.na(P), arr.ind = TRUE)

for(i in 1:nrow(Pidx)){

  x <- Pidx[i, ]

  Px <- 0

  for(k in 1:max(lenJ)) {  # loop over different length of J

    for(j in which(lenJ == k)){

      Px <- Px + A[[j]][matrix(x[J[[j]]], nrow = 1)]

    }


  }

  P[matrix(x, nrow = 1)] <- exp(Px) / (1 + exp(Px))

}

p <- seq(0, 1, .0001)
plot(log(p)~p)
