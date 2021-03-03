
dfA <- melt(A)

demoFram = matrix(nrow = nrow(dfA), ncol = 24)

for(i in 1:nrow(dfA)){

  v = dfA[i, -ncol(dfA)]
  v = as.numeric(v)

  # Need to use value of sex[v[1]] and geo[v[4]] to put together the correct
  #    row of the sexgeo contrast form needed
  sexgeoChar = paste(sex[v[1]], geo[v[4]], sep = ".")

  demoFram[i, ] <- c(
                      contrasts(sex)[v[1], ],
                      contrasts(own)[v[2], ],
                      contrasts(rac)[v[3], ],
                      contrasts(geo)[v[4], ],
                      contrasts(sexgeo)[sexgeoChar, ]
                    )

}


# Are all rows unique?
temp <-  data.frame(demoFram) %>%
         group_by_all() %>%
         summarise(COUNT = n())
dim(temp)


is_eigen_zero <- function(M, thresh = 10^-8){
  decomp = eigen(t(M) %*% M)
  v = decomp$values
  return(any(v < thresh))
}



# What will be dimension of reduced demoFram?
nc = qr(demoFram)$rank

# Full set of column indices. Will remove those which are linearly dependent
colIdxSet <- 1:ncol(demoFram)

i = 2
while( i <= length(colIdxSet)){

  print(i)

  subMat = demoFram[, colIdxSet[1:i]]

  if(is_eigen_zero(subMat)){
      colIdxSet = colIdxSet[-i]
      next # don't increase i since you removed a column
  }

  i = i + 1 # Only increase i if all columns are linearly indep
}

# Check that my colIdxSet has same length as QR-rank
length(colIdxSet) == nc


demoFramReduced = demoFram[, colIdxSet]














