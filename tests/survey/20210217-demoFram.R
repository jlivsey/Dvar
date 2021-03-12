A <- array(0, dim = c(2, 2, 4, 7))
dimnames(A)[[1]] <- c("mal", "fem")
dimnames(A)[[2]] <- c("own", "rnt")
dimnames(A)[[3]] <- c(letters[1:4])
dimnames(A)[[4]] <- c(LETTERS[1:7])

(n  = 2*2*4*7)
(N  = 20*n)
int = rep(1, n)

sex = factor(rep(0:1, n/2), labels = c("mal", "fem"))
own = factor(rep(0:1, n/2), labels = c("own" , "rnt"))
rac = factor(rep(0:3, n/4), labels = c("wht", "blc", "pac", "oth"))
geo = factor(rep(0:6, n/7), labels = c("ge1", "ge2", "ge3", "ge4", "ge5", "ge6", "ge7"))
sxg = interaction(sex, geo)

sex = factor(rep(0:1, n/2), labels = c("male", "female"))
own = factor(rep(0:1, n/2), labels = c("own" , "rent"))
rac = factor(rep(0:3, n/4))
geo = factor(rep(0:6, n/7))
sxg = interaction(sex, geo)

dfA <- melt(A)
colnames(dfA) <- c("sex", "own", "rac", "geo", "value")
head(dfA)


demoFram = matrix(nrow = nrow(dfA), ncol = 1+1+3+6+13)

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
                      contrasts(sxg)[sexgeoChar, ]
                    )

}

# add intercept column to demoFram
demoFram = cbind(int, demoFram)

head(demoFram)

# Are all rows unique?
# temp <-  data.frame(demoFram) %>%
#          group_by_all() %>%
#          summarise(COUNT = n())
# dim(temp)


is_eigen_zero <- function(M, thresh = 10^-8){
  decomp = eigen(t(M) %*% M)
  v = decomp$values
  return(any(v < thresh))
}

# What will be dimension of reduced demoFram?
nc = qr(demoFram)$rank
nc

# Full set of column indices. Will remove those which are linearly dependent
colIdxSet <- 1:ncol(demoFram)

i = 2
while( i <= length(colIdxSet)){

  # print(i)

  subMat = demoFram[, colIdxSet[1:i]]

  if(is_eigen_zero(subMat)){
      colIdxSet = colIdxSet[-i]
      next # don't increase i since you removed a column
  }

  i = i + 1 # Only increase i if all columns are linearly indep
}

# Check that my colIdxSet has same length as QR-rank
length(colIdxSet) == nc

# Removed columns
(1:ncol(demoFram))[-colIdxSet]

demoFramReduced = demoFram[, colIdxSet]


















