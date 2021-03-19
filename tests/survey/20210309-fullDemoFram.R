A <- array(0, dim = c(2, 2, 7, 3, 2, 20))
dimnames(A)[[1]] <- c("own", "rnt")
dimnames(A)[[2]] <- c("mal", "fem")
dimnames(A)[[3]] <- c("wh", "bl", "as", "aian", "pac", "oth", "twp")
dimnames(A)[[4]] <- c("0-17", "18-62", "62p")
dimnames(A)[[5]] <- c("hisp", "nonhips")
dimnames(A)[[6]] <- paste0(rep("tr", 20), 1:20)

(n  = prod(dim(A)))
(N  = 20*n)
int = rep(1, n)

own = factor(rep(0:1, n/2),   labels = c("own" , "rnt"))
sex = factor(rep(0:1, n/2),   labels = c("mal", "fem"))
rac = factor(rep(0:6, n/7),   labels = c("wh", "bl", "as", "aian", "pac", "oth", "twp"))
age = factor(rep(0:2, n/3),   labels = c("0-17", "18-62", "62p"))
his = factor(rep(0:1, n/2),   labels = c("hisp", "nonhips"))
geo = factor(rep(0:19, n/20), labels = paste0(rep("tr", 20), 1:20))
asx = interaction(age, sex)
sxg = interaction(sex, geo)
ahr = interaction(age, his, rac)


dfA <- reshape2::melt(A)
head(dfA)

demoFram = matrix(nrow = nrow(dfA), ncol = 1+1+6+2+1+19+5+39+41)

for(i in 1:nrow(dfA)){

  v = dfA[i, -ncol(dfA)]
  v = as.numeric(v)

  # Need to use value of sex[v[1]] and geo[v[4]] to put together the correct
  #    row of the sexgeo contrast form needed
  asxChar = paste(age[v[4]], sex[v[2]], sep = ".")
  sxgChar = paste(sex[v[2]], geo[v[6]], sep = ".")
  ahrChar    = paste(age[v[4]], his[v[5]], rac[v[3]], sep = ".")

  demoFram[i, ] <- c(
    contrasts(own)[v[1], ],
    contrasts(sex)[v[2], ],
    contrasts(rac)[v[3], ],
    contrasts(age)[v[4], ],
    contrasts(his)[v[5], ],
    contrasts(geo)[v[6], ],
    contrasts(asx)[agesexChar, ],
    contrasts(sxg)[sexgeoChar, ],
    contrasts(ahr)[ahrChar, ]
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


















