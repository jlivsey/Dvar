library(forcats) # for combining factor with fct_collapse()

n_tracts <- 43

A <- array(0, dim = c(2, 2, 7, 3, 2, n_tracts))
dimnames(A)[[1]] <- c("own", "rnt")
dimnames(A)[[2]] <- c("mal", "fem")
dimnames(A)[[3]] <- c("wh", "bl", "as", "aian", "pac", "oth", "twp")
dimnames(A)[[4]] <- c("0-17", "18-62", "62p")
dimnames(A)[[5]] <- c("hisp", "nonhisp")
dimnames(A)[[6]] <- paste0(rep("tr", n_tracts), 1:n_tracts)

(n  = prod(dim(A)))
(N  = 20*n)


# One-way margins
own = factor(rep(0:1, n/2),   labels = c("own" , "rnt"))
sex = factor(rep(0:1, n/2),   labels = c("mal", "fem"))
rac = factor(rep(0:6, n/7),   labels = c("wh", "bl", "as", "aian", "pac", "oth", "twp"))
age = factor(rep(0:2, n/3),   labels = c("0-17", "18-62", "62p"))
his = factor(rep(0:1, n/2),   labels = c("hisp", "nonhisp"))
geo = factor(rep(0:(n_tracts - 1), n/n_tracts),
             labels = paste0(rep("tr", n_tracts), 1:n_tracts))

# raceHispanicity margin
rac_his <- interaction(rac, his)
hi2 <- fct_collapse(rac_his,
                    hisp = c("wh.hisp",
                             "bl.hisp",
                             "as.hisp",
                             "aian.hisp",
                             "pac.hisp",
                             "oth.hisp",
                             "twp.hisp"  ),
                    nhwh = "wh.nonhisp",
                    nhbl = "bl.nonhisp",
                    nhas = "as.nonhisp",
                    nhaian = "aian.nonhisp",
                    nhpac = "pac.nonhisp",
                    nhoth = "oth.nonhisp",
                    nhtwp = "twp.nonhisp")

# Two-way interactions
asx = interaction(age, sex)
sxg = interaction(sex, geo)
orh = interaction(own, rac, his)
rag = interaction(rac, age, geo)
rhg = interaction(hi2, geo)
# ahr = interaction(age, his, rac)


dfA <- reshape2::melt(A)
head(dfA)


ncol_demoFram <- ncol(contrasts(own)) +
                 ncol(contrasts(sex)) +
                 ncol(contrasts(rac)) +
                 ncol(contrasts(age)) +
                 ncol(contrasts(his)) +
                 ncol(contrasts(geo)) +
                 ncol(contrasts(rhg))


demoFram = matrix(nrow = nrow(dfA), ncol = ncol_demoFram)

for(i in 1:nrow(dfA)){

  v = dfA[i, -ncol(dfA)]
  v = as.numeric(v)

  # Construct char vector for 2-way (or higher) margins
  # Example: Need to use value of sex[v[1]] and geo[v[4]] to put together the
  #          correct row of the sexgeo contrast form needed
  asxChar = paste(age[v[4]], sex[v[2]], sep = ".")
  sxgChar = paste(sex[v[2]], geo[v[6]], sep = ".")
  orhChar = paste(own[v[1]], rac[v[3]], his[v[5]], sep = ".")
  ragChar = paste(rac[v[3]], age[v[4]], geo[v[6]], sep = ".")
  rac_hisChar = paste(rac[v[3]], his[v[5]], sep = ".")
  isHisp = (v[5] == 1)
  rhgChar = if(isHisp){
    paste("hisp", geo[v[6]], sep = ".")
  } else{
    nhrace <- paste0("nh", rac[v[3]])
    paste(nhrace, geo[v[6]], sep = ".")
  }

  hi2Char = if(isHisp){
    "hisp"
  } else{
    nhrace <- paste0("nh", rac[v[3]])
    nhrace
  }
  #ahrChar    = paste(age[v[4]], his[v[5]], rac[v[3]], sep = ".")

  demoFram[i, ] <- c(
    contrasts(own)[v[1], ],
    contrasts(sex)[v[2], ],
    contrasts(rac)[v[3], ],
    contrasts(age)[v[4], ],
    contrasts(his)[v[5], ],
    contrasts(geo)[v[6], ],
    # contrasts(asx)[asxChar, ],
    # contrasts(sxg)[sxgChar, ],
    # contrasts(orh)[orhChar, ],
    # contrasts(rag)[ragChar, ]
    contrasts(rhg)[rhgChar, ]
    # contrasts(ahr)[ahrChar, ]
    # contrasts(hi2)[hi2Char, ]
  )

}

# add intercept column to demoFram
int = rep(1, n) # intercept
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



# save(demoFramReduced, colIdxSet, file = '~/github/Dvar/tests/survey/20210421-demoFramReduced-colIdxSet.Rdata')












