
#Exploration of Basic Steps for ML in Laplace-noise infused contingency tables
#Observed with selected marginals
# ======================================================== E Slud, 1/15/2010 ff

#Begin with toy example, trying to get L1pack to provide ML values

#2x2 "true" table

# load(L1pack)   ### package previously installed locally
library(L1pack)
tab0 = array(c(15,3,4,8), c(2,2))
b=1/3
d0 = nrow(tab0)
tab0m = array(0, rep(d0+1,2))
tab0m[1:d0,1:d0]=tab0
tab0m[1:d0,d0+1]= tab0 %*% rep(1,d0)
tab0m[d0+1,1:d0] = t(tab0) %*% rep(1,d0)
tab0m[d0+1,d0+1] = sum(tab0m[1:d0,d0+1])
tab0m
# [,1] [,2] [,3]
# [1,]   15    4   19
# [2,]    3    8   11
# [3,]   18   12   30    ## four param's defined in order t11, t21, t12, t22
## same as vector indexing of tab0m[1:2,1:2]
### Also make order of rows of Cmat same as natural indexing of tab0m

aux0m = array(rlaplace(9, scale=b*sqrt(2)), c(3,3))
## LAPLACE density in L1pack has scale/sqrt(2) as dbl-exp scale param
datm = aux0m + tab0m


## Now use l1fit to get ML, noting that the (non-integer) table-entry
##   estimates with NO marginal data would be:
round(datm[1:2,1:2], 4)
# [,1]   [,2]
# [1,] 15.9210 3.7106
# [2,]  3.1111 8.4200

## compare fitted coeff's made into 2x2 table
round(array( l1fit(Cmat, c(datm), int=F)$coef, c(2,2) ), 4)
# [,1]   [,2]
# [1,] 15.3958 3.7106
# [2,]  3.1075 7.7369

### Looks good, took only 5 iterations !!!   Now look at 1000 simulations,
###     all from same table. Contrast variances with/without full set of marginals.

outest = array(0, c(1000,4,2))
noise = array(rlaplace(9000, scale=b*sqrt(2)), c(1000,9))
for(i in 1:1000) {
  datm[,] = tab0m + noise[i,]
  outest[i,,] = cbind(c(datm[1:2,1:2]), l1fit(Cmat, c(datm), int=F)$coef) }
## warnings of non-unique solution ...; very fast
mumat = round(apply(outest,2:3, mean),4)
# [,1]    [,2]
# [1,] 14.9999 14.9948
# [2,]  3.0022  2.9803
# [3,]  3.9967  3.9690
# [4,]  8.0271  7.9363       ### very small biases in 2nd col

rmsemat = round(sqrt(apply(outest^2,2:3,mean)-mumat^2),4)
# [,1]   [,2]
# [1,] 0.4374 0.3581
# [2,] 0.4474 0.3726
# [3,] 0.4785 0.3858
# [4,] 0.4564 0.3826          #### Nice !

## Now code this into a function that allows us to select set of marginals
## Inputs would be: Nrep, true table tab0, b, maximal "workload" matrix Cmat,
##    and set of marginals indexed from vectorized augmented tab0m.
## Outputs would be means and rmse's for estimated tab0 entry estimates.

Sim2Way = function(Nrep, intab, bpar, Wmat, margs) {
  ### margs must be a nonempty vector of integers
  dim0 = dim(intab)
  tabm = array(0, dim0+c(1,1))
  tabm[1:dim0[1],1:dim0[2]]=intab
  tabm[1:dim0[1],dim0[2]+1]= intab %*% rep(1,dim0[1])
  tabm[dim0[1]+1,1:dim0[2]] = t(intab) %*% rep(1,dim0[2])
  tabm[dim0[1]+1,dim0[2]+1] = sum(tabm[1:dim0[1],dim0[2]+1])
  ndat=length(margs) + prod(dim0)
  out = array(0, c(Nrep,prod(dim0)))
  ### prod(dim0) is the number of parameters to solve for as coeff's
  ### and their indices within vectorized tabm is given next
  ind0 = setdiff(1:(dim0[2]*(dim0[1]+1)), (dim0[1]+1)*(1:dim0[2]))
  if(length(intersect(margs,ind0))) {
    print("Error in specifying marginals!")
    break }
  indm = sort(c(ind0, margs))
  vecm = c(tabm)[indm]
  Bmat = Wmat[indm,]
  noise = array(rlaplace(Nrep*ndat, scale=bpar*sqrt(2)), c(Nrep,ndat))
  for(i in 1:Nrep) out[i,] = l1fit(Bmat, vecm+noise[i,], int=F)$coef
  mu = apply(out,2,mean)
  list(tabm=tabm, ndat=ndat, ind0=ind0, indm=indm, vecm=vecm,
       out=cbind(mean=mu, rmse=sqrt(apply(out^2,2,mean)-mu^2)))  }

set.seed(3333)
system.time({ tmp = Sim2Way(5000, tab0, 1/3, Cmat, c(3,6,7:9))$out })
# user  system elapsed
# 0.47    0.00    0.46
# There were 50 or more warnings (use warnings() to see the first 50)
round(tmp,4)
# mean   rmse
# [1,] 15.0067 0.3644
# [2,]  2.9787 0.3651
# [3,]  3.9733 0.3847
# [4,]  7.9233 0.3727

### Since this is so fast, let's run the sequence of simulations with various sets
###   of marginals

p1_p2 <- round(Sim2Way(5000, tab0, 1/3, Cmat, c(3,6))$out,4)
# mean   rmse                  ## +1 and +2 marginals only
# [1,] 14.9881 0.4870
# [2,]  2.6895 0.4813
# [3,]  4.0064 0.4617
# [4,]  7.6873 0.4812

p1_1p <- round(Sim2Way(5000, tab0, 1/3, Cmat, c(3,7))$out,4)
# mean   rmse                  ## +1 and 1+ marginals only
# [1,] 14.9970 0.3565
# [2,]  3.0065 0.4885
# [3,]  4.0045 0.4829
# [4,]  8.0019 0.4685

p1_1p_pp <- round(Sim2Way(5000, tab0, 1/3, Cmat, c(3,7,9))$out,4)
# mean   rmse                 ## +1, 1+ and ++ marginals
# [1,] 15.0010 0.3528
# [2,]  2.9978 0.3764
# [3,]  3.9986 0.3729
# [4,]  7.8147 0.4406

p1_p2_pp <- round(Sim2Way(5000, tab0, 1/3, Cmat, c(3,6,9))$out,4)
# mean   rmse
# [1,] 15.2345 0.4488
# [2,]  2.8063 0.4465
# [3,]  4.1978 0.4521
# [4,]  7.7672 0.4519

p1_p2_1p <- round(Sim2Way(5000, tab0, 1/3, Cmat, c(3,6,7))$out,4)
# mean   rmse
# [1,] 15.0002 0.3531
# [2,]  3.0105 0.4759
# [3,]  3.9999 0.3429
# [4,]  7.7750 0.4498

p1_p2_1p_2p <- round(Sim2Way(5000, tab0, 1/3, Cmat, c(3,6:8))$out,4)
# mean   rmse
# [1,] 14.9870 0.3551
# [2,]  2.9940 0.3454
# [3,]  3.9920 0.3574
# [4,]  8.0175 0.3645

all <- round(Sim2Way(5000, tab0, 1/3, Cmat, c(3,6:9))$out,4)
# mean   rmse
# [1,] 15.0038 0.3570
# [2,]  2.9816 0.3679
# [3,]  3.9787 0.3845
# [4,]  7.9132 0.3752

## So it does not look as though the table-total marginal helps, although that
##   might be due to the way that l1fit chooses asmong the nonunique solutions.

