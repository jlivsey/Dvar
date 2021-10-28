library(L1pack)
options(warn = -1)

#####################################  10/14/21 EVS

### let's look at 3x3 with 2 margins

bpar = 1
bpar_mar = 3
bpar_vec = c(rep(bpar,9), rep(bpar_mar,6))

workld = rbind(diag(9),
               rep(c(1,0,0),3),
               rep(c(0,1,0),3),
               rep(c(0,0,1),3),
               c(1,1,1,rep(0,6)),
               c(0,0,0,1,1,1,0,0,0),
               c(rep(0,6),1,1,1))     ### 15 workload rows

x = sample(1:10, 9)

noise = (2 * rbinom(15, 1, 1/2)-1)*rexp(15)*bpar_vec

xDP = c(workld %*% x + noise)

df <- data.frame(noise = noise,
                 xDP   = xDP,
                 true  = workld %*% x + noise)
print(df)

NonWgtEst = l1fit(workld, xDP, int=F)

WgtdEst = l1fit((1/bpar_vec)*workld, xDP/bpar_vec, int=F)

round(rbind(xDP[1:9],NonWgtEst$coef, WgtdEst$coef),5)

## solutions disagree only in entries 1, 5, 7, 9

### So the issue seems to be that there is a non-unique solution for the
##    unweighted problem

sum(abs(c(workld  %*% NonWgtEst$coef - xDP)))        ###  15.76867
sum(abs(c(workld  %*% WgtdEst$coef - xDP)))          ###  21.85907

sum(abs(c(workld  %*% NonWgtEst$coef - xDP)/bpar_vec))        ###   9.31649
sum(abs(c(workld  %*% WgtdEst$coef - xDP)/bpar_vec))          ###   7.28636

## This says that each of the solutions (wghtd or unwghtd) is definitely
##   better for its own objective function.

l1fit((1/(bpar_vec+.01))*workld, xDP/(bpar_vec+.01), int=F)$coef
u9 = runif(15,.01)
l1fit((1/(bpar_vec+u9))*workld, xDP/(bpar_vec+u9), int=F)$coef
l1fit((1/(1+u9))*workld, xDP/(1+u9), int=F)$coef

## Solutions quite sensitive to small changes in weights !!?

bpar_vc2 = c(rep(1,9),rep(2,6))
l1fit((1/bpar_vc2)*workld, xDP/bpar_vc2, int=F)$coef

## So we are seeing many different solutions based on small or large weight modifications
##   This is not exactly the pattern we saw in the toy 2-cell 1-margin example.

## Let's look at the original problem with either bpar_mar=1 (ie unweighted or bpar_mar =2 or 3)
##  and tally the results of 1000 replications as Jim did before. The tally will give the
##  prob vector of relative freq's with which the entries of xDP and the $coef solution differ.

SlackMat = array(T, c(1000,9,3), dimnames=list(NULL,1:9,paste0("bmar=",1:3)))

for(i in 1:1000) {
  noisT = (2 * rbinom(15, 1, 1/2)-1)*rexp(15)*bpar_vec
  yDP = c(workld %*% x + noisT)
  SlackMat[i,,1] = (abs(yDP[1:9] - l1fit(workld, yDP, int=F)$coef)>1e-5)
  SlackMat[i,,2] = (abs(yDP[1:9] - l1fit((1/bpar_vc2)*workld, yDP/bpar_vc2, int=F)$coef)>1e-5)
  SlackMat[i,,3] = (abs(yDP[1:9] - l1fit((1/bpar_vec)*workld, yDP/bpar_vec, int=F)$coef)>1e-5) }

dim(SlackMat)
apply(SlackMat,2:3,mean)

## Interesting: this makes the outcome not a paradox about linear programming
##    but a statement about the key importance of the choice of marginal weights !!

## NB in all of these examples, the marginal noise was large. We could do this again
##   with marginal noise generated with smaller scale:

SlackMat = array(T, c(1000,9,3), dimnames=list(NULL,1:9,paste0("bmar=",1:3)))

for(i in 1:1000) {
  noisT = (2 * rbinom(15, 1, 1/2)-1)*rexp(15)
  yDP = c(workld %*% x + noisT)
  SlackMat[i,,1] = (abs(yDP[1:9] - l1fit(workld, yDP, int=F)$coef)>1e-5)
  SlackMat[i,,2] = (abs(yDP[1:9] - l1fit((1/bpar_vc2)*workld, yDP/bpar_vc2, int=F)$coef)>1e-5)
  SlackMat[i,,3] = (abs(yDP[1:9] - l1fit((1/bpar_vec)*workld, yDP/bpar_vec, int=F)$coef)>1e-5) }

apply(SlackMat,2:3,mean)
  ### Pattern is quite similar !!?

### Now do it again with DP data that is much noisier at the margins

for(i in 1:1000) {
  noisT = (2 * rbinom(15, 1, 1/2)-1)*rexp(15)*c(rep(1,9),rep(5,6))
  yDP = c(workld %*% x + noisT)
  SlackMat[i,,1] = (abs(yDP[1:9] - l1fit(workld, yDP, int=F)$coef)>1e-5)
  SlackMat[i,,2] = (abs(yDP[1:9] - l1fit((1/bpar_vc2)*workld, yDP/bpar_vc2, int=F)$coef)>1e-5)
  SlackMat[i,,3] = (abs(yDP[1:9] - l1fit((1/bpar_vec)*workld, yDP/bpar_vec, int=F)$coef)>1e-5) }

apply(SlackMat,2:3,mean)

### The overall conclusion has to do with the effect of magnitudes of marginal weights.
### My conjecture is that when the weights are all equal in the interior cells and are
###    as large as the number on interior cells in each margin, then the marginal noise
###    ceases to play a role in the L1fit solutions.

## The situation in decennial census data may be that there are a few marginal factors
##    with many levels and not too large weights and these may play a real role in
##    the modification of MLE cell-inferences.



