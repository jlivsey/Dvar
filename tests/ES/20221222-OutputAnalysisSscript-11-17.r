
#Output Analysis of 50 Simulation Replication Sent 11/17/22 by James
#===================================================================
#Eric Slud                                                11/18/22

# Imitate coding steps and script in OutputAnalysisScript10-07.RLog
#   but now develop these and expand them into utility scripts

## loaded 20221117-bottomUp-forEric-simdate20221105.RData
##   after sending from Census in pieces, combined R array with 50 reps into Rarr


attach("20221008-forEric-simdate20221008.RData")
load("~/GitHub/Dvar/tests/sim/20221105-sim/20221117-bottomUp-forEric-simdate20221105.RData")
Rarr = R
WLlist_fullLen = WLlist_fullLen
 ## recalll list of 12 logical vectors of length 9105 telling indices of cells in workloads
sapply(WLlist_fullLen, sum)
#[1] 7224 7310 8514 8772 8786 8982 9024 9028 9084 9096 9103 9105
X = X
dim(X)          ## matrix of 0's and 1's for constructing margins
#[1] 9105 7224


### Next count nonslack interior cells

   nonslac_int = array(0, c(11,50), dimnames=list(paste0("WL",2:12), paste0("Rep",1:50)))
   for(i in 1:11) nonslac_int[i,] =
      apply(abs(Rarr[,i+1,] - Rarr[,1,]) < 1e-08,2,sum)   ## same result with 1e-09
  t(nonslac_int)
#Rep1  7138 6029 5875 5871 5903 5909 5902 5900 5898 5903 5902
#Rep2  7138 6028 5854 5858 5899 5888 5890 5884 5884 5882 5873
#Rep3  7138 6023 5881 5875 5882 5882 5890 5886 5883 5883 5886
#Rep4  7138 6033 5877 5884 5917 5916 5899 5895 5902 5902 5887
#Rep5  7138 6035 5870 5890 5904 5910 5907 5896 5899 5897 5891
#...
#Rep46 7138 6033 5876 5878 5907 5916 5913 5903 5908 5904 5905
#Rep47 7138 6033 5894 5879 5904 5903 5899 5886 5886 5885 5885
#Rep48 7138 6035 5876 5876 5904 5901 5889 5889 5896 5894 5884
#Rep49 7138 6039 5876 5870 5887 5886 5882 5885 5876 5878 5873
#Rep50 7138 6036 5885 5881 5914 5912 5905 5901 5901 5900 5896

### There is a lot of consistency within workloads !!
round(rbind(mean=apply(nonslac_int,1,mean), sd=apply(nonslac_int,1,sd)), 1)
#      WL2    WL3    WL4    WL5    WL6    WL7    WL8    WL9   WL10   WL11   WL12
#mean 7138 6034.2 5877.9 5874.8 5902.4 5900.7 5897.6 5891.3 5891.7 5891.4 5890.4
#sd      0    9.5   10.0   10.3   11.4   11.0   10.5   10.2   12.4   11.3   12.6


  mar_nonslac_lst = NULL         ### this will be a list of 11 lists of length 50
  sim.names = paste0("sim",1:10)
  for (i in 1:11) {
      tmpind = which(WLlist_fullLen[[i+1]])[-(1:7224)]
      mar.obs = c(X[ tmpind, ] %*% c(A)) + noise[tmpind,1:50]
      mar.dif = X[ tmpind, ] %*% Rarr[,i+1,] - mar.obs     ## K x 50
      sim_lst = NULL
      for(j in 1:50) sim_lst = c(sim_lst, list(which(abs(mar.dif[,j]) < 1e-8)))
      names(sim_lst) = sim.names
      mar_nonslac_lst = c(mar_nonslac_lst, list(sim_lst)) }
  names(mar_nonslac_lst) = paste0("Run",2:12)

  nonslac_mar = array(0, c(11,50), dimnames=list(paste0("WL",2:12), paste0("Rep",1:50)))
  for(i in 1:11) nonslac_mar[i,] = sapply(mar_nonslac_lst[[i]], length)

  t(nonslac_mar)
#      WL2  WL3  WL4  WL5  WL6  WL7  WL8  WL9 WL10 WL11 WL12
#Rep1   86 1195 1349 1353 1321 1315 1322 1324 1326 1321 1322
#Rep2   86 1196 1370 1366 1325 1336 1334 1340 1340 1342 1351
#Rep3   86 1201 1343 1349 1342 1342 1334 1338 1341 1341 1338
#Rep4   86 1191 1347 1340 1307 1308 1325 1329 1322 1322 1337
#Rep5   86 1189 1354 1334 1320 1314 1317 1328 1325 1327 1333
#...
#Rep45  86 1199 1344 1350 1290 1296 1309 1309 1308 1316 1311
#Rep46  86 1191 1348 1346 1317 1308 1311 1321 1316 1320 1319
#Rep47  86 1191 1330 1345 1320 1321 1325 1338 1338 1339 1339
#Rep48  86 1189 1348 1348 1320 1323 1335 1335 1328 1330 1340
#Rep49  86 1185 1348 1354 1337 1338 1342 1339 1348 1346 1351
#Rep50  86 1188 1339 1343 1310 1312 1319 1323 1323 1324 1328

summary(c(nonslac_int+nonslac_mar))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   7224    7224    7224    7224    7224    7224    ## as expected


## The other thing we wanted to do is examine the nonslack margins that become
##   slack at the next run. This is, within each sim j, the count of
##   mar_nonslac_lst[[i]][[j]] elements NOT in mar_nonslac_lst[[i+1]][[j]]

newslack_mar = array(0, c(10,50), dimnames=list(paste0("WL",3:12), paste0("Rep",1:50)))
  for (i in 1:10) for (j in 1:50) newslack_mar[i,j] =
           length(setdiff(mar_nonslac_lst[[i]][[j]],mar_nonslac_lst[[i+1]][[j]]))

  t(newslack_mar)
      #WL3 WL4 WL5 WL6 WL7 WL8 WL9 WL10 WL11 WL12
#Rep1    0 165 170 298 354 331 185  339  393  389
#Rep2    0 142 158 312 319 323 190  318  381  361
#Rep3    0 150 173 278 323 332 183  330  368  367
#Rep4    0 159 179 309 326 330 182  340  379  370
#Rep5    0 146 188 285 350 338 179  350  391  357
#Rep6    0 172 185 296 337 326 190  306  364  384
#Rep7    0 156 169 287 355 334 177  332  362  372
#Rep8    0 152 151 299 317 323 175  298  353  367
#Rep9    0 168 185 289 341 336 182  326  375  380
#Rep10   0 162 159 313 324 328 175  320  371  383
#Rep11   0 149 162 302 332 311 199  332  379  376
#Rep12   0 158 160 298 338 330 210  349  389  369
#Rep13   0 161 174 283 340 315 186  334  353  381
#Rep14   0 172 171 276 332 348 181  333  370  357
#Rep15   0 167 170 316 334 332 215  338  364  358
#Rep16   0 165 164 324 342 330 216  339  384  385
#Rep17   0 155 179 275 342 335 181  329  368  372
#Rep18   0 161 163 302 331 344 174  348  382  361
#Rep19   0 141 161 296 339 337 197  318  360  364
#Rep20   0 148 167 305 320 331 181  327  375  355
#Rep21   0 174 174 293 348 345 197  332  396  372
#Rep22   0 154 170 292 327 342 177  339  358  376
#Rep23   0 160 176 311 328 344 177  316  370  377
#Rep24   0 151 178 308 342 341 193  342  384  379
#Rep25   0 146 178 289 341 330 197  326  362  379
#Rep26   0 171 161 300 324 331 172  348  348  361
#Rep27   0 164 169 294 346 319 178  330  369  374
#Rep28   0 174 186 296 336 359 179  342  379  372
#Rep29   0 154 155 293 324 322 199  331  373  359
#Rep30   0 151 167 274 351 326 205  333  364  355
#Rep31   0 146 183 296 341 324 172  357  365  368
#Rep32   0 183 189 312 340 339 183  328  380  376
#Rep33   0 147 165 279 343 327 188  330  386  358
#Rep34   0 160 169 287 334 339 214  345  395  362
#Rep35   0 151 170 310 324 337 189  329  359  353
#Rep36   0 156 169 293 328 335 184  340  361  379
#Rep37   0 143 163 313 337 336 185  328  379  384
#Rep38   0 152 165 312 328 325 193  330  367  357
#Rep39   0 149 162 311 334 353 189  336  371  373
#Rep40   0 157 169 301 314 319 174  324  362  365
#Rep41   0 172 180 285 316 334 170  339  381  371
#Rep42   0 166 189 296 350 341 182  354  393  386
#Rep43   0 156 178 300 328 338 192  336  371  366
#Rep44   0 170 191 316 337 323 183  347  386  375
#Rep45   0 152 170 329 346 353 203  364  384  370
#Rep46   0 163 179 294 345 350 176  339  387  377
#Rep47   0 170 167 296 342 346 200  344  364  362
#Rep48   0 163 163 288 327 319 179  348  380  375
#Rep49   0 156 176 280 334 327 201  336  359  349
#Rep50   0 165 172 311 348 339 186  352  396  362

#==============================================================================

## Many of the next steps involve quantifying the turnover of slack cells
##   from one replicate to another

  nonslac_chng = array(0, c(11,50,50))
  for(i in 1:11) {
      mat = (abs(Rarr[,i+1,] - Rarr[,1,]) > 1e-08)
      nonslac_chng[i,,] = outer(7224-nonslac_int[i,],
          7224-nonslac_int[i,],"+") - 2*(t(mat)%*% mat) }

## from this we learn the mean number changed across rep's

round(apply(nonslac_chng,1,sum)/(50*49),1)
# [1]  147.9 1756.4 2147.0 2152.6 2127.3 2131.0 2134.6 2142.9 2141.5 2142.2 2143.7

## A different analysis shows, by cell, the expected squared change
##  from one rep to another
  cell_msdiff = array(0, c(7224,11), dimnames=list(NULL, paste0("WL",2:12)))
  for(i in 1:11) {
      big = array(0,c(7224,50,50))
      mat = (abs(Rarr[,i+1,] - Rarr[,1,]) > 1e-08)
      for(j in 1:50) big[,j,] = (mat[,j] - mat)^2
      cell_msdiff[,i] = apply(big,1,sum)/(4900) }

round(apply(cell_msdiff,2,summary),3)
#          WL2   WL3   WL4   WL5   WL6   WL7   WL8   WL9  WL10  WL11  WL12
#Min.    0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
#1st Qu. 0.000 0.058 0.123 0.123 0.123 0.123 0.123 0.123 0.123 0.123 0.123
#Median  0.000 0.123 0.151 0.151 0.151 0.151 0.151 0.151 0.151 0.151 0.151
#Mean    0.010 0.122 0.149 0.149 0.147 0.147 0.148 0.148 0.148 0.148 0.148
#3rd Qu. 0.000 0.196 0.186 0.186 0.175 0.186 0.175 0.186 0.175 0.175 0.178
#Max.    0.249 0.255 0.255 0.255 0.253 0.255 0.253 0.253 0.253 0.253 0.249
    ### very stable across workloads ! meansq change between reps is 12% to 18%
    ### across cells

apply(cell_msdiff,2, function(col) sum(col>0))
# WL2  WL3  WL4  WL5  WL6  WL7  WL8  WL9 WL10 WL11 WL12
# 904 6459 7217 7222 7221 7221 7223 7223 7221 7221 7219
par(mfrow=c(3,4))
   for(i in 1:11)  hist(cell_msdiff[,i], prob=T,
          main=paste0("WL ",1+i), xlab="meansq diff")
   ### saved as "MeanSqSlacDiff.pdf"
 ### Can also plot mean-sq diff versus size
  par(mfrow=c(1,1))
  par(mfrow=c(3,4))
  for(i in 1:11)  plot(c(A),cell_msdiff[,i], main=paste0("WL ",1+i),
          xlab="size", ylab="meansq diff")
 ### surprisingly weak correlation with size
 for(i in 1:11) cat(round(cor(c(A),cell_msdiff[,i]),3),"  ")
#-0.045   -0.111   -0.034   -0.034   -0.018   -0.006   -0.007   0.002   -0.001   0.002   -0.007

## NOTE: that these calculations tally only how relatively often the cell estimate changes
##   slack status across rep's, not how large the changes are

  cell_mschng = array(0, c(7224,11), dimnames=list(NULL, paste0("WL",2:12)))
  for(i in 1:11) {
      big = array(0,c(7224,50,50))
      for(j in 1:50) big[,j,] = (Rarr[,i,j] - Rarr[,i,])^2
      cell_mschng[,i] = apply(big,1,sum)/(4900) }
  round(apply(cell_mschng,2,summary),3)
#           WL2     WL3    WL4    WL5    WL6    WL7    WL8    WL9   WL10   WL11   WL12
#Min.     1.552   1.621  1.621  1.766  2.094  1.902  2.016  1.983  1.851  1.653  2.307
#1st Qu.  4.305   4.437  4.981  5.518  5.616  5.573  5.606  5.563  5.626  5.627  5.618
#Median   5.340   5.612  6.436  6.933  7.072  6.931  6.973  6.974  7.030  7.027  6.996
#Mean     5.569   9.986  7.164  7.394  7.460  7.331  7.362  7.363  7.419  7.376  7.409
#3rd Qu.  6.544   7.194  8.489  8.765  8.865  8.691  8.731  8.688  8.754  8.705  8.798
#Max.    15.675 303.197 35.412 32.673 29.668 23.033 24.813 25.008 26.040 23.992 21.319

 for(i in 1:11) cat(round(cor(c(A),cell_mschng[,i]),3),"  ")
#-0.012  0.021  0.117  0.161  0.163  0.107  0.098  0.094  0.089  0.075  0.086
   ## now positive correlation
  par(mfrow=c(1,1))
  par(mfrow=c(3,4))
  for(i in 1:11)  plot(c(A),cell_mschng[,i], main=paste0("WL ",1+i),
          xlab="size", ylab="meansq chng")
    ### saved as "MeanSqChng.pdf"

### NEXT we can cross-tabulate entries in cell_mschng  or cell_msdiff by size-intervals,
##  which I do not expect to tell us much more than we can see in the saved scatterplots,
##  or we can try to cross-tabulate according to various properties of the 7224 interior
##  cells, such as how extreme are the various proportions for margins containing the cell.
## But I did not have time to do anything with that idea yet ...



#=============================================================================
## Suggestions for tallies:
# (i) Previously we discussed calculating (roots of) average variances
#    for "typical" or "large" or "small" interior cells and/or margins.
#    It makes sense to think of doing this for interior cells and margins
#    grouped into size-classes. (Initially we can create the size classes
#    from the y_true sizes.)
# (ii) We can also, as we go, tally some measure of the flux across some
#    number of runs (which we might make smaller than the total number of
#    runs from which we compute variances) of slack/nonslack status. Again,
#    these measures might be grouped according the size-classes of interior
#    cells or margins.
# (iii) A possible idea in relation to (iii) is to use these slack <-> nonslack
#    fluxes across limited numbers of runs as regression variables in
#    "predicting" either by regression or Machine-Learning techniques (using
#    gradient boosting or random forests or whatever.

## WE DISCUSSED THESE THINGS BRIEFLY IN OUR MEETING OF 10/13/2022.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  10/20/22
#  (iv) Look especially at WL8 -> WL9 transition of nonslack to slack using
#    size-classes to see what is going on

## Scratch from 12/8/22 for week's task ahead:
##     subsetting interior cells & margins with respect to characteristics
#
#  different level of implied errors based on intermediate workloads:
#  NOTES on programming task for next week (to meet 9:30am Thursday 12/14:
#    (a)  make lists to say which cells and margins are part of the specific margins
#      that we look at for implied errors at each workload stage;
#    (b)  will use these lists for two purposes,
#               (1) to tally what are the "nested" implied-error contrasts for
#            margins based on released interior-cell and margin data (based on
#            earlier and current workload), from all runs for all specific margins
#            at all workloads;
#               (2) to calculate for those same nested implied-error contrasts
#            what are their variances doe to the added double-exponential noise for
#            released data at all earlier and current workloads; these variances will
#            be calculated for all implied errors for ll

## Calculation (b)(2) uses epsilon-allocation schedule as well as the specific numbers
#     of levels of categorical variables summed over. Calculation b(1) will still look
#     at actual implied errors from simulated data (using actual noise values at interior
#     and marginal cell  released values at successive workload stages.

## NB: "implied error" for a higher-level margin means a contrast between the released
#     value of that margin and the sum of lover-level released interior cells or margins
#     ideally would sum up to the current released margin.

# EMPHASIZE THE CODING OF (a)  THIS WEEK !

#===============================================================================
## CODING IN PREPARATION FOR MEETING Thurs 12/15

## Open   20221117-bottomUp-forEric-simDate20221105.RData
## contans all the objects above.

## Recall object A is array 2x2x7x3x2x43, with dimensions
## OWNRENT, SEX, RACE, AGEGP3, HISP, TRACT

## Recall that we need to augment the dimensions to accommodate COUNTY, STATE
## This was done in script TablShell.RLog, using counties built from numbers
##    c(8,9,11,6,2,3,4) of tracts, and first 3 counties were State S1, last 4 were S2
## NB in this way, Geography is nested but do not have same number of lower-geography
##    levels within each higher-geography level. Could put 11 tract levels in each
##    county, 4 counties in each state, with many empty.

## Alternatively, we could equip each tract with county and state, through the array

TractArr = array(0, c(43,2), dimnames=list(paste0("tr",1:43), c("County","State")))
TractArr[,2] = rep(1:2, c(28,15))
TractArr[,1] = rep(1:7, c(8,9,11,6,2,3,4))

t(TractArr)
#       tr1 tr2 tr3 tr4 tr5 tr6 tr7 tr8 tr9 tr10 tr11 tr12 tr13 tr14 tr15 tr16
#County   1   1   1   1   1   1   1   1   2    2    2    2    2    2    2    2
#State    1   1   1   1   1   1   1   1   1    1    1    1    1    1    1    1
#       tr17 tr18 tr19 tr20 tr21 tr22 tr23 tr24 tr25 tr26 tr27 tr28 tr29 tr30
#County    2    3    3    3    3    3    3    3    3    3    3    3    4    4
#State     1    1    1    1    1    1    1    1    1    1    1    1    2    2
#       tr31 tr32 tr33 tr34 tr35 tr36 tr37 tr38 tr39 tr40 tr41 tr42 tr43
#County    4    4    4    4    5    5    6    6    6    7    7    7    7
#State     2    2    2    2    2    2    2    2    2    2    2    2    2


## Now decide to represent margins according to which variables are summed over,
##  and which fixed

## list  WLlist_fullLen contains 9105 logical entries for X-rows
##    in 12 successive workloads, where X is 9105 x 7224 incidence matrix

## Recall left-to-right dimensions in interior table A are
##   OWNRENT=HHGQ, SEX, RACE, AGE3, HISP, TRACT

## successive workloads are defined incrementally by:
#1  'trct_dtld'       interior cells only    7224
#2  'trct_hhgq'       OWNRENT x TRACT        2x43=86
#3  'trct_vhcr'       rac x vot x hisp x TR  7x2x2x43=1204
#4  'trct_agsx'       SEX x AGE3 x TRACT     2x3x43=258
#5  'cnty_hhgq'       OWNRENT x CTY          2x7 = 14
#6  'cnty_vhcr'       rac x vot x hisp x CTY 7x2x2x7 = 196
#7  'cnty_agsx'       SEX x AGE3 x CTY       2x3x7 = 42
#8  'stat_hhgq'       OWNRENT x ST           2x2 = 4
#9  'stat_vhcr'       rac x vot x hisp x ST  7x2x2x2 = 56
#10 'stat_agsx'       SEX x AGE3 x ST        2x3x2 = 12
#11 'cnty'            CTY                    7
#12 'state'           ST                     2

## NB. Misnomer to call 11 and 12 "detailed"
##     Also: "vot" differs from other categories in pooling two AGE3GP levels

## Problem now is to represent lower-level cells and margins in each released margin

## For example, the 3rd row among the 196 newly entering workload 6 (not in previous workloads)
##   should correspond to: Asian, Hisp, AGE3 levels 2:3, Cty1
##   and we can represent this margin as:  (+,+,3,2:3,1,1:8)   vector of 6 char-strings

## no lower-level margins are nested in this other than the constituent cells
## and these are nested in rac x hisp x vot  and  CTY

## the lower-level nested ones corresp. to rows of X with 1's a subset of the 1's in current row
## So create a list with a set for each row of all earlier rows with sets of 1's.
## However, the rows of X already show by TRUE values exactly which interior cells fall inside
##     a specified margin, so we create in our list only the earlier subsumed rows with # > 7224

MargNams = array("+", c(1881,6), dimnames=list(paste0("marg",1:1881),
        c("ownr","sex","rac","age3","hisp","tr")))
  MargNams[1:86, c(1,6)] = as.character(cbind(rep(1:2,43), rep(1:43, rep(2,43))))

  sapply(WLlist_fullLen,sum)-7224
#[1]    0   86 1290 1548 1562 1758 1800 1804 1860 1872 1879 1881

## another way to do this:
  library(reshape2)
  A.num = A
  dimnames(A.num) = list(1:2,1:2,1:7,1:3,1:2,1:43)

  MargNams[1:86,c(1,6)] = as.character(data.matrix(melt(apply(A.num,c(1,6),sum))[,1:2]))

## and we can generalize this second method
  MargNams[1290+1:258,c(2,4,6)] = as.character(data.matrix(melt(apply(A.num,c(2,4,6),sum))[,1:3]))

## But to continue this method, need to construct separate arrays of counties and states
##    before melting

  A.cty = array(0, c(2,2,7,3,2,7), dimnames=list(1:2,1:2,1:7,1:3,1:2,
    c("1:8","9:17","18:28","29:34","35:36","37:39","40:43")))
  trnums = cumsum(c(0,8,9,11,6,2,3,4))
  for(j in 1:7) A.cty[,,,,,j] = apply(A.num[,,,,,(trnums[j]+1):trnums[j+1]],1:5,sum)

  A.st = array(0, c(2,2,7,3,2,2), dimnames=list(1:2,1:2,1:7,1:3,1:2,c("1:28","29:43")))
  A.st[,,,,,1] = apply(A.num[,,,,,1:28],1:5,sum)
  A.st[,,,,,2] = apply(A.num[,,,,,29:43],1:5,sum)

  MargNams[1548+1:14,c(1,6)] = as.character(data.matrix(melt(apply(A.cty,c(1,6),sum))[,1:2]))
  MargNams[1800+1:4,c(1,6)] = as.character(data.matrix(melt(apply(A.st,c(1,6),sum))[,1:2]))
  dimnames(A.cty)[[6]]
#[1] "1:8"   "9:17"  "18:28" "29:34" "35:36" "37:39" "40:43"

 MargNams[1800+1:4,]
#         ownr sex rac age3 hisp tr
#marg1801 "1"  "+" "+" "+"  "+"  "1"
#marg1802 "2"  "+" "+" "+"  "+"  "1"
#marg1803 "1"  "+" "+" "+"  "+"  "2"
#marg1804 "2"  "+" "+" "+"  "+"  "2"
#
### need to fix these so that the char-strings underr "tr" are the dimnames from
### the 6th dimension, respectively of the A.cty and A.st arrays

  MargNams[1548+1:14,6] = dimnames(A.cty)[[6]][as.numeric(MargNams[1548+1:14,6])]
  MargNams[1548+1:14,]
#         ownr sex rac age3 hisp tr
#marg1549 "1"  "+" "+" "+"  "+"  "1:8"
#marg1550 "2"  "+" "+" "+"  "+"  "1:8"
#marg1551 "1"  "+" "+" "+"  "+"  "9:17"
#marg1552 "2"  "+" "+" "+"  "+"  "9:17"
#marg1553 "1"  "+" "+" "+"  "+"  "18:28"
#marg1554 "2"  "+" "+" "+"  "+"  "18:28"
#marg1555 "1"  "+" "+" "+"  "+"  "29:34"
#marg1556 "2"  "+" "+" "+"  "+"  "29:34"
#marg1557 "1"  "+" "+" "+"  "+"  "35:36"
#marg1558 "2"  "+" "+" "+"  "+"  "35:36"
#marg1559 "1"  "+" "+" "+"  "+"  "37:39"
#marg1560 "2"  "+" "+" "+"  "+"  "37:39"
#marg1561 "1"  "+" "+" "+"  "+"  "40:43"
#marg1562 "2"  "+" "+" "+"  "+"  "40:43"
#
  MargNams[1800+1:4,6] = dimnames(A.st)[[6]][as.numeric(MargNams[1800+1:4,6])]
  MargNams[1800+1:4,]
#         ownr sex rac age3 hisp tr
#marg1801 "1"  "+" "+" "+"  "+"  "1:28"
#marg1802 "2"  "+" "+" "+"  "+"  "1:28"
#marg1803 "1"  "+" "+" "+"  "+"  "29:43"
#marg1804 "2"  "+" "+" "+"  "+"  "29:43"

  MargNams[1758+1:42,c(2,4,6)] = as.character(data.matrix(melt(apply(A.cty,c(2,4,6),sum))[,1:3]))
  MargNams[1758+1:14,6] = dimnames(A.cty)[[6]][as.numeric(MargNams[1758+1:14,6])]

  MargNams[1860+1:12,c(2,4,6)] = as.character(data.matrix(melt(apply(A.st,c(2,4,6),sum))[,1:3]))
  MargNams[1860+1:12,6] = dimnames(A.st)[[6]][as.numeric(MargNams[1860+1:12,6])]

## Similarly can do CTY and ST workloads 11 and 12 resp by summing out all non-geo covariates
#    in A.cty and A.st

  MargNams[1872+1:7,6] = dimnames(A.cty)[[6]]
  MargNams[1879+1:2,6] = dimnames(A.st)[[6]]

  sapply(WLlist_fullLen,sum)-7224
#[1]    0   86 1290 1548 1562 1758 1800 1804 1860 1872 1879 1881

#So far filled in:

#1:86, XXX , 1291:1548, 1549:1562, XXX, 1759:1800, 1801:1804, XXX, 1861:1872,
#      1873:1879, 1880:1881

## The ones that have not been done are are the TR, CTY and ST margins involving vot,
##   and for that we need new arrays in which the 2:3 levels of AGE3 (dimension 4) are
##   rolled up into "vot"="2:3"

  A.num.vot = array(0, c(2,2,7,2,2,43), dimnames=list(1:2,1:2,1:7,c("1","2:3"),1:2,1:43))
  A.num.vot[,,,2,,] = apply(A.num[,,,2:3,,],4,sum)
  A.cty.vot = array(0, c(2,2,7,2,2,7), dimnames=list(1:2,1:2,1:7,c("1","2:3"),1:2,
           c("1:8","9:17","18:28","29:34","35:36","37:39","40:43")))
  A.cty.vot[,,,2,,] = apply(A.cty[,,,2:3,,],4,sum)
  A.st.vot = array(0, c(2,2,7,2,2,2), dimnames=list(1:2,1:2,1:7,c("1","2:3"),1:2,c("1:28","29:43")))
  A.st.vot[,,,2,,] = apply(A.st[,,,2:3,,],4,sum)

## Now with these arrays use same idea as before to fill in rac x hisp x vot x Geo workload labels

  MargNams[86+1:1204, 3:6] = as.character(data.matrix(melt(apply(A.num.vot,3:6,sum))[,1:4]))

  MargNams[1562+1:196, 3:6] = as.character(data.matrix(melt(apply(A.cty.vot,3:6,sum))[,1:4]))

  MargNams[1804+1:56, 3:6] = as.character(data.matrix(melt(apply(A.st.vot,3:6,sum))[,1:4]))

## and after doing this, we still need to convert numeric county and state levels
##  to the right character strings from dimnames(A.cty)[[6]] and dimnames(A.st)[[6]]
##  and also convert the vot level 2 to the string "2:3"

  MargNams[1562+1:196, 6] = dimnames(A.cty.vot)[[6]][as.numeric(MargNams[1562+1:196,6])]
  MargNams[1562+1:196, 4] = dimnames(A.cty.vot)[[4]][as.numeric(MargNams[1562+1:196,4])]

  MargNams[1804+1:56, 6] = dimnames(A.st.vot)[[6]][as.numeric(MargNams[1804+1:56,6])]
  MargNams[1804+1:56, 4] = dimnames(A.st.vot)[[4]][as.numeric(MargNams[1804+1:56,4])]

##------------- I THINK THAT COMPLETES THE FILLING-IN OF THE MargNams ARRAY
##
### NOTE FOLLOWING MEETING 12/22/22 WITH JAMES: HE HAS CODE TO PROVIDE EXACTLY
##    THE ORDERING OF X columns within Workloads. He thinks it should be exactly
##    the same as the c(A) index order. Must check this ...
##---------------------------------------------------------------------------------------

  MargLabs = apply(MargNams,1,paste0, collapse="|")
  MargLabs[150]
#      marg150
#"+|+|1|2|1|3"    ### so these labels are character strings
       ###  separating the column fields of MargNams by the symbol "|"

##======================================================================================
## Next steps week of 12/16 -- 12/22 was to fill in lists allowing exact calculation
##  of implied-margin contrasts, including for each margin only the contrasts of
##  it with each of the different sums of earlier (interior cells or) margins that
##  are nested within it.

 ## could do this by brute force by filling out the list based on checking earlier rows of X
 ## but it is easier to define NULL list component for rows in workloads 2:4
 ## then e.g. for each row in OWNRENT x CTY workload list component is set of
 ##    all OWNRENT x TR rows with TR in CTY, etc.

  ## This will be a list with one element for each of the 1881 margins containing
 ## the index numbers 7225:9105 of all of the earlier margins
 ## (but not the interior cells) nested within it.
 ## The margins for workloads 2:4 have no non-interior margins nested in them.
 ## The 14 margins in workload 5 have nested within them the magins in workload 2,
 ## specifically  2*(1:8)-1 within the 1st, 2*(1:8) within the 2nd,
 ##               2*(1:9)+15 within the 3rd, 2*(1:9)+16 within the 4th,
 ##               2*(1:11)+33 in the 5th, 2*(1:11)+34 in the 6th
 ##               2*(1:6)+55  in the 7th, 2*(1:6)+56  in the 8th
 ##               2*(1:2)+67  in the 9th, 2*(1:2)+68  in the 10th
 ##               2*(1:3)+71  in the 11th, 2*(1:3)+72  in the 12th
 ##               2*(1:4)+77  in the 13th, 2*(1:4)+78 in the 14th
 ## BUT THIS WOULD BE RIGHT ONLY IF THE WORLOAD ROWS WERE IN A-index order !!

 NestList = NULL
 for(i in 1:(86+1204+258)) NestList=c(NestList, list(NULL))  ## currently length 1548

 setdiff(which(WLlist_fullLen[[5]]),which(WLlist_fullLen[[4]]))
#   [1] 7320 7321 7322 7323 7324 7325 7326 7327 7328 7329 7330 7331 7332 7333
 apply(X[.Last.value,],1,sum)
# [1] 672 756 924 504 168 252 336 672 756 924 504 168 252 336

#------------------ now get ready to fill in non-null NestList components for
#------------------ margins numbered  1549  and beyond

 newWL = NULL                        ## this will be the list of new WL rows, WL2:12
            ## newWL[[i]] is the vector of all numbers 7225:9105
            ## newly entering workload i+1 that were not in workload i

 for(i in 1:11) newWL= c(newWL,
        list(setdiff(which(WLlist_fullLen[[i+1]]),which(WLlist_fullLen[[i]]))))

 sapply(newWL,length)
# [1]   86 1204  258   14  196   42    4   56   12    7    2       ### OK

## This object    newWL   is a list of 11 elements ---------------------------

                       ##  check for nesting within the current margin (row of X)
     ## by looking for earlier X rows that had 1's only where the current row does

 for(i in 1:14) {              ## this is Workload5
        vec = NULL
        for(j in newWL[[1]]) if(min(X[newWL[[4]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }              ### now length 1562

NestList[1549:1562]
NestList[1549:1562]
#[[1]]
#[1] 7234 7235 7236 7237 7238 7239 7240 7241
#
#[[2]]
#[1] 7242 7243 7244 7245 7246 7247 7248 7249 7250
#
#[[3]]
# [1] 7251 7252 7253 7254 7255 7256 7257 7258 7259 7260 7261
#
#[[4]]
#[1] 7262 7263 7264 7265 7266 7267
#
#[[5]]
#[1] 7268 7269
#
#[[6]]
#[1] 7270 7271 7272
#
#[[7]]
#[1] 7273 7274 7275 7276
#
#[[8]]
#[1] 7277 7278 7279 7280 7281 7282 7283 7284
#
#[[9]]
#[1] 7285 7286 7287 7288 7289 7290 7291 7292 7293
#
#[[10]]
# [1] 7294 7295 7296 7297 7298 7299 7300 7301 7302 7303 7304
#
#[[11]]
#[1] 7305 7306 7307 7308 7309 7310
#
#[[12]]
#[1] 7311 7312
#
#[[13]]
#[1] 7313 7314 7315
#
#[[14]]
#[1] 7316 7317 7318 7319

### This shows that we do get the right nesting, but the workload rows were
##  in an order where tract number moves first and OWNRENT second !!
## (James confirmed that in our meeting 12/22/22.)

 for(i in 1:196) {                 ### Workload 6
        vec = NULL
        for(j in newWL[[2]]) if(min(X[newWL[[5]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }
            ### this took a while, more than a minute, but is is a long loop 1204 x 196
           ## of operations with vectors of length 7224

 for(i in 1:42) {                  ### Workload 7
        vec = NULL
        for(j in newWL[[3]]) if(min(X[newWL[[6]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }              ## length 1800 at this point

### But now, with workloads 8:10, we begin finding more implied margin errors
##  because for example each OWNRENT x ST level can be aggregated directly either
##  from OWNRENT x TRACT levels or from OWNRENT x CTY levels

  for(i in 1:4) {                  ### Workload 8
        vec = NULL
        for(j in unlist(newWL[c(1,4)])) if(min(X[newWL[[7]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }

  for(i in 1:56) {                 ### Workload 9
        vec = NULL
        for(j in unlist(newWL[c(2,5)])) if(min(X[newWL[[8]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }

  for(i in 1:12) {                 ### Workload 10
        vec = NULL
        for(j in unlist(newWL[c(3,6)])) if(min(X[newWL[[9]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }            ### length 1872 now

## Now for county and state, MANY more cases to check:

  for(i in 1:7) {                  ### Workload 11
        vec = NULL
        for(j in unlist(newWL[1:6])) if(min(X[newWL[[10]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }

  for(i in 1:2) {                  ### Workload 12
        vec = NULL
        for(j in unlist(newWL[1:10])) if(min(X[newWL[[11]][i],]- X[j,]) >=0) vec=c(vec, j)
        NestList=c(NestList, list(vec))    }

length(NestList)
#[1] 1881
names(NestList) = 1:1881

### Unfortunately, we cannot yet use the MargLabs character-strings as labels.
###    The workload blocks of X[7224+(1:1881),] row-indices are now correctly indexed by newWL
###    but the order of variables within those blocks is not A-index order, which is the one
###    followed by MargNams and MargLabs.

#1  'trct_dtld'       interior cells only    7224
#2  'trct_hhgq'       OWNRENT x TRACT        2x43=86
#3  'trct_vhcr'       rac x vot x hisp x TR  7x2x2x43=1204
#4  'trct_agsx'       SEX x AGE3 x TRACT     2x3x43=258
#5  'cnty_hhgq'       OWNRENT x CTY          2x7 = 14
#6  'cnty_vhcr'       rac x vot x hisp x CTY 7x2x2x7 = 196
#7  'cnty_agsx'       SEX x AGE3 x CTY       2x3x7 = 42
#8  'stat_hhgq'       OWNRENT x ST           2x2 = 4
#  9  'stat_vhcr'       rac x vot x hisp x ST  7x2x2x2 = 56
#10 'stat_agsx'       SEX x AGE3 x ST        2x3x2 = 12
#11 'cnty'            CTY                    7
#12 'state'           ST                     2

sapply(NestList[1549:1881], length)
#1549 1550 1551 1552 1553 1554 1555 1556 1557 1558 1559 1560 1561 1562 1563 1564
#   8    9   11    6    2    3    4    8    9   11    6    2    3    4    8    8
#1565 1566 1567 1568 1569 1570 1571 1572 1573 1574 1575 1576 1577 1578 1579 1580
#   8    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8
#1581 1582 1583 1584 1585 1586 1587 1588 1589 1590 1591 1592 1593 1594 1595 1596
#   8    8    8    8    8    8    8    8    8    8    9    9    9    9    9    9
#1597 1598 1599 1600 1601 1602 1603 1604 1605 1606 1607 1608 1609 1610 1611 1612
#   9    9    9    9    9    9    9    9    9    9    9    9    9    9    9    9
#1613 1614 1615 1616 1617 1618 1619 1620 1621 1622 1623 1624 1625 1626 1627 1628
#   9    9    9    9    9    9   11   11   11   11   11   11   11   11   11   11
#1629 1630 1631 1632 1633 1634 1635 1636 1637 1638 1639 1640 1641 1642 1643 1644
#  11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11
#1645 1646 1647 1648 1649 1650 1651 1652 1653 1654 1655 1656 1657 1658 1659 1660
#  11   11    6    6    6    6    6    6    6    6    6    6    6    6    6    6
#1661 1662 1663 1664 1665 1666 1667 1668 1669 1670 1671 1672 1673 1674 1675 1676
#   6    6    6    6    6    6    6    6    6    6    6    6    6    6    2    2
#1677 1678 1679 1680 1681 1682 1683 1684 1685 1686 1687 1688 1689 1690 1691 1692
#   2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2
#1693 1694 16<THINK THAT95 1696 1697 1698 1699 1700 1701 1702 1703 1704 1705 1706 1707 1708
#   2    2    2    2    2    2    2    2    2    2    3    3    3    3    3    3
#1709 1710 1711 1712 1713 1714 1715 1716 1717 1718 1719 1720 1721 1722 1723 1724
#   3    3    3    3    3    3    3    3    3    3    3    3    3    3    3    3
#1725 1726 1727 1728 1729 1730 1731 1732 1733 1734 1735 1736 1737 1738 1739 1740
#   3    3    3    3    3    3    4    4    4    4    4    4    4    4    4    4
#1741 1742 1743 1744 1745 1746 1747 1748 1749 1750 1751 1752 1753 1754 1755 1756
#   4    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4
#1757 1758 1759 1760 1761 1762 1763 1764 1765 1766 1767 1768 1769 1770 1771 1772
#   4    4    8    8    8    8    8    8    9    9    9    9    9    9   11   11
#1773 1774 1775 1776 1777 1778 1779 1780 1781 1782 1783 1784 1785 1786 1787 1788
#  11   11   11   11    6    6    6    6    6    6    2    2    2    2    2    2
#1789 1790 1791 1792 1793 1794 1795 1796 1797 1798 1799 1800 1801 1802 1803 1804
#   3    3    3    3    3    3    4    4    4    4    4    4   31   19   31   19
#1805 1806 1807 1808 1809 1810 1811 1812 1813 1814 1815 1816 1817 1818 1819 1820
#  31   31   31   31   31   31   31   31   31   31   31   31   31   31   31   31
#1821 1822 1823 1824 1825 1826 1827 1828 1829 1830 1831 1832 1833 1834 1835 1836
#  31   31   31   31   31   31   31   31   31   31   31   31   19   19   19   19
#1837 1838 1839 1840 1841 1842 1843 1844 1845 1846 1847 1848 1849 1850 1851 1852
#  19   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19
#1853 1854 1855 1856 1857 1858 1859 1860 1861 1862 1863 1864 1865 1866 1867 1868
#  19   19   19   19   19   19   19   19   31   31   31   31   31   31   19   19
#1869 1870 1871 1872 1873 1874 1875 1876 1877 1878 1879 1880 1881
#  19   19   19   19  324  360  432  252  108  144  180 1155  724

## NOTE THAT THESE MARGINS ARE IN THE ORDER OF THE WORKLOADS USING A INDEXING ORDER
## and we need to supply the indices of rows of X
##-----------------------------------------------------------------------------------

## Now for every one of the 1881 margins, there are implied margin errors corresponding
##    to the interior cells (the rows of X indexed by  unlist(newWL) plus all the
## ways of obtaining the given margin as sums of earlier margins

## We create a new list, for 1881 margins, of the earlier vectors of row indices that
##   sum to that margin. For every margin, that includes the indices of 1's in
##   the corresponding rows of X. This involves cycling through our loops one more time,
##   from the beginning. The new data-structure is a list of 1881 lists of row-index vectors

 rowinds = unlist(newWL)
 RowIndList = NULL
 for(i in 1:(86+1204+258)) RowIndList=c(RowIndList, list(list(which(X[rowinds[i],]==1))))
 for(i in 1548+1:(14+196+42))   RowIndList=c(RowIndList, list(
                 list(which(X[rowinds[i],]==1), NestList[[i]])  ))
 for(i in 1:4) {
        vec1 = vec2 = NULL
        for(j in unlist(newWL[1])) if(min(X[newWL[[7]][i],]- X[j,]) >=0) vec1=c(vec1, j)
        for(j in unlist(newWL[4])) if(min(X[newWL[[7]][i],]- X[j,]) >=0) vec2=c(vec2, j)
        RowIndList=c(RowIndList, list(list(which(X[rowinds[1800+i],]==1),vec1, vec2)))    }

  for(i in 1:56) {
        vec1 = vec2 = NULL
        for(j in unlist(newWL[2])) if(min(X[newWL[[8]][i],]- X[j,]) >=0) vec1=c(vec1, j)
        for(j in unlist(newWL[5])) if(min(X[newWL[[8]][i],]- X[j,]) >=0) vec2=c(vec2, j)
        RowIndList=c(RowIndList, list(list(which(X[rowinds[1804+i],]==1),vec1, vec2)))    }

  for(i in 1:12) {
        vec1 = vec2 = NULL
        for(j in unlist(newWL[3])) if(min(X[newWL[[9]][i],]- X[j,]) >=0) vec1=c(vec1, j)
        for(j in unlist(newWL[6])) if(min(X[newWL[[9]][i],]- X[j,]) >=0) vec2=c(vec2, j)
        RowIndList=c(RowIndList, list(list(which(X[rowinds[1860+i],]==1),vec1, vec2)))    }

table(sapply(RowIndList, length))
#   1    2    3
#1548  252   72                      ##  OK, so far so good
      ## now cycle through the larger sets of earlier margins nested in CTY and STATE totals

  for(i in 1:7) {
        veclst = NULL
        for(k in 1:6) {
             vec=NULL
             for(j in unlist(newWL[k]))
                  if(min(X[newWL[[10]][i],]- X[j,]) >=0) vec=c(vec, j)
             veclst = c(veclst, list(vec)) }
        tmp0 = c(list(which(X[rowinds[1872+i],]==1)),veclst)
        RowIndList = c(RowIndList, list(tmp0)) }

  for(i in 1:2) {
        veclst = NULL
        for(k in 1:10) {
             vec=NULL
             for(j in unlist(newWL[k]))
                  if(min(X[newWL[[11]][i],]- X[j,]) >=0) vec=c(vec, j)
             veclst = c(veclst, list(vec)) }
        tmp0 = c(list(which(X[rowinds[1879+i],]==1)),veclst)
        RowIndList=c(RowIndList, list(tmp0)) }

table(sapply(RowIndList, length))
#   1    2    3    7   11
#1548  252   72    7    2                    ### OK, this is right !!
#
#1  'trct_dtld'       interior cells only    7224
#2  'trct_hhgq'       OWNRENT x TRACT        2x43=86
#3  'trct_vhcr'       rac x vot x hisp x TR  7x2x2x43=1204
#4  'trct_agsx'       SEX x AGE3 x TRACT     2x3x43=258
#5  'cnty_hhgq'       OWNRENT x CTY          2x7 = 14
#6  'cnty_vhcr'       rac x vot x hisp x CTY 7x2x2x7 = 196
#7  'cnty_agsx'       SEX x AGE3 x CTY       2x3x7 = 42
#8  'stat_hhgq'       OWNRENT x ST           2x2 = 4
#9  'stat_vhcr'       rac x vot x hisp x ST  7x2x2x2 = 56
#10 'stat_agsx'       SEX x AGE3 x ST        2x3x2 = 12
#11 'cnty'            CTY                    7
#12 'state'           ST                     2

#---------------
# A little bit of checking
which(X[rowinds[100],]==1)
#[1] 109 110 111 112                ## this is RowIndList[[100]][[1]]
      ## 100 indexes the 100th margin, the 14th in WL3, which sums only
  ## over the 4 entries in OWNRENT and SEX
table(sapply(RowIndList[1:86],function(lst) length(lst[[1]])))
#84
#86      ## OK because each of the OWNRENT x TR marginss is summed over 2x2x7x3 cells
table(sapply(RowIndList[86+1:1204],function(lst) length(lst[[1]])))
#  4   8
#602 602 ## OK because the rac x vot x hisp x TR cells with vot=1 are summed over 2x2
        ##  while those with vot=2 are also summed over the 2 age-groups of voting age

## -------------
# Next step is to use these indexed lists to create sets of implied errors in margins

## For this, begin with only the errors in newly released margins, by workload; recall that
#     for i in 1:11, newWL[[i]] contains the row indices for the new margins in WL i+1
     mar.obs = noise[ c(1:7224, unlist(newWL)), ]

## now for each margin j in 1:1881 and vector vec in its RowIndList
##     we record the difference between mar.obs[j,] and
##     the sum c(t(mar.obs[vec,])%*%rep(1,length(vec)))

## These differences are saved in list with components  K x 50 where K is the number of
##     different implied errors based on earlier margins (including) detailed cells.
##     As seen above, this number K depends on margin number i and is equal to
##        1 for i in 1:1548 , 2 for i in 1549:1800 , 3 for i in 1801:1872
##        7 for i in 1873:1879,   and   11  for i in 1880:1881

     MargDiff = NULL
     for(i in 1:1548) {
          newmarg = mar.obs[7224+i,]
          vec = RowIndList[[i]][[1]]
          MargDiff = c(MargDiff, list( array(newmarg -
              c(t(mar.obs[vec,])%*%rep(1,length(vec))),c(1,50)) ) )  }

## OK so far; but note that not all of these implied margin differences appear in the
##   logLikelihood for the least absolute discrepancy regression: only the one for
##   the current margin minus the sum of its detailed-cell constituents.

     for (i in 1548+(1:252)) {
          diffmat = NULL
          newmarg = mar.obs[7224+i,]
          for(k in 1:2) {
                vec = RowIndList[[i]][[k]]
                diffmat = rbind( diffmat, newmarg -
                       c(t(mar.obs[vec,])%*%rep(1,length(vec))) ) }
          MargDiff = c(MargDiff, list(diffmat)) }

table(sapply(MargDiff, nrow))
#   1    2
#1548  252            ### OK so far

     for (i in 1800+(1:72)) {
          diffmat = NULL
          newmarg = mar.obs[7224+i,]
          for(k in 1:3) {
                vec = RowIndList[[i]][[k]]
                diffmat = rbind( diffmat, newmarg -
                       c(t(mar.obs[vec,])%*%rep(1,length(vec))) ) }
          MargDiff = c(MargDiff, list(diffmat)) }

table(sapply(MargDiff, nrow))
#   1    2    3
#1548  252   72                    ### Again, as expected

   for (i in 1872+(1:7)) {
          diffmat = NULL
          newmarg = mar.obs[7224+i,]
          for(k in 1:7) {
                vec = RowIndList[[i]][[k]]
                diffmat = rbind( diffmat, newmarg -
                       c(t(mar.obs[vec,])%*%rep(1,length(vec))) ) }
          MargDiff = c(MargDiff, list(diffmat)) }


   for (i in 1879+(1:2)) {
          diffmat = NULL
          newmarg = mar.obs[7224+i,]
          for(k in 1:11) {
                vec = RowIndList[[i]][[k]]
                diffmat = rbind( diffmat, newmarg -
                       c(t(mar.obs[vec,])%*%rep(1,length(vec))) ) }
          MargDiff = c(MargDiff, list(diffmat)) }

table(sapply(MargDiff, nrow))

#   1    2    3    7   11
#1548  252   72    7    2

## MargDiff contains a list of arrays K x 50 for each margin i in 1:1881
##    where the number of contrasts K for each of 50 runs is the implied margin
##    difference for all the ways that nested margins sum to the i'th one

## Example: in the 1620 margin there are 2 such ways that unions of nested
##    margins coincide with the current one:

NestList[[1620]]
# [1] 8127 8155 8183 8211 8239 8267 8295 8323 8351 8379 8407
           ## these are the 11 rac x vot x hisp x TRACT margins adding up to
     ##  the 7224+1620 margin  which is a   rac x vot x hisp x CTY  margin

RowIndList[[1620]]         ## previous interior cells and row margins
                       ### providing implied margins contrasting with the 1620'th margin
#[[1]]
# [1] 2861 2862 2863 2864 3029 3030 3031 3032 3197 3198 3199 3200 3365 3366 3367 3368
#[17] 3533 3534 3535 3536 3701 3702 3703 3704 3869 3870 3871 3872 4037 4038 4039 4040
#[33] 4205 4206 4207 4208 4373 4374 4375 4376 4541 4542 4543 4544
#
#[[2]]
# [1] 8127 8155 8183 8211 8239 8267 8295 8323 8351 8379 8407
#
#
#### Now that we have filled this latest list of arrays, it may next help to create a
####   list of the margins associated with each interior cell. That would have been
###   easier if we could map the A-index ordering to the first 7224 rows of X.

MargDiff[[1620]]            ### the implied-margin contrasts:
               ## first row is the contrast from the sum of interior cells
               ## 2nd row is contrast from the sum of rac x vot x hisp x TRACT margins
#           [,1]      [,2]     [,3]      [,4]      [,5]      [,6]       [,7]      [,8]
#[1,] -23.228405 -13.31168 9.263764 -9.649150 -1.829044  6.801410  -8.833241 -29.44768
#[2,]   3.551393  17.04136 4.008390  4.876524 -4.165135 -1.030642 -12.390688  10.92918
#          [,9]      [,10]       [,11]     [,12]     [,13]      [,14]     [,15]
#[1,]  9.006602 -12.310912 16.23889382  7.553310 -7.410530  0.5051045 10.933184
#[2,] -3.997553  -4.515877 -0.09929269 -3.045575 -3.721203 -1.4090402  4.429048
#          [,16]    [,17]     [,18]      [,19]    [,20]     [,21]      [,22]      [,23]
#[1,] -14.411434 8.760040  5.803423 -0.7596261 1.403972 18.144667 -11.692537 28.5039374
#[2,]   7.219898 5.639815 -1.364733 -5.3877772 9.399058  5.986769  -3.580529 -0.5461449
#          [,24]     [,25]     [,26]     [,27]     [,28]    [,29]     [,30]     [,31]
#[1,] -50.189397 10.661616 17.464419 -6.054493 -20.50627 11.33984  3.856081 24.643019
#[2,]   2.830159  1.180432  3.048697  8.135988   1.22456 -9.27170 -4.072918  8.113832
#        [,32]     [,33]      [,34]     [,35]    [,36]      [,37]     [,38]      [,39]
#[1,] 8.997137 34.987146 -28.086480 -8.314622 7.297000 -12.483605 -3.548574 -28.834387
#[2,] 4.853604  0.688219  -3.181613  3.104619 2.384036  -4.828946  1.745575  -5.419929
#         [,40]    [,41]     [,42]      [,43]    [,44]    [,45]      [,46]     [,47]
#[1,] -15.74810 4.928993 -8.981500 -39.215051 8.576294 13.43941 -25.512343 -8.778985
#[2,] -11.68719 2.996520  4.074859   6.436117 5.253780 -2.36253   0.423954 -7.273695
#          [,48]     [,49]     [,50]
#[1,] -13.769185 -18.83200 11.101360
#[2,]  -7.196524  -1.71091 -7.323273
