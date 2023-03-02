
#Script extending OutputAnalysisScript-11-17.RLog Computing Implied Margin Error Contrasts
#===========================================================================================
#
#Eric Slud    12/14/22ff

#===============================================================================
## CODING IN PREPARATION FOR MEETING Thurs 12/15

## Open   20221117-bottomUp-forEric-simDate20221105.RData
## contains all the objects above.
load("~/GitHub/Dvar/tests/sim/20221105-sim/20221117-bottomUp-forEric-simdate20221105.RData")

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
#$$    in A.cty and A.st

MargNams[1872+1:7,6] = dimnames(A.cty)[[6]]
MargNams[1879+1:2,6] = dimnames(A.st)[[6]]

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
