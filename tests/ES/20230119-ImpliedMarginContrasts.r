
#Script extending OutputAnalysisScript-11-17.RLog Computing Implied Margin Error Contrasts
#===========================================================================================
#
#Eric Slud    12/14/22ff

#===============================================================================
## CODING IN PREPARATION FOR MEETING Thurs 12/15

## Open   20221117-bottomUp-forEric-simDate20221105.RData
## contains all the objects above.

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
 #[1] 8127 8155 8183 8211 8239 8267 8295 8323 8351 8379 8407
           ## these are the 11 rac x vot x hisp x TRACT margins adding up to
     ##  the 7224+1620 margin  which is a   rac x vot x hisp x CTY  margin

RowIndList[[1620]]         ## previous interior cells and row margins
#                       ### providing implied margins contrasting with the 1620'th margin
#[[1]]
# [1] 2861 2862 2863 2864 3029 3030 3031 3032 3197 3198 3199 3200 3365 3366 3367 3368
#[17] 3533 3534 3535 3536 3701 3702 3703 3704 3869 3870 3871 3872 4037 4038 4039 4040
#[33] 4205 4206 4207 4208 4373 4374 4375 4376 4541 4542 4543 4544
#
#[[2]]
# [1] 8127 8155 8183 8211 8239 8267 8295 8323 8351 8379 8407


### Now that we have filled this latest list of arrays, it may next help to create a
###   list of the margins associated with each interior cell. That would have been
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
#
#------------------------------------------------------------------------------- 1/11/23
# Next step descriptive: how to examine all of the implied-margin contrasts of different types--
#   do the distributions of the contrasts differ systematically for different heights of the
#   constituent cells or margins being aggregated to a high-level margin ??

cum.mar = cumsum(sapply(newWL, length))
# [1]  86 1290 1548 1562 1758 1800 1804 1860 1872 1879 1881

  WLgrp = rep(2:12, sapply(newWL, length))

## Look first at the overall sizes of implied margin differences for the margins themselves
##   versus the detailed-cells making them up

  tmp = lapply(MargDiff, function(arr) arr[1,])
  mar1 = array(0, c(1881,50))
  for (i in 1:1881) mar1[i,] = tmp[[i]]
  boxplot(mar1 ~ WLgrp, main=           ### interesting picture
          "Implied Margin Differences for Detailed-Cell Sums, by WL")
    ### saved as "BoxCellPic.pdf"

## Now generate similar picture, boxplot showing CTY & ST higher-order margin implied
##    differences wrt Tract-level margin sums

  tmp = lapply(MargDiff[1549:1881], function(arr) arr[2,])
  mar2 = array(0, c(333,50))
  for (i in 1:333) mar2[i,] = tmp[[i]]
  boxplot(mar2 ~ WLgrp[1549:1881], main=           ### another interesting picture
          "Implied Margin Differences for Tract-Margin Sums, by WL")
       ### saved as "BoxTractMargPic.pdf"

  tmp = lapply(MargDiff[1801:1881], function(arr) arr[3,])
  mar3 = array(0, c(81,50))
  for (i in 1:81) mar3[i,] = tmp[[i]]
  boxplot(mar3 ~ WLgrp[1801:1881], main=
          "Implied Margin Differences for Tract-Margin Sums, by WL")
       ### saved as "BoxCtyMargPic.pdf"

### Main finding here is that the implied margin differences are actually largest
###   in the first of the 3 boxplots, related to mar1

## AT THE HIGHEST LEVEL MARGINALS  WE MUST ALSO BE SEEING THE EFFECT OF THE EPSILON-
##  PARTITION SCHEDULE !! James made this comment, and we should want to work this
##  in to the regression below of log abs margdiff's on other size-type and
##  WL-type variables

#------------------------------------------------------------------------
# Next ask: can we see a direct relationship between margin size and implied margin diff ?

trumarg = X[7224 +(1:1881),] %*% c(A)
  summary(lm(abs(c(t(mar1))) ~ rep(trumarg, rep(50,1881))))$coef
#                                Estimate   Std. Error   t value     Pr(>|t|)
#(Intercept)                 8.4748118520 4.426421e-02 191.45970 0.000000e+00
#rep(trumarg, rep(50, 1881)) 0.0001484183 1.069978e-05  13.87115 1.046381e-43

### There is a very "significant" relation between the margin size and
##   the implied margin difference, but it is not easily visible from the scatterplot
plot(rep(trumarg, rep(50,1881)), abs(c(t(mar1))))

#------------------------------------------------------------------------
## The boxplots above seemed to show weak relationship between scale
#    of margin difference and workload. Now look at this in terms of SD

 plot(WLgrp,apply(mar1,1,sd), ylab="SD of MargDiff", main=
   "SD of Implied Margin Differences for Detailed-Cell Sums, by WL")
          ### Gives a different impression!  SD scale definitely increases with workload!
     ## saved as "MargDiffSD-by-WL.pdf"

## This suggests that in data from WL 2:10 we can get a clearer picture using an additive effect ##   from the different margin types (as in WL 2:4, 5:7, 8:10), using log SD

tmplm = lm(log(abs(c(t(mar1)))) ~ rep(trumarg, rep(50,1881)) + rep((WLgrp-2) %% 3,
         rep(50,1881)))
summary(tmplm)$coef
#                                       Estimate   Std. Error   t value     Pr(>|t|)
#(Intercept)                        1.324900e+00 1.109249e-02 119.44124 0.000000e+00
#rep(trumarg, rep(50, 1881))        1.461019e-05 1.041470e-06  14.02843 1.158722e-44
#rep((WLgrp - 2)%%3, rep(50, 1881)) 1.337343e-01 9.128773e-03  14.64976 1.529556e-48

## look at residuals
  plot(tmplm$fitted, tmplm$resid)

### ACTUALLY SHOULD HAVE OMITTED WL 11:12  FROM THIS ANALYSIS !

  margsiz = rep(trumarg[1:1872], rep(50,1872))
  margfac = factor(rep((WLgrp[1:1872]-2) %% 3, rep(50,1872)))
tmplm2 = lm(log(abs(c(t(mar1[1:1872,])))) ~ margsiz + margfac)
summary(tmplm2)$coef
#                 Estimate   Std. Error    t value      Pr(>|t|)
#(Intercept)  2.585303e+00 1.721192e-02 150.204255  0.000000e+00
#margsiz      1.582431e-06 9.978625e-07   1.585821  1.127834e-01
#margfac1    -1.315064e+00 1.762899e-02 -74.596677  0.000000e+00
#margfac2    -5.372643e-01 1.970158e-02 -27.270117 4.187749e-163

  ## CONCLUSION: IN THIS SETTING, True size of Margin dooes not matter at all,
  #     only the type of MARGIN: margfac 0, 1, 2 respectively indicate that the
  #     respective variable-combinations
  #                OWNRENT ,   rac x vot x hisp , or  SEX x AGE3
  #     were used (at  geo  level Tract, CTY or ST) to create margins

tmplm2A = lm(log(abs(c(t(mar1[1:1872,])))) ~ log1p(margsiz) + margfac)
summary(tmplm2A)$coef
#                  Estimate  Std. Error    t value      Pr(>|t|)
#(Intercept)     2.59703546 0.019241037 134.973781  0.000000e+00
#log1p(margsiz) -0.00080533 0.001437701  -0.560151  5.753778e-01
#margfac1       -1.32289343 0.017929332 -73.783754  0.000000e+00
#margfac2       -0.54673034 0.020217132 -27.042922 1.923712e-160

tmplm2B = lm(log1p(abs(c(t(mar1[1:1872,])))) ~ log1p(margsiz) + margfac)
summary(tmplm2B)$coef
#                   Estimate  Std. Error     t value      Pr(>|t|)
#(Intercept)     2.745276663 0.013118942 209.2605175  0.000000e+00
#log1p(margsiz) -0.000828158 0.000980255  -0.8448394  3.982027e-01
#margfac1       -1.094036990 0.012224595 -89.4947445  0.000000e+00
#margfac2       -0.480391365 0.013784465 -34.8502005 2.115545e-264

### NB   log1p(x) = log(1+x)


geoMod = c(0.4,0.3,0.3)
names(geoMod) <- c('stat', 'cnty', 'trct')
queryMod = c(0.2,0.25,0.25,0.3)
names(queryMod) <- c('dtld', 'owrt', 'vhcr', 'agsx')



y = log1p(abs(c(t(mar1[1:1872,]))))

epsX = rep(epsMod[1:1872], rep(50,1872))

tmplm3 = lm(y ~ log1p(margsiz) + margfac + epsX)
summary(tmplm3)$coef








