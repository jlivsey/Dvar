


## MATERIAL ADDED 10/24/2020 FF
#==========================

  ### The steps above are embodied in the following complete package function.
source("tests/ES/MtgScript10-23.R")

  ## In this segment, J is a list of tuples subsets (containing distinct elements) of 1:K,
  ##   ASSUME (can check later if desired) that entries of J are ordered
  ##        nondecreasing by length and lexicographically within vectors of given length

Jcheck = function(Jlist) {  ### apply only if max(sapply(Jlist,length)) > 1
    lj = sapply(Jlist, length)
    lmax = max(lj)
    jbylgth = split(1:length(Jlist), lj)
    lgths =
      ### splits the indices components of Jlist by length of their tuples
      uj = runif(max(unlist(Jlist)))     ### RANDOM #s for hash-coding
    jhash = sapply(Jlist, function(vec) sum(uj[vec]))
    ### next for each k > 1 in set of lengths lj, create a vector of the
    ###    hash-coded (k-1)-tuples of all k-tuples in J
    ### and check if there are some not in the hash-codes jhash[jbylgth[[k-1]]]
    Jvalid=TRUE
    if(lmax>1) for (k in setdiff(as.numeric(names(jbylgth)),1))     {
      oldhash = jhash[jbylgth[[as.character(k-1)]]]
      tmphash = NULL
      for ( i in jbylgth[[as.character(k)]] )
        tmphash = c(tmphash, apply(combn(Jlist[[i]],k-1),2,
                                   function(vec) sum(uj[vec])))
      if( sum(is.na(match(tmphash, oldhash))) > 0 ) {
        Jvalid=FALSE
        return(list( Jvalid = Jvalid, kvalue = k )) }
    }
    Jvalid

}

  ### Define J
J = list(1,2,3,4,5,6,c(2,4),c(2,6),c(3,6),c(1,2,3))

Jcheck(J[1:9])
  # [1] TRUE

Jcheck(J)
  # $Jvalid
  # [1] FALSE
  #
  # $kvalue
  # [1] 3

Jalt = list(1,3,5,6,7,1:2,3:4,c(1,3,6,7))
Jcheck(Jalt)
  # $Jvalid
  # [1] FALSE
  #
  # $kvalue
  # [1] 2

Jalt2 = c(Jalt, list(2,4))
split(1:length(Jalt2), sapply(Jalt2,length))
  # $`1`
  # [1]  1  2  3  4  5  9 10
  #
  # $`2`
  # [1] 6 7
  #
  # $`4`
  # [1] 8
  #
Jcheck(Jalt2)
  # $Jvalid
  # [1] FALSE
  #
  # $kvalue
  # [1] 4

Jcheck(c(Jalt2,list(c(1,3,6))))
  # $Jvalid
  # [1] FALSE
  #
  # $kvalue
  # [1] 3

  ### Function seems to work OK

  #--------------------------------------------------------------------------------
  ## Next steps: development of a function
  ## to fill in the array components in an A list of arrays with
  ## dimensions indexed as and determine by a J list as above.        10/24/2020

  ## Now create function to fill in a multiway array satisfying side conditions
  ##    that all marginal sums are 0 starting from array in which
  ##    the last level of all dimensional initially have elts NA.

  # Input: array  A with dimensions  I1, I2, ..., IK with numerical entries
  ##    in sub-array 1:(I1-1),...,1:(IK-1) and NA elements elsewhere

  # We code the inductive step of filling in NA elements.

  Afill.Step = function(Atmp, dim1) {
    ###  Atmp  is multiway array, with dim I(1)x...xI(K)
    ###     d= dim1 is the dimension in which NA elements are to be filled in
    ### Assume Atmp[1:I1,...,1:I(d-1),1:(I(d)-1),...,1:(I(K)-1)] is
    ##         initially filled in with numbers, and all other entries are NA
    ## Output will be Anew, dim1+1 with Anew having elements
    ###      Atmp[1:I1,...,1:I(d-1),I(d),...,1:(I(K)-1)] filled in but all other
    ###      exactly the same as those of Atmp
    Idim = dim(Atmp)    ## all entries > 1
    nd = length(Idim)
    K = Idim[nd]
    Anew = Atmp
    expr0.ch = c("Anew[",rep(",", dim1-1))
    expr1.ch = NULL
    if(dim1<nd){
      for(i in (dim1+1):nd){
        expr1.ch = c(expr1.ch,", 1:",Idim[i]-1)
      }
    }
    NAfill.nam = paste( c(expr0.ch, Idim[dim1], expr1.ch,
                          "]"), collapse="")
    FillArr.nam = paste( c(expr0.ch, "1:",
                           Idim[dim1]-1,
                           expr1.ch,
                           ", drop=F]"), collapse="")
    eval( str2expression( paste( NAfill.nam,
                                 "<- apply(",
                                 FillArr.nam,
                                 ", setdiff(1:nd,",
                                 dim1,
                                 "), function(arr) -sum(arr))",
                                 collapse="")))
    #return
    list(Anew=Anew,
         dim.new=dim1+1,
         nam1=NAfill.nam,
         nam2=FillArr.nam)
  }

Atmp0 = array(c(1,NA,1,NA,NA,NA), c(2,3))
Atmp0
  # [,1] [,2] [,3]
  # [1,]    1    1   NA
  # [2,]   NA   NA   NA

tmp = Afill.Step(Atmp0,1)
tmp
  # $Anew
  # [,1] [,2] [,3]
  # [1,]    1    1   NA
  # [2,]   -1   -1   NA
  #
  # $dim.new
  # [1] 2
  #
  # $nam1
  # [1] "Anew[2, 1:2]"
  #
  # $nam2
  # [1] "Anew[1:1, 1:2, drop=F]"

Afill.Step(tmp$Anew,2)[1:2]
  # $Anew
  # [,1] [,2] [,3]
  # [1,]    1    1   -2
  # [2,]   -1   -1    2
  #
  # $dim.new
  # [1] 3


Atmp1 = array(runif(60),c(3,4,5))
Atmp1[,,5] = NA; Atmp1[,4,] = NA; Atmp1[3,,]=NA

Anew = Atmp1
  for(j in 1:3) Anew = Afill.Step(Anew,j)$Anew

apply(Anew,1:2,sum)
  # [,1] [,2] [,3] [,4]
  # [1,]    0    0    0    0
  # [2,]    0    0    0    0
  # [3,]    0    0    0    0

apply(Anew,c(1,3),sum)
  # [,1] [,2] [,3] [,4] [,5]
  # [1,]    0    0    0    0    0
  # [2,]    0    0    0    0    0
  # [3,]    0    0    0    0    0

apply(Anew,2:3,sum)
  # [,1] [,2] [,3] [,4] [,5]
  # [1,]    0    0    0    0    0
  # [2,]    0    0    0    0    0
  # [3,]    0    0    0    0    0
  # [4,]    0    0    0    0    0



