
# JLivsey Meeting Notes  10/23/2020

all_subtuples = function(v) {
         A = NULL
         for(j in 1:(length(v)-1))
             A = c(A, as.list(data.frame(combn(v,j))))
         A }

Aexmp1 = all_subtuples(1:4)

Algth = split(1:length(Aexmp1), sapply(Aexmp1,length))

Algth    ### tells which list components have respective lengths

### Now try to check whether list J (ordered by increasing length)
###  has property that all sub-tuples are also in list J

### For this, suffices to check that for all components of  J, of length k,
###   all subtuples of length k-1 occur earlier

J = list(1,2,3,4,5,6,c(2,4),c(2,6),c(3,6),c(1,2,3))
Jlgth = split(1:length(J), sapply(J,length))
Jlgth

## look at  k  , find for each k-tuple the set of all (k-1)-subtuples

#### IDEA of hash-coding:
### start with list, assumed in increasing order of vector-length


### generate as many uniforms as largest index


LJ = sapply(J, length)
UJ = runif(max(unlist(J)))

J2 =unlist(lapply(J, function(vec) sum(UJ[vec])))
J2


### This is the hash-coded set of tuples in the list
### Now create a vector of the hash-coded (k-1)-tuples of all k-tuples in J

newhash = function(k) {   ### apply only for k>1
       out = NULL
       for(j in Jlgth[[k]])
             out=c(out, apply(combn(J[[j]],k-1),2, function(col) sum(UJ[col])))
       out  }

newhash(2)  ### all in J2

newhash(3)  ### none in J2


