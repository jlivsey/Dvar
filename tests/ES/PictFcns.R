
# Last coding steps for new simulations with permuted WLs and altered
#      bpar's aregiven in BparWLseq.RLog
##====================================================================
# 5/17/23

# Continue now with displays
# Starting point:
#          X, A, Rarr 7224 x K x Nrep (with WL indexing done in new order)
#          X2Eric, Eric2X, MargLabs
#
# Outputs: tables like the summary table (possibly picture) for VarCellS,
#      the summary table (for all WL2:WL12 columns) for Margin CVs across runs
#      and maybe some new tables (illustrated with old 50-run data) showing behavior
#         of variances or CVs on subsets of margins identified by WL and size

# Load "Eric2X"     "MargLabs"   "MargLabs_X" "X2Eric"
 attach("C:/Users/livse301/Downloads/EricXMarg.RData")
# Load X
attach("~/GitHub/Dvar/tests/sim/20230312-sim/RData/X.RData")
# Load A
attach("~/GitHub/Dvar/tests/sim/20230312-sim/RData/true-table.RData")
# Set Rarr
temp <- readRDS("C:/Users/livse301/Downloads/out.rds")
Rarr <- temp$R

## Recall display commands:

  # RarrS = pmax(Rarr,0)    ## still 7224 x 12 x 50
  # MargArrS = MargArr
  # for(i in 1:50) MargArrS[,,i] = X[7224+Eric2X, ] %*% RarrS[,,i]
  #
  # MargVarS = MargVar
  # for(i in 1:11) {
  #    tmp = apply(MargArrS[(cumWL[i]+1):cumWL[i+1],,],1:2,var)
  #    MargVarS[i,] = apply(tmp,2,mean) }
  #
  # MargMeanS = array(0, c(11,12), dimnames=list(paste0("WL",2:12), paste0("WL",1:12)))
  # for(i in 1:11) {
  #    tmp = apply(MargArrS[(cumWL[i]+1):cumWL[i+1],,],1:2,mean)
  #    MargMeanS[i,] = apply(tmp,2,mean) }
  #
  # VarCell = array(apply(Rarr,1:2,var), c(7224, 12), dimnames=list(NULL, paste0("WL",1:12)))
  #
  # round(apply(VarCell, 2, quantile, probs=c(.25, .5, .75, .9, .95, .99)), 2)
  #
  # 1000*round(sqrt(MargVarS)/MargMeanS, 4)

## First display function just reproduces these tables as pictured curves when restricting
##      attention to an Rarr-type array and a size-range for the cells


colrvec = c("black","red","blue","cyan","orange","brown","green")

PictCellQ <-
function(rarr, a.arr, sizrng, quant =
                    c(.25, .5, .75, .9, .95, .99), plt=T, txt=T, lgnd=T) {
        K = dim(rarr)[2]
        q = length(quant)
        rarrs = pmax(rarr,0)
        cellind = which(c(a.arr) >= sizrng[1] & c(a.arr) <= sizrng[2])
        nc = length(cellind)                     ### fixed 6/11/23
        varcells =  array(apply(rarrs[cellind,,],1:2,var), c(nc, K),
               dimnames=list(NULL, paste0("WLn",1:K)))
        Q.varr = round(apply(varcells, 2, quantile, probs=quant), 2)
        ncell=length(cellind)
        if(plt) {
           plot(1:K, rep(0,K), type="n", xlab="WL index", ylab=
               "Variance", ylim = c(min(Q.varr)-0.1, max(Q.varr)+0.1),
               main = paste0("Quantiles of Cell Variances by WL, size-rng=c(",
                      sizrng[1],",",sizrng[2],")"))
               for(i in 1:q) lines(1:K, Q.varr[i,], lty=i, col=colrvec[i])
        }
        if(plt & txt) {
               print("Point to location in picture for beginning text on #cells:")
               text(locator(), paste0("# cells in range = ",ncell), adj=0)   }
        if(lgnd & plt) {
               print("Point to location in picture for ul corner of legend box")
               legend(locator(), legend=paste0("Q = ",quant), lty=1:q, col=colrvec[1:q]) }
        list(Cell.Varr = varcells, ncell.siz = ncell,
             Q.Varr = Q.varr)   }


  tmp = PictCellQ(Rarr, A, c(0,Inf), plt = FALSE)
 names(tmp)
# [1] "Cell.Varr" "ncell.siz" "Q.Varr"
tmp$Q.Varr
#     WLn1  WLn2  WLn3  WLn4  WLn5  WLn6  WLn7  WLn8  WLn9 WLn10 WLn11 WLn12
# 25% 1.84  1.91  2.02  2.25  2.31  2.28  2.31  2.28  2.33  2.31  2.31  2.34
# 50% 2.86  3.07  3.39  3.53  3.60  3.55  3.60  3.64  3.60  3.63  3.67  3.64
# 75% 4.42  4.95  5.46  5.96  5.97  5.86  5.87  5.88  5.84  5.80  5.83  5.86
# 90% 5.98  7.49  8.08  8.72  8.70  8.36  8.29  8.20  8.23  8.16  8.27  8.37
# 95% 6.90 16.95 10.65 10.59 10.48  9.93  9.89  9.81  9.95  9.73  9.86  9.94
# 99% 8.84 95.78 18.46 14.10 14.23 13.02 12.87 13.39 13.33 12.94 13.19 12.96
tmp$ncell.siz
# [1] 7224
dim(tmp$Cell.Varr)
# [1] 7224   12

## X, A, trumarg, cumWL and Eric2X must be in workspace
# (recall trumarg was c(X[7224+Eric2X, ] %*% c(A))  and cumWL gave the
# cumulative number of margins in the successive WLs
trumarg <- c(X[7224+Eric2X, ] %*% c(A))
cumWL <- c(0, 86, 1290, 1548, 1562, 1758, 1800, 1804, 1860, 1872, 1879,
           1881)
 # [1]    0   86 1290 1548 1562 1758 1800 1804 1860 1872 1879 1881

## then the rows of X corresponding to margins newly appearing in WL k > 1
##    are  Eric2X[(cumWL[k-1]+1):cumWL[k]]

PictMarCV <-
function(rarr, WLseq, sizrng, plt=T, lgnd=T) {
        K = dim(rarr)[2]
    ## NB when WLseq has 8 entries (decoupled-county case), entry 8 actually
    ##   corresponds to county-totals previously indexed as WL 11
        if(length(WLseq) > 4) for(k in 5:K) if(WLseq[k]==8) WLseq[k]=11
        R = dim(rarr)[3]
        nc = dim(rarr)[1]
        rarrs = pmax(rarr,0)
        margarrS = array(0, c(1881,K-1,R))
        for(i in 1:R)  margarrS[,,i] = X[7224+Eric2X, ] %*% rarrs[,-1,i]
        margmeanS = margvarS = array(0, c(K-1,K), dimnames=
                list(paste0("WL",2:K), paste0("WL",c(2:K,12))))
        mar.ind = NULL
       ## next create a list of row-indices of X for all margin cells in size-range
        for(k in 2:K) {
             j = WLseq[k]
             aux = Eric2X[(cumWL[j-1]+1):cumWL[j]]
             aux = aux[trumarg[aux] >= sizrng[1] & trumarg[aux] <= sizrng[2]]
             mar.ind = c(mar.ind, list(aux)) }
       ## add a K'th list component for full WL
        aux = which(trumarg >= sizrng[1] & trumarg <= sizrng[2])
        mar.ind = c(mar.ind, list(aux))
       ## Now average variances and means across margins within the size-range
       for(i in 1:K) {
                tmp1 = margarrS[ mar.ind[[i]],,]
                margmeanS[,i] = apply(tmp1,2,mean)
                margvarS[,i] = apply(apply(tmp1,1:2,var),2,mean) }
        margCVs = round(100*sqrt(margvarS)/margmeanS, 2)
        if(plt) {
           plot(1:K, rep(0,K), type="n", xlab="WL index", xaxp=c(1,K,K-1),
               ylab="100*CV", ylim = c(min(margCVs)-0.1, max(margCVs)+0.1),
               main = paste0("CVs of by WL-incr Margins for each set of WL-estimated cells",
                 "\n","Averaged over WL-increment Margins in Size-rng (",
                      sizrng[1],",",sizrng[2],")"))
               for(i in 1:(K-1)) lines(1:K, margCVs[i,], lty=i, type="b", col=colrvec[i])
        }
        if(lgnd & plt) {
               print("Point to location in picture for ul corner of legend box")
               legend(locator(), legend=paste0("WL",2:K), lty=1:(K-1), col=colrvec[1:(K-1)]) }
        list(margarrS=margarrS, mar.ind=mar.ind, margmeanS=margmeanS,
             margvarS = margvarS, margCVs = margCVs)  }


tmpA = PictMarCV(Rarr[,1:4,],1:4,c(0,Inf), plt = FALSE)
tmpA$margCVs
#      WL2  WL3  WL4 WL12
# WL2 0.66 2.43 4.32 1.53
# WL3 0.60 1.59 1.85 1.00
# WL4 0.58 1.17 1.81 0.74
tmpA = PictMarCV(Rarr[,1:4,],1:4,c(10,100), plt = FALSE)
tmpA$margCVs
#       WL2   WL3   WL4  WL12
# WL2 10.59 21.18 17.43 24.11
# WL3  4.13  9.45  6.71 10.64
# WL4  4.56  9.29  6.56 10.42

tmpB = PictMarCV(Rarr[,1:7,],1:7,c(0,Inf), plt = FALSE)
tmpB$margCVs
#      WL2  WL3  WL4  WL5  WL6  WL7 WL12
# WL2 0.66 2.43 4.32 3.05 1.02 9.07 1.53
# WL3 0.60 1.59 1.85 1.34 0.69 4.37 1.00
# WL4 0.58 1.17 1.81 1.41 0.49 4.38 0.74
# WL5 0.58 1.17 1.82 1.38 0.50 4.29 0.76
# WL6 0.58 1.13 1.80 1.35 0.47 4.37 0.73
# WL7 0.58 1.14 1.80 1.33 0.46 4.37 0.73

