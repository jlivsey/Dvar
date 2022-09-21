# o handle voting-age (a combination of the 2nd and 3rd levels of AGE) in some margins and 3-level AGE in others, we need a variable called VOT with two levels "<18" and "18+" and OLD with two levels "<62" and "62+".(where OLD always has the first level "<62" when VOT has the first level "<18".  Recall that in our toy example there are 43 tracts grouped into counties in consecutive blocks of 8, 9,11,6,2,3,4 and that the first 3 counties make up state 1 with the next 4 making up state 2 .
#

# OWN, SEX, RACE, VOT, OLD, HISP, ST, CTY, TRACT,

# To see how to enumerate all tracts in the multi-index notation, consider Workload 2 (that we discussed today). This has 86 margins consisting of all multi-indices with (1,0,0,0,0,0) or (2,0,0,0,0,0) in the first 6 positions, and in the last 3 positions (1,1,1:8),  (1,2,1:9), (1,3,1:11), (2,1,1:6), (2,2,1:2), (2,3,1:3), or (2,4,1:4).
#
# Workload 3: here we want Workload 2 plus all margins VOT x HISP x cenRace x Tract. The first 6 positions in the multi-indices for the new margins in this workload take all possible values (0, 0, 1:7, 2, 0, 1), and the last 3 positions take all the same possibilities as in Workload 2.
#
# Workload 4: here we want Workload 2 plus all margins Age x Sex. The first .6 positions in the multi-indices for the new margins in this workload take all possible values (0, 1:2, 0, 1, 1, 0) or (0, 1:2, 0, 2, 1:2, 0) , and the  last 3 positions take all the same possibilities as in Workload 2.
#


load("marPacks-BU.RData")

vec2char <- function(v){
  paste(v, collapse = ",")
}

charVec2 <-
unlist(
lapply(marPack2, function(x){
  paste(unlist(lapply(x, vec2char)), collapse = "|")
})
)

charVec3 <-
  unlist(
    lapply(marPack3, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec4 <-
  unlist(
    lapply(marPack4, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec5 <-
  unlist(
    lapply(marPack5, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec6 <-
  unlist(
    lapply(marPack6, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec7 <-
  unlist(
    lapply(marPack7, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec8 <-
  unlist(
    lapply(marPack8, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec9 <-
  unlist(
    lapply(marPack9, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec10 <-
  unlist(
    lapply(marPack10, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec11 <-
  unlist(
    lapply(marPack11, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )

charVec12 <-
  unlist(
    lapply(marPack12, function(x){
      paste(unlist(lapply(x, vec2char)), collapse = "|")
    })
  )


allCharVecList <-
  list(
    charVec2,
    charVec3,
    charVec4,
    charVec5,
    charVec6,
    charVec7,
    charVec8,
    charVec9,
    charVec10,
    charVec11,
    charVec12
  )

save(allCharVecList, file = "allCharVecList.RData")











