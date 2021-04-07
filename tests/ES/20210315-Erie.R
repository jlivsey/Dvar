#' This script does not work correctly yet. There is a problem with negative
#' counts when trying to create the "other race" category by subtracting all
#' the rest of the race categories from "total".


library(dplyr)
# I mentioned that we could start by downloading or simply recording tables of partially cross-tabulated totals by county. You schema has 7 counties, so I suggest that we more or less haphazardly choose 4 counties each from 2 different states.

# The website is:

# https://www.census.gov/content/census/en/data/datasets/time-series/demo/popest/2010s-counties-detail.html

# First Try HH, Counties in Virginia pop by M/F, AGE <20, 20-64, 65+,
# Hisp and major CenRace

# Age-groups are 1-4, 5-13 and 14-18

# Details:

x = read.csv("~/Downloads/cc-est2019-alldata-36.csv")
dim(x)
# [1] 30324    80
names(x)
#  [1] "SUMLEV"       "STATE"        "COUNTY"       "STNAME"       "CTYNAME"      "YEAR"
#  [7] "AGEGRP"       "TOT_POP"      "TOT_MALE"     "TOT_FEMALE"   "WA_MALE"      "WA_FEMALE"
# [13] "BA_MALE"      "BA_FEMALE"    "IA_MALE"      "IA_FEMALE"    "AA_MALE"      "AA_FEMALE"
# [19] "NA_MALE"      "NA_FEMALE"    "TOM_MALE"     "TOM_FEMALE"   "WAC_MALE"     "WAC_FEMALE"
# [25] "BAC_MALE"     "BAC_FEMALE"   "IAC_MALE"     "IAC_FEMALE"   "AAC_MALE"     "AAC_FEMALE"
# [31] "NAC_MALE"     "NAC_FEMALE"   "NH_MALE"      "NH_FEMALE"    "NHWA_MALE"    "NHWA_FEMALE"
# [37] "NHBA_MALE"    "NHBA_FEMALE"  "NHIA_MALE"    "NHIA_FEMALE"  "NHAA_MALE"    "NHAA_FEMALE"
# [43] "NHNA_MALE"    "NHNA_FEMALE"  "NHTOM_MALE"   "NHTOM_FEMALE" "NHWAC_MALE"   "NHWAC_FEMALE"
# [49] "NHBAC_MALE"   "NHBAC_FEMALE" "NHIAC_MALE"   "NHIAC_FEMALE" "NHAAC_MALE"   "NHAAC_FEMALE"
# [55] "NHNAC_MALE"   "NHNAC_FEMALE" "H_MALE"       "H_FEMALE"     "HWA_MALE"     "HWA_FEMALE"
# [61] "HBA_MALE"     "HBA_FEMALE"   "HIA_MALE"     "HIA_FEMALE"   "HAA_MALE"     "HAA_FEMALE"
# [67] "HNA_MALE"     "HNA_FEMALE"   "HTOM_MALE"    "HTOM_FEMALE"  "HWAC_MALE"    "HWAC_FEMALE"
# [73] "HBAC_MALE"    "HBAC_FEMALE"  "HIAC_MALE"    "HIAC_FEMALE"  "HAAC_MALE"    "HAAC_FEMALE"
# [79] "HNAC_MALE"    "HNAC_FEMALE"

### essential variable set
VarSet = c("HWA_MALE", "HWA_FEMALE", "NHWA_MALE", "NHWA_FEMALE",
           "HBA_MALE", "HBA_FEMALE", "NHBA_MALE", "NHBA_FEMALE",
           "HAA_MALE", "HAA_FEMALE", "NHAA_MALE", "NHAA_FEMALE",
           "HIA_MALE", "HIA_FEMALE", "NHIA_MALE", "NHIA_FEMALE",
           "HNA_MALE", "HNA_FEMALE", "NHNA_MALE", "NHNA_FEMALE",
           "H_MALE"  ,   "H_FEMALE",   "NH_MALE",   "NH_FEMALE",
           "HTOM_MALE","HTOM_FEMALE","NHTOM_MALE","NHTOM_FEMALE")

x2 <- x %>%
  subset(CTYNAME == "Erie County") %>%
  subset(YEAR == 12) %>%
  # subset(AGEGRP != 0) %>%
  select(all_of(VarSet))

### Now create an array

Arr = array(data.matrix(x2),
           c(19,2,2,7),
           dimnames=list(paste0("Agegp",0:18),
                         c("M","F"),
                         c("Hsp","NH"),
                         c("WA","BA","AA","IA","NPIA","Tot","2+"))
            )

dimnames(Arr)

### This worked properly
## Now collapse Age-groups: Age-groups are 1-4, 5-13 and 14-18
##  0   was total and is omitted

Arr2 = array(0, c(3,2,2,7), dimnames=c(list(
  c("0-19","20-64","65+")),dimnames(Arr)[-1]))

dimnames(Arr2)
# [[1]]
# [1] "0-19"  "20-64" "65+"
#
# [[2]]
# [1] "Cty47"  "Cty83"  "Cty113" "Cty133" "Cty163" "Cty660" "Cty670"
#
# [[3]]
# [1] "M" "F"
#
# [[4]]
# [1] "Hsp" "NH"
#
# [[5]]
# [1] "WA"   "BA"   "AA"   "IA"   "NPIA" "Tot"  "2+"



Arr2[1,,,] = apply(Arr[1:4,,,],2:4,sum)
Arr2[2,,,] = apply(Arr[5:13,,,],2:4,sum)
Arr2[3,,,] = apply(Arr[14:18,,,],2:4,sum)

## Now finish off by redefining Race categories with
# "Tot" changed to "Other" after subtracting from Tot
# the sum of all other categories

Arr2[,,,"Tot"] = Arr2[,,,"Tot"] -
                  apply(Arr2[,,,c(1:5, 7)], 1:3, sum)

cbind(c(Arr2[,,,"Tot"]), c(apply(Arr2[,,,c(1:5, 7)], 1:3, sum)))

summary(c(Arr2))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.0     1.0    18.0   706.4   161.5 22033.0

dimnames(Arr2)[[4]][6] = "Oth"

### FINALLY RENAME this table "CrossArr"

CrossArr = XtrArr2
dimnames(CrossArr)
# [[1]]
# [1] "0-19"  "20-64" "65+"
#
# [[2]]
# [1] "Cty47"  "Cty83"  "Cty113" "Cty133" "Cty163" "Cty660" "Cty670"
#
# [[3]]
# [1] "M" "F"
#
# [[4]]
# [1] "Hsp" "NH"
#
# [[5]]
# [1] "WA"   "BA"   "AA"   "IA"   "NPIA" "Oth"  "2+"

## and removed XtrArr2

# save.image("C:\\EricStf\\CensusProj\\DiffPrivacy\\PopEsts-VA\\PopEst.RData")

## AN EXAMPLE OF A Little sub-table follows:

CrossArr[,1,1,2,]
#          WA   BA  AA IA NPIA Oth   2+
# 0-19  21212 4526 511 92   10   0 1115
# 20-64 10255 2515 235 54    7   0  366
# 65+    4253  661  43 14    1   0   44

## NB: the race-category "Other" is empty

apply(CrossArr[,,,,"Oth"],2,sum)
# Cty47  Cty83 Cty113 Cty133 Cty163 Cty660 Cty670
#     0      0      0      0      0      0      0

