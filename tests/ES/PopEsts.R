
# I mentioned that we could start by downloading or simply recording tables of partially cross-tabulated totals by county. You schema has 7 counties, so I suggest that we more or less haphazardly choose 4 counties each from 2 different states.

# The website is:

# https://www.census.gov/content/census/en/data/datasets/time-series/demo/popest/2010s-counties-detail.html

# First Try HH, Counties in Virginia pop by M/F, AGE <20, 20-64, 65+,
              # Hisp and major CenRace

# Age-groups are 1-4, 5-13 and 14-18

# Details:

PopEstVA = read.csv("data/cc-est2019-alldata-51.csv")
dim(PopEstVA)
# [1] 30324    80
names(PopEstVA)
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

# Randomly select counties
county_list <- c(
  "Culpeper County",
  "Halifax County",
  "Madison County",
  "Northumberland County",
  "Rockbridge County",
  "Harrisonburg city",
  "Hopewell city"
)

XtrCty = PopEstVA[PopEstVA$CTYNAME %in% county_list,-c(1,2,4)]
dim(XtrCty)
# [1] 1596   76
unique(XtrCty$CTYNAME)
# [1] Culpeper County       Halifax County        Madison County        Northumberland County
# [5] Rockbridge County     Harrisonburg city     Hopewell city

## Want only YEAR=12 for the 2019 population estimates
## Selected 7 counties at random, above

XtrCty = XtrCty[XtrCty$YEAR==12,]   ## now 133 x 77
CTYnames = as.character(XtrCty$CTYNAME[(1:7)*19])
CTYnames
# [1] "Culpeper County"       "Halifax County"        "Madison County"        "Northumberland County"
# [5] "Rockbridge County"     "Harrisonburg city"     "Hopewell city"

### Now reduce variable set to the essential

VarSet = c("HWA_MALE", "HWA_FEMALE", "NHWA_MALE", "NHWA_FEMALE",
           "HBA_MALE", "HBA_FEMALE", "NHBA_MALE", "NHBA_FEMALE",
           "HAA_MALE", "HAA_FEMALE", "NHAA_MALE", "NHAA_FEMALE",
           "HIA_MALE", "HIA_FEMALE", "NHIA_MALE", "NHIA_FEMALE",
           "HNA_MALE", "HNA_FEMALE", "NHNA_MALE", "NHNA_FEMALE",
           "H_MALE"  ,   "H_FEMALE",   "NH_MALE",   "NH_FEMALE",
           "HTOM_MALE","HTOM_FEMALE","NHTOM_MALE","NHTOM_FEMALE")

XtrCty = XtrCty[,-(2:3)]              ### 133 x 75
XtrCty = XtrCty[,VarSet]
dim(XtrCty)
# [1] 133  28

# Copied from ES .rlog file
CTYnum <- c(660,  47,  83, 670, 113, 133, 163)        ### but counties are in alphabetical
### order in original file with
### all counties before all cities
CTYnum = sort(CTYnum)

### Now create an array
XtrArr = array(data.matrix(XtrCty),
              c(19,7,2,2,7),
              dimnames=list(paste0("Agegp",0:18),
                                    county_list,
                                    c("M","F"),
                                    c("Hsp","NH"),
                                    c("WA","BA","AA","IA","NPIA","Tot","2+"))
              )

XtrCty[0:18,1:4]                                    ## data for 1st county
#      HWA_MALE HWA_FEMALE NHWA_MALE NHWA_FEMALE
# 5454     2643       2445     17975       18726
# 5455      295        245      1005         993
# 5456      265        308      1117        1098
# 5457      279        274      1115        1216
# 5458      230        251      1092        1032
# 5459      168        149       884         879
# 5460      129        129      1059        1056
# 5461      160        146      1058        1130
# 5462      236        194      1200        1163
# 5463      244        205      1040        1114
# 5464      203        158      1191        1189
# 5465      142        101      1284        1269
# 5466      128        104      1447        1481
# 5467       62         73      1211        1366
# 5468       69         48      1071        1140
# 5469       11         28       912        1061
# 5470       11         18       653         690
# 5471        7         10       406         443

XtrArr[,1,,,1]
XtrCty[0:18,1:4]
#      HWA_MALE HWA_FEMALE NHWA_MALE NHWA_FEMALE
# 5454     2643       2445     17975       18726
# 5455      295        245      1005         993
# 5456      265        308      1117        1098
# 5457      279        274      1115        1216
# 5458      230        251      1092        1032
# 5459      168        149       884         879
# 5460      129        129      1059        1056
# 5461      160        146      1058        1130
# 5462      236        194      1200        1163
# 5463      244        205      1040        1114
# 5464      203        158      1191        1189
# 5465      142        101      1284        1269
# 5466      128        104      1447        1481
# 5467       62         73      1211        1366
# 5468       69         48      1071        1140
# 5469       11         28       912        1061
# 5470       11         18       653         690
# 5471        7         10       406         443
XtrArr[,1,,,1]
# , , Hsp
#
#            M    F
# Agegp0  2643 2445
# Agegp1   295  245
# Agegp2   265  308
# Agegp3   279  274
# Agegp4   230  251
# Agegp5   168  149
# Agegp6   129  129
# Agegp7   160  146
# Agegp8   236  194
# Agegp9   244  205
# Agegp10  203  158
# Agegp11  142  101
# Agegp12  128  104
# Agegp13   62   73
# Agegp14   69   48
# Agegp15   11   28
# Agegp16   11   18
# Agegp17    7   10
# Agegp18    4    4
#
# , , NH
#
#             M     F
# Agegp0  17975 18726
# Agegp1   1005   993
# Agegp2   1117  1098
# Agegp3   1115  1216
# Agegp4   1092  1032
# Agegp5    884   879
# Agegp6   1059  1056
# Agegp7   1058  1130
# Agegp8   1200  1163
# Agegp9   1040  1114
# Agegp10  1191  1189
# Agegp11  1284  1269
# Agegp12  1447  1481
# Agegp13  1211  1366
# Agegp14  1071  1140
# Agegp15   912  1061
# Agegp16   653   690
# Agegp17   406   443
# Agegp18   230   406


### This worked properly
## Now collapse Age-groups: Age-groups are 1-4, 5-13 and 14-18
##  0   was total and is omitted

XtrArr2 = array(0, c(3,7,2,2,7), dimnames=c(list(
      c("0-19","20-64","65+")),dimnames(XtrArr)[-1]))

dimnames(XtrArr2)
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



XtrArr2[1,,,,] = apply(XtrArr[1:4,,,,],2:5,sum)
XtrArr2[2,,,,] = apply(XtrArr[5:13,,,,],2:5,sum)
XtrArr2[3,,,,] = apply(XtrArr[14:18,,,,],2:5,sum)

## Now finish off by redefining Race categories with
# "Tot" changed to "Other" after subtracting from Tot
# the sum of all other categories

XtrArr2[,,,,"Tot"] = XtrArr2[,,,,"Tot"] -
           apply(XtrArr2[,,,,c(1:5,7)],1:4,sum)

summary(c(XtrArr2))
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
   #  0.0     1.0    18.0   706.4   161.5 22033.0

dimnames(XtrArr2)[[5]][6] = "Oth"

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

