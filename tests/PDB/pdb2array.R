# Convert PDB data into array
# similar to tests/ES/PopEst.R

county_list <-
  c("Culpeper County",
    "Halifax County",
    "Harrisonburg city",
    "Hopewell city",
    "Madison County",
    "Northumberland County",
    "Rockbridge County"
  )

x_orig <- read.csv("data/pdb2020trv2_us.csv")

x <- x_orig %>%
  filter(State_name == "Virginia") %>%
  filter(County_name %in% county_list)

y <- x %>%
  filter(County_name == "Culpeper County") %>%
  select()
View(y)


# ---- Sex margin ----
x_MF <- x %>%
  filter(County_name == "Culpeper County") %>%
  select(Males_CEN_2010, Females_CEN_2010) %>%
  as.matrix()

pt = prop.table(x_MF, 1)
# xtable(pt)

prop.table(apply(x_MF, 2, sum))


# ---- Hisp margin ----

x_hisp <- x %>%
  filter(County_name == "Culpeper County") %>%
  select(Tract, Tot_Population_CEN_2010, Hispanic_CEN_2010) %>%
  mutate(NonHispanic_CEN_2010 = Tot_Population_CEN_2010 - Hispanic_CEN_2010) %>%
  select(Hispanic_CEN_2010, NonHispanic_CEN_2010) %>%
  as.matrix()

head(x_hisp)
pt_hisp <- prop.table(x_hisp, 1)
xtable(pt_hisp)

prop.table(apply(x_hisp, 2, sum))

# ---- Create Array ----

# "HWA_MALE", "HWA_FEMALE",   --> TOTAL_POP - NH_White_alone_CEN_2010
# "NHWA_MALE", "NHWA_FEMALE", --> NH_White_alone_CEN_2010
# "HBA_MALE", "HBA_FEMALE",   --> TOTAL_POP - NH_Blk_alone_CEN_2010
# "NHBA_MALE", "NHBA_FEMALE", --> NH_Blk_alone_CEN_2010
# "HAA_MALE", "HAA_FEMALE",   --> TOTAL_POP - NH_Asian_alone_CEN_2010
# "NHAA_MALE", "NHAA_FEMALE", --> NH_Asian_alone_CEN_2010
# "HIA_MALE", "HIA_FEMALE",   --> TOTAL_POP - NH_AIAN_alone_CEN_2010
# "NHIA_MALE", "NHIA_FEMALE", --> NH_AIAN_alone_CEN_2010
# "HNA_MALE", "HNA_FEMALE",   --> TOTAL_POP - NH_NHOPI_alone_CEN_2010
# "NHNA_MALE", "NHNA_FEMALE", --> NH_NHOPI_alone_CEN_2010
# "H_MALE"  ,   "H_FEMALE",   --> Hispanic_CEN_2010
# "NH_MALE",   "NH_FEMALE",  --> TOTAL_POP - Hispanic_CEN_2010
# "HTOM_MALE","HTOM_FEMALE",
# "NHTOM_MALE","NHTOM_FEMALE"
#
#
# Additional --> NH_SOR_alone_CEN_2010 ("some other race")

x_arr <- x %>%
  select(County_name,
         Tot_Population_CEN_2010,
         NH_White_alone_CEN_2010,
         NH_Blk_alone_CEN_2010,
         NH_Asian_alone_CEN_2010,
         NH_AIAN_alone_CEN_2010,
         NH_NHOPI_alone_CEN_2010,
         NH_SOR_alone_CEN_2010) %>%
  mutate(raceSum = rowSums(across(c(NH_White_alone_CEN_2010,
                                    NH_Blk_alone_CEN_2010,
                                    NH_Asian_alone_CEN_2010,
                                    NH_AIAN_alone_CEN_2010,
                                    NH_NHOPI_alone_CEN_2010,
                                    NH_SOR_alone_CEN_2010))))

pdbArr <- array(data.matrix(x_arr),
                 c(19,7,2,2,7),
                 dimnames=list(paste0("Agegp",0:18),
                               county_list,
                               c("M","F"),
                               c("Hsp","NH"),
                               c("WA","BA","AA","IA","NPIA","Tot","2+"))
  )
