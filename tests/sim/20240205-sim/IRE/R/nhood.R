#' Neighborhood of specific cell in array
#'
#' @param A data array
#' @param idx specific cell to find its neighborhood
#' @param ownRent_vars levels of ownRent variable to return
#' @param sex_vars levels of sex variable to return
#' @param cenRace_vars levels of CenRace variable to return
#' @param age_vars levels of age variable to return
#' @param hispDes_vars levels of hisp variable to return
#' @param minCount min value to return
#' @param maxCount max value to return
#'
#' @return data frame with neighborhood cell of idx
#' @export
#'
nhood <- function(A, idx,
                  ownRent_vars  = c("own", "rnt"),
                  sex_vars      = c("mal", "fem"),
                  cenRace_vars  = c("wh", "bl", "as", "aian", "pac", "oth", "twp"),
                  age_vars      = c("0-17", "18-62", "62p"),
                  hispDes_vars  = c("hisp", "nonhisp"),
                  minCount      = -10^5,
                  maxCount      = 10^5){



# Reform array as data.frame for easier filtering
df <- as.data.frame.table(A) %>%
  rename(ownRent = Var1) %>%
  rename(sex = Var2) %>%
  rename(cenRace = Var3) %>%
  rename(age = Var4) %>%
  rename(hispDes = Var5) %>%
  rename(tract = Var6) %>%
  rename(value = Freq)

# Tract that idx is in
tract_vars = idx[6]

# Filter for user selected variables
df2 <- df %>%
  filter(ownRent %in% ownRent_vars) %>%
  filter(sex %in% sex_vars) %>%
  filter(cenRace %in% cenRace_vars) %>%
  filter(age %in% age_vars) %>%
  filter(hispDes %in% hispDes_vars) %>%
  filter(tract %in% paste0("tr", tract_vars)) %>%
  filter(value > minCount) %>%
  filter(value < maxCount)

return(df2)

} # END nhood() function



