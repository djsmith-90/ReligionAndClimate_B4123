### Script for paper 'Associations between religiosity and climate change beliefs and behaviours in the Avon Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4123
### Script 1: Data preparation/cleaning
### Created 3/11/2023 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://osf.io/p5vjz/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4123 - RSBB and Climate Change")

#install.packages("tidyverse")
library("tidyverse")

#install.packages("haven")
library(haven)

#install.packages("psych")
library(psych)


###########################################################################################
#### Read in the raw data

data_raw <- read_csv("B4123_dataset.csv")

## Quick check of data
head(data_raw)
glimpse(data_raw)



###########################################################################################
#### Start by processing the mother's data
data_mum <- data_raw


### Removing some observations 

## Want to drop data from one mother if linked to two pregnancies (else data possibly repeated)
table(data_mum$mz005l, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz005l != "Yes, drop these mult mums")

## Drop data if mother withdrew consent for data to be used
table(data_mum$Y3000, useNA = "ifany")

data_mum <- data_mum %>%
  filter(Y3000 != ".a" | is.na(Y3000))

## Drop if pregnancy not alive at 1 year of age
table(data_mum$mz014, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz014 == "All survived" | mz014 == "1 survivor")

## Drop if non-Christian religion (so only compare Christian vs non-religious participants)
table(data_mum$Y3040, useNA = "ifany")

data_mum <- data_mum %>%
  filter((Y3040 != "Buddhist" & Y3040 != "Jewish" & Y3040 != "Muslim" & Y3040 != "Rastafarian" & 
            Y3040 != "Sikh or Hindu" & Y3040 != "Other (e.g. New Age, Taoist, Spiritualist)") |
           is.na(Y3040))

table(data_mum$Y3040, useNA = "ifany")

## Drop second-born twin 
table(data_mum$qlet, useNA = "ifany")

data_mum <- data_mum %>%
  filter(qlet == "A")



### Keep just the variables of interest and re-order
data_mum <- data_mum %>%
  relocate(aln, mz028b, Z6500, c800, a525, jan2014ur01ind_M, c645a, c755, h470, jan2014imd2010q5_M, a006,
           Y3000, Y3040, Y3080, mother_LCA, Z3000:Z3075) %>%
  select(aln:Z3075)

colnames(data_mum)




#### Now process the data and prep variables for analysis

### Start with exposures (religious belief, affiliation and attendance)

## Religious belief (No vs Not sure vs Yes)
table(data_mum$Y3000, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(Y3000 = na_if(Y3000, "Questionnaire not completed")) %>%
  mutate(Y3000 = na_if(Y3000, "Missed whole section C")) %>%
  mutate(Y3000 = na_if(Y3000, "Missing")) %>%
  mutate(Y3000 = na_if(Y3000, "Unresolvable")) %>%
  mutate(Y3000 = factor(Y3000, levels = c("No", "Not sure", "Yes"))) %>%
  rename(belief = Y3000)

table(data_mum$belief, useNA = "ifany")
round(prop.table(table(data_mum$belief)) * 100, 1)
sum(table(data_mum$belief))


## Religious affiliation/identity (None vs Christian)
table(data_mum$Y3040, useNA = "ifany")

# If missing, code as NA, combine all religious affiliations together, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(Y3040 = na_if(Y3040, "Questionnaire not completed")) %>%
  mutate(Y3040 = na_if(Y3040, "Missed whole section C")) %>%
  mutate(Y3040 = na_if(Y3040, "Missing")) %>%
  mutate(Y3040 = na_if(Y3040, "Unresolvable")) %>%
  mutate(Y3040 = recode(Y3040, "Church of England" = "CofE", "Baptist/Evangelical" = "Other", 
                        "Jehovahs Witness" = "Other", "Mormon" = "Other", "Methodist" = "Other",
                        "Other Christian (e.g. Christian Science, Mormon, Presbyterian, Evangelical, Orth" 
                        = "Other", "Roman Catholic" = "Catholic")) %>%
  mutate(Y3040 = factor(Y3040, levels = c("None", "CofE", "Catholic", "Other"))) %>%
  rename(identity_denom = Y3040) %>%
  mutate(identity = recode(identity_denom, "CofE" = "Christian", "Catholic" = "Christian", 
                           "Other" = "Christian")) %>%
  mutate(identity = factor(identity, levels = c("None", "Christian"))) %>%
  relocate(aln:belief, identity)

table(data_mum$identity, useNA = "ifany")
round(prop.table(table(data_mum$identity)) * 100, 1)
sum(table(data_mum$identity))

table(data_mum$identity_denom, useNA = "ifany")
round(prop.table(table(data_mum$identity_denom)) * 100, 1)
sum(table(data_mum$identity_denom))


## Religious attendance (Occasional/non-attendance vs Regular)
table(data_mum$Y3080, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(Y3080 = na_if(Y3080, "Missing")) %>%
  mutate(Y3080 = na_if(Y3080, "Missed whole section C")) %>%
  mutate(Y3080 = na_if(Y3080, "Questionnaire not completed")) %>%
  mutate(Y3080 = na_if(Y3080, "Unresolvable")) %>%
  mutate(Y3080 = recode(Y3080, "At least once a month" = "Regular", "At least once a week" = "Regular", 
                            "At least once a year" = "Occasional/None", "Not at all" = "Occasional/None",
                            "Occasionally" = "Occasional/None")) %>%
  mutate(Y3080 = factor(Y3080, levels = c("Occasional/None", "Regular"))) %>%
  rename(attend = Y3080)

table(data_mum$attend, useNA = "ifany")
round(prop.table(table(data_mum$attend)) * 100, 1)
sum(table(data_mum$attend))


## Latent classes
table(data_mum$mother_LCA, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(mother_LCA = recode(mother_LCA, "1. Highly religious" = "Highly religious", 
                             "2. Moderately religious" = "Moderately religious", 
                             "3. Agnostic" = "Agnostic", "4. Atheist" = "Atheist")) %>%
  mutate(mother_LCA = factor(mother_LCA, levels = c("Atheist", "Agnostic", "Moderately religious", 
                                                    "Highly religious"))) %>%
  rename(lca = mother_LCA)

table(data_mum$lca, useNA = "ifany")
round(prop.table(table(data_mum$lca)) * 100, 1)
sum(table(data_mum$lca))



### Now to outcomes (climate beliefs and behaviours)

# Believes that the climate is changing - ordered categorical variable 
table(data_mum$Z3000, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(Z3000 = na_if(Z3000, "Did not complete questionnaire")) %>%
  mutate(Z3000 = na_if(Z3000, "Missed section E")) %>%
  mutate(Z3000 = na_if(Z3000, "Missing")) %>%
  mutate(Z3000 = recode(Z3000, "Yes, definitely" = "Yes definitely")) %>%
  mutate(Z3000 = factor(Z3000, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely"), ordered = TRUE)) %>%
  rename(climateChanging = Z3000)

table(data_mum$climateChanging, useNA = "ifany")
round(prop.table(table(data_mum$climateChanging)) * 100, 1)
sum(table(data_mum$climateChanging))


# Degree to which YP is concerned about the impact of climate change (only answered if believe climate is changing) - ordered categorical variable
table(data_mum$Z3001, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(Z3001 = na_if(Z3001, "Did not complete questionnaire")) %>%
  mutate(Z3001 = na_if(Z3001, "Missed section E")) %>%
  mutate(Z3001 = na_if(Z3001, "Missing")) %>%
  mutate(Z3001 = na_if(Z3001, "Does not believe in climate change")) %>%
  mutate(Z3001 = na_if(Z3001, "Unresolvable/value out of possible range")) %>%
  mutate(Z3001 = factor(Z3001, levels = c("Not at all concerned", "Not very concerned", "Somewhat concerned",
                                          "Very concerned"), ordered = TRUE)) %>%
  rename(climateConcern = Z3001)

table(data_mum$climateConcern, useNA = "ifany")
round(prop.table(table(data_mum$climateConcern)) * 100, 1)
sum(table(data_mum$climateConcern))


# Believes that humans are to blame for climate change (only answered if believe climate is changing) - ordered categorical variable
table(data_mum$Z3002, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(Z3002 = na_if(Z3002, "Did not complete questionnaire")) %>%
  mutate(Z3002 = na_if(Z3002, "Missed section E")) %>%
  mutate(Z3002 = na_if(Z3002, "Missing")) %>%
  mutate(Z3002 = na_if(Z3002, "Does not believe in climate change")) %>%
  mutate(Z3002 = na_if(Z3002, "Unresolvable/value out of possible range")) %>%
  mutate(Z3002 = factor(Z3002, levels = c("Not at all", "Yes, for some of it", "Yes, for most of it",
                                          "Yes, for all of it"), ordered = TRUE)) %>%
  rename(climateHumans = Z3002)

table(data_mum$climateHumans, useNA = "ifany")
round(prop.table(table(data_mum$climateHumans)) * 100, 1)
sum(table(data_mum$climateHumans))


# Personal actions will make difference to long-term climate changes (only answered if believe climate is changing) - unordered categorical variable
table(data_mum$Z3003, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(Z3003 = na_if(Z3003, "Did not complete questionnaire")) %>%
  mutate(Z3003 = na_if(Z3003, "Missed section E")) %>%
  mutate(Z3003 = na_if(Z3003, "Missing")) %>%
  mutate(Z3003 = na_if(Z3003, "Does not believe in climate change")) %>%
  mutate(Z3003 = na_if(Z3003, "Unresolvable/value out of possible range")) %>%
  mutate(Z3003 = factor(Z3003, levels = c("No", "Not sure", "Yes"))) %>%
  rename(climateAction = Z3003)

table(data_mum$climateAction, useNA = "ifany")
round(prop.table(table(data_mum$climateAction)) * 100, 1)
sum(table(data_mum$climateAction))


## Now go through the 'actions taken due to climate change' questions and recode as appropriate into 'not done this' vs 'done for non-climate reasons' vs 'done for climate reasons' vs 'done for both climate and non-climate reasons' (while excluding impossible combinations of values - e.g., 'not done and done')

# Changed way travelled locally
data_mum <- data_mum %>%
  mutate(Z3020 = ifelse(Z3020 == "Yes" | Z3020 == "No", Z3020, NA)) %>%
  mutate(Z3021 = ifelse(Z3021 == "Yes" | Z3021 == "No", Z3021, NA)) %>%
  mutate(Z3022 = ifelse(Z3022 == "Yes" | Z3022 == "No", Z3022, NA)) %>%
  mutate(travel = ifelse(is.na(Z3020) | is.na(Z3021) | is.na(Z3022), NA,
                        ifelse(Z3020 == "No" & Z3021 == "No" & Z3022 == "Yes", "No",
                               ifelse(Z3020 == "Yes" & Z3021 == "No" & Z3022 == "No", "Climate",
                                      ifelse(Z3020 == "No" & Z3021 == "Yes" & Z3022 == "No", "Other", 
                                             ifelse(Z3020 == "Yes" & Z3021 == "Yes" & Z3022 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(travel = factor(travel, levels = c("No", "Climate", "Other", "ClimateOther")))
  
table(data_mum$Z3020, data_mum$Z3021)
table(data_mum$travel, useNA = "ifany")


# Reduced household waste
data_mum <- data_mum %>%
  mutate(Z3023 = ifelse(Z3023 == "Yes" | Z3023 == "No", Z3023, NA)) %>%
  mutate(Z3024 = ifelse(Z3024 == "Yes" | Z3024 == "No", Z3024, NA)) %>%
  mutate(Z3025 = ifelse(Z3025 == "Yes" | Z3025 == "No", Z3025, NA)) %>%
  mutate(waste = ifelse(is.na(Z3023) | is.na(Z3024) | is.na(Z3025), NA,
                         ifelse(Z3023 == "No" & Z3024 == "No" & Z3025 == "Yes", "No",
                                ifelse(Z3023 == "Yes" & Z3024 == "No" & Z3025 == "No", "Climate",
                                       ifelse(Z3023 == "No" & Z3024 == "Yes" & Z3025 == "No", "Other", 
                                              ifelse(Z3023 == "Yes" & Z3024 == "Yes" & Z3025 == "No", 
                                                     "ClimateOther", NA)))))) %>%
  mutate(waste = factor(waste, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3023, data_mum$Z3024)
table(data_mum$waste, useNA = "ifany")


# Reduced energy use
data_mum <- data_mum %>%
  mutate(Z3026 = ifelse(Z3026 == "Yes" | Z3026 == "No", Z3026, NA)) %>%
  mutate(Z3027 = ifelse(Z3027 == "Yes" | Z3027 == "No", Z3027, NA)) %>%
  mutate(Z3028 = ifelse(Z3028 == "Yes" | Z3028 == "No", Z3028, NA)) %>%
  mutate(energy = ifelse(is.na(Z3026) | is.na(Z3027) | is.na(Z3028), NA,
                        ifelse(Z3026 == "No" & Z3027 == "No" & Z3028 == "Yes", "No",
                               ifelse(Z3026 == "Yes" & Z3027 == "No" & Z3028 == "No", "Climate",
                                      ifelse(Z3026 == "No" & Z3027 == "Yes" & Z3028 == "No", "Other", 
                                             ifelse(Z3026 == "Yes" & Z3027 == "Yes" & Z3028 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(energy = factor(energy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3026, data_mum$Z3027)
table(data_mum$energy, useNA = "ifany")


# Changed what buy
data_mum <- data_mum %>%
  mutate(Z3029 = ifelse(Z3029 == "Yes" | Z3029 == "No", Z3029, NA)) %>%
  mutate(Z3030 = ifelse(Z3030 == "Yes" | Z3030 == "No", Z3030, NA)) %>%
  mutate(Z3031 = ifelse(Z3031 == "Yes" | Z3031 == "No", Z3031, NA)) %>%
  mutate(buy = ifelse(is.na(Z3029) | is.na(Z3030) | is.na(Z3031), NA,
                        ifelse(Z3029 == "No" & Z3030 == "No" & Z3031 == "Yes", "No",
                               ifelse(Z3029 == "Yes" & Z3030 == "No" & Z3031 == "No", "Climate",
                                      ifelse(Z3029 == "No" & Z3030 == "Yes" & Z3031 == "No", "Other", 
                                             ifelse(Z3029 == "Yes" & Z3030 == "Yes" & Z3031 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(buy = factor(buy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3029, data_mum$Z3030)
table(data_mum$buy, useNA = "ifany")


# Reduced air travel
data_mum <- data_mum %>%
  mutate(Z3032 = ifelse(Z3032 == "Yes" | Z3032 == "No", Z3032, NA)) %>%
  mutate(Z3033 = ifelse(Z3033 == "Yes" | Z3033 == "No", Z3033, NA)) %>%
  mutate(Z3034 = ifelse(Z3034 == "Yes" | Z3034 == "No", Z3034, NA)) %>%
  mutate(airTravel = ifelse(is.na(Z3032) | is.na(Z3033) | is.na(Z3034), NA,
                      ifelse(Z3032 == "No" & Z3033 == "No" & Z3034 == "Yes", "No",
                             ifelse(Z3032 == "Yes" & Z3033 == "No" & Z3034 == "No", "Climate",
                                    ifelse(Z3032 == "No" & Z3033 == "Yes" & Z3034 == "No", "Other", 
                                           ifelse(Z3032 == "Yes" & Z3033 == "Yes" & Z3034 == "No", 
                                                  "ClimateOther", NA)))))) %>%
  mutate(airTravel = factor(airTravel, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3032, data_mum$Z3033)
table(data_mum$airTravel, useNA = "ifany")


# Electric/hybrid car
data_mum <- data_mum %>%
  mutate(Z3035 = ifelse(Z3035 == "Yes" | Z3035 == "No", Z3035, NA)) %>%
  mutate(Z3036 = ifelse(Z3036 == "Yes" | Z3036 == "No", Z3036, NA)) %>%
  mutate(Z3037 = ifelse(Z3037 == "Yes" | Z3037 == "No", Z3037, NA)) %>%
  mutate(elecCar = ifelse(is.na(Z3035) | is.na(Z3036) | is.na(Z3037), NA,
                            ifelse(Z3035 == "No" & Z3036 == "No" & Z3037 == "Yes", "No",
                                   ifelse(Z3035 == "Yes" & Z3036 == "No" & Z3037 == "No", "Climate",
                                          ifelse(Z3035 == "No" & Z3036 == "Yes" & Z3037 == "No", "Other", 
                                                 ifelse(Z3035 == "Yes" & Z3036 == "Yes" & Z3037 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(elecCar = factor(elecCar, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3035, data_mum$Z3036)
table(data_mum$elecCar, useNA = "ifany")


# Bought local food
data_mum <- data_mum %>%
  mutate(Z3038 = ifelse(Z3038 == "Yes" | Z3038 == "No", Z3038, NA)) %>%
  mutate(Z3039 = ifelse(Z3039 == "Yes" | Z3039 == "No", Z3039, NA)) %>%
  mutate(Z3040 = ifelse(Z3040 == "Yes" | Z3040 == "No", Z3040, NA)) %>%
  mutate(localFood = ifelse(is.na(Z3038) | is.na(Z3039) | is.na(Z3040), NA,
                          ifelse(Z3038 == "No" & Z3039 == "No" & Z3040 == "Yes", "No",
                                 ifelse(Z3038 == "Yes" & Z3039 == "No" & Z3040 == "No", "Climate",
                                        ifelse(Z3038 == "No" & Z3039 == "Yes" & Z3040 == "No", "Other", 
                                               ifelse(Z3038 == "Yes" & Z3039 == "Yes" & Z3040 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(localFood = factor(localFood, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3038, data_mum$Z3039)
table(data_mum$localFood, useNA = "ifany")


# Recycled more
data_mum <- data_mum %>%
  mutate(Z3041 = ifelse(Z3041 == "Yes" | Z3041 == "No", Z3041, NA)) %>%
  mutate(Z3042 = ifelse(Z3042 == "Yes" | Z3042 == "No", Z3042, NA)) %>%
  mutate(Z3043 = ifelse(Z3043 == "Yes" | Z3043 == "No", Z3043, NA)) %>%
  mutate(recycle = ifelse(is.na(Z3041) | is.na(Z3042) | is.na(Z3043), NA,
                            ifelse(Z3041 == "No" & Z3042 == "No" & Z3043 == "Yes", "No",
                                   ifelse(Z3041 == "Yes" & Z3042 == "No" & Z3043 == "No", "Climate",
                                          ifelse(Z3041 == "No" & Z3042 == "Yes" & Z3043 == "No", "Other", 
                                                 ifelse(Z3041 == "Yes" & Z3042 == "Yes" & Z3043 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(recycle = factor(recycle, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3041, data_mum$Z3042)
table(data_mum$recycle, useNA = "ifany")


# Reduced plastic use
data_mum <- data_mum %>%
  mutate(Z3044 = ifelse(Z3044 == "Yes" | Z3044 == "No", Z3044, NA)) %>%
  mutate(Z3045 = ifelse(Z3045 == "Yes" | Z3045 == "No", Z3045, NA)) %>%
  mutate(Z3046 = ifelse(Z3046 == "Yes" | Z3046 == "No", Z3046, NA)) %>%
  mutate(plastic = ifelse(is.na(Z3044) | is.na(Z3045) | is.na(Z3046), NA,
                          ifelse(Z3044 == "No" & Z3045 == "No" & Z3046 == "Yes", "No",
                                 ifelse(Z3044 == "Yes" & Z3045 == "No" & Z3046 == "No", "Climate",
                                        ifelse(Z3044 == "No" & Z3045 == "Yes" & Z3046 == "No", "Other", 
                                               ifelse(Z3044 == "Yes" & Z3045 == "Yes" & Z3046 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(plastic = factor(plastic, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3044, data_mum$Z3045)
table(data_mum$plastic, useNA = "ifany")


# Chosen sustainable items
data_mum <- data_mum %>%
  mutate(Z3047 = ifelse(Z3047 == "Yes" | Z3047 == "No", Z3047, NA)) %>%
  mutate(Z3048 = ifelse(Z3048 == "Yes" | Z3048 == "No", Z3048, NA)) %>%
  mutate(Z3049 = ifelse(Z3049 == "Yes" | Z3049 == "No", Z3049, NA)) %>%
  mutate(sustainable = ifelse(is.na(Z3047) | is.na(Z3048) | is.na(Z3049), NA,
                          ifelse(Z3047 == "No" & Z3048 == "No" & Z3049 == "Yes", "No",
                                 ifelse(Z3047 == "Yes" & Z3048 == "No" & Z3049 == "No", "Climate",
                                        ifelse(Z3047 == "No" & Z3048 == "Yes" & Z3049 == "No", "Other", 
                                               ifelse(Z3047 == "Yes" & Z3048 == "Yes" & Z3049 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(sustainable = factor(sustainable, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3047, data_mum$Z3048)
table(data_mum$sustainable, useNA = "ifany")


# Improved home insulation
data_mum <- data_mum %>%
  mutate(Z3050 = ifelse(Z3050 == "Yes" | Z3050 == "No", Z3050, NA)) %>%
  mutate(Z3051 = ifelse(Z3051 == "Yes" | Z3051 == "No", Z3051, NA)) %>%
  mutate(Z3052 = ifelse(Z3052 == "Yes" | Z3052 == "No", Z3052, NA)) %>%
  mutate(insulation = ifelse(is.na(Z3050) | is.na(Z3051) | is.na(Z3052), NA,
                              ifelse(Z3050 == "No" & Z3051 == "No" & Z3052 == "Yes", "No",
                                     ifelse(Z3050 == "Yes" & Z3051 == "No" & Z3052 == "No", "Climate",
                                            ifelse(Z3050 == "No" & Z3051 == "Yes" & Z3052 == "No", "Other", 
                                                   ifelse(Z3050 == "Yes" & Z3051 == "Yes" & Z3052 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(insulation = factor(insulation, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3050, data_mum$Z3051)
table(data_mum$insulation, useNA = "ifany")


# Installed solar panels
data_mum <- data_mum %>%
  mutate(Z3053 = ifelse(Z3053 == "Yes" | Z3053 == "No", Z3053, NA)) %>%
  mutate(Z3054 = ifelse(Z3054 == "Yes" | Z3054 == "No", Z3054, NA)) %>%
  mutate(Z3055 = ifelse(Z3055 == "Yes" | Z3055 == "No", Z3055, NA)) %>%
  mutate(solar = ifelse(is.na(Z3053) | is.na(Z3054) | is.na(Z3055), NA,
                             ifelse(Z3053 == "No" & Z3054 == "No" & Z3055 == "Yes", "No",
                                    ifelse(Z3053 == "Yes" & Z3054 == "No" & Z3055 == "No", "Climate",
                                           ifelse(Z3053 == "No" & Z3054 == "Yes" & Z3055 == "No", "Other", 
                                                  ifelse(Z3053 == "Yes" & Z3054 == "Yes" & Z3055 == "No", 
                                                         "ClimateOther", NA)))))) %>%
  mutate(solar = factor(solar, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3053, data_mum$Z3054)
table(data_mum$solar, useNA = "ifany")


# Started growing vegetables
data_mum <- data_mum %>%
  mutate(Z3056 = ifelse(Z3056 == "Yes" | Z3056 == "No", Z3056, NA)) %>%
  mutate(Z3057 = ifelse(Z3057 == "Yes" | Z3057 == "No", Z3057, NA)) %>%
  mutate(Z3058 = ifelse(Z3058 == "Yes" | Z3058 == "No", Z3058, NA)) %>%
  mutate(veg = ifelse(is.na(Z3056) | is.na(Z3057) | is.na(Z3058), NA,
                        ifelse(Z3056 == "No" & Z3057 == "No" & Z3058 == "Yes", "No",
                               ifelse(Z3056 == "Yes" & Z3057 == "No" & Z3058 == "No", "Climate",
                                      ifelse(Z3056 == "No" & Z3057 == "Yes" & Z3058 == "No", "Other", 
                                             ifelse(Z3056 == "Yes" & Z3057 == "Yes" & Z3058 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(veg = factor(veg, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3056, data_mum$Z3057)
table(data_mum$veg, useNA = "ifany")


# Planted trees
data_mum <- data_mum %>%
  mutate(Z3059 = ifelse(Z3059 == "Yes" | Z3059 == "No", Z3059, NA)) %>%
  mutate(Z3060 = ifelse(Z3060 == "Yes" | Z3060 == "No", Z3060, NA)) %>%
  mutate(Z3061 = ifelse(Z3061 == "Yes" | Z3061 == "No", Z3061, NA)) %>%
  mutate(trees = ifelse(is.na(Z3059) | is.na(Z3060) | is.na(Z3061), NA,
                      ifelse(Z3059 == "No" & Z3060 == "No" & Z3061 == "Yes", "No",
                             ifelse(Z3059 == "Yes" & Z3060 == "No" & Z3061 == "No", "Climate",
                                    ifelse(Z3059 == "No" & Z3060 == "Yes" & Z3061 == "No", "Other", 
                                           ifelse(Z3059 == "Yes" & Z3060 == "Yes" & Z3061 == "No", 
                                                  "ClimateOther", NA)))))) %>%
  mutate(trees = factor(trees, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3059, data_mum$Z3060)
table(data_mum$trees, useNA = "ifany")


# Avoided fossil fuel organisation
data_mum <- data_mum %>%
  mutate(Z3062 = ifelse(Z3062 == "Yes" | Z3062 == "No", Z3062, NA)) %>%
  mutate(Z3063 = ifelse(Z3063 == "Yes" | Z3063 == "No", Z3063, NA)) %>%
  mutate(Z3064 = ifelse(Z3064 == "Yes" | Z3064 == "No", Z3064, NA)) %>%
  mutate(avoidFossil = ifelse(is.na(Z3062) | is.na(Z3063) | is.na(Z3064), NA,
                        ifelse(Z3062 == "No" & Z3063 == "No" & Z3064 == "Yes", "No",
                               ifelse(Z3062 == "Yes" & Z3063 == "No" & Z3064 == "No", "Climate",
                                      ifelse(Z3062 == "No" & Z3063 == "Yes" & Z3064 == "No", "Other", 
                                             ifelse(Z3062 == "Yes" & Z3063 == "Yes" & Z3064 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(avoidFossil = factor(avoidFossil, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3062, data_mum$Z3063)
table(data_mum$avoidFossil, useNA = "ifany")


# Planned fewer children
data_mum <- data_mum %>%
  mutate(Z3065 = ifelse(Z3065 == "Yes" | Z3065 == "No", Z3065, NA)) %>%
  mutate(Z3066 = ifelse(Z3066 == "Yes" | Z3066 == "No", Z3066, NA)) %>%
  mutate(Z3067 = ifelse(Z3067 == "Yes" | Z3067 == "No", Z3067, NA)) %>%
  mutate(children = ifelse(is.na(Z3065) | is.na(Z3066) | is.na(Z3067), NA,
                              ifelse(Z3065 == "No" & Z3066 == "No" & Z3067 == "Yes", "No",
                                     ifelse(Z3065 == "Yes" & Z3066 == "No" & Z3067 == "No", "Climate",
                                            ifelse(Z3065 == "No" & Z3066 == "Yes" & Z3067 == "No", "Other", 
                                                   ifelse(Z3065 == "Yes" & Z3066 == "Yes" & Z3067 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(children = factor(children, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3065, data_mum$Z3066)
table(data_mum$children, useNA = "ifany")


# Taken other climate action
data_mum <- data_mum %>%
  mutate(Z3068 = ifelse(Z3068 == "Yes" | Z3068 == "No", Z3068, NA)) %>%
  mutate(Z3069 = ifelse(Z3069 == "Yes" | Z3069 == "No", Z3069, NA)) %>%
  mutate(Z3070 = ifelse(Z3070 == "Yes" | Z3070 == "No", Z3070, NA)) %>%
  mutate(otherAction = ifelse(is.na(Z3068) | is.na(Z3069) | is.na(Z3070), NA,
                           ifelse(Z3068 == "No" & Z3069 == "No" & Z3070 == "Yes", "No",
                                  ifelse(Z3068 == "Yes" & Z3069 == "No" & Z3070 == "No", "Climate",
                                         ifelse(Z3068 == "No" & Z3069 == "Yes" & Z3070 == "No", "Other", 
                                                ifelse(Z3068 == "Yes" & Z3069 == "Yes" & Z3070 == "No", 
                                                       "ClimateOther", NA)))))) %>%
  mutate(otherAction = factor(otherAction, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3068, data_mum$Z3069)
table(data_mum$otherAction, useNA = "ifany")


# Reduced meat/dairy consumption
data_mum <- data_mum %>%
  mutate(Z3071 = ifelse(Z3071 == "Yes" | Z3071 == "No", Z3071, NA)) %>%
  mutate(Z3072 = ifelse(Z3072 == "Yes" | Z3072 == "No", Z3072, NA)) %>%
  mutate(Z3073 = ifelse(Z3073 == "Yes" | Z3073 == "No", Z3073, NA)) %>%
  mutate(meatDairy = ifelse(is.na(Z3071) | is.na(Z3072) | is.na(Z3073), NA,
                              ifelse(Z3071 == "No" & Z3072 == "No" & Z3073 == "Yes", "No",
                                     ifelse(Z3071 == "Yes" & Z3072 == "No" & Z3073 == "No", "Climate",
                                            ifelse(Z3071 == "No" & Z3072 == "Yes" & Z3073 == "No", "Other", 
                                                   ifelse(Z3071 == "Yes" & Z3072 == "Yes" & Z3073 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(meatDairy = factor(meatDairy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_mum$Z3071, data_mum$Z3072)
table(data_mum$meatDairy, useNA = "ifany")

# Check answers if vegan/vegatarian - This is complicated, as some vegetarians/vegans answered this question, while others did not (vegetarians should also not be a separate category as they can still reduce dairy consumption...). Will exclude answers from those who said 'always vegan' (as should not consume any meat or dairy products), but keep answers from those who said 'always vegetarian' 
table(data_mum$Z3074)
table(data_mum$Z3075)
table(data_mum$Z3074, data_mum$Z3075)

table(data_mum$meatDairy[data_mum$Z3074 == "Yes"])
table(data_mum$meatDairy[data_mum$Z3075 == "Yes"])

data_mum$meatDairy[data_mum$Z3075 == "Yes"] <- NA
table(data_mum$meatDairy, useNA = "ifany")


## Drop all of the original behaviour variables
data_mum <- data_mum %>%
  select(-c(Z3020:Z3075))

glimpse(data_mum)



### Now tidy the potential confounders

# Age (at birth)
table(data_mum$mz028b, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(mz028b = na_if(mz028b, "Not in core sample")) %>%
  mutate(mz028b = recode(mz028b, "< 16" = "16", ">43" = "43")) %>%
  mutate(mz028b = as.numeric(mz028b)) %>%
  rename(ageAtBirth = mz028b)

table(data_mum$ageAtBirth, useNA = "ifany")
summary(data_mum$ageAtBirth)


# Age (at climate questionnaire)
table(data_mum$Z6500, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(Z6500 = na_if(Z6500, "Did not complete questionnaire")) %>%
  mutate(Z6500 = as.numeric(Z6500)) %>%
  rename(ageAtQ = Z6500)

table(data_mum$ageAtQ, useNA = "ifany")
summary(data_mum$ageAtQ)


# Mother's ethnicity (White vs other than White)
table(data_mum$c800, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c800 = na_if(c800, "Missing")) %>%
  mutate(c800 = recode(c800, "Bangladeshi" = "Other than White", "Black African" = "Other than White", 
                       "Black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "Other" = "Other than White", 
                       "Other black" = "Other than White", "Pakistani" = "Other than White")) %>%
  mutate(c800 = factor(c800, levels = c("White", "Other than White"))) %>%
  rename(ethnicity = c800)

table(data_mum$ethnicity, useNA = "ifany")


# Marital status (Married vs Never married vs widowed/divorced/separated)
table(data_mum$a525, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a525 = na_if(a525, "Missing")) %>%
  mutate(a525 = recode(a525, "1st marriage" = "Married", "Divorced" = "Sep/Div/Widow", 
                       "Marriage 2 or 3" = "Married", "Separated" = "Sep/Div/Widow", 
                       "Widowed" = "Sep/Div/Widow")) %>%
  mutate(a525 = factor(a525, levels = c("Married", "Never married", "Sep/Div/Widow"))) %>%
  rename(marital = a525)

table(data_mum$marital, useNA = "ifany")


# Urban/rural status (Urban vs Rural)
table(data_mum$jan2014ur01ind_M, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(jan2014ur01ind_M = na_if(jan2014ur01ind_M, "Missing")) %>%
  mutate(jan2014ur01ind_M = na_if(jan2014ur01ind_M, "Triplets/Quadruplets")) %>%
  mutate(jan2014ur01ind_M = recode(jan2014ur01ind_M, "Hamlet and Isolated Dwelling" = "Rural", 
                                   "Town and Fringe" = "Rural", "Village" = "Rural", 
                                   "Urban (pop. >= 10k)" = "Urban")) %>%
  mutate(jan2014ur01ind_M = factor(jan2014ur01ind_M, levels = c("Urban", "Rural"))) %>%
  rename(rural = jan2014ur01ind_M)

table(data_mum$rural, useNA = "ifany")


# Mother's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_mum$c645a, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c645a = na_if(c645a, "Missing")) %>%
  mutate(c645a = recode(c645a, "CSE" = "CSE/None")) %>%
  mutate(c645a = factor(c645a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(edu = c645a)

table(data_mum$edu, useNA = "ifany")


# Mother's occupational social class (I vs II vs III (non-manual) vs III (manual) vs IV/V)
table(data_mum$c755, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c755 = na_if(c755, "Missing")) %>%
  mutate(c755 = na_if(c755, "Armed forces")) %>%
  mutate(c755 = recode(c755, "IV" = "IV/V", "V" = "IV/V")) %>%
  mutate(c755 = factor(c755, levels = c("I", "II", "III (non-manual)", "III (manual)", "IV/V"))) %>%
  rename(occClass = c755)

table(data_mum$occClass, useNA = "ifany")


# Weekly household income after tax ((£0-£100 vs £100-£199 vs £200-£299 vs £300-£399 vs £400 and above)
table(data_mum$h470, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(h470 = na_if(h470, "Not stated")) %>%
  mutate(h470 = factor(h470, levels = c("<100", "100 - 199", "200 - 299", "300 - 399", ">400"))) %>%
  rename(income = h470)

table(data_mum$income, useNA = "ifany")


# Area-level index of multiple deprivation (IMD; quintiles)
table(data_mum$jan2014imd2010q5_M, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(jan2014imd2010q5_M = na_if(jan2014imd2010q5_M, "Missing")) %>%
  mutate(jan2014imd2010q5_M = na_if(jan2014imd2010q5_M, "Triplets/Quadruplets")) %>%
  mutate(jan2014imd2010q5_M = recode(jan2014imd2010q5_M, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan2014imd2010q5_M = factor(jan2014imd2010q5_M, 
                                     levels = c("Quin. 1/Least deprived", 
                                                "Quintile 2", "Quintile 3", 
                                                "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan2014imd2010q5_M)

table(data_mum$imd, useNA = "ifany")


# Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other)
table(data_mum$a006, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = a006)

table(data_mum$home, useNA = "ifany")


### Save mother's data (in R, CSV and Stata format; the R and Stata formats will keep all the factor formatting, while the CSV file will lose this)
save(data_mum, file = "data_mum_processed_B4123.RData")
write_csv(data_mum, file = "data_mum_processed_B4123.csv")
write_dta(data_mum, "data_mum_processed_B4123.dta")



###########################################################################################
#### Next, process the partner's data
data_partner <- data_raw

### Removing some observations 

## Want to drop data from one mother if linked to two pregnancies (else data possibly repeated)
table(data_partner$mz005l, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz005l != "Yes, drop these mult mums")

## See if any additional repeated partners linked to two pregnancies (are no extra partners)
table(data_partner$pz_mult, useNA = "ifany")

## Drop data if mother withdrew consent for data to be used
table(data_partner$Y3000, useNA = "ifany")

data_partner <- data_partner %>%
  filter(Y3000 != ".a" | is.na(Y3000))

## Drop data if partner withdrew consent for data to be used
table(data_partner$FPC3000, useNA = "ifany")

data_partner <- data_partner %>%
  filter(FPC3000 != ".c" | is.na(FPC3000))

## Drop if pregnancy not alive at 1 year of age
table(data_partner$mz014, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz014 == "All survived" | mz014 == "1 survivor")

## Drop if non-Christian religion (so only compare Christian vs non-religious participants)
table(data_partner$FPC3040, useNA = "ifany")

data_partner <- data_partner %>%
  filter((FPC3040 != "Buddhist" & FPC3040 != "Jewish" & FPC3040 != "Jewish/Sikh/Hindu/Muslim" 
          & FPC3040 != "Other (e.g. New Age, Taoist, Spiritualist)") | is.na(FPC3040))

table(data_partner$FPC3040, useNA = "ifany")

## Drop partners without any ALSPAC data/not enrolled
table(data_partner$partner_in_alspac, useNA = "ifany")

data_partner <- data_partner %>%
  filter(!is.na(partner_in_alspac))

## Also remove any partners who changed identity over course of study
table(data_partner$partner_changed, useNA = "ifany")

data_partner <- data_partner %>%
  filter(partner_changed == "Partner has not changed")

## Drop second-born twin 
table(data_partner$qlet, useNA = "ifany")

data_partner <- data_partner %>%
  filter(qlet == "A")



### Keep just the variables of interest and re-order
data_partner <- data_partner %>%
  relocate(aln, partner_age, FPD6500, c801, pa065, jan2014ur01ind_M, c666a, c765, h470, 
           jan2014imd2010q5_M, a006, FPC3000, FPC3040, FPC3080, partner_LCA, FPD3000:FPD3075) %>%
  select(aln:FPD3075)

colnames(data_partner)




#### Now process the data and prep variables for analysis

### Start with exposures (religious belief, affiliation and attendance)

## Religious belief (No vs Not sure vs Yes)
table(data_partner$FPC3000, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(FPC3000 = na_if(FPC3000, "Questionnaire not completed")) %>%
  mutate(FPC3000 = na_if(FPC3000, "Missed whole section C")) %>%
  mutate(FPC3000 = na_if(FPC3000, "Missing")) %>%
  mutate(FPC3000 = factor(FPC3000, levels = c("No", "Not sure", "Yes"))) %>%
  rename(belief = FPC3000)

table(data_partner$belief, useNA = "ifany")
round(prop.table(table(data_partner$belief)) * 100, 1)
sum(table(data_partner$belief))


## Religious affiliation/identity (None vs Christian)
table(data_partner$FPC3040, useNA = "ifany")

# If missing, code as NA, combine all religious affiliations together, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(FPC3040 = na_if(FPC3040, "Questionnaire not completed")) %>%
  mutate(FPC3040 = na_if(FPC3040, "Missed whole section C")) %>%
  mutate(FPC3040 = na_if(FPC3040, "Missing")) %>%
  mutate(FPC3040 = na_if(FPC3040, "Unresolvable")) %>%
  mutate(FPC3040 = recode(FPC3040, "Church of England" = "CofE", "Baptist/Evangelical" = "Other", 
                        "Jehovahs Witness" = "Other", "Mormon" = "Other", "Methodist" = "Other",
                        "Other Christian (e.g. Christian Science, Mormon, Presbyterian, Evangelical, Orth" 
                        = "Other", "Roman Catholic" = "Catholic")) %>%
  mutate(FPC3040 = factor(FPC3040, levels = c("None", "CofE", "Catholic", "Other"))) %>%
  rename(identity_denom = FPC3040) %>%
  mutate(identity = recode(identity_denom, "CofE" = "Christian", "Catholic" = "Christian", 
                           "Other" = "Christian")) %>%
  mutate(identity = factor(identity, levels = c("None", "Christian"))) %>%
  relocate(aln:belief, identity)

table(data_partner$identity, useNA = "ifany")
round(prop.table(table(data_partner$identity)) * 100, 1)
sum(table(data_partner$identity))

table(data_partner$identity_denom, useNA = "ifany")
round(prop.table(table(data_partner$identity_denom)) * 100, 1)
sum(table(data_partner$identity_denom))


## Religious attendance (Occasional/non-attendance vs Regular)
table(data_partner$FPC3080, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(FPC3080 = na_if(FPC3080, "Missing")) %>%
  mutate(FPC3080 = na_if(FPC3080, "Missed whole section C")) %>%
  mutate(FPC3080 = na_if(FPC3080, "Questionnaire not completed")) %>%
  mutate(FPC3080 = na_if(FPC3080, "Unresolvable")) %>%
  mutate(FPC3080 = recode(FPC3080, "At least once a month" = "Regular", "At least once a week" = "Regular", 
                        "At least once a year" = "Occasional/None", "Not at all" = "Occasional/None",
                        "Occasionally" = "Occasional/None")) %>%
  mutate(FPC3080 = factor(FPC3080, levels = c("Occasional/None", "Regular"))) %>%
  rename(attend = FPC3080)

table(data_partner$attend, useNA = "ifany")
round(prop.table(table(data_partner$attend)) * 100, 1)
sum(table(data_partner$attend))


## Latent classes
table(data_partner$partner_LCA, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(partner_LCA = recode(partner_LCA, "1. Highly religious" = "Highly religious", 
                             "2. Moderately religious" = "Moderately religious", 
                             "3. Agnostic" = "Agnostic", "4. Atheist" = "Atheist")) %>%
  mutate(partner_LCA = factor(partner_LCA, levels = c("Atheist", "Agnostic", "Moderately religious", 
                                                    "Highly religious"))) %>%
  rename(lca = partner_LCA)

table(data_partner$lca, useNA = "ifany")
round(prop.table(table(data_partner$lca)) * 100, 1)
sum(table(data_partner$lca))



### Now to outcomes (climate beliefs and behaviours)

# Believes that the climate is changing - ordered categorical variable 
table(data_partner$FPD3000, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(FPD3000 = na_if(FPD3000, "Did not complete questionnaire")) %>%
  mutate(FPD3000 = na_if(FPD3000, "Missed section E")) %>%
  mutate(FPD3000 = na_if(FPD3000, "Missing")) %>%
  mutate(FPD3000 = recode(FPD3000, "Yes, definitely" = "Yes definitely")) %>%
  mutate(FPD3000 = factor(FPD3000, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely"), ordered = TRUE)) %>%
  rename(climateChanging = FPD3000)

table(data_partner$climateChanging, useNA = "ifany")
round(prop.table(table(data_partner$climateChanging)) * 100, 1)
sum(table(data_partner$climateChanging))


# Degree to which YP is concerned about the impact of climate change (only answered if believe climate is changing) - ordered categorical variable
table(data_partner$FPD3001, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(FPD3001 = na_if(FPD3001, "Did not complete questionnaire")) %>%
  mutate(FPD3001 = na_if(FPD3001, "Missed section E")) %>%
  mutate(FPD3001 = na_if(FPD3001, "Missing")) %>%
  mutate(FPD3001 = na_if(FPD3001, "Does not believe in climate change")) %>%
  mutate(FPD3001 = na_if(FPD3001, "Unresolvable/value out of possible range")) %>%
  mutate(FPD3001 = factor(FPD3001, levels = c("Not at all concerned", "Not very concerned", "Somewhat concerned",
                                          "Very concerned"), ordered = TRUE)) %>%
  rename(climateConcern = FPD3001)

table(data_partner$climateConcern, useNA = "ifany")
round(prop.table(table(data_partner$climateConcern)) * 100, 1)
sum(table(data_partner$climateConcern))


# Believes that humans are to blame for climate change (only answered if believe climate is changing) - ordered categorical variable
table(data_partner$FPD3002, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(FPD3002 = na_if(FPD3002, "Did not complete questionnaire")) %>%
  mutate(FPD3002 = na_if(FPD3002, "Missed section E")) %>%
  mutate(FPD3002 = na_if(FPD3002, "Missing")) %>%
  mutate(FPD3002 = na_if(FPD3002, "Does not believe in climate change")) %>%
  mutate(FPD3002 = na_if(FPD3002, "Unresolvable/value out of possible range")) %>%
  mutate(FPD3002 = factor(FPD3002, levels = c("Not at all", "Yes, for some of it", "Yes, for most of it",
                                          "Yes, for all of it"), ordered = TRUE)) %>%
  rename(climateHumans = FPD3002)

table(data_partner$climateHumans, useNA = "ifany")
round(prop.table(table(data_partner$climateHumans)) * 100, 1)
sum(table(data_partner$climateHumans))


# Personal actions will make difference to long-term climate changes (only answered if believe climate is changing) - unordered categorical variable
table(data_partner$FPD3003, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(FPD3003 = na_if(FPD3003, "Did not complete questionnaire")) %>%
  mutate(FPD3003 = na_if(FPD3003, "Missed section E")) %>%
  mutate(FPD3003 = na_if(FPD3003, "Missing")) %>%
  mutate(FPD3003 = na_if(FPD3003, "Does not believe in climate change")) %>%
  mutate(FPD3003 = na_if(FPD3003, "Unresolvable/value out of possible range")) %>%
  mutate(FPD3003 = factor(FPD3003, levels = c("No", "Not sure", "Yes"))) %>%
  rename(climateAction = FPD3003)

table(data_partner$climateAction, useNA = "ifany")
round(prop.table(table(data_partner$climateAction)) * 100, 1)
sum(table(data_partner$climateAction))


## Now go through the 'actions taken due to climate change' questions and recode as appropriate into 'not done this' vs 'done for non-climate reasons' vs 'done for climate reasons' vs 'done for both climate and non-climate reasons' (while excluding impossible combinations of values - e.g., 'not done and done')

# Changed way travelled locally
data_partner <- data_partner %>%
  mutate(FPD3020 = ifelse(FPD3020 == "Yes" | FPD3020 == "No", FPD3020, NA)) %>%
  mutate(FPD3021 = ifelse(FPD3021 == "Yes" | FPD3021 == "No", FPD3021, NA)) %>%
  mutate(FPD3022 = ifelse(FPD3022 == "Yes" | FPD3022 == "No", FPD3022, NA)) %>%
  mutate(travel = ifelse(is.na(FPD3020) | is.na(FPD3021) | is.na(FPD3022), NA,
                         ifelse(FPD3020 == "No" & FPD3021 == "No" & FPD3022 == "Yes", "No",
                                ifelse(FPD3020 == "Yes" & FPD3021 == "No" & FPD3022 == "No", "Climate",
                                       ifelse(FPD3020 == "No" & FPD3021 == "Yes" & FPD3022 == "No", "Other", 
                                              ifelse(FPD3020 == "Yes" & FPD3021 == "Yes" & FPD3022 == "No", 
                                                     "ClimateOther", NA)))))) %>%
  mutate(travel = factor(travel, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3020, data_partner$FPD3021)
table(data_partner$travel, useNA = "ifany")


# Reduced household waste
data_partner <- data_partner %>%
  mutate(FPD3023 = ifelse(FPD3023 == "Yes" | FPD3023 == "No", FPD3023, NA)) %>%
  mutate(FPD3024 = ifelse(FPD3024 == "Yes" | FPD3024 == "No", FPD3024, NA)) %>%
  mutate(FPD3025 = ifelse(FPD3025 == "Yes" | FPD3025 == "No", FPD3025, NA)) %>%
  mutate(waste = ifelse(is.na(FPD3023) | is.na(FPD3024) | is.na(FPD3025), NA,
                        ifelse(FPD3023 == "No" & FPD3024 == "No" & FPD3025 == "Yes", "No",
                               ifelse(FPD3023 == "Yes" & FPD3024 == "No" & FPD3025 == "No", "Climate",
                                      ifelse(FPD3023 == "No" & FPD3024 == "Yes" & FPD3025 == "No", "Other", 
                                             ifelse(FPD3023 == "Yes" & FPD3024 == "Yes" & FPD3025 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(waste = factor(waste, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3023, data_partner$FPD3024)
table(data_partner$waste, useNA = "ifany")


# Reduced energy use
data_partner <- data_partner %>%
  mutate(FPD3026 = ifelse(FPD3026 == "Yes" | FPD3026 == "No", FPD3026, NA)) %>%
  mutate(FPD3027 = ifelse(FPD3027 == "Yes" | FPD3027 == "No", FPD3027, NA)) %>%
  mutate(FPD3028 = ifelse(FPD3028 == "Yes" | FPD3028 == "No", FPD3028, NA)) %>%
  mutate(energy = ifelse(is.na(FPD3026) | is.na(FPD3027) | is.na(FPD3028), NA,
                         ifelse(FPD3026 == "No" & FPD3027 == "No" & FPD3028 == "Yes", "No",
                                ifelse(FPD3026 == "Yes" & FPD3027 == "No" & FPD3028 == "No", "Climate",
                                       ifelse(FPD3026 == "No" & FPD3027 == "Yes" & FPD3028 == "No", "Other", 
                                              ifelse(FPD3026 == "Yes" & FPD3027 == "Yes" & FPD3028 == "No", 
                                                     "ClimateOther", NA)))))) %>%
  mutate(energy = factor(energy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3026, data_partner$FPD3027)
table(data_partner$energy, useNA = "ifany")


# Changed what buy
data_partner <- data_partner %>%
  mutate(FPD3029 = ifelse(FPD3029 == "Yes" | FPD3029 == "No", FPD3029, NA)) %>%
  mutate(FPD3030 = ifelse(FPD3030 == "Yes" | FPD3030 == "No", FPD3030, NA)) %>%
  mutate(FPD3031 = ifelse(FPD3031 == "Yes" | FPD3031 == "No", FPD3031, NA)) %>%
  mutate(buy = ifelse(is.na(FPD3029) | is.na(FPD3030) | is.na(FPD3031), NA,
                      ifelse(FPD3029 == "No" & FPD3030 == "No" & FPD3031 == "Yes", "No",
                             ifelse(FPD3029 == "Yes" & FPD3030 == "No" & FPD3031 == "No", "Climate",
                                    ifelse(FPD3029 == "No" & FPD3030 == "Yes" & FPD3031 == "No", "Other", 
                                           ifelse(FPD3029 == "Yes" & FPD3030 == "Yes" & FPD3031 == "No", 
                                                  "ClimateOther", NA)))))) %>%
  mutate(buy = factor(buy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3029, data_partner$FPD3030)
table(data_partner$buy, useNA = "ifany")


# Reduced air travel
data_partner <- data_partner %>%
  mutate(FPD3032 = ifelse(FPD3032 == "Yes" | FPD3032 == "No", FPD3032, NA)) %>%
  mutate(FPD3033 = ifelse(FPD3033 == "Yes" | FPD3033 == "No", FPD3033, NA)) %>%
  mutate(FPD3034 = ifelse(FPD3034 == "Yes" | FPD3034 == "No", FPD3034, NA)) %>%
  mutate(airTravel = ifelse(is.na(FPD3032) | is.na(FPD3033) | is.na(FPD3034), NA,
                            ifelse(FPD3032 == "No" & FPD3033 == "No" & FPD3034 == "Yes", "No",
                                   ifelse(FPD3032 == "Yes" & FPD3033 == "No" & FPD3034 == "No", "Climate",
                                          ifelse(FPD3032 == "No" & FPD3033 == "Yes" & FPD3034 == "No", "Other", 
                                                 ifelse(FPD3032 == "Yes" & FPD3033 == "Yes" & FPD3034 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(airTravel = factor(airTravel, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3032, data_partner$FPD3033)
table(data_partner$airTravel, useNA = "ifany")


# Electric/hybrid car
data_partner <- data_partner %>%
  mutate(FPD3035 = ifelse(FPD3035 == "Yes" | FPD3035 == "No", FPD3035, NA)) %>%
  mutate(FPD3036 = ifelse(FPD3036 == "Yes" | FPD3036 == "No", FPD3036, NA)) %>%
  mutate(FPD3037 = ifelse(FPD3037 == "Yes" | FPD3037 == "No", FPD3037, NA)) %>%
  mutate(elecCar = ifelse(is.na(FPD3035) | is.na(FPD3036) | is.na(FPD3037), NA,
                          ifelse(FPD3035 == "No" & FPD3036 == "No" & FPD3037 == "Yes", "No",
                                 ifelse(FPD3035 == "Yes" & FPD3036 == "No" & FPD3037 == "No", "Climate",
                                        ifelse(FPD3035 == "No" & FPD3036 == "Yes" & FPD3037 == "No", "Other", 
                                               ifelse(FPD3035 == "Yes" & FPD3036 == "Yes" & FPD3037 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(elecCar = factor(elecCar, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3035, data_partner$FPD3036)
table(data_partner$elecCar, useNA = "ifany")


# Bought local food
data_partner <- data_partner %>%
  mutate(FPD3038 = ifelse(FPD3038 == "Yes" | FPD3038 == "No", FPD3038, NA)) %>%
  mutate(FPD3039 = ifelse(FPD3039 == "Yes" | FPD3039 == "No", FPD3039, NA)) %>%
  mutate(FPD3040 = ifelse(FPD3040 == "Yes" | FPD3040 == "No", FPD3040, NA)) %>%
  mutate(localFood = ifelse(is.na(FPD3038) | is.na(FPD3039) | is.na(FPD3040), NA,
                            ifelse(FPD3038 == "No" & FPD3039 == "No" & FPD3040 == "Yes", "No",
                                   ifelse(FPD3038 == "Yes" & FPD3039 == "No" & FPD3040 == "No", "Climate",
                                          ifelse(FPD3038 == "No" & FPD3039 == "Yes" & FPD3040 == "No", "Other", 
                                                 ifelse(FPD3038 == "Yes" & FPD3039 == "Yes" & FPD3040 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(localFood = factor(localFood, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3038, data_partner$FPD3039)
table(data_partner$localFood, useNA = "ifany")


# Recycled more
data_partner <- data_partner %>%
  mutate(FPD3041 = ifelse(FPD3041 == "Yes" | FPD3041 == "No", FPD3041, NA)) %>%
  mutate(FPD3042 = ifelse(FPD3042 == "Yes" | FPD3042 == "No", FPD3042, NA)) %>%
  mutate(FPD3043 = ifelse(FPD3043 == "Yes" | FPD3043 == "No", FPD3043, NA)) %>%
  mutate(recycle = ifelse(is.na(FPD3041) | is.na(FPD3042) | is.na(FPD3043), NA,
                          ifelse(FPD3041 == "No" & FPD3042 == "No" & FPD3043 == "Yes", "No",
                                 ifelse(FPD3041 == "Yes" & FPD3042 == "No" & FPD3043 == "No", "Climate",
                                        ifelse(FPD3041 == "No" & FPD3042 == "Yes" & FPD3043 == "No", "Other", 
                                               ifelse(FPD3041 == "Yes" & FPD3042 == "Yes" & FPD3043 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(recycle = factor(recycle, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3041, data_partner$FPD3042)
table(data_partner$recycle, useNA = "ifany")


# Reduced plastic use
data_partner <- data_partner %>%
  mutate(FPD3044 = ifelse(FPD3044 == "Yes" | FPD3044 == "No", FPD3044, NA)) %>%
  mutate(FPD3045 = ifelse(FPD3045 == "Yes" | FPD3045 == "No", FPD3045, NA)) %>%
  mutate(FPD3046 = ifelse(FPD3046 == "Yes" | FPD3046 == "No", FPD3046, NA)) %>%
  mutate(plastic = ifelse(is.na(FPD3044) | is.na(FPD3045) | is.na(FPD3046), NA,
                          ifelse(FPD3044 == "No" & FPD3045 == "No" & FPD3046 == "Yes", "No",
                                 ifelse(FPD3044 == "Yes" & FPD3045 == "No" & FPD3046 == "No", "Climate",
                                        ifelse(FPD3044 == "No" & FPD3045 == "Yes" & FPD3046 == "No", "Other", 
                                               ifelse(FPD3044 == "Yes" & FPD3045 == "Yes" & FPD3046 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(plastic = factor(plastic, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3044, data_partner$FPD3045)
table(data_partner$plastic, useNA = "ifany")


# Chosen sustainable items
data_partner <- data_partner %>%
  mutate(FPD3047 = ifelse(FPD3047 == "Yes" | FPD3047 == "No", FPD3047, NA)) %>%
  mutate(FPD3048 = ifelse(FPD3048 == "Yes" | FPD3048 == "No", FPD3048, NA)) %>%
  mutate(FPD3049 = ifelse(FPD3049 == "Yes" | FPD3049 == "No", FPD3049, NA)) %>%
  mutate(sustainable = ifelse(is.na(FPD3047) | is.na(FPD3048) | is.na(FPD3049), NA,
                              ifelse(FPD3047 == "No" & FPD3048 == "No" & FPD3049 == "Yes", "No",
                                     ifelse(FPD3047 == "Yes" & FPD3048 == "No" & FPD3049 == "No", "Climate",
                                            ifelse(FPD3047 == "No" & FPD3048 == "Yes" & FPD3049 == "No", "Other", 
                                                   ifelse(FPD3047 == "Yes" & FPD3048 == "Yes" & FPD3049 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(sustainable = factor(sustainable, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3047, data_partner$FPD3048)
table(data_partner$sustainable, useNA = "ifany")


# Improved home insulation
data_partner <- data_partner %>%
  mutate(FPD3050 = ifelse(FPD3050 == "Yes" | FPD3050 == "No", FPD3050, NA)) %>%
  mutate(FPD3051 = ifelse(FPD3051 == "Yes" | FPD3051 == "No", FPD3051, NA)) %>%
  mutate(FPD3052 = ifelse(FPD3052 == "Yes" | FPD3052 == "No", FPD3052, NA)) %>%
  mutate(insulation = ifelse(is.na(FPD3050) | is.na(FPD3051) | is.na(FPD3052), NA,
                             ifelse(FPD3050 == "No" & FPD3051 == "No" & FPD3052 == "Yes", "No",
                                    ifelse(FPD3050 == "Yes" & FPD3051 == "No" & FPD3052 == "No", "Climate",
                                           ifelse(FPD3050 == "No" & FPD3051 == "Yes" & FPD3052 == "No", "Other", 
                                                  ifelse(FPD3050 == "Yes" & FPD3051 == "Yes" & FPD3052 == "No", 
                                                         "ClimateOther", NA)))))) %>%
  mutate(insulation = factor(insulation, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3050, data_partner$FPD3051)
table(data_partner$insulation, useNA = "ifany")


# Installed solar panels
data_partner <- data_partner %>%
  mutate(FPD3053 = ifelse(FPD3053 == "Yes" | FPD3053 == "No", FPD3053, NA)) %>%
  mutate(FPD3054 = ifelse(FPD3054 == "Yes" | FPD3054 == "No", FPD3054, NA)) %>%
  mutate(FPD3055 = ifelse(FPD3055 == "Yes" | FPD3055 == "No", FPD3055, NA)) %>%
  mutate(solar = ifelse(is.na(FPD3053) | is.na(FPD3054) | is.na(FPD3055), NA,
                        ifelse(FPD3053 == "No" & FPD3054 == "No" & FPD3055 == "Yes", "No",
                               ifelse(FPD3053 == "Yes" & FPD3054 == "No" & FPD3055 == "No", "Climate",
                                      ifelse(FPD3053 == "No" & FPD3054 == "Yes" & FPD3055 == "No", "Other", 
                                             ifelse(FPD3053 == "Yes" & FPD3054 == "Yes" & FPD3055 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(solar = factor(solar, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3053, data_partner$FPD3054)
table(data_partner$solar, useNA = "ifany")


# Started growing vegetables
data_partner <- data_partner %>%
  mutate(FPD3056 = ifelse(FPD3056 == "Yes" | FPD3056 == "No", FPD3056, NA)) %>%
  mutate(FPD3057 = ifelse(FPD3057 == "Yes" | FPD3057 == "No", FPD3057, NA)) %>%
  mutate(FPD3058 = ifelse(FPD3058 == "Yes" | FPD3058 == "No", FPD3058, NA)) %>%
  mutate(veg = ifelse(is.na(FPD3056) | is.na(FPD3057) | is.na(FPD3058), NA,
                      ifelse(FPD3056 == "No" & FPD3057 == "No" & FPD3058 == "Yes", "No",
                             ifelse(FPD3056 == "Yes" & FPD3057 == "No" & FPD3058 == "No", "Climate",
                                    ifelse(FPD3056 == "No" & FPD3057 == "Yes" & FPD3058 == "No", "Other", 
                                           ifelse(FPD3056 == "Yes" & FPD3057 == "Yes" & FPD3058 == "No", 
                                                  "ClimateOther", NA)))))) %>%
  mutate(veg = factor(veg, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3056, data_partner$FPD3057)
table(data_partner$veg, useNA = "ifany")


# Planted trees
data_partner <- data_partner %>%
  mutate(FPD3059 = ifelse(FPD3059 == "Yes" | FPD3059 == "No", FPD3059, NA)) %>%
  mutate(FPD3060 = ifelse(FPD3060 == "Yes" | FPD3060 == "No", FPD3060, NA)) %>%
  mutate(FPD3061 = ifelse(FPD3061 == "Yes" | FPD3061 == "No", FPD3061, NA)) %>%
  mutate(trees = ifelse(is.na(FPD3059) | is.na(FPD3060) | is.na(FPD3061), NA,
                        ifelse(FPD3059 == "No" & FPD3060 == "No" & FPD3061 == "Yes", "No",
                               ifelse(FPD3059 == "Yes" & FPD3060 == "No" & FPD3061 == "No", "Climate",
                                      ifelse(FPD3059 == "No" & FPD3060 == "Yes" & FPD3061 == "No", "Other", 
                                             ifelse(FPD3059 == "Yes" & FPD3060 == "Yes" & FPD3061 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(trees = factor(trees, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3059, data_partner$FPD3060)
table(data_partner$trees, useNA = "ifany")


# Avoided fossil fuel organisation
data_partner <- data_partner %>%
  mutate(FPD3062 = ifelse(FPD3062 == "Yes" | FPD3062 == "No", FPD3062, NA)) %>%
  mutate(FPD3063 = ifelse(FPD3063 == "Yes" | FPD3063 == "No", FPD3063, NA)) %>%
  mutate(FPD3064 = ifelse(FPD3064 == "Yes" | FPD3064 == "No", FPD3064, NA)) %>%
  mutate(avoidFossil = ifelse(is.na(FPD3062) | is.na(FPD3063) | is.na(FPD3064), NA,
                              ifelse(FPD3062 == "No" & FPD3063 == "No" & FPD3064 == "Yes", "No",
                                     ifelse(FPD3062 == "Yes" & FPD3063 == "No" & FPD3064 == "No", "Climate",
                                            ifelse(FPD3062 == "No" & FPD3063 == "Yes" & FPD3064 == "No", "Other", 
                                                   ifelse(FPD3062 == "Yes" & FPD3063 == "Yes" & FPD3064 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(avoidFossil = factor(avoidFossil, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3062, data_partner$FPD3063)
table(data_partner$avoidFossil, useNA = "ifany")


# Planned fewer children
data_partner <- data_partner %>%
  mutate(FPD3065 = ifelse(FPD3065 == "Yes" | FPD3065 == "No", FPD3065, NA)) %>%
  mutate(FPD3066 = ifelse(FPD3066 == "Yes" | FPD3066 == "No", FPD3066, NA)) %>%
  mutate(FPD3067 = ifelse(FPD3067 == "Yes" | FPD3067 == "No", FPD3067, NA)) %>%
  mutate(children = ifelse(is.na(FPD3065) | is.na(FPD3066) | is.na(FPD3067), NA,
                           ifelse(FPD3065 == "No" & FPD3066 == "No" & FPD3067 == "Yes", "No",
                                  ifelse(FPD3065 == "Yes" & FPD3066 == "No" & FPD3067 == "No", "Climate",
                                         ifelse(FPD3065 == "No" & FPD3066 == "Yes" & FPD3067 == "No", "Other", 
                                                ifelse(FPD3065 == "Yes" & FPD3066 == "Yes" & FPD3067 == "No", 
                                                       "ClimateOther", NA)))))) %>%
  mutate(children = factor(children, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3065, data_partner$FPD3066)
table(data_partner$children, useNA = "ifany")


# Taken other climate action
data_partner <- data_partner %>%
  mutate(FPD3068 = ifelse(FPD3068 == "Yes" | FPD3068 == "No", FPD3068, NA)) %>%
  mutate(FPD3069 = ifelse(FPD3069 == "Yes" | FPD3069 == "No", FPD3069, NA)) %>%
  mutate(FPD3070 = ifelse(FPD3070 == "Yes" | FPD3070 == "No", FPD3070, NA)) %>%
  mutate(otherAction = ifelse(is.na(FPD3068) | is.na(FPD3069) | is.na(FPD3070), NA,
                              ifelse(FPD3068 == "No" & FPD3069 == "No" & FPD3070 == "Yes", "No",
                                     ifelse(FPD3068 == "Yes" & FPD3069 == "No" & FPD3070 == "No", "Climate",
                                            ifelse(FPD3068 == "No" & FPD3069 == "Yes" & FPD3070 == "No", "Other", 
                                                   ifelse(FPD3068 == "Yes" & FPD3069 == "Yes" & FPD3070 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(otherAction = factor(otherAction, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3068, data_partner$FPD3069)
table(data_partner$otherAction, useNA = "ifany")


# Reduced meat/dairy consumption
data_partner <- data_partner %>%
  mutate(FPD3071 = ifelse(FPD3071 == "Yes" | FPD3071 == "No", FPD3071, NA)) %>%
  mutate(FPD3072 = ifelse(FPD3072 == "Yes" | FPD3072 == "No", FPD3072, NA)) %>%
  mutate(FPD3073 = ifelse(FPD3073 == "Yes" | FPD3073 == "No", FPD3073, NA)) %>%
  mutate(meatDairy = ifelse(is.na(FPD3071) | is.na(FPD3072) | is.na(FPD3073), NA,
                            ifelse(FPD3071 == "No" & FPD3072 == "No" & FPD3073 == "Yes", "No",
                                   ifelse(FPD3071 == "Yes" & FPD3072 == "No" & FPD3073 == "No", "Climate",
                                          ifelse(FPD3071 == "No" & FPD3072 == "Yes" & FPD3073 == "No", "Other", 
                                                 ifelse(FPD3071 == "Yes" & FPD3072 == "Yes" & FPD3073 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(meatDairy = factor(meatDairy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_partner$FPD3071, data_partner$FPD3072)
table(data_partner$meatDairy, useNA = "ifany")

# Check answers if vegan/vegatarian - This is complicated, as some vegetarians/vegans answered this question, while others did not (vegetarians should also not be a separate category as they can still reduce dairy consumption...). Will exclude answers from those who said 'always vegan' (as should not consume any meat or dairy products), but keep answers from those who said 'always vegetarian' 
table(data_partner$FPD3074)
table(data_partner$FPD3075)
table(data_partner$FPD3074, data_partner$FPD3075)

table(data_partner$meatDairy[data_partner$FPD3074 == "Yes"])
table(data_partner$meatDairy[data_partner$FPD3075 == "Yes"])

data_partner$meatDairy[data_partner$FPD3075 == "Yes"] <- NA
table(data_partner$meatDairy, useNA = "ifany")


## Drop all of the original behaviour variables
data_partner <- data_partner %>%
  select(-c(FPD3020:FPD3075))

glimpse(data_partner)



### Now tidy the potential confounders

# Age (at birth)
table(data_partner$partner_age, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(partner_age = na_if(partner_age, "Insufficient DOB information")) %>%
  mutate(partner_age = as.numeric(partner_age)) %>%
  mutate(partner_age = ifelse(is.na(partner_age), partner_age,
                              ifelse(partner_age > 55, 55, partner_age))) %>%
  rename(ageAtBirth = partner_age)

table(data_partner$ageAtBirth, useNA = "ifany")
summary(data_partner$ageAtBirth)


# Age (at climate questionnaire)
table(data_partner$FPD6500, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(FPD6500 = na_if(FPD6500, "Did not complete questionnaire")) %>%
  mutate(FPD6500 = as.numeric(FPD6500)) %>%
  rename(ageAtQ = FPD6500)

table(data_partner$ageAtQ, useNA = "ifany")
summary(data_partner$ageAtQ)


# Partner's ethnicity (White vs other than White)
table(data_partner$c801, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c801 = na_if(c801, "Missing")) %>%
  mutate(c801 = recode(c801, "Bangladeshi" = "Other than White", "Black African" = "Other than White", 
                       "Black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "Other" = "Other than White", 
                       "Other black" = "Other than White", "Pakistani" = "Other than White")) %>%
  mutate(c801 = factor(c801, levels = c("White", "Other than White"))) %>%
  rename(ethnicity = c801)

table(data_partner$ethnicity, useNA = "ifany")


# Marital status (Married vs Never married vs widowed/divorced/separated)
table(data_partner$pa065, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa065 = na_if(pa065, "-1")) %>%
  mutate(pa065 = recode(pa065, "1st marriage" = "Married", "Divorced" = "Sep/Div/Widow", 
                        "Marriage 2 or 3" = "Married",
                        "Separated" = "Sep/Div/Widow", "Widowed" = "Sep/Div/Widow")) %>%
  mutate(pa065 = factor(pa065, levels = c("Married", "Never married", "Sep/Div/Widow"))) %>%
  rename(marital = pa065)

table(data_partner$marital, useNA = "ifany")


# Urban/rural status (Urban vs Rural)
table(data_partner$jan2014ur01ind_M, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(jan2014ur01ind_M = na_if(jan2014ur01ind_M, "Missing")) %>%
  mutate(jan2014ur01ind_M = na_if(jan2014ur01ind_M, "Triplets/Quadruplets")) %>%
  mutate(jan2014ur01ind_M = recode(jan2014ur01ind_M, "Hamlet and Isolated Dwelling" = "Rural", 
                                   "Town and Fringe" = "Rural", "Village" = "Rural", 
                                   "Urban (pop. >= 10k)" = "Urban")) %>%
  mutate(jan2014ur01ind_M = factor(jan2014ur01ind_M, levels = c("Urban", "Rural"))) %>%
  rename(rural = jan2014ur01ind_M)

table(data_partner$rural, useNA = "ifany")


# Partner's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_partner$c666a, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c666a = na_if(c666a, "Missing")) %>%
  mutate(c666a = recode(c666a, "CSE" = "CSE/None")) %>%
  mutate(c666a = factor(c666a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(edu = c666a)

table(data_partner$edu, useNA = "ifany")


# Partner's occupational social class (I vs II vs III (non-manual) vs III (manual) vs IV/V)
table(data_partner$c765, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c765 = na_if(c765, "Missing")) %>%
  mutate(c765 = na_if(c765, "Armed forces")) %>%
  mutate(c765 = recode(c765, "IV" = "IV/V", "V" = "IV/V")) %>%
  mutate(c765 = factor(c765, levels = c("I", "II", "III (non-manual)", "III (manual)", "IV/V"))) %>%
  rename(occClass = c765)

table(data_partner$occClass, useNA = "ifany")


# Weekly household income after tax (£0-£100 vs £100-£199 vs £200-£299 vs £300-£399 vs £400 and above)
table(data_partner$h470, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(h470 = na_if(h470, "Not stated")) %>%
  mutate(h470 = factor(h470, levels = c("<100", "100 - 199", "200 - 299", "300 - 399", ">400"))) %>%
  rename(income = h470)

table(data_partner$income, useNA = "ifany")


# Area-level index of multiple deprivation (IMD; quintiles)
table(data_partner$jan2014imd2010q5_M, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(jan2014imd2010q5_M = na_if(jan2014imd2010q5_M, "Missing")) %>%
  mutate(jan2014imd2010q5_M = na_if(jan2014imd2010q5_M, "Triplets/Quadruplets")) %>%
  mutate(jan2014imd2010q5_M = recode(jan2014imd2010q5_M, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan2014imd2010q5_M = factor(jan2014imd2010q5_M, 
                                     levels = c("Quin. 1/Least deprived", 
                                                "Quintile 2", "Quintile 3", 
                                                "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan2014imd2010q5_M)

table(data_partner$imd, useNA = "ifany")


# Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other)
table(data_partner$a006, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = a006)

table(data_partner$home, useNA = "ifany")


### Save partner's data (in R, CSV and Stata format; the R and Stata formats will keep all the factor formatting, while the CSV file will lose this)
save(data_partner, file = "data_partner_processed_B4123.RData")
write_csv(data_partner, file = "data_partner_processed_B4123.csv")
write_dta(data_partner, "data_partner_processed_B4123.dta")




###########################################################################################
#### Next, process the offspring's data
data_offspring <- data_raw


### Removing some observations 

## Drop data if offspring withdrew consent for data to be used
table(data_offspring$YPG3000, useNA = "ifany")

data_offspring <- data_offspring %>%
  filter(YPG3000 != ".b" | is.na(YPG3000))

## Drop data if mother withdrew consent for data to be used
table(data_offspring$kz021, useNA = "ifany")

data_offspring <- data_offspring %>%
  filter(kz021 != ".a")

## Drop if pregnancy not alive at 1 year of age
table(data_offspring$kz011b, useNA = "ifany")

data_offspring <- data_offspring %>%
  filter(kz011b == "Yes")

## Drop if non-Christian religion (so only compare Christian vs non-religious participants)
table(data_offspring$YPG3040, useNA = "ifany")

data_offspring <- data_offspring %>%
  filter((YPG3040 != "Buddhist" & YPG3040 != "Jewish" & YPG3040 != "Hindu" & YPG3040 != "Muslim" 
          & YPG3040 != "Sikh" & YPG3040 != "Other (e.g. New Age Taoist Spiritualist)") | is.na(YPG3040))

table(data_offspring$YPG3040, useNA = "ifany")


### Keep just the variables of interest and re-order
data_offspring <- data_offspring %>%
  relocate(aln, qlet, YPJ7500, kz021, c804, YPG1052, jan2021ur01ind_YP, YPF7970, YPC2492, YPE6020,
           jan2021imd2015q5_YP, YPG1060, YPB9530:YPB9539,
           YPG3000, YPG3040, YPG3080, CLASS, YPJ3000:YPJ3075) %>%
  select(aln:YPJ3075)

colnames(data_offspring)


#### Now process the data and prep variables for analysis

### Start with exposures (religious belief, affiliation and attendance)

## Religious belief (No vs Not sure vs Yes)
table(data_offspring$YPG3000, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_offspring <- data_offspring %>%
  mutate(YPG3000 = na_if(YPG3000, "Questionnaire not completed")) %>%
  mutate(YPG3000 = na_if(YPG3000, "Missed whole section C")) %>%
  mutate(YPG3000 = na_if(YPG3000, "NS/NK")) %>%
  mutate(YPG3000 = na_if(YPG3000, "Unresolvable")) %>%
  mutate(YPG3000 = factor(YPG3000, levels = c("No", "Not sure", "Yes"))) %>%
  rename(belief = YPG3000)

table(data_offspring$belief, useNA = "ifany")
round(prop.table(table(data_offspring$belief)) * 100, 1)
sum(table(data_offspring$belief))


## Religious affiliation/identity (None vs Christian)
table(data_offspring$YPG3040, useNA = "ifany")

# If missing, code as NA, combine all religious affiliations together, then convert to factor and order levels
data_offspring <- data_offspring %>%
  mutate(YPG3040 = na_if(YPG3040, "Questionnaire not completed")) %>%
  mutate(YPG3040 = na_if(YPG3040, "Missed whole section C")) %>%
  mutate(YPG3040 = na_if(YPG3040, "NS/NK")) %>%
  mutate(YPG3040 = na_if(YPG3040, "Unresolvable")) %>%
  mutate(YPG3040 = recode(YPG3040, "Church of England" = "CofE", "Baptist/Evangelical" = "Other", 
                        "Jehovah's Witness" = "Other", "Methodist" = "Other",
                        "Other Christian (e.g. Christian Science Mormon Presbyterian Evangelical Orthodox" 
                        = "Other", "Roman Catholic" = "Catholic")) %>%
  mutate(YPG3040 = factor(YPG3040, levels = c("None", "CofE", "Catholic", "Other"))) %>%
  rename(identity_denom = YPG3040) %>%
  mutate(identity = recode(identity_denom, "CofE" = "Christian", "Catholic" = "Christian", 
                           "Other" = "Christian")) %>%
  mutate(identity = factor(identity, levels = c("None", "Christian"))) %>%
  relocate(aln:belief, identity)

table(data_offspring$identity, useNA = "ifany")
round(prop.table(table(data_offspring$identity)) * 100, 1)
sum(table(data_offspring$identity))

table(data_offspring$identity_denom, useNA = "ifany")
round(prop.table(table(data_offspring$identity_denom)) * 100, 1)
sum(table(data_offspring$identity_denom))


## Religious attendance (Occasional/non-attendance vs Regular)
table(data_offspring$YPG3080, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_offspring <- data_offspring %>%
  mutate(YPG3080 = na_if(YPG3080, "NS/NK")) %>%
  mutate(YPG3080 = na_if(YPG3080, "Missed whole section C")) %>%
  mutate(YPG3080 = na_if(YPG3080, "Questionnaire not completed")) %>%
  mutate(YPG3080 = na_if(YPG3080, "Unresolvable")) %>%
  mutate(YPG3080 = recode(YPG3080, "At least once a month" = "Regular", "At least once a week" = "Regular", 
                        "At least once a year" = "Occasional/None", "Not at all" = "Occasional/None",
                        "Occasionally" = "Occasional/None")) %>%
  mutate(YPG3080 = factor(YPG3080, levels = c("Occasional/None", "Regular"))) %>%
  rename(attend = YPG3080)

table(data_offspring$attend, useNA = "ifany")
round(prop.table(table(data_offspring$attend)) * 100, 1)
sum(table(data_offspring$attend))


## Latent classes
table(data_offspring$CLASS, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(CLASS = factor(CLASS, levels = c("Atheist", "Agnostic", "Moderately religious", 
                                                    "Highly religious"))) %>%
  rename(lca = CLASS)

table(data_offspring$lca, useNA = "ifany")
round(prop.table(table(data_offspring$lca)) * 100, 1)
sum(table(data_offspring$lca))



### Now to outcomes (climate beliefs and behaviours)

# Believes that the climate is changing - ordered categorical variable 
table(data_offspring$YPJ3000, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPJ3000 = na_if(YPJ3000, "Did not complete questionnaire")) %>%
  mutate(YPJ3000 = na_if(YPJ3000, "NS/NK")) %>%
  mutate(YPJ3000 = recode(YPJ3000, "Yes, definitely" = "Yes definitely")) %>%
  mutate(YPJ3000 = factor(YPJ3000, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely"), ordered = TRUE)) %>%
  rename(climateChanging = YPJ3000)

table(data_offspring$climateChanging, useNA = "ifany")
round(prop.table(table(data_offspring$climateChanging)) * 100, 1)
sum(table(data_offspring$climateChanging))


# Degree to which YP is concerned about the impact of climate change (only answered if believe climate is changing) - ordered categorical variable
table(data_offspring$YPJ3001, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPJ3001 = na_if(YPJ3001, "Did not complete questionnaire")) %>%
  mutate(YPJ3001 = na_if(YPJ3001, "NS/NK")) %>%
  mutate(YPJ3001 = na_if(YPJ3001, "Definitely does not believe climate is changing")) %>%
  mutate(YPJ3001 = factor(YPJ3001, levels = c("Not at all concerned", "Not very concerned", 
                                              "Somewhat concerned", "Very concerned"), ordered = TRUE)) %>%
  rename(climateConcern = YPJ3001)

table(data_offspring$climateConcern, useNA = "ifany")
round(prop.table(table(data_offspring$climateConcern)) * 100, 1)
sum(table(data_offspring$climateConcern))


# Believes that humans are to blame for climate change (only answered if believe climate is changing) - ordered categorical variable
table(data_offspring$YPJ3002, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPJ3002 = na_if(YPJ3002, "Did not complete questionnaire")) %>%
  mutate(YPJ3002 = na_if(YPJ3002, "NS/NK")) %>%
  mutate(YPJ3002 = na_if(YPJ3002, "Definitely does not believe climate is changing")) %>%
  mutate(YPJ3002 = factor(YPJ3002, levels = c("Not at all", "Yes, for some of it", "Yes, for most of it",
                                          "Yes, for all of it"), ordered = TRUE)) %>%
  rename(climateHumans = YPJ3002)

table(data_offspring$climateHumans, useNA = "ifany")
round(prop.table(table(data_offspring$climateHumans)) * 100, 1)
sum(table(data_offspring$climateHumans))


# Personal actions will make difference to long-term climate changes (only answered if believe climate is changing) - unordered categorical variable
table(data_offspring$YPJ3003, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPJ3003 = na_if(YPJ3003, "Did not complete questionnaire")) %>%
  mutate(YPJ3003 = na_if(YPJ3003, "NS/NK")) %>%
  mutate(YPJ3003 = na_if(YPJ3003, "Definitely does not believe climate is changing")) %>%
  mutate(YPJ3003 = na_if(YPJ3003, "Unresolvable/value out of possible range")) %>%
  mutate(YPJ3003 = factor(YPJ3003, levels = c("No", "Not sure", "Yes"))) %>%
  rename(climateAction = YPJ3003)

table(data_offspring$climateAction, useNA = "ifany")
round(prop.table(table(data_offspring$climateAction)) * 100, 1)
sum(table(data_offspring$climateAction))


## Now go through the 'actions taken due to climate change' questions and recode as appropriate into 'not done this' vs 'done for non-climate reasons' vs 'done for climate reasons' vs 'done for both climate and non-climate reasons' (while excluding impossible combinations of values - e.g., 'not done and done')

# Changed way travelled locally
data_offspring <- data_offspring %>%
  mutate(YPJ3020 = ifelse(YPJ3020 == "Yes" | YPJ3020 == "No", YPJ3020, NA)) %>%
  mutate(YPJ3021 = ifelse(YPJ3021 == "Yes" | YPJ3021 == "No", YPJ3021, NA)) %>%
  mutate(YPJ3022 = ifelse(YPJ3022 == "Yes" | YPJ3022 == "No", YPJ3022, NA)) %>%
  mutate(travel = ifelse(is.na(YPJ3020) | is.na(YPJ3021) | is.na(YPJ3022), NA,
                         ifelse(YPJ3020 == "No" & YPJ3021 == "No" & YPJ3022 == "Yes", "No",
                                ifelse(YPJ3020 == "Yes" & YPJ3021 == "No" & YPJ3022 == "No", "Climate",
                                       ifelse(YPJ3020 == "No" & YPJ3021 == "Yes" & YPJ3022 == "No", "Other", 
                                              ifelse(YPJ3020 == "Yes" & YPJ3021 == "Yes" & YPJ3022 == "No", 
                                                     "ClimateOther", NA)))))) %>%
  mutate(travel = factor(travel, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3020, data_offspring$YPJ3021)
table(data_offspring$travel, useNA = "ifany")


# Reduced household waste
data_offspring <- data_offspring %>%
  mutate(YPJ3023 = ifelse(YPJ3023 == "Yes" | YPJ3023 == "No", YPJ3023, NA)) %>%
  mutate(YPJ3024 = ifelse(YPJ3024 == "Yes" | YPJ3024 == "No", YPJ3024, NA)) %>%
  mutate(YPJ3025 = ifelse(YPJ3025 == "Yes" | YPJ3025 == "No", YPJ3025, NA)) %>%
  mutate(waste = ifelse(is.na(YPJ3023) | is.na(YPJ3024) | is.na(YPJ3025), NA,
                        ifelse(YPJ3023 == "No" & YPJ3024 == "No" & YPJ3025 == "Yes", "No",
                               ifelse(YPJ3023 == "Yes" & YPJ3024 == "No" & YPJ3025 == "No", "Climate",
                                      ifelse(YPJ3023 == "No" & YPJ3024 == "Yes" & YPJ3025 == "No", "Other", 
                                             ifelse(YPJ3023 == "Yes" & YPJ3024 == "Yes" & YPJ3025 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(waste = factor(waste, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3023, data_offspring$YPJ3024)
table(data_offspring$waste, useNA = "ifany")


# Reduced energy use
data_offspring <- data_offspring %>%
  mutate(YPJ3026 = ifelse(YPJ3026 == "Yes" | YPJ3026 == "No", YPJ3026, NA)) %>%
  mutate(YPJ3027 = ifelse(YPJ3027 == "Yes" | YPJ3027 == "No", YPJ3027, NA)) %>%
  mutate(YPJ3028 = ifelse(YPJ3028 == "Yes" | YPJ3028 == "No", YPJ3028, NA)) %>%
  mutate(energy = ifelse(is.na(YPJ3026) | is.na(YPJ3027) | is.na(YPJ3028), NA,
                         ifelse(YPJ3026 == "No" & YPJ3027 == "No" & YPJ3028 == "Yes", "No",
                                ifelse(YPJ3026 == "Yes" & YPJ3027 == "No" & YPJ3028 == "No", "Climate",
                                       ifelse(YPJ3026 == "No" & YPJ3027 == "Yes" & YPJ3028 == "No", "Other", 
                                              ifelse(YPJ3026 == "Yes" & YPJ3027 == "Yes" & YPJ3028 == "No", 
                                                     "ClimateOther", NA)))))) %>%
  mutate(energy = factor(energy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3026, data_offspring$YPJ3027)
table(data_offspring$energy, useNA = "ifany")


# Changed what buy
data_offspring <- data_offspring %>%
  mutate(YPJ3029 = ifelse(YPJ3029 == "Yes" | YPJ3029 == "No", YPJ3029, NA)) %>%
  mutate(YPJ3030 = ifelse(YPJ3030 == "Yes" | YPJ3030 == "No", YPJ3030, NA)) %>%
  mutate(YPJ3031 = ifelse(YPJ3031 == "Yes" | YPJ3031 == "No", YPJ3031, NA)) %>%
  mutate(buy = ifelse(is.na(YPJ3029) | is.na(YPJ3030) | is.na(YPJ3031), NA,
                      ifelse(YPJ3029 == "No" & YPJ3030 == "No" & YPJ3031 == "Yes", "No",
                             ifelse(YPJ3029 == "Yes" & YPJ3030 == "No" & YPJ3031 == "No", "Climate",
                                    ifelse(YPJ3029 == "No" & YPJ3030 == "Yes" & YPJ3031 == "No", "Other", 
                                           ifelse(YPJ3029 == "Yes" & YPJ3030 == "Yes" & YPJ3031 == "No", 
                                                  "ClimateOther", NA)))))) %>%
  mutate(buy = factor(buy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3029, data_offspring$YPJ3030)
table(data_offspring$buy, useNA = "ifany")


# Reduced air travel
data_offspring <- data_offspring %>%
  mutate(YPJ3032 = ifelse(YPJ3032 == "Yes" | YPJ3032 == "No", YPJ3032, NA)) %>%
  mutate(YPJ3033 = ifelse(YPJ3033 == "Yes" | YPJ3033 == "No", YPJ3033, NA)) %>%
  mutate(YPJ3034 = ifelse(YPJ3034 == "Yes" | YPJ3034 == "No", YPJ3034, NA)) %>%
  mutate(airTravel = ifelse(is.na(YPJ3032) | is.na(YPJ3033) | is.na(YPJ3034), NA,
                            ifelse(YPJ3032 == "No" & YPJ3033 == "No" & YPJ3034 == "Yes", "No",
                                   ifelse(YPJ3032 == "Yes" & YPJ3033 == "No" & YPJ3034 == "No", "Climate",
                                          ifelse(YPJ3032 == "No" & YPJ3033 == "Yes" & YPJ3034 == "No", "Other", 
                                                 ifelse(YPJ3032 == "Yes" & YPJ3033 == "Yes" & YPJ3034 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(airTravel = factor(airTravel, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3032, data_offspring$YPJ3033)
table(data_offspring$airTravel, useNA = "ifany")


# Electric/hybrid car
data_offspring <- data_offspring %>%
  mutate(YPJ3035 = ifelse(YPJ3035 == "Yes" | YPJ3035 == "No", YPJ3035, NA)) %>%
  mutate(YPJ3036 = ifelse(YPJ3036 == "Yes" | YPJ3036 == "No", YPJ3036, NA)) %>%
  mutate(YPJ3037 = ifelse(YPJ3037 == "Yes" | YPJ3037 == "No", YPJ3037, NA)) %>%
  mutate(elecCar = ifelse(is.na(YPJ3035) | is.na(YPJ3036) | is.na(YPJ3037), NA,
                          ifelse(YPJ3035 == "No" & YPJ3036 == "No" & YPJ3037 == "Yes", "No",
                                 ifelse(YPJ3035 == "Yes" & YPJ3036 == "No" & YPJ3037 == "No", "Climate",
                                        ifelse(YPJ3035 == "No" & YPJ3036 == "Yes" & YPJ3037 == "No", "Other", 
                                               ifelse(YPJ3035 == "Yes" & YPJ3036 == "Yes" & YPJ3037 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(elecCar = factor(elecCar, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3035, data_offspring$YPJ3036)
table(data_offspring$elecCar, useNA = "ifany")


# Bought local food
data_offspring <- data_offspring %>%
  mutate(YPJ3038 = ifelse(YPJ3038 == "Yes" | YPJ3038 == "No", YPJ3038, NA)) %>%
  mutate(YPJ3039 = ifelse(YPJ3039 == "Yes" | YPJ3039 == "No", YPJ3039, NA)) %>%
  mutate(YPJ3040 = ifelse(YPJ3040 == "Yes" | YPJ3040 == "No", YPJ3040, NA)) %>%
  mutate(localFood = ifelse(is.na(YPJ3038) | is.na(YPJ3039) | is.na(YPJ3040), NA,
                            ifelse(YPJ3038 == "No" & YPJ3039 == "No" & YPJ3040 == "Yes", "No",
                                   ifelse(YPJ3038 == "Yes" & YPJ3039 == "No" & YPJ3040 == "No", "Climate",
                                          ifelse(YPJ3038 == "No" & YPJ3039 == "Yes" & YPJ3040 == "No", "Other", 
                                                 ifelse(YPJ3038 == "Yes" & YPJ3039 == "Yes" & YPJ3040 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(localFood = factor(localFood, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3038, data_offspring$YPJ3039)
table(data_offspring$localFood, useNA = "ifany")


# Recycled more
data_offspring <- data_offspring %>%
  mutate(YPJ3041 = ifelse(YPJ3041 == "Yes" | YPJ3041 == "No", YPJ3041, NA)) %>%
  mutate(YPJ3042 = ifelse(YPJ3042 == "Yes" | YPJ3042 == "No", YPJ3042, NA)) %>%
  mutate(YPJ3043 = ifelse(YPJ3043 == "Yes" | YPJ3043 == "No", YPJ3043, NA)) %>%
  mutate(recycle = ifelse(is.na(YPJ3041) | is.na(YPJ3042) | is.na(YPJ3043), NA,
                          ifelse(YPJ3041 == "No" & YPJ3042 == "No" & YPJ3043 == "Yes", "No",
                                 ifelse(YPJ3041 == "Yes" & YPJ3042 == "No" & YPJ3043 == "No", "Climate",
                                        ifelse(YPJ3041 == "No" & YPJ3042 == "Yes" & YPJ3043 == "No", "Other", 
                                               ifelse(YPJ3041 == "Yes" & YPJ3042 == "Yes" & YPJ3043 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(recycle = factor(recycle, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3041, data_offspring$YPJ3042)
table(data_offspring$recycle, useNA = "ifany")


# Reduced plastic use
data_offspring <- data_offspring %>%
  mutate(YPJ3044 = ifelse(YPJ3044 == "Yes" | YPJ3044 == "No", YPJ3044, NA)) %>%
  mutate(YPJ3045 = ifelse(YPJ3045 == "Yes" | YPJ3045 == "No", YPJ3045, NA)) %>%
  mutate(YPJ3046 = ifelse(YPJ3046 == "Yes" | YPJ3046 == "No", YPJ3046, NA)) %>%
  mutate(plastic = ifelse(is.na(YPJ3044) | is.na(YPJ3045) | is.na(YPJ3046), NA,
                          ifelse(YPJ3044 == "No" & YPJ3045 == "No" & YPJ3046 == "Yes", "No",
                                 ifelse(YPJ3044 == "Yes" & YPJ3045 == "No" & YPJ3046 == "No", "Climate",
                                        ifelse(YPJ3044 == "No" & YPJ3045 == "Yes" & YPJ3046 == "No", "Other", 
                                               ifelse(YPJ3044 == "Yes" & YPJ3045 == "Yes" & YPJ3046 == "No", 
                                                      "ClimateOther", NA)))))) %>%
  mutate(plastic = factor(plastic, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3044, data_offspring$YPJ3045)
table(data_offspring$plastic, useNA = "ifany")


# Chosen sustainable items
data_offspring <- data_offspring %>%
  mutate(YPJ3047 = ifelse(YPJ3047 == "Yes" | YPJ3047 == "No", YPJ3047, NA)) %>%
  mutate(YPJ3048 = ifelse(YPJ3048 == "Yes" | YPJ3048 == "No", YPJ3048, NA)) %>%
  mutate(YPJ3049 = ifelse(YPJ3049 == "Yes" | YPJ3049 == "No", YPJ3049, NA)) %>%
  mutate(sustainable = ifelse(is.na(YPJ3047) | is.na(YPJ3048) | is.na(YPJ3049), NA,
                              ifelse(YPJ3047 == "No" & YPJ3048 == "No" & YPJ3049 == "Yes", "No",
                                     ifelse(YPJ3047 == "Yes" & YPJ3048 == "No" & YPJ3049 == "No", "Climate",
                                            ifelse(YPJ3047 == "No" & YPJ3048 == "Yes" & YPJ3049 == "No", "Other", 
                                                   ifelse(YPJ3047 == "Yes" & YPJ3048 == "Yes" & YPJ3049 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(sustainable = factor(sustainable, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3047, data_offspring$YPJ3048)
table(data_offspring$sustainable, useNA = "ifany")


# Improved home insulation
data_offspring <- data_offspring %>%
  mutate(YPJ3050 = ifelse(YPJ3050 == "Yes" | YPJ3050 == "No", YPJ3050, NA)) %>%
  mutate(YPJ3051 = ifelse(YPJ3051 == "Yes" | YPJ3051 == "No", YPJ3051, NA)) %>%
  mutate(YPJ3052 = ifelse(YPJ3052 == "Yes" | YPJ3052 == "No", YPJ3052, NA)) %>%
  mutate(insulation = ifelse(is.na(YPJ3050) | is.na(YPJ3051) | is.na(YPJ3052), NA,
                             ifelse(YPJ3050 == "No" & YPJ3051 == "No" & YPJ3052 == "Yes", "No",
                                    ifelse(YPJ3050 == "Yes" & YPJ3051 == "No" & YPJ3052 == "No", "Climate",
                                           ifelse(YPJ3050 == "No" & YPJ3051 == "Yes" & YPJ3052 == "No", "Other", 
                                                  ifelse(YPJ3050 == "Yes" & YPJ3051 == "Yes" & YPJ3052 == "No", 
                                                         "ClimateOther", NA)))))) %>%
  mutate(insulation = factor(insulation, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3050, data_offspring$YPJ3051)
table(data_offspring$insulation, useNA = "ifany")


# Installed solar panels
data_offspring <- data_offspring %>%
  mutate(YPJ3053 = ifelse(YPJ3053 == "Yes" | YPJ3053 == "No", YPJ3053, NA)) %>%
  mutate(YPJ3054 = ifelse(YPJ3054 == "Yes" | YPJ3054 == "No", YPJ3054, NA)) %>%
  mutate(YPJ3055 = ifelse(YPJ3055 == "Yes" | YPJ3055 == "No", YPJ3055, NA)) %>%
  mutate(solar = ifelse(is.na(YPJ3053) | is.na(YPJ3054) | is.na(YPJ3055), NA,
                        ifelse(YPJ3053 == "No" & YPJ3054 == "No" & YPJ3055 == "Yes", "No",
                               ifelse(YPJ3053 == "Yes" & YPJ3054 == "No" & YPJ3055 == "No", "Climate",
                                      ifelse(YPJ3053 == "No" & YPJ3054 == "Yes" & YPJ3055 == "No", "Other", 
                                             ifelse(YPJ3053 == "Yes" & YPJ3054 == "Yes" & YPJ3055 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(solar = factor(solar, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3053, data_offspring$YPJ3054)
table(data_offspring$solar, useNA = "ifany")


# Started growing vegetables
data_offspring <- data_offspring %>%
  mutate(YPJ3056 = ifelse(YPJ3056 == "Yes" | YPJ3056 == "No", YPJ3056, NA)) %>%
  mutate(YPJ3057 = ifelse(YPJ3057 == "Yes" | YPJ3057 == "No", YPJ3057, NA)) %>%
  mutate(YPJ3058 = ifelse(YPJ3058 == "Yes" | YPJ3058 == "No", YPJ3058, NA)) %>%
  mutate(veg = ifelse(is.na(YPJ3056) | is.na(YPJ3057) | is.na(YPJ3058), NA,
                      ifelse(YPJ3056 == "No" & YPJ3057 == "No" & YPJ3058 == "Yes", "No",
                             ifelse(YPJ3056 == "Yes" & YPJ3057 == "No" & YPJ3058 == "No", "Climate",
                                    ifelse(YPJ3056 == "No" & YPJ3057 == "Yes" & YPJ3058 == "No", "Other", 
                                           ifelse(YPJ3056 == "Yes" & YPJ3057 == "Yes" & YPJ3058 == "No", 
                                                  "ClimateOther", NA)))))) %>%
  mutate(veg = factor(veg, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3056, data_offspring$YPJ3057)
table(data_offspring$veg, useNA = "ifany")


# Planted trees
data_offspring <- data_offspring %>%
  mutate(YPJ3059 = ifelse(YPJ3059 == "Yes" | YPJ3059 == "No", YPJ3059, NA)) %>%
  mutate(YPJ3060 = ifelse(YPJ3060 == "Yes" | YPJ3060 == "No", YPJ3060, NA)) %>%
  mutate(YPJ3061 = ifelse(YPJ3061 == "Yes" | YPJ3061 == "No", YPJ3061, NA)) %>%
  mutate(trees = ifelse(is.na(YPJ3059) | is.na(YPJ3060) | is.na(YPJ3061), NA,
                        ifelse(YPJ3059 == "No" & YPJ3060 == "No" & YPJ3061 == "Yes", "No",
                               ifelse(YPJ3059 == "Yes" & YPJ3060 == "No" & YPJ3061 == "No", "Climate",
                                      ifelse(YPJ3059 == "No" & YPJ3060 == "Yes" & YPJ3061 == "No", "Other", 
                                             ifelse(YPJ3059 == "Yes" & YPJ3060 == "Yes" & YPJ3061 == "No", 
                                                    "ClimateOther", NA)))))) %>%
  mutate(trees = factor(trees, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3059, data_offspring$YPJ3060)
table(data_offspring$trees, useNA = "ifany")


# Avoided fossil fuel organisation
data_offspring <- data_offspring %>%
  mutate(YPJ3062 = ifelse(YPJ3062 == "Yes" | YPJ3062 == "No", YPJ3062, NA)) %>%
  mutate(YPJ3063 = ifelse(YPJ3063 == "Yes" | YPJ3063 == "No", YPJ3063, NA)) %>%
  mutate(YPJ3064 = ifelse(YPJ3064 == "Yes" | YPJ3064 == "No", YPJ3064, NA)) %>%
  mutate(avoidFossil = ifelse(is.na(YPJ3062) | is.na(YPJ3063) | is.na(YPJ3064), NA,
                              ifelse(YPJ3062 == "No" & YPJ3063 == "No" & YPJ3064 == "Yes", "No",
                                     ifelse(YPJ3062 == "Yes" & YPJ3063 == "No" & YPJ3064 == "No", "Climate",
                                            ifelse(YPJ3062 == "No" & YPJ3063 == "Yes" & YPJ3064 == "No", "Other", 
                                                   ifelse(YPJ3062 == "Yes" & YPJ3063 == "Yes" & YPJ3064 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(avoidFossil = factor(avoidFossil, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3062, data_offspring$YPJ3063)
table(data_offspring$avoidFossil, useNA = "ifany")


# Planned fewer children
data_offspring <- data_offspring %>%
  mutate(YPJ3065 = ifelse(YPJ3065 == "Yes" | YPJ3065 == "No", YPJ3065, NA)) %>%
  mutate(YPJ3066 = ifelse(YPJ3066 == "Yes" | YPJ3066 == "No", YPJ3066, NA)) %>%
  mutate(YPJ3067 = ifelse(YPJ3067 == "Yes" | YPJ3067 == "No", YPJ3067, NA)) %>%
  mutate(children = ifelse(is.na(YPJ3065) | is.na(YPJ3066) | is.na(YPJ3067), NA,
                           ifelse(YPJ3065 == "No" & YPJ3066 == "No" & YPJ3067 == "Yes", "No",
                                  ifelse(YPJ3065 == "Yes" & YPJ3066 == "No" & YPJ3067 == "No", "Climate",
                                         ifelse(YPJ3065 == "No" & YPJ3066 == "Yes" & YPJ3067 == "No", "Other", 
                                                ifelse(YPJ3065 == "Yes" & YPJ3066 == "Yes" & YPJ3067 == "No", 
                                                       "ClimateOther", NA)))))) %>%
  mutate(children = factor(children, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3065, data_offspring$YPJ3066)
table(data_offspring$children, useNA = "ifany")


# Taken other climate action
data_offspring <- data_offspring %>%
  mutate(YPJ3068 = ifelse(YPJ3068 == "Yes" | YPJ3068 == "No", YPJ3068, NA)) %>%
  mutate(YPJ3069 = ifelse(YPJ3069 == "Yes" | YPJ3069 == "No", YPJ3069, NA)) %>%
  mutate(YPJ3070 = ifelse(YPJ3070 == "Yes" | YPJ3070 == "No", YPJ3070, NA)) %>%
  mutate(otherAction = ifelse(is.na(YPJ3068) | is.na(YPJ3069) | is.na(YPJ3070), NA,
                              ifelse(YPJ3068 == "No" & YPJ3069 == "No" & YPJ3070 == "Yes", "No",
                                     ifelse(YPJ3068 == "Yes" & YPJ3069 == "No" & YPJ3070 == "No", "Climate",
                                            ifelse(YPJ3068 == "No" & YPJ3069 == "Yes" & YPJ3070 == "No", "Other", 
                                                   ifelse(YPJ3068 == "Yes" & YPJ3069 == "Yes" & YPJ3070 == "No", 
                                                          "ClimateOther", NA)))))) %>%
  mutate(otherAction = factor(otherAction, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3068, data_offspring$YPJ3069)
table(data_offspring$otherAction, useNA = "ifany")


# Reduced meat/dairy consumption
data_offspring <- data_offspring %>%
  mutate(YPJ3071 = ifelse(YPJ3071 == "Yes" | YPJ3071 == "No", YPJ3071, NA)) %>%
  mutate(YPJ3072 = ifelse(YPJ3072 == "Yes" | YPJ3072 == "No", YPJ3072, NA)) %>%
  mutate(YPJ3073 = ifelse(YPJ3073 == "Yes" | YPJ3073 == "No", YPJ3073, NA)) %>%
  mutate(meatDairy = ifelse(is.na(YPJ3071) | is.na(YPJ3072) | is.na(YPJ3073), NA,
                            ifelse(YPJ3071 == "No" & YPJ3072 == "No" & YPJ3073 == "Yes", "No",
                                   ifelse(YPJ3071 == "Yes" & YPJ3072 == "No" & YPJ3073 == "No", "Climate",
                                          ifelse(YPJ3071 == "No" & YPJ3072 == "Yes" & YPJ3073 == "No", "Other", 
                                                 ifelse(YPJ3071 == "Yes" & YPJ3072 == "Yes" & YPJ3073 == "No", 
                                                        "ClimateOther", NA)))))) %>%
  mutate(meatDairy = factor(meatDairy, levels = c("No", "Climate", "Other", "ClimateOther")))

table(data_offspring$YPJ3071, data_offspring$YPJ3072)
table(data_offspring$meatDairy, useNA = "ifany")

# Check answers if vegan/vegatarian - This is complicated, as some vegetarians/vegans answered this question, while others did not (vegetarians should also not be a separate category as they can still reduce dairy consumption...). Will exclude answers from those who said 'always vegan' (as should not consume any meat or dairy products), but keep answers from those who said 'always vegetarian' 
table(data_offspring$YPJ3074)
table(data_offspring$YPJ3075)
table(data_offspring$YPJ3074, data_offspring$YPJ3075)

table(data_offspring$meatDairy[data_offspring$YPJ3074 == "Yes"])
table(data_offspring$meatDairy[data_offspring$YPJ3075 == "Yes"])

data_offspring$meatDairy[data_offspring$YPJ3075 == "Yes"] <- NA
table(data_offspring$meatDairy, useNA = "ifany")


## Drop all of the original behaviour variables
data_offspring <- data_offspring %>%
  select(-c(YPJ3020:YPJ3075))

glimpse(data_offspring)



### Now tidy the potential confounders

# Age (at climate questionnaire)
table(data_offspring$YPJ7500, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPJ7500 = na_if(YPJ7500, "Did not complete questionnaire")) %>%
  mutate(YPJ7500 = as.numeric(YPJ7500)) %>%
  rename(ageAtQ = YPJ7500)

table(data_offspring$ageAtQ, useNA = "ifany")
summary(data_offspring$ageAtQ)


# Assigned sex at birth
table(data_offspring$kz021, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(kz021 = factor(kz021, levels = c("Male", "Female"))) %>%
  rename(sex = kz021)

table(data_offspring$sex, useNA = "ifany")


# Offspring's ethnicity (White vs other than White)
table(data_offspring$c804, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(c804 = na_if(c804, "Missing")) %>%
  mutate(c804 = recode(c804, "Non-white" = "Other than White")) %>%
  mutate(c804 = factor(c804, levels = c("White", "Other than White"))) %>%
  rename(ethnicity = c804)

table(data_offspring$ethnicity, useNA = "ifany")


# Relationship status (Living with a partner)
table(data_offspring$YPG1052, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPG1052 = na_if(YPG1052, "Missed whole section A")) %>%
  mutate(YPG1052 = na_if(YPG1052, "Not selected any A6 option")) %>%
  mutate(YPG1052 = na_if(YPG1052, "Questionnaire not completed")) %>%
  mutate(YPG1052 = factor(YPG1052, levels = c("No", "Yes"))) %>%
  rename(relationship = YPG1052)

table(data_offspring$relationship, useNA = "ifany")


# Urban/rural status (Urban vs Rural)
table(data_offspring$jan2021ur01ind_YP, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(jan2021ur01ind_YP = na_if(jan2021ur01ind_YP, "Missing")) %>%
  mutate(jan2021ur01ind_YP = na_if(jan2021ur01ind_YP, "Triplets/Quadruplets")) %>%
  mutate(jan2021ur01ind_YP = recode(jan2021ur01ind_YP, "Hamlet and Isolated Dwelling" = "Rural", 
                                   "Town and Fringe" = "Rural", "Village" = "Rural", 
                                   "Urban (pop. >= 10k)" = "Urban")) %>%
  mutate(jan2021ur01ind_YP = factor(jan2021ur01ind_YP, levels = c("Urban", "Rural"))) %>%
  rename(rural = jan2021ur01ind_YP)

table(data_offspring$rural, useNA = "ifany")


# Highest education qualification (GCSE/equivalent vs A-level/equivalent vs Degree vs post-graduate degree)
table(data_offspring$YPF7970, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPF7970 = na_if(YPF7970, "Missing")) %>%
  mutate(YPF7970 = na_if(YPF7970, "Missed whole section I")) %>%
  mutate(YPF7970 = na_if(YPF7970, "Questionnaire not completed")) %>%
  mutate(YPF7970 = recode(YPF7970, "A-Level, NVQ 3, BTEC 3" = "A level", 
                          "GCSE (C/B/A/A*), NVQ 2, BTEC 2" = "GCSE", 
                          "GCSE (G/F/E/D) NVQ 1, BTEC 1" = "GCSE", "Masters, PGCE" = "PostGrad", 
                          "NVQ 4, BTEC 4" = "A level", "NVQ 5, BTEC 5, HNC/HND" = "A level",
                          "PhD" = "PostGrad")) %>%
  mutate(YPF7970 = factor(YPF7970, levels = c("GCSE", "A level", "Degree", "PostGrad"))) %>%
  rename(edu = YPF7970)

table(data_offspring$edu, useNA = "ifany")


# Occupational social class (I vs II vs III (non-manual) vs III (manual) vs IV/V)
table(data_offspring$YPC2492, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPC2492 = na_if(YPC2492, "NS/NK")) %>%
  mutate(YPC2492 = na_if(YPC2492, "Questionnaire not completed")) %>%
  mutate(YPC2492 = recode(YPC2492, 
                          "Higher and lower managerial, administrative and professional occupations" = 
                            "Manager/Prof", "Intermediate occupations" = "Intermediate",
                          "Lower supervisory and technical occupations" = "Lower sup/tech",
                          "Semi-routine and routine occupations" = "Routine",
                          "Small employers and own account workers" = "Small employ")) %>%
  mutate(YPC2492 = factor(YPC2492, levels = c("Manager/Prof", "Intermediate", "Small employ", 
                                              "Lower sup/tech", "Routine"))) %>%
  rename(occClass = YPC2492)

table(data_offspring$occClass, useNA = "ifany")


# Weekly household income after tax (£0-£499 vs £500-£999 vs £1000-£1499 vs £1500-£1999 vs £2000 and above)
table(data_offspring$YPE6020, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPE6020 = na_if(YPE6020, "Missed whole section I")) %>%
  mutate(YPE6020 = na_if(YPE6020, "Missing")) %>%
  mutate(YPE6020 = na_if(YPE6020, "Questionnaire not completed")) %>%
  mutate(YPE6020 = recode(YPE6020, "\xa31 - \xa3499" = "<£500", "\xa3500 - \xa3999" = "£500-£999",
                          "\xa31000 - \xa31499" = "£1000-£1499", "Not doing paid work" = "<£500",
                          "\xa31500 - \xa31999" = "£1500-£1999", "\xa33000 and above" = ">£2000",
                          "\xa32000 - \xa32499" = ">£2000", "\xa32500 - \xa32999" = ">£2000")) %>%
  mutate(YPE6020 = factor(YPE6020, levels = c("<£500", "£500-£999", "£1000-£1499", "£1500-£1999", 
                                              ">£2000"))) %>%
  rename(income = YPE6020)

table(data_offspring$income, useNA = "ifany")


# Area-level index of multiple deprivation (IMD; quintiles)
table(data_offspring$jan2021imd2015q5_YP, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(jan2021imd2015q5_YP = na_if(jan2021imd2015q5_YP, "Missing")) %>%
  mutate(jan2021imd2015q5_YP = recode(jan2021imd2015q5_YP, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan2021imd2015q5_YP = factor(jan2021imd2015q5_YP, 
                                     levels = c("Quin. 1/Least deprived", 
                                                "Quintile 2", "Quintile 3", 
                                                "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan2021imd2015q5_YP)

table(data_offspring$imd, useNA = "ifany")


# Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other)
table(data_offspring$YPG1060, useNA = "ifany")

data_offspring <- data_offspring %>%
  mutate(YPG1060 = na_if(YPG1060, "NS/NK")) %>%
  mutate(YPG1060 = na_if(YPG1060, "Missed whole section A")) %>%
  mutate(YPG1060 = na_if(YPG1060, "Questionnaire not completed")) %>%
  mutate(YPG1060 = recode(YPG1060, "Rented from council/housing association" = "Council/HA",
                       "Being bought/mortgaged" = "Owned/Mortgaged", 
                       "Owned (with no mortgage to pay)" = "Owned/Mortgaged",
                       "Rented from private landlord" = "Rented")) %>%
  mutate(YPG1060 = factor(YPG1060, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = YPG1060)

table(data_offspring$home, useNA = "ifany")


## Political beliefs - Need to clean all of the variables then run a PCA
data_offspring <- data_offspring %>%
  mutate(crime = ifelse(YPB9530 == "Yes", 1,
                        ifelse(YPB9530 == "No", 0, NA))) %>%
  mutate(economy = ifelse(YPB9531 == "Yes", 1,
                        ifelse(YPB9531 == "No", 0, NA))) %>%
  mutate(education = ifelse(YPB9532 == "Yes", 1,
                        ifelse(YPB9532 == "No", 0, NA))) %>%
  mutate(europe = ifelse(YPB9534 == "Yes", 1,
                        ifelse(YPB9534 == "No", 0, NA))) %>%
  mutate(immigration = ifelse(YPB9535 == "Yes", 1,
                        ifelse(YPB9535 == "No", 0, NA))) %>%
  mutate(LGBTQ = ifelse(YPB9536 == "Yes", 1,
                        ifelse(YPB9536 == "No", 0, NA))) %>%
  mutate(NHS = ifelse(YPB9537 == "Yes", 1,
                        ifelse(YPB9537 == "No", 0, NA))) %>%
  mutate(unemployment = ifelse(YPB9538 == "Yes", 1,
                        ifelse(YPB9538 == "No", 0, NA))) %>%
  mutate(womensRights = ifelse(YPB9539 == "Yes", 1,
                        ifelse(YPB9539 == "No", 0, NA))) %>%
  select(-c(YPB9530:YPB9539))


# Extract just political data
data_pol <- data_offspring %>%
  select(c(crime:womensRights))
head(data_pol)


## Using a tetrachoric correlation, as variables are binary

# Parallel analysis to find number of components to extract - Suggests 5, but seems only to be 1 main principal component, or perhaps 2, which is where the 'bend' in the scree plot occurs
fa.parallel(data_pol, fm = "pa", fa = "fa", cor = "tet")

# Now run PCA - Comparing 1 and 2 components

# 1 component - This single factor is sensible, with positive loadings for more 'liberal' (LGBTQ and women's rights) and social issues (education and NHS), and with negative loadings for more 'conservative' issues (crime, economy, immigration). But the variance explained is quite low (23%)
pca1 <- pca(data_pol, nfactors = 1, rotate = "promax", cor = "tet")
print(pca1)

# 2 components - This is similar to the 1-factor solution, but now the first PCA is liberal vs conservative, with PC2 mainly being for social issues (education and NHS). The variance explained for PC1 is about the same as the 1-factor solution (22/23%), while PC2 seems to pick up some additional previously-unaccounted-for variation, and together account for nearly 40% of the variance. Based on variance explained and interpretability, will go with the 2 component solution.
pca2 <- pca(data_pol, nfactors = 2, rotate = "promax", cor = "tet")
print(pca2)


# Extract factor loadings
data_offspring$politics_PCA1 <- pca2$scores[, 1]
summary(data_offspring$politics_PCA1)
sum(!is.na(data_offspring$politics_PCA1))

data_offspring$politics_PCA2 <- pca2$scores[, 2]
summary(data_offspring$politics_PCA2)
sum(!is.na(data_offspring$politics_PCA2))

# Drop the original political variables and move the PCAs to after the confounder variables
data_offspring <- data_offspring %>%
  select(-c(crime:womensRights)) %>%
  relocate(aln:home, politics_PCA1, politics_PCA2, belief:meatDairy)

names(data_offspring)



### Save offspring's data (in R, CSV and Stata format; the R and Stata formats will keep all the factor formatting, while the CSV file will lose this)
save(data_offspring, file = "data_offspring_processed_B4123.RData")
write_csv(data_offspring, file = "data_offspring_processed_B4123.csv")
write_dta(data_offspring, "data_offspring_processed_B4123.dta")



