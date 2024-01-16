### Script for paper 'Associations between religiosity and climate change beliefs and behaviours in the Avon Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4123
### Script 4: Analysis on partner's data
### Created 6/11/2023 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://osf.io/p5vjz/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4123 - RSBB and Climate Change")

#install.packages("tidyverse")
library(tidyverse)

library(nnet)

#install.packages("ordinal")
library(ordinal)

#install.packages("gofcat")
library(gofcat)

#install.packages("marginaleffects")
library(marginaleffects)

#install.packages("gridExtra")
library(gridExtra)

#install.packages("lmtest")
library(lmtest)

#install.packages("pscl")
library(pscl)


##########################################################################################
#### Read in the partner's processed data

load("data_partner_processed_B4123.RData")

# Or, if using the synthetic data
#load("./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_partner_B4123.RData")
#data_partner <- data_partner_syn_df


## Create a summary variable for 'total number of climate actions' (both using all actions [other than 'planned fewer children' and 'other'], and also excluding potentially prohibitively-costly actions
data_partner <- data_partner %>%
  mutate(travel_bin = ifelse(is.na(travel), NA,
                             ifelse(travel == "Climate" | travel == "ClimateOther", 1, 0))) %>%
  mutate(waste_bin = ifelse(is.na(waste), NA,
                            ifelse(waste == "Climate" | waste == "ClimateOther", 1, 0))) %>%
  mutate(energy_bin = ifelse(is.na(energy), NA,
                             ifelse(energy == "Climate" | energy == "ClimateOther", 1, 0))) %>%
  mutate(buy_bin = ifelse(is.na(buy), NA,
                          ifelse(buy == "Climate" | buy == "ClimateOther", 1, 0))) %>%
  mutate(airTravel_bin = ifelse(is.na(airTravel), NA,
                                ifelse(airTravel == "Climate" | airTravel == "ClimateOther", 1, 0))) %>%
  mutate(elecCar_bin = ifelse(is.na(elecCar), NA,
                              ifelse(elecCar == "Climate" | elecCar == "ClimateOther", 1, 0))) %>%
  mutate(localFood_bin = ifelse(is.na(localFood), NA,
                                ifelse(localFood == "Climate" | localFood == "ClimateOther", 1, 0))) %>%
  mutate(recycle_bin = ifelse(is.na(recycle), NA,
                              ifelse(recycle == "Climate" | recycle == "ClimateOther", 1, 0))) %>%
  mutate(plastic_bin = ifelse(is.na(plastic), NA,
                              ifelse(plastic == "Climate" | plastic == "ClimateOther", 1, 0))) %>%
  mutate(sustainable_bin = ifelse(is.na(sustainable), NA,
                                  ifelse(sustainable == "Climate" | sustainable == "ClimateOther", 1, 0))) %>%
  mutate(insulation_bin = ifelse(is.na(insulation), NA,
                                 ifelse(insulation == "Climate" | insulation == "ClimateOther", 1, 0))) %>%
  mutate(solar_bin = ifelse(is.na(solar), NA,
                            ifelse(solar == "Climate" | solar == "ClimateOther", 1, 0))) %>%
  mutate(veg_bin = ifelse(is.na(veg), NA,
                          ifelse(veg == "Climate" | veg == "ClimateOther", 1, 0))) %>%
  mutate(trees_bin = ifelse(is.na(trees), NA,
                            ifelse(trees == "Climate" | trees == "ClimateOther", 1, 0))) %>%
  mutate(avoidFossil_bin = ifelse(is.na(avoidFossil), NA,
                                  ifelse(avoidFossil == "Climate" | avoidFossil == "ClimateOther", 1, 0))) %>%
  #mutate(children_bin = ifelse(is.na(children), NA,
  #                           ifelse(children == "Climate" | children == "ClimateOther", 1, 0))) %>%
  #mutate(otherAction_bin = ifelse(is.na(otherAction), NA,
  #                           ifelse(otherAction == "Climate" | otherAction == "ClimateOther", 1, 0))) %>%
  mutate(meatDairy_bin = ifelse(is.na(meatDairy), NA,
                                ifelse(meatDairy == "Climate" | meatDairy == "ClimateOther", 1, 0))) %>%
  rowwise() %>%
  mutate(totalActions = sum(c_across(travel_bin:meatDairy_bin))) %>%
  mutate(totalActions_reduced = sum(travel_bin, waste_bin, energy_bin, buy_bin, localFood_bin, 
                                    recycle_bin, plastic_bin, sustainable_bin, trees_bin,
                                    avoidFossil_bin, meatDairy_bin)) %>%
  ungroup() %>%
  select(-c(travel_bin:meatDairy_bin))



## Make variables for those with complete confounder data, any exposure data, and any outcome data (note: have excluded ethnicity here, as so few other than white partners that there are model estimation issues)
data_partner <- data_partner %>%
  mutate(cca_confounds = complete.cases(ageAtBirth, ageAtQ, marital, rural, edu, occClass,
                                        income, imd, home)) %>%
  mutate(cca_exposures = ifelse((!is.na(belief) | !is.na(identity) | !is.na(attend) | !is.na(lca)), 1, 0)) %>%
  mutate(cca_outcomes = ifelse((!is.na(climateChanging) | !is.na(climateConcern) | !is.na(climateHumans)
                                | !is.na(climateAction) | !is.na(travel) | !is.na(waste) | !is.na(energy) 
                                | !is.na(buy) | !is.na(airTravel)  | !is.na(elecCar) | !is.na(localFood) 
                                | !is.na(recycle) | !is.na(plastic) | !is.na(sustainable) | !is.na(insulation)
                                | !is.na(solar) | !is.na(veg) | !is.na(trees) | !is.na(avoidFossil) 
                                | !is.na(children) | !is.na(otherAction) | !is.na(meatDairy)), 1, 0)) %>%
  mutate(cca_combined = ifelse(cca_confounds == TRUE & cca_exposures == 1 & cca_outcomes == 1, 1, 0))

table(data_partner$cca_confounds)
table(data_partner$cca_exposures)
table(data_partner$cca_outcomes)
table(data_partner$cca_combined)


### Descriptive stats

## Exposures in full sample and CCA sample

# Belief
data_partner %>%
  group_by(belief) %>%
  filter(!is.na(belief)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$belief))
round(sum(is.na(data_partner$belief)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(belief) %>%
  filter(!is.na(belief)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$belief[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$belief[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Affiliation
data_partner %>%
  group_by(identity) %>%
  filter(!is.na(identity)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$identity))
round(sum(is.na(data_partner$identity)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(identity) %>%
  filter(!is.na(identity)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$identity[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$identity[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Affiliation (by Christian denomination)
data_partner %>%
  group_by(identity_denom) %>%
  filter(!is.na(identity_denom)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$identity_denom))
round(sum(is.na(data_partner$identity_denom)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(identity_denom) %>%
  filter(!is.na(identity_denom)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$identity_denom[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$identity_denom[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Attendance
data_partner %>%
  group_by(attend) %>%
  filter(!is.na(attend)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$attend))
round(sum(is.na(data_partner$attend)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(attend) %>%
  filter(!is.na(attend)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$attend[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$attend[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Latent classes
data_partner %>%
  group_by(lca) %>%
  filter(!is.na(lca)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$lca))
round(sum(is.na(data_partner$lca)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(lca) %>%
  filter(!is.na(lca)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$lca[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$lca[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)



## Outcomes in full sample and CCA sample

# Climate is changing
data_partner %>%
  group_by(climateChanging) %>%
  filter(!is.na(climateChanging)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$climateChanging))
round(sum(is.na(data_partner$climateChanging)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(climateChanging) %>%
  filter(!is.na(climateChanging)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$climateChanging[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$climateChanging[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Concern over climate change
data_partner %>%
  group_by(climateConcern) %>%
  filter(!is.na(climateConcern)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$climateConcern))
round(sum(is.na(data_partner$climateConcern)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(climateConcern) %>%
  filter(!is.na(climateConcern)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$climateConcern[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$climateConcern[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Humans to blame for climate change
data_partner %>%
  group_by(climateHumans) %>%
  filter(!is.na(climateHumans)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$climateHumans))
round(sum(is.na(data_partner$climateHumans)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(climateHumans) %>%
  filter(!is.na(climateHumans)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$climateHumans[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$climateHumans[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Individual actions can alter climate change
data_partner %>%
  group_by(climateAction) %>%
  filter(!is.na(climateAction)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$climateAction))
round(sum(is.na(data_partner$climateAction)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(climateAction) %>%
  filter(!is.na(climateAction)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$climateAction[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$climateAction[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Changed how traveled locally
data_partner %>%
  group_by(travel) %>%
  filter(!is.na(travel)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$travel))
round(sum(is.na(data_partner$travel)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(travel) %>%
  filter(!is.na(travel)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$travel[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$travel[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Reduced household waste
data_partner %>%
  group_by(waste) %>%
  filter(!is.na(waste)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$waste))
round(sum(is.na(data_partner$waste)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(waste) %>%
  filter(!is.na(waste)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$waste[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$waste[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Reduced energy consumption
data_partner %>%
  group_by(energy) %>%
  filter(!is.na(energy)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$energy))
round(sum(is.na(data_partner$energy)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(energy) %>%
  filter(!is.na(energy)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$energy[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$energy[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Changed what buy
data_partner %>%
  group_by(buy) %>%
  filter(!is.na(buy)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$buy))
round(sum(is.na(data_partner$buy)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(buy) %>%
  filter(!is.na(buy)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$buy[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$buy[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Reduced air travel
data_partner %>%
  group_by(airTravel) %>%
  filter(!is.na(airTravel)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$airTravel))
round(sum(is.na(data_partner$airTravel)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(airTravel) %>%
  filter(!is.na(airTravel)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$airTravel[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$airTravel[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Electric/hybrid car
data_partner %>%
  group_by(elecCar) %>%
  filter(!is.na(elecCar)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$elecCar))
round(sum(is.na(data_partner$elecCar)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(elecCar) %>%
  filter(!is.na(elecCar)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$elecCar[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$elecCar[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Bought local food
data_partner %>%
  group_by(localFood) %>%
  filter(!is.na(localFood)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$localFood))
round(sum(is.na(data_partner$localFood)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(localFood) %>%
  filter(!is.na(localFood)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$localFood[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$localFood[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Recycled more
data_partner %>%
  group_by(recycle) %>%
  filter(!is.na(recycle)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$recycle))
round(sum(is.na(data_partner$recycle)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(recycle) %>%
  filter(!is.na(recycle)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$recycle[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$recycle[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Reduced plastic use
data_partner %>%
  group_by(plastic) %>%
  filter(!is.na(plastic)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$plastic))
round(sum(is.na(data_partner$plastic)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(plastic) %>%
  filter(!is.na(plastic)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$plastic[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$plastic[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Bought sustainable products
data_partner %>%
  group_by(sustainable) %>%
  filter(!is.na(sustainable)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$sustainable))
round(sum(is.na(data_partner$sustainable)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(sustainable) %>%
  filter(!is.na(sustainable)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$sustainable[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$sustainable[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Improved home insulation
data_partner %>%
  group_by(insulation) %>%
  filter(!is.na(insulation)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$insulation))
round(sum(is.na(data_partner$insulation)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(insulation) %>%
  filter(!is.na(insulation)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$insulation[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$insulation[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Installed solar panels
data_partner %>%
  group_by(solar) %>%
  filter(!is.na(solar)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$solar))
round(sum(is.na(data_partner$solar)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(solar) %>%
  filter(!is.na(solar)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$solar[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$solar[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Grew own vegetables
data_partner %>%
  group_by(veg) %>%
  filter(!is.na(veg)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$veg))
round(sum(is.na(data_partner$veg)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(veg) %>%
  filter(!is.na(veg)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$veg[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$veg[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Planted trees
data_partner %>%
  group_by(trees) %>%
  filter(!is.na(trees)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$trees))
round(sum(is.na(data_partner$trees)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(trees) %>%
  filter(!is.na(trees)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$trees[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$trees[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Avoided fossil fuel organisation
data_partner %>%
  group_by(avoidFossil) %>%
  filter(!is.na(avoidFossil)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$avoidFossil))
round(sum(is.na(data_partner$avoidFossil)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(avoidFossil) %>%
  filter(!is.na(avoidFossil)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$avoidFossil[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$avoidFossil[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Reduced number of children planned
data_partner %>%
  group_by(children) %>%
  filter(!is.na(children)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$children))
round(sum(is.na(data_partner$children)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(children) %>%
  filter(!is.na(children)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$children[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$children[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Other climate action
data_partner %>%
  group_by(otherAction) %>%
  filter(!is.na(otherAction)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$otherAction))
round(sum(is.na(data_partner$otherAction)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(otherAction) %>%
  filter(!is.na(otherAction)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$otherAction[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$otherAction[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Reduced meat/dairy consumption
data_partner %>%
  group_by(meatDairy) %>%
  filter(!is.na(meatDairy)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$meatDairy))
round(sum(is.na(data_partner$meatDairy)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(meatDairy) %>%
  filter(!is.na(meatDairy)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))

sum(is.na(data_partner$meatDairy[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$meatDairy[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


# Total number of climate actions
data_partner %>%
  filter(!is.na(totalActions)) %>%
  summarise(n = n(), min = min(totalActions), max = max(totalActions), mean = mean(totalActions),
            sd = sd(totalActions), median = median(totalActions), per_25 = quantile(totalActions, 0.25),
            per_75 = quantile(totalActions, 0.75)) 

sum(is.na(data_partner$totalActions))
round(sum(is.na(data_partner$totalActions)) / nrow(data_partner) * 100, 1)

(hist <- ggplot(data_partner, aes(x = totalActions)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
    labs(x = "Total number of climate actions", y = "Frequency") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))

pdf("./Results_Partners/totalActions_hist.pdf", height = 5, width = 8)
hist
dev.off()

data_partner %>%
  filter(cca_combined == 1) %>%
  filter(!is.na(totalActions)) %>%
  summarise(n = n(), min = min(totalActions), max = max(totalActions), mean = mean(totalActions),
            sd = sd(totalActions), median = median(totalActions), per_25 = quantile(totalActions, 0.25),
            per_75 = quantile(totalActions, 0.75)) 

sum(is.na(data_partner$totalActions[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$totalActions[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)



# Total number of climate actions (excluding ones which may be prohibitively costly)
data_partner %>%
  filter(!is.na(totalActions_reduced)) %>%
  summarise(n = n(), min = min(totalActions_reduced), max = max(totalActions_reduced), 
            mean = mean(totalActions_reduced), sd = sd(totalActions_reduced), 
            median = median(totalActions_reduced), per_25 = quantile(totalActions_reduced, 0.25),
            per_75 = quantile(totalActions_reduced, 0.75)) 

sum(is.na(data_partner$totalActions_reduced))
round(sum(is.na(data_partner$totalActions_reduced)) / nrow(data_partner) * 100, 1)

ggplot(data_partner, aes(x = totalActions_reduced)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

data_partner %>%
  filter(cca_combined == 1) %>%
  filter(!is.na(totalActions_reduced)) %>%
  summarise(n = n(), min = min(totalActions_reduced), max = max(totalActions_reduced), 
            mean = mean(totalActions_reduced), sd = sd(totalActions_reduced), 
            median = median(totalActions_reduced), per_25 = quantile(totalActions_reduced, 0.25),
            per_75 = quantile(totalActions_reduced, 0.75)) 

sum(is.na(data_partner$totalActions_reduced[data_partner$cca_combined == 1]))
round(sum(is.na(data_partner$totalActions_reduced[data_partner$cca_combined == 1])) / 
        nrow(data_partner[data_partner$cca_combined == 1, ]) * 100, 1)


## Confounders in full sample and CCA sample

# Age at birth
data_partner %>%
  filter(!is.na(ageAtBirth)) %>%
  summarise(n = n(), min = min(ageAtBirth), max = max(ageAtBirth), 
            mean = mean(ageAtBirth), sd = sd(ageAtBirth), 
            median = median(ageAtBirth), per_25 = quantile(ageAtBirth, 0.25),
            per_75 = quantile(ageAtBirth, 0.75)) 

sum(is.na(data_partner$ageAtBirth))
round(sum(is.na(data_partner$ageAtBirth)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  filter(!is.na(ageAtBirth)) %>%
  summarise(n = n(), min = min(ageAtBirth), max = max(ageAtBirth), 
            mean = mean(ageAtBirth), sd = sd(ageAtBirth), 
            median = median(ageAtBirth), per_25 = quantile(ageAtBirth, 0.25),
            per_75 = quantile(ageAtBirth, 0.75)) 


# Age at questionnaire
data_partner %>%
  filter(!is.na(ageAtQ)) %>%
  summarise(n = n(), min = min(ageAtQ), max = max(ageAtQ), 
            mean = mean(ageAtQ), sd = sd(ageAtQ), 
            median = median(ageAtQ), per_25 = quantile(ageAtQ, 0.25),
            per_75 = quantile(ageAtQ, 0.75)) 

sum(is.na(data_partner$ageAtQ))
round(sum(is.na(data_partner$ageAtQ)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  filter(!is.na(ageAtQ)) %>%
  summarise(n = n(), min = min(ageAtQ), max = max(ageAtQ), 
            mean = mean(ageAtQ), sd = sd(ageAtQ), 
            median = median(ageAtQ), per_25 = quantile(ageAtQ, 0.25),
            per_75 = quantile(ageAtQ, 0.75)) 


# Ethnicity
data_partner %>%
  group_by(ethnicity) %>%
  filter(!is.na(ethnicity)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$ethnicity))
round(sum(is.na(data_partner$ethnicity)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(ethnicity) %>%
  filter(!is.na(ethnicity)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))


# Marital status
data_partner %>%
  group_by(marital) %>%
  filter(!is.na(marital)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$marital))
round(sum(is.na(data_partner$marital)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(marital) %>%
  filter(!is.na(marital)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))


# Urban vs rural location
data_partner %>%
  group_by(rural) %>%
  filter(!is.na(rural)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$rural))
round(sum(is.na(data_partner$rural)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(rural) %>%
  filter(!is.na(rural)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))


# Education
data_partner %>%
  group_by(edu) %>%
  filter(!is.na(edu)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$edu))
round(sum(is.na(data_partner$edu)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(edu) %>%
  filter(!is.na(edu)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))


# occupational social class
data_partner %>%
  group_by(occClass) %>%
  filter(!is.na(occClass)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$occClass))
round(sum(is.na(data_partner$occClass)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(occClass) %>%
  filter(!is.na(occClass)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))


# Income
data_partner %>%
  group_by(income) %>%
  filter(!is.na(income)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$income))
round(sum(is.na(data_partner$income)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(income) %>%
  filter(!is.na(income)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))


# IMD
data_partner %>%
  group_by(imd) %>%
  filter(!is.na(imd)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$imd))
round(sum(is.na(data_partner$imd)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(imd) %>%
  filter(!is.na(imd)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))


# Home ownership status
data_partner %>%
  group_by(home) %>%
  filter(!is.na(home)) %>%
  summarise(n = n()) %>% 
  mutate(percent = round((n / sum(n)) * 100, 2))

sum(is.na(data_partner$home))
round(sum(is.na(data_partner$home)) / nrow(data_partner) * 100, 1)

data_partner %>%
  filter(cca_combined == 1) %>%
  group_by(home) %>%
  filter(!is.na(home)) %>%
  summarise(n_cca = n()) %>% 
  mutate(percent_cca = round((n_cca / sum(n_cca)) * 100, 2))




#############################################################################################
#### Analyses


####################################
### Believes the climate is changing

table(data_partner$climateChanging[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1],
      data_partner$climateChanging[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$climateChanging[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1],
      data_partner$climateChanging[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$climateChanging[data_partner$cca_combined == 1])


## Religious belief

# Unadjusted ordinal model
mod_unadj <- clm(climateChanging ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "belief")

# Extract results
(a <- cbind(y = rep("ClimateChanging", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[5:6]), 
            LCI = exp(confint.default(mod_unadj)[5:6, 1]), UCI = exp(confint.default(mod_unadj)[5:6, 2]),
            p = coef(summary(mod_unadj))[5:6, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE)),
            p_total = hypotheses(mod_unadj, joint = "belief")$p))



# Adjusted ordinal model - This model fails to converge, and is primarily due to so few partners of 'white' ethnicity (<10), so will drop this covariate
#mod_adj <- clm(climateChanging ~ belief + ageAtQ + marital + rural + edu +
#                   occClass + income + imd + home, data = data_partner)
mod_adj <- clm(climateChanging ~ belief + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "belief")

# Extract results
(b <- cbind(y = rep("ClimateChanging", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = mod_adj$nobs, OR = exp(coef(mod_adj)[5:6]), 
            LCI = exp(confint.default(mod_adj)[5:6, 1]), UCI = exp(confint.default(mod_adj)[5:6, 2]),
            p = coef(summary(mod_adj))[5:6, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE)),
            p_total = hypotheses(mod_adj, joint = "belief")$p))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = factor(group, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted ordinal model
mod_unadj <- clm(climateChanging ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Extract results
(c <- cbind(y = "ClimateChanging", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[5]), 
            LCI = exp(confint.default(mod_unadj)[5, 1]), UCI = exp(confint.default(mod_unadj)[5, 2]),
            p = coef(summary(mod_unadj))[5, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


# Adjusted ordinal model
mod_adj <- clm(climateChanging ~ identity + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Extract results
(d <- cbind(y = "ClimateChanging", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = mod_adj$nobs, OR = exp(coef(mod_adj)[5]), 
            LCI = exp(confint.default(mod_adj)[5, 1]), UCI = exp(confint.default(mod_adj)[5, 2]),
            p = coef(summary(mod_adj))[5, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = factor(group, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted ordinal model
mod_unadj <- clm(climateChanging ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Extract results
(e <- cbind(y = "ClimateChanging", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[5]), 
            LCI = exp(confint.default(mod_unadj)[5, 1]), UCI = exp(confint.default(mod_unadj)[5, 2]),
            p = coef(summary(mod_unadj))[5, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


# Adjusted ordinal model
mod_adj <- clm(climateChanging ~ attend + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Extract results
(f <- cbind(y = "ClimateChanging", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = mod_adj$nobs, OR = exp(coef(mod_adj)[5]), 
            LCI = exp(confint.default(mod_adj)[5, 1]), UCI = exp(confint.default(mod_adj)[5, 2]),
            p = coef(summary(mod_adj))[5, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = factor(group, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted ordinal model
mod_unadj <- clm(climateChanging ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "lca")

# Extract results
(g <- cbind(y = rep("ClimateChanging", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[5:7]), 
            LCI = exp(confint.default(mod_unadj)[5:7, 1]), UCI = exp(confint.default(mod_unadj)[5:7, 2]),
            p = coef(summary(mod_unadj))[5:7, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                      pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
            p_total = hypotheses(mod_unadj, joint = "lca")$p))


# Adjusted ordinal model
mod_adj <- clm(climateChanging ~ lca + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "lca")

# Extract results
(h <- cbind(y = rep("ClimateChanging", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = mod_adj$nobs, OR = exp(coef(mod_adj)[5:7]), 
            LCI = exp(confint.default(mod_adj)[5:7, 1]), UCI = exp(confint.default(mod_adj)[5:7, 2]),
            p = coef(summary(mod_adj))[5:7, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                      pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
            p_total = hypotheses(mod_adj, joint = "lca")$p))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = factor(group, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), OR = as.numeric(OR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), brant = as.numeric(brant),
         p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/ClimateChanging.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted")))
res

(plot <- ggplot(res, aes(x = exp, y = OR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(trans = "log", breaks = c(0.67, 1, 1.5, 2)) +
    labs(x = "", y = "Odds ratio") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/ClimateChanging.pdf", height = 5, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/ClimateChanging_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/ClimateChanging_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$climateChanging[data_partner$cca_combined == 1])

# Unadjusted ordinal model
mod_unadj <- clm(climateChanging ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "identity_denom")

# Extract results
(c_denom <- cbind(y = rep("ClimateChanging", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[5:7]), 
                  LCI = exp(confint.default(mod_unadj)[5:7, 1]), UCI = exp(confint.default(mod_unadj)[5:7, 2]),
                  p = coef(summary(mod_unadj))[5:7, "Pr(>|z|)"],
                  brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                            pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                            pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
                  p_total = hypotheses(mod_unadj, joint = "identity_denom")$p))


# Adjusted ordinal model
mod_adj <- clm(climateChanging ~ identity_denom + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "identity_denom")

# Extract results
(d_denom <- cbind(y = rep("ClimateChanging", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = mod_adj$nobs, OR = exp(coef(mod_adj)[5:7]), 
                  LCI = exp(confint.default(mod_adj)[5:7, 1]), UCI = exp(confint.default(mod_adj)[5:7, 2]),
                  p = coef(summary(mod_adj))[5:7, "Pr(>|z|)"],
                  brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                            pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                            pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
                  p_total = hypotheses(mod_adj, joint = "identity_denom")$p))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), OR = as.numeric(OR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), brant = as.numeric(brant),
         p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/ClimateChanging_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = factor(group, levels = c("Definitely not", "Probably not", "Yes maybe",
                                          "Yes probably", "Yes definitely")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/ClimateChanging_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/ClimateChanging_preds_denom.csv")



####################################
### Concern over climate change

table(data_partner$climateConcern[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$climateConcern[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$climateConcern[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$climateConcern[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$climateConcern[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted ordinal model
mod_unadj <- clm(climateConcern ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "belief")

# Extract results
(a <- cbind(y = rep("climateConcern", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4:5]), 
            LCI = exp(confint.default(mod_unadj)[4:5, 1]), UCI = exp(confint.default(mod_unadj)[4:5, 2]),
            p = coef(summary(mod_unadj))[4:5, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE)),
            p_total = hypotheses(mod_unadj, joint = "belief")$p))



# Adjusted ordinal model
mod_adj <- clm(climateConcern ~ belief + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "belief")

# Extract results
(b <- cbind(y = rep("climateConcern", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = mod_adj$nobs, OR = exp(coef(mod_adj)[4:5]), 
            LCI = exp(confint.default(mod_adj)[4:5, 1]), UCI = exp(confint.default(mod_adj)[4:5, 2]),
            p = coef(summary(mod_adj))[4:5, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE)),
            p_total = hypotheses(mod_adj, joint = "belief")$p))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = factor(group, levels = c("Not at all concerned", "Not very concerned", 
                                          "Somewhat concerned", "Very concerned")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted ordinal model
mod_unadj <- clm(climateConcern ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Extract results
(c <- cbind(y = "climateConcern", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4]), 
            LCI = exp(confint.default(mod_unadj)[4, 1]), UCI = exp(confint.default(mod_unadj)[4, 2]),
            p = coef(summary(mod_unadj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


# Adjusted ordinal model
mod_adj <- clm(climateConcern ~ identity + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Extract results
(d <- cbind(y = "climateConcern", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = mod_adj$nobs, OR = exp(coef(mod_adj)[4]), 
            LCI = exp(confint.default(mod_adj)[4, 1]), UCI = exp(confint.default(mod_adj)[4, 2]),
            p = coef(summary(mod_adj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = factor(group, levels = c("Not at all concerned", "Not very concerned", 
                                          "Somewhat concerned", "Very concerned")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted ordinal model
mod_unadj <- clm(climateConcern ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Extract results
(e <- cbind(y = "climateConcern", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4]), 
            LCI = exp(confint.default(mod_unadj)[4, 1]), UCI = exp(confint.default(mod_unadj)[4, 2]),
            p = coef(summary(mod_unadj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


# Adjusted ordinal model
mod_adj <- clm(climateConcern ~ attend + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Extract results
(f <- cbind(y = "climateConcern", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = mod_adj$nobs, OR = exp(coef(mod_adj)[4]), 
            LCI = exp(confint.default(mod_adj)[4, 1]), UCI = exp(confint.default(mod_adj)[4, 2]),
            p = coef(summary(mod_adj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = factor(group, levels = c("Not at all concerned", "Not very concerned", 
                                          "Somewhat concerned", "Very concerned")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted ordinal model
mod_unadj <- clm(climateConcern ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "lca")

# Extract results
(g <- cbind(y = rep("climateConcern", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4:6]), 
            LCI = exp(confint.default(mod_unadj)[4:6, 1]), UCI = exp(confint.default(mod_unadj)[4:6, 2]),
            p = coef(summary(mod_unadj))[4:6, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                      pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
            p_total = hypotheses(mod_unadj, joint = "lca")$p))


# Adjusted ordinal model
mod_adj <- clm(climateConcern ~ lca + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "lca")

# Extract results
(h <- cbind(y = rep("climateConcern", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = mod_adj$nobs, OR = exp(coef(mod_adj)[4:6]), 
            LCI = exp(confint.default(mod_adj)[4:6, 1]), UCI = exp(confint.default(mod_adj)[4:6, 2]),
            p = coef(summary(mod_adj))[4:6, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                      pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
            p_total = hypotheses(mod_adj, joint = "lca")$p))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = factor(group, levels = c("Not at all concerned", "Not very concerned", 
                                          "Somewhat concerned", "Very concerned")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), OR = as.numeric(OR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), brant = as.numeric(brant),
         p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/climateConcern.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted")))
res

(plot <- ggplot(res, aes(x = exp, y = OR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(trans = "log", breaks = c(0.67, 1, 1.5)) +
    labs(x = "", y = "Odds ratio") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/climateConcern.pdf", height = 5, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/climateConcern_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/ClimateConcern_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$climateConcern[data_partner$cca_combined == 1])

# Unadjusted ordinal model
mod_unadj <- clm(climateConcern ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "identity_denom")

# Extract results
(c_denom <- cbind(y = rep("climateConcern", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4:6]), 
                  LCI = exp(confint.default(mod_unadj)[4:6, 1]), UCI = exp(confint.default(mod_unadj)[4:6, 2]),
                  p = coef(summary(mod_unadj))[4:6, "Pr(>|z|)"],
                  brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                            pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                            pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
                  p_total = hypotheses(mod_unadj, joint = "identity_denom")$p))


# Adjusted ordinal model
mod_adj <- clm(climateConcern ~ identity_denom + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "identity_denom")

# Extract results
(d_denom <- cbind(y = rep("climateConcern", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = mod_adj$nobs, OR = exp(coef(mod_adj)[4:6]), 
                  LCI = exp(confint.default(mod_adj)[4:6, 1]), UCI = exp(confint.default(mod_adj)[4:6, 2]),
                  p = coef(summary(mod_adj))[4:6, "Pr(>|z|)"],
                  brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                            pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                            pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
                  p_total = hypotheses(mod_adj, joint = "identity_denom")$p))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), OR = as.numeric(OR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), brant = as.numeric(brant),
         p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/climateConcern_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = factor(group, levels = c("Not at all concerned", "Not very concerned", 
                                          "Somewhat concerned", "Very concerned")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/climateConcern_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/climateConcern_preds_denom.csv")



####################################
### Believes humans are to blame for climate change

table(data_partner$climateHumans[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$climateHumans[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$climateHumans[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$climateHumans[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$climateHumans[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted ordinal model
mod_unadj <- clm(climateHumans ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "belief")

# Extract results
(a <- cbind(y = rep("climateHumans", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4:5]), 
            LCI = exp(confint.default(mod_unadj)[4:5, 1]), UCI = exp(confint.default(mod_unadj)[4:5, 2]),
            p = coef(summary(mod_unadj))[4:5, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE)),
            p_total = hypotheses(mod_unadj, joint = "belief")$p))



# Adjusted ordinal model
mod_adj <- clm(climateHumans ~ belief + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "belief")

# Extract results
(b <- cbind(y = rep("climateHumans", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = mod_adj$nobs, OR = exp(coef(mod_adj)[4:5]), 
            LCI = exp(confint.default(mod_adj)[4:5, 1]), UCI = exp(confint.default(mod_adj)[4:5, 2]),
            p = coef(summary(mod_adj))[4:5, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE)),
            p_total = hypotheses(mod_adj, joint = "belief")$p))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = factor(group, levels = c("Not at all", "Yes, for some of it", 
                                          "Yes, for most of it", "Yes, for all of it")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted ordinal model
mod_unadj <- clm(climateHumans ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Extract results
(c <- cbind(y = "climateHumans", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4]), 
            LCI = exp(confint.default(mod_unadj)[4, 1]), UCI = exp(confint.default(mod_unadj)[4, 2]),
            p = coef(summary(mod_unadj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


# Adjusted ordinal model
mod_adj <- clm(climateHumans ~ identity + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Extract results
(d <- cbind(y = "climateHumans", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = mod_adj$nobs, OR = exp(coef(mod_adj)[4]), 
            LCI = exp(confint.default(mod_adj)[4, 1]), UCI = exp(confint.default(mod_adj)[4, 2]),
            p = coef(summary(mod_adj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = factor(group, levels = c("Not at all", "Yes, for some of it", 
                                          "Yes, for most of it", "Yes, for all of it")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted ordinal model
mod_unadj <- clm(climateHumans ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Extract results
(e <- cbind(y = "climateHumans", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4]), 
            LCI = exp(confint.default(mod_unadj)[4, 1]), UCI = exp(confint.default(mod_unadj)[4, 2]),
            p = coef(summary(mod_unadj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


# Adjusted ordinal model
mod_adj <- clm(climateHumans ~ attend + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Extract results
(f <- cbind(y = "climateHumans", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = mod_adj$nobs, OR = exp(coef(mod_adj)[4]), 
            LCI = exp(confint.default(mod_adj)[4, 1]), UCI = exp(confint.default(mod_adj)[4, 2]),
            p = coef(summary(mod_adj))[4, "Pr(>|z|)"],
            brant = pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE),
            p_total = NA))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = factor(group, levels = c("Not at all", "Yes, for some of it", 
                                          "Yes, for most of it", "Yes, for all of it")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted ordinal model
mod_unadj <- clm(climateHumans ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "lca")

# Extract results
(g <- cbind(y = rep("climateHumans", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4:6]), 
            LCI = exp(confint.default(mod_unadj)[4:6, 1]), UCI = exp(confint.default(mod_unadj)[4:6, 2]),
            p = coef(summary(mod_unadj))[4:6, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                      pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
            p_total = hypotheses(mod_unadj, joint = "lca")$p))


# Adjusted ordinal model
mod_adj <- clm(climateHumans ~ lca + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "lca")

# Extract results
(h <- cbind(y = rep("climateHumans", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = mod_adj$nobs, OR = exp(coef(mod_adj)[4:6]), 
            LCI = exp(confint.default(mod_adj)[4:6, 1]), UCI = exp(confint.default(mod_adj)[4:6, 2]),
            p = coef(summary(mod_adj))[4:6, "Pr(>|z|)"],
            brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                      pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                      pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
            p_total = hypotheses(mod_adj, joint = "lca")$p))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = factor(group, levels = c("Not at all", "Yes, for some of it", 
                                          "Yes, for most of it", "Yes, for all of it")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), OR = as.numeric(OR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), brant = as.numeric(brant),
         p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/climateHumans.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted")))
res

(plot <- ggplot(res, aes(x = exp, y = OR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(trans = "log", breaks = c(0.67, 1, 1.5)) +
    labs(x = "", y = "Odds ratio") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/climateHumans.pdf", height = 5, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/climateHumans_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/climateHumans_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$climateHumans[data_partner$cca_combined == 1])

# Unadjusted ordinal model
mod_unadj <- clm(climateHumans ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_unadj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "identity_denom")

# Extract results
(c_denom <- cbind(y = rep("climateHumans", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = mod_unadj$nobs, OR = exp(coef(mod_unadj)[4:6]), 
                  LCI = exp(confint.default(mod_unadj)[4:6, 1]), UCI = exp(confint.default(mod_unadj)[4:6, 2]),
                  p = coef(summary(mod_unadj))[4:6, "Pr(>|z|)"],
                  brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                            pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                            pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
                  p_total = hypotheses(mod_unadj, joint = "identity_denom")$p))


# Adjusted ordinal model
mod_adj <- clm(climateHumans ~ identity_denom + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check proportional odds assumption via Brant test
(br <- brant.test(mod_adj, global = FALSE))

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "identity_denom")

# Extract results
(d_denom <- cbind(y = rep("climateHumans", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = mod_adj$nobs, OR = exp(coef(mod_adj)[4:6]), 
                  LCI = exp(confint.default(mod_adj)[4:6, 1]), UCI = exp(confint.default(mod_adj)[4:6, 2]),
                  p = coef(summary(mod_adj))[4:6, "Pr(>|z|)"],
                  brant = c(pchisq(q = br$chisq[2], df = br$df[2], lower.tail = FALSE), 
                            pchisq(q = br$chisq[3], df = br$df[3], lower.tail = FALSE), 
                            pchisq(q = br$chisq[4], df = br$df[4], lower.tail = FALSE)),
                  p_total = hypotheses(mod_adj, joint = "identity_denom")$p))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), OR = as.numeric(OR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), brant = as.numeric(brant),
         p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/climateHumans_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = factor(group, levels = c("Not at all", "Yes, for some of it", 
                                          "Yes, for most of it", "Yes, for all of it")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/climateHumans_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/climateHumans_preds_denom.csv")



####################################
### Believes individual actions can mitigate impact of climate change

table(data_partner$climateAction[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$climateAction[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$climateAction[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$climateAction[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$climateAction[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(climateAction ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(climateAction ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("climateAction", 4), y_level = rep(c("Not sure", "Yes"), 2), 
            x = rep("Belief", 4), x_level = rep(c("Not sure", "Yes"), each = 2),
            mod = rep("Unadjusted", 4), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$climateAction) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:2, 2], exp(coef(mod_unadj))[1:2, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[3, 1, 1], exp(confint(mod_unadj))[3, 1, 2]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[3, 2, 1], exp(confint(mod_unadj))[3, 2, 2]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:2, 2], coef(mod_unadj)[1:2, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:2, 2], 
                                   summary(mod_unadj)$standard.errors[1:2, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(climateAction ~ belief + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(climateAction ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                           subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("climateAction", 4), y_level = rep(c("Not sure", "Yes"), 2), 
            x = rep("Belief", 4), x_level = rep(c("Not sure", "Yes"), each = 2),
            mod = rep("Adjusted", 4), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$climateAction) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:2, 2], exp(coef(mod_adj))[1:2, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[3, 1, 1], exp(confint(mod_adj))[3, 1, 2]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[3, 2, 1], exp(confint(mod_adj))[3, 2, 2]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:2, 2], coef(mod_adj)[1:2, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:2, 2], 
                                   summary(mod_adj)$standard.errors[1:2, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = factor(group, levels = c("No", "Not sure", "Yes")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(climateAction ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(climateAction ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("climateAction", 2), y_level = c("Not sure", "Yes"), 
            x = rep("Identity", 2), x_level = c("Christian", "Christian"),
            mod = rep("Unadjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$climateAction) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:2, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:2, 2] / 
                                 summary(mod_unadj)$standard.errors[1:2, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(climateAction ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(climateAction ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("climateAction", 2), y_level = c("Not sure", "Yes"), 
            x = rep("Identity", 2), x_level = c("Christian", "Christian"),
            mod = rep("Adjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$climateAction) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:2, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:2, 2] / 
                                 summary(mod_adj)$standard.errors[1:2, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = factor(group, levels = c("No", "Not sure", "Yes")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(climateAction ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(climateAction ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("climateAction", 2), y_level = c("Not sure", "Yes"), 
            x = rep("Attendance", 2), x_level = c("Regular", "Regular"),
            mod = rep("Unadjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$climateAction) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:2, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:2, 2] / 
                                 summary(mod_unadj)$standard.errors[1:2, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(climateAction ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(climateAction ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("climateAction", 2), y_level = c("Not sure", "Yes"), 
            x = rep("Attendance", 2), x_level = c("Regular", "Regular"),
            mod = rep("Adjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$climateAction) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:2, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:2, 2] / 
                                 summary(mod_adj)$standard.errors[1:2, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = factor(group, levels = c("No", "Not sure", "Yes")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(climateAction ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(climateAction ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("climateAction", 6), y_level = rep(c("Not sure", "Yes"), 3), 
            x = rep("Latent class", 6), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 2),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$climateAction) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:2, 2], exp(coef(mod_unadj))[1:2, 3],
                    exp(coef(mod_unadj))[1:2, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[3, 1, 1], exp(confint(mod_unadj))[3, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[3, 2, 1], exp(confint(mod_unadj))[3, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:2, 2], coef(mod_unadj)[1:2, 3],
                                 coef(mod_unadj)[1:2, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:2, 2], 
                                   summary(mod_unadj)$standard.errors[1:2, 3],
                                   summary(mod_unadj)$standard.errors[1:2, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(climateAction ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(climateAction ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("climateAction", 6), y_level = rep(c("Not sure", "Yes"), 3), 
            x = rep("Latent class", 6), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 2),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$climateAction) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:2, 2], exp(coef(mod_adj))[1:2, 3],
                    exp(coef(mod_adj))[1:2, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[3, 1, 1], exp(confint(mod_adj))[3, 1, 2],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[3, 2, 1], exp(confint(mod_adj))[3, 2, 2],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:2, 2], coef(mod_adj)[1:2, 3],
                                 coef(mod_adj)[1:2, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:2, 2], 
                                   summary(mod_adj)$standard.errors[1:2, 3],
                                   summary(mod_adj)$standard.errors[1:2, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = factor(group, levels = c("No", "Not sure", "Yes")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/climateAction.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "climateAction" = "Indiv. actions can help climate")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Indiv. actions can help climate - Not sure (ref = No)", 
                                      "Indiv. actions can help climate - Yes (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(trans = "log", breaks = c(1, 1.5, 2, 3)) +
    labs(x = "", y = "Relative risk ratio") +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/climateAction.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/climateAction_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/climateAction_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$climateAction[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(climateAction ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(climateAction ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("climateAction", 6), y_level = rep(c("Not sure", "Yes"), 3), 
                  x = rep("Identity (denom)", 6), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 2),
                  mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$climateAction) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:2, 2], exp(coef(mod_unadj))[1:2, 3],
                          exp(coef(mod_unadj))[1:2, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[3, 1, 1], exp(confint(mod_unadj))[3, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[3, 2, 1], exp(confint(mod_unadj))[3, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:2, 2], coef(mod_unadj)[1:2, 3],
                                       coef(mod_unadj)[1:2, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:2, 2], 
                                         summary(mod_unadj)$standard.errors[1:2, 3],
                                         summary(mod_unadj)$standard.errors[1:2, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(climateAction ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(climateAction ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("climateAction", 6), y_level = rep(c("Not sure", "Yes"), 3), 
                  x = rep("Identity (denom)", 6), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 2),
                  mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$climateAction) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:2, 2], exp(coef(mod_adj))[1:2, 3],
                          exp(coef(mod_adj))[1:2, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[3, 1, 1], exp(confint(mod_adj))[3, 1, 2],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[3, 2, 1], exp(confint(mod_adj))[3, 2, 2],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:2, 2], coef(mod_adj)[1:2, 3],
                                       coef(mod_adj)[1:2, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:2, 2], 
                                         summary(mod_adj)$standard.errors[1:2, 3],
                                         summary(mod_adj)$standard.errors[1:2, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/climateAction_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = factor(group, levels = c("No", "Not sure", "Yes")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/climateAction_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/climateAction_preds_denom.csv")



###################################################################################
##### Climate behaviours


#########################
### Changed the way travel locally

table(data_partner$travel[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$travel[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$travel[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1],
      data_partner$travel[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$travel[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(travel ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(travel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("travel", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(travel ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(travel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("travel", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(travel ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(travel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("travel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(travel ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(travel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("travel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(travel ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(travel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("travel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(travel ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(travel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("travel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(travel ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(travel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("travel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(travel ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(travel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("travel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$travel) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/travel.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "travel" = "Changed local travel")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Changed local travel - for climate (ref = No)", 
                                      "Changed local travel - for other (ref = No)",
                                      "Changed local travel - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.33, 0.5, 0.67, 1, 1.5, 2)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/travel.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/travel_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/travel_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$travel[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(travel ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(travel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("travel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$travel) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(travel ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(travel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("travel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$travel) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/travel_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/travel_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/travel_preds_denom.csv")



#########################
### Reduced household waste

table(data_partner$waste[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$waste[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$waste[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$waste[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$waste[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(waste ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(waste ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("waste", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$waste) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(waste ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(waste ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("waste", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$waste) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(waste ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(waste ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("waste", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$waste) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(waste ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(waste ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("waste", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$waste) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(waste ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(waste ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("waste", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$waste) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(waste ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(waste ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("waste", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$waste) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(waste ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(waste ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("waste", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$waste) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(waste ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(waste ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("waste", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$waste) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/waste.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "waste" = "Reduced household waste")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Reduced household waste - for climate (ref = No)", 
                                      "Reduced household waste - for other (ref = No)",
                                      "Reduced household waste - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.5, 0.67, 1, 1.5, 2, 3)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/waste.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/waste_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/waste_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$waste[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(waste ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(waste ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("waste", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$waste) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(waste ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(waste ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("waste", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$waste) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/waste_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/waste_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/waste_preds_denom.csv")



#########################
### Reduced energy use

table(data_partner$energy[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1],
      data_partner$energy[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$energy[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$energy[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$energy[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(energy ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(energy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("energy", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$energy) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(energy ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(energy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("energy", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$energy) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(energy ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(energy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("energy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$energy) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(energy ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(energy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("energy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$energy) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(energy ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(energy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("energy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$energy) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(energy ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(energy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("energy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$energy) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(energy ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(energy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("energy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$energy) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(energy ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(energy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("energy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$energy) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/energy.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "energy" = "Reduced energy use")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Reduced energy use - for climate (ref = No)", 
                                      "Reduced energy use - for other (ref = No)",
                                      "Reduced energy use - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.5, 0.67, 1, 1.5, 2, 3)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/energy.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/energy_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/energy_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$energy[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(energy ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(energy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("energy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$energy) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(energy ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(energy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("energy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$energy) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/energy_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/energy_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/energy_preds_denom.csv")


#########################
### Changed what buy

table(data_partner$buy[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$buy[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$buy[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$buy[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1],
      data_partner$buy[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(buy ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(buy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("buy", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$buy) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(buy ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(buy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("buy", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$buy) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(buy ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(buy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("buy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$buy) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(buy ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(buy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("buy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$buy) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(buy ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(buy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("buy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$buy) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(buy ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(buy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("buy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$buy) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(buy ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(buy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("buy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$buy) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(buy ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(buy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("buy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$buy) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/buy.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "buy" = "Changed what buy")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Changed what buy - for climate (ref = No)", 
                                      "Changed what buy - for other (ref = No)",
                                      "Changed what buy - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1, 2, 3, 5)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/buy.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/buy_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/buy_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$buy[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(buy ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(buy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("buy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$buy) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(buy ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(buy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("buy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$buy) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/buy_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/buy_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/buy_preds_denom.csv")


#########################
### Reduced air travel

table(data_partner$airTravel[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$airTravel[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$airTravel[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$airTravel[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$airTravel[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(airTravel ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(airTravel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("airTravel", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$airTravel) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(airTravel ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(airTravel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("airTravel", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$airTravel) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(airTravel ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(airTravel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("airTravel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$airTravel) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(airTravel ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(airTravel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("airTravel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$airTravel) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(airTravel ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(airTravel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("airTravel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$airTravel) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(airTravel ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(airTravel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("airTravel", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$airTravel) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(airTravel ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(airTravel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("airTravel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$airTravel) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(airTravel ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(airTravel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("airTravel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$airTravel) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/airTravel.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "airTravel" = "Reduced air travel")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Reduced air travel - for climate (ref = No)", 
                                      "Reduced air travel - for other (ref = No)",
                                      "Reduced air travel - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.1, 0.25, 0.5, 1, 2, 5)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/airTravel.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/airTravel_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/airTravel_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$airTravel[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(airTravel ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(airTravel ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("airTravel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$airTravel) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(airTravel ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(airTravel ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("airTravel", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$airTravel) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/airTravel_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/airTravel_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/airTravel_preds_denom.csv")



#########################
### Bought/hired electric/hybrid car

table(data_partner$elecCar[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$elecCar[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$elecCar[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$elecCar[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$elecCar[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(elecCar ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(elecCar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("elecCar", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$elecCar) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(elecCar ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(elecCar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("elecCar", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$elecCar) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(elecCar ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(elecCar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("elecCar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$elecCar) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(elecCar ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(elecCar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("elecCar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$elecCar) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(elecCar ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(elecCar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("elecCar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$elecCar) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(elecCar ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(elecCar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("elecCar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$elecCar) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(elecCar ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(elecCar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("elecCar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$elecCar) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(elecCar ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(elecCar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("elecCar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$elecCar) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/elecCar.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "elecCar" = "Electric/hybrid car")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Electric/hybrid car - for climate (ref = No)", 
                                      "Electric/hybrid car - for other (ref = No)",
                                      "Electric/hybrid car - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.5, 1, 2, 5, 10, 30)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/elecCar.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/elecCar_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/elecCar_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$elecCar[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(elecCar ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(elecCar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("elecCar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$elecCar) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(elecCar ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(elecCar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("elecCar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$elecCar) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/elecCar_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/elecCar_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/elecCar_preds_denom.csv")



#########################
### Bought local food

table(data_partner$localFood[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$localFood[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$localFood[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1],
      data_partner$localFood[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$localFood[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(localFood ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(localFood ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("localFood", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$localFood) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(localFood ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(localFood ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("localFood", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$localFood) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(localFood ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(localFood ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("localFood", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$localFood) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(localFood ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(localFood ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("localFood", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$localFood) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(localFood ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(localFood ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("localFood", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$localFood) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(localFood ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(localFood ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("localFood", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$localFood) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(localFood ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(localFood ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("localFood", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$localFood) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(localFood ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(localFood ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("localFood", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$localFood) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/localFood.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "localFood" = "Bought local food")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Bought local food - for climate (ref = No)", 
                                      "Bought local food - for other (ref = No)",
                                      "Bought local food - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/localFood.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/localFood_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/localFood_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$localFood[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(localFood ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(localFood ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("localFood", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$localFood) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(localFood ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(localFood ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("localFood", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$localFood) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/localFood_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/localFood_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/localFood_preds_denom.csv")



#########################
### Recycled more

table(data_partner$recycle[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$recycle[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$recycle[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$recycle[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$recycle[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(recycle ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(recycle ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("recycle", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$recycle) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(recycle ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(recycle ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("recycle", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$recycle) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(recycle ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(recycle ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("recycle", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$recycle) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(recycle ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(recycle ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("recycle", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$recycle) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(recycle ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(recycle ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("recycle", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$recycle) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(recycle ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(recycle ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("recycle", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$recycle) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(recycle ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(recycle ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("recycle", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$recycle) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(recycle ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(recycle ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("recycle", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$recycle) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/recycle.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "recycle" = "Recycled more")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Recycled more - for climate (ref = No)", 
                                      "Recycled more - for other (ref = No)",
                                      "Recycled more - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/recycle.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/recycle_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/recycle_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$recycle[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(recycle ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(recycle ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("recycle", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$recycle) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(recycle ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(recycle ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("recycle", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$recycle) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/recycle_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/recycle_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/recycle_preds_denom.csv")



#########################
### Reduced plastic use

table(data_partner$plastic[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$plastic[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$plastic[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1],
      data_partner$plastic[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$plastic[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(plastic ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(plastic ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("plastic", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$plastic) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(plastic ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(plastic ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("plastic", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$plastic) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(plastic ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(plastic ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("plastic", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$plastic) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(plastic ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(plastic ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("plastic", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$plastic) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(plastic ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(plastic ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("plastic", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$plastic) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(plastic ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(plastic ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("plastic", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$plastic) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(plastic ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(plastic ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("plastic", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$plastic) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(plastic ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(plastic ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("plastic", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$plastic) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/plastic.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "plastic" = "Reduced plastic use")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Reduced plastic use - for climate (ref = No)", 
                                      "Reduced plastic use - for other (ref = No)",
                                      "Reduced plastic use - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/plastic.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/plastic_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/plastic_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$plastic[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(plastic ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(plastic ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("plastic", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$plastic) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(plastic ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(plastic ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("plastic", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$plastic) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/plastic_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/plastic_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/plastic_preds_denom.csv")



#########################
### Sustainable purchases

table(data_partner$sustainable[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$sustainable[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$sustainable[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$sustainable[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$sustainable[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(sustainable ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(sustainable ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("sustainable", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$sustainable) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(sustainable ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(sustainable ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("sustainable", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$sustainable) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(sustainable ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(sustainable ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("sustainable", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$sustainable) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(sustainable ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(sustainable ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("sustainable", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$sustainable) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(sustainable ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(sustainable ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("sustainable", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$sustainable) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(sustainable ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(sustainable ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("sustainable", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$sustainable) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(sustainable ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(sustainable ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("sustainable", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$sustainable) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(sustainable ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(sustainable ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("sustainable", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$sustainable) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/sustainable.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "sustainable" = "Sustainable purchases")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Sustainable purchases - for climate (ref = No)", 
                                      "Sustainable purchases - for other (ref = No)",
                                      "Sustainable purchases - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/sustainable.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/sustainable_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/sustainable_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$sustainable[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(sustainable ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(sustainable ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("sustainable", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$sustainable) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(sustainable ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(sustainable ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("sustainable", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$sustainable) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/sustainable_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/sustainable_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/sustainable_preds_denom.csv")



#########################
### Home insulation

table(data_partner$insulation[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$insulation[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$insulation[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$insulation[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$insulation[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(insulation ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(insulation ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("insulation", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$insulation) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(insulation ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(insulation ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("insulation", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$insulation) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(insulation ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(insulation ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("insulation", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$insulation) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(insulation ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(insulation ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("insulation", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$insulation) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(insulation ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(insulation ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("insulation", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$insulation) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(insulation ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(insulation ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("insulation", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$insulation) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(insulation ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(insulation ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("insulation", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$insulation) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(insulation ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(insulation ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("insulation", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$insulation) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Code infinite values (with no data) as missing
res$RRR[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
res$LCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
res$UCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
res$p[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA

# Save output
write_csv(res, file = "./Results_Partners/insulation.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "insulation" = "Home insulation")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Home insulation - for climate (ref = No)", 
                                      "Home insulation - for other (ref = No)",
                                      "Home insulation - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/insulation.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/insulation_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/insulation_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$insulation[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(insulation ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(insulation ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("insulation", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$insulation) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(insulation ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(insulation ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("insulation", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$insulation) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/insulation_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/insulation_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/insulation_preds_denom.csv")



#########################
### Solar panels

table(data_partner$solar[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1],
      data_partner$solar[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$solar[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$solar[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1],
      data_partner$solar[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(solar ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(solar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("solar", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$solar) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(solar ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(solar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("solar", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$solar) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(solar ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(solar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("solar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$solar) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(solar ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(solar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("solar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$solar) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(solar ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(solar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("solar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$solar) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(solar ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(solar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("solar", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$solar) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(solar ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(solar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("solar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$solar) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(solar ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(solar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("solar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$solar) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/solar.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "solar" = "Solar panels")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Solar panels - for climate (ref = No)", 
                                      "Solar panels - for other (ref = No)",
                                      "Solar panels - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.125, 0.25, 0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/solar.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/solar_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/solar_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$solar[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(solar ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(solar ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("solar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$solar) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(solar ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(solar ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("solar", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$solar) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/solar_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/solar_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/solar_preds_denom.csv")



#########################
### Growing vegetables

table(data_partner$veg[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$veg[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$veg[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1],
      data_partner$veg[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$veg[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(veg ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(veg ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("veg", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$veg) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(veg ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(veg ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("veg", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$veg) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(veg ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(veg ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("veg", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$veg) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(veg ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(veg ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("veg", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$veg) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(veg ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(veg ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("veg", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$veg) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(veg ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(veg ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("veg", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$veg) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(veg ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(veg ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("veg", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$veg) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(veg ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(veg ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("veg", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$veg) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Code infinite values (with no data) as missing
res$RRR[res$y_level == "ClimateOther" & res$x_level == "Agnostic"] <- NA
res$LCI[res$y_level == "ClimateOther" & res$x_level == "Agnostic"] <- NA
res$UCI[res$y_level == "ClimateOther" & res$x_level == "Agnostic"] <- NA
res$p[res$y_level == "ClimateOther" & res$x_level == "Agnostic"] <- NA
res$RRR[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
res$LCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
res$UCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
res$p[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA

# Save output
write_csv(res, file = "./Results_Partners/veg.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "veg" = "Growing vegetables")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Growing vegetables - for climate (ref = No)", 
                                      "Growing vegetables - for other (ref = No)",
                                      "Growing vegetables - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/veg.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/veg_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/veg_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$veg[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(veg ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(veg ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("veg", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$veg) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(veg ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(veg ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("veg", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$veg) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/veg_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/veg_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/veg_preds_denom.csv")



#########################
### Planted trees

table(data_partner$trees[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$trees[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$trees[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$trees[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$trees[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(trees ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(trees ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("trees", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$trees) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(trees ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(trees ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("trees", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$trees) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(trees ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(trees ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("trees", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$trees) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(trees ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(trees ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("trees", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$trees) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(trees ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(trees ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("trees", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$trees) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(trees ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(trees ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("trees", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$trees) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(trees ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(trees ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("trees", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$trees) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(trees ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(trees ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("trees", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$trees) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/trees.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "trees" = "Planted trees")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Planted trees - for climate (ref = No)", 
                                      "Planted trees - for other (ref = No)",
                                      "Planted trees - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.1, 0.25, 0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/trees.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/trees_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/trees_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$trees[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(trees ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(trees ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("trees", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$trees) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(trees ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(trees ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("trees", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$trees) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/trees_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/trees_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/trees_preds_denom.csv")



#########################
### Avoid fossil fuel orgs

table(data_partner$avoidFossil[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$avoidFossil[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$avoidFossil[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$avoidFossil[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$avoidFossil[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(avoidFossil ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(avoidFossil ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("avoidFossil", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$avoidFossil) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(avoidFossil ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(avoidFossil ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("avoidFossil", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$avoidFossil) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(avoidFossil ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(avoidFossil ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("avoidFossil", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$avoidFossil) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(avoidFossil ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(avoidFossil ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("avoidFossil", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$avoidFossil) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(avoidFossil ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(avoidFossil ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("avoidFossil", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$avoidFossil) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(avoidFossil ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(avoidFossil ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("avoidFossil", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$avoidFossil) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(avoidFossil ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(avoidFossil ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("avoidFossil", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$avoidFossil) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(avoidFossil ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(avoidFossil ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("avoidFossil", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$avoidFossil) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))



## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Code infinite values (with no data) as missing
res$RRR[res$y_level == "ClimateOther" & res$x_level == "Regular"] <- NA
res$LCI[res$y_level == "ClimateOther" & res$x_level == "Regular"] <- NA
res$UCI[res$y_level == "ClimateOther" & res$x_level == "Regular"] <- NA
res$p[res$y_level == "ClimateOther" & res$x_level == "Regular"] <- NA
res$RRR[res$y_level == "ClimateOther" & res$x_level == "Highly religious"] <- NA
res$LCI[res$y_level == "ClimateOther" & res$x_level == "Highly religious"] <- NA
res$UCI[res$y_level == "ClimateOther" & res$x_level == "Highly religious"] <- NA
res$p[res$y_level == "ClimateOther" & res$x_level == "Highly religious"] <- NA

# Save output
write_csv(res, file = "./Results_Partners/avoidFossil.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "avoidFossil" = "Avoid fossil fuel orgs")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Avoid fossil fuel orgs - for climate (ref = No)", 
                                      "Avoid fossil fuel orgs - for other (ref = No)",
                                      "Avoid fossil fuel orgs - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.1, 0.25, 0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/avoidFossil.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/avoidFossil_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/avoidFossil_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$avoidFossil[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(avoidFossil ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(avoidFossil ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("avoidFossil", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$avoidFossil) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(avoidFossil ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(avoidFossil ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("avoidFossil", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$avoidFossil) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/avoidFossil_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/avoidFossil_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/avoidFossil_preds_denom.csv")



#########################
### Planned fewer children (although not really relevant for the G0 cohort and very few answered 'yes', so will not analyse here)

table(data_partner$children[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1],
      data_partner$children[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$children[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$children[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$children[data_partner$cca_combined == 1])

# ## Religious belief
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(children ~ belief, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(children ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(belief))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (a <- cbind(y = rep("children", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
#             x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
#             mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$children) &
#                                                   !is.na(data_partner$belief)),
#             RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
#                     exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
#                     exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
#                     exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
#                     exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
#                                  c(summary(mod_unadj)$standard.errors[1:3, 2], 
#                                    summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(children ~ belief + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(children ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(belief))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (b <- cbind(y = rep("children", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
#             x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
#             mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$children) &
#                                                 !is.na(data_partner$belief)),
#             RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
#                     exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
#                     exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
#                     exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
#                     exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
#                                  c(summary(mod_adj)$standard.errors[1:3, 2], 
#                                    summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred1 <- avg_predictions(mod_adj, variable = "belief"))
# pred1 <- pred1 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Belief", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# ## Religious affiliation
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(children ~ identity, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(children ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(identity))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (c <- cbind(y = rep("children", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
#             mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$children) &
#                                                   !is.na(data_partner$identity)),
#             RRR = exp(coef(mod_unadj))[1:3, 2], 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
#                     exp(confint(mod_unadj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
#                     exp(confint(mod_unadj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
#                                  summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(children ~ identity + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(children ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(identity))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (d <- cbind(y = rep("children", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
#             mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$children) &
#                                                 !is.na(data_partner$identity)),
#             RRR = exp(coef(mod_adj))[1:3, 2], 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
#                     exp(confint(mod_adj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
#                     exp(confint(mod_adj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
#                                  summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred2 <- avg_predictions(mod_adj, variable = "identity"))
# pred2 <- pred2 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Identity", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# ## Religious attendance
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(children ~ attend, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(children ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(attend))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (e <- cbind(y = rep("children", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
#             mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$children) &
#                                                   !is.na(data_partner$attend)),
#             RRR = exp(coef(mod_unadj))[1:3, 2], 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
#                     exp(confint(mod_unadj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
#                     exp(confint(mod_unadj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
#                                  summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(children ~ attend + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(children ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(attend))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (f <- cbind(y = rep("children", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
#             mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$children) &
#                                                 !is.na(data_partner$attend)),
#             RRR = exp(coef(mod_adj))[1:3, 2], 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
#                     exp(confint(mod_adj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
#                     exp(confint(mod_adj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
#                                  summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred3 <- avg_predictions(mod_adj, variable = "attend"))
# pred3 <- pred3 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Attendance", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# ## Religious LCAs
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(children ~ lca, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(children ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(lca))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (g <- cbind(y = rep("children", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#             x = rep("Latent class", 9), 
#             x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
#             mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$children) &
#                                                   !is.na(data_partner$lca)),
#             RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
#                     exp(coef(mod_unadj))[1:3, 4]), 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
#                     exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
#                     exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
#                     exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
#                     exp(confint(mod_unadj))[4, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
#                     exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
#                     exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
#                     exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
#                     exp(confint(mod_unadj))[4, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
#                                  coef(mod_unadj)[1:3, 4]) / 
#                                  c(summary(mod_unadj)$standard.errors[1:3, 2], 
#                                    summary(mod_unadj)$standard.errors[1:3, 3],
#                                    summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(children ~ lca + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(children ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(lca))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (h <- cbind(y = rep("children", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#             x = rep("Latent class", 9), 
#             x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
#             mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$children) &
#                                                 !is.na(data_partner$lca)),
#             RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
#                     exp(coef(mod_adj))[1:3, 4]), 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
#                     exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
#                     exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
#                     exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
#                     exp(confint(mod_adj))[4, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
#                     exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
#                     exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
#                     exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
#                     exp(confint(mod_adj))[4, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
#                                  coef(mod_adj)[1:3, 4]) / 
#                                  c(summary(mod_adj)$standard.errors[1:3, 2], 
#                                    summary(mod_adj)$standard.errors[1:3, 3],
#                                    summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred4 <- avg_predictions(mod_adj, variable = "lca"))
# pred4 <- pred4 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Latent classes", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# 
# ## Combining all these results together
# res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
# res <- res %>%
#   mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
#          UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
# res
# 
# # Code infinite values (with no data) as missing
# res$RRR[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# res$LCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# res$UCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# res$p[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# 
# # Save output
# write_csv(res, file = "./Results_Partners/children.csv")
# 
# 
# ## Make a nice plot of these results
# res <- res %>%
#   mutate(exp = paste0(x, " - ", x_level)) %>%
#   mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
#                       ifelse(x == "Identity", paste0(exp, " (ref = None)"),
#                              ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
#                                     paste0(exp, " (ref = Atheist)"))))) %>%
#   mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
#                                       "Identity - Christian (ref = None)",
#                                       "Attendance - Regular (ref = Never)",
#                                       "Latent class - Agnostic (ref = Atheist)", 
#                                       "Latent class - Moderately religious (ref = Atheist)", 
#                                       "Latent class - Highly religious (ref = Atheist)"))) %>%
#   mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
#   mutate(y = recode(y, "children" = "Planned fewer children")) %>%
#   mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
#                           "ClimateOther" = "for climate & other")) %>%
#   mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
#   mutate(out = factor(out, levels = c("Planned fewer children - for climate (ref = No)", 
#                                       "Planned fewer children - for other (ref = No)",
#                                       "Planned fewer children - for climate & other (ref = No)")))
# res
# 
# (plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
#     geom_hline(yintercept = 1, lty = 2) +
#     geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
#     geom_point(size = 2, position = position_dodge(width = 0.75)) +
#     geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
#     scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
#     scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
#     scale_x_discrete(limits = rev) +
#     labs(x = "", y = "Relative risk ratio") +
#     scale_y_continuous(trans = "log", breaks = c(0.2, 0.33, 0.5, 0.75, 1, 2, 3)) +
#     coord_flip() +
#     theme_bw() +
#     facet_wrap(out ~ ., ncol = 1) +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12), strip.text = element_text(size = 12)))
# 
# # Save this plot
# pdf("./Results_Partners/children.pdf", height = 8, width = 10)
# plot
# dev.off()
# 
# 
# ## Also make a combined plot of all the predicted probabilities
# grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
# 
# pdf("./Results_Partners/children_preds.pdf", height = 10, width = 18)
# grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
# dev.off()
# 
# # And save all the predicted probabilities results too
# names(pred1)[names(pred1) == "belief"] <- "exposure"
# names(pred2)[names(pred2) == "identity"] <- "exposure"
# names(pred3)[names(pred3) == "attend"] <- "exposure"
# names(pred4)[names(pred4) == "lca"] <- "exposure"
# 
# (pred <- rbind(pred1, pred2, pred3, pred4))
# 
# # Save output
# write_csv(pred, file = "./Results_Partners/children_preds.csv")


# ### And run and store results for religious identity split by denomination
# 
# ## Religious affiliation (split by denomination)
# 
# table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$children[data_partner$cca_combined == 1])
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(children ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(children ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(identity_denom))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (c_denom <- cbind(y = rep("children", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#                   x = rep("Identity (denom)", 9), 
#                   x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
#                   mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                         !is.na(data_partner$children) &
#                                                         !is.na(data_partner$identity_denom)),
#                   RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
#                           exp(coef(mod_unadj))[1:3, 4]), 
#                   LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
#                           exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
#                           exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
#                           exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
#                           exp(confint(mod_unadj))[4, 1, 3]), 
#                   UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
#                           exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
#                           exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
#                           exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
#                           exp(confint(mod_unadj))[4, 2, 3]),
#                   p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
#                                        coef(mod_unadj)[1:3, 4]) / 
#                                        c(summary(mod_unadj)$standard.errors[1:3, 2], 
#                                          summary(mod_unadj)$standard.errors[1:3, 3],
#                                          summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#                   p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(children ~ identity_denom + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(children ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(identity_denom))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (d_denom <- cbind(y = rep("children", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#                   x = rep("Identity (denom)", 9), 
#                   x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
#                   mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                       !is.na(data_partner$children) &
#                                                       !is.na(data_partner$identity_denom)),
#                   RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
#                           exp(coef(mod_adj))[1:3, 4]), 
#                   LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
#                           exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
#                           exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
#                           exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
#                           exp(confint(mod_adj))[4, 1, 3]), 
#                   UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
#                           exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
#                           exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
#                           exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
#                           exp(confint(mod_adj))[4, 2, 3]),
#                   p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
#                                        coef(mod_adj)[1:3, 4]) / 
#                                        c(summary(mod_adj)$standard.errors[1:3, 2], 
#                                          summary(mod_adj)$standard.errors[1:3, 3],
#                                          summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#                   p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Combining results together
# res <- as_tibble(rbind(c_denom, d_denom))
# res <- res %>%
#   mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
#          UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
# res
# 
# # Save output
# write_csv(res, file = "./Results_Partners/children_denom.csv")
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
# pred2_denom <- pred2_denom %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Identity", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# # Save plot and results
# pdf("./Results_Partners/children_denom.pdf", height = 5, width = 8)
# pred_identity_denom
# dev.off()
# 
# names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
# write_csv(pred2_denom, file = "./Results_Partners/children_preds_denom.csv")



#########################
### Other climate action (very few responses, so will comment out)

table(data_partner$otherAction[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$otherAction[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1],
      data_partner$otherAction[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1], 
      data_partner$otherAction[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$otherAction[data_partner$cca_combined == 1])

# ## Religious belief
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(otherAction ~ belief, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(otherAction ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(belief))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (a <- cbind(y = rep("otherAction", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
#             x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
#             mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$otherAction) &
#                                                   !is.na(data_partner$belief)),
#             RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
#                     exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
#                     exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
#                     exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
#                     exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
#                                  c(summary(mod_unadj)$standard.errors[1:3, 2], 
#                                    summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(otherAction ~ belief + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(otherAction ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(belief))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (b <- cbind(y = rep("otherAction", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
#             x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
#             mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$otherAction) &
#                                                 !is.na(data_partner$belief)),
#             RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
#                     exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
#                     exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
#                     exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
#                     exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
#                                  c(summary(mod_adj)$standard.errors[1:3, 2], 
#                                    summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred1 <- avg_predictions(mod_adj, variable = "belief"))
# pred1 <- pred1 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Belief", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# ## Religious affiliation
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(otherAction ~ identity, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(otherAction ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(identity))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (c <- cbind(y = rep("otherAction", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
#             mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$otherAction) &
#                                                   !is.na(data_partner$identity)),
#             RRR = exp(coef(mod_unadj))[1:3, 2], 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
#                     exp(confint(mod_unadj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
#                     exp(confint(mod_unadj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
#                                  summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(otherAction ~ identity + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(otherAction ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(identity))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (d <- cbind(y = rep("otherAction", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
#             mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$otherAction) &
#                                                 !is.na(data_partner$identity)),
#             RRR = exp(coef(mod_adj))[1:3, 2], 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
#                     exp(confint(mod_adj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
#                     exp(confint(mod_adj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
#                                  summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred2 <- avg_predictions(mod_adj, variable = "identity"))
# pred2 <- pred2 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Identity", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# ## Religious attendance
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(otherAction ~ attend, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(otherAction ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(attend))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (e <- cbind(y = rep("otherAction", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
#             mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$otherAction) &
#                                                   !is.na(data_partner$attend)),
#             RRR = exp(coef(mod_unadj))[1:3, 2], 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
#                     exp(confint(mod_unadj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
#                     exp(confint(mod_unadj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
#                                  summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(otherAction ~ attend + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(otherAction ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(attend))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (f <- cbind(y = rep("otherAction", 3), y_level = c("Climate", "Other", "ClimateOther"), 
#             x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
#             mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$otherAction) &
#                                                 !is.na(data_partner$attend)),
#             RRR = exp(coef(mod_adj))[1:3, 2], 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
#                     exp(confint(mod_adj))[2, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
#                     exp(confint(mod_adj))[2, 2, 3]),
#             p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
#                                  summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred3 <- avg_predictions(mod_adj, variable = "attend"))
# pred3 <- pred3 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Attendance", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# ## Religious LCAs
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(otherAction ~ lca, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(otherAction ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(lca))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (g <- cbind(y = rep("otherAction", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#             x = rep("Latent class", 9), 
#             x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
#             mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                   !is.na(data_partner$otherAction) &
#                                                   !is.na(data_partner$lca)),
#             RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
#                     exp(coef(mod_unadj))[1:3, 4]), 
#             LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
#                     exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
#                     exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
#                     exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
#                     exp(confint(mod_unadj))[4, 1, 3]), 
#             UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
#                     exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
#                     exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
#                     exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
#                     exp(confint(mod_unadj))[4, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
#                                  coef(mod_unadj)[1:3, 4]) / 
#                                  c(summary(mod_unadj)$standard.errors[1:3, 2], 
#                                    summary(mod_unadj)$standard.errors[1:3, 3],
#                                    summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#             p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(otherAction ~ lca + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(otherAction ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(lca))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (h <- cbind(y = rep("otherAction", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#             x = rep("Latent class", 9), 
#             x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
#             mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                 !is.na(data_partner$otherAction) &
#                                                 !is.na(data_partner$lca)),
#             RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
#                     exp(coef(mod_adj))[1:3, 4]), 
#             LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
#                     exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
#                     exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
#                     exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
#                     exp(confint(mod_adj))[4, 1, 3]), 
#             UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
#                     exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
#                     exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
#                     exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
#                     exp(confint(mod_adj))[4, 2, 3]),
#             p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
#                                  coef(mod_adj)[1:3, 4]) / 
#                                  c(summary(mod_adj)$standard.errors[1:3, 2], 
#                                    summary(mod_adj)$standard.errors[1:3, 3],
#                                    summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#             p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred4 <- avg_predictions(mod_adj, variable = "lca"))
# pred4 <- pred4 %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Latent classes", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# 
# 
# ## Combining all these results together
# res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
# res <- res %>%
#   mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
#          UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
# res
# 
# # Code infinite values (with no data) as missing
# res$RRR[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# res$LCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# res$UCI[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# res$p[res$y_level == "ClimateOther" & res$x_level == "Moderately religious"] <- NA
# 
# # Save output
# write_csv(res, file = "./Results_Partners/otherAction.csv")
# 
# 
# ## Make a nice plot of these results
# res <- res %>%
#   mutate(exp = paste0(x, " - ", x_level)) %>%
#   mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
#                       ifelse(x == "Identity", paste0(exp, " (ref = None)"),
#                              ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
#                                     paste0(exp, " (ref = Atheist)"))))) %>%
#   mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
#                                       "Identity - Christian (ref = None)",
#                                       "Attendance - Regular (ref = Never)",
#                                       "Latent class - Agnostic (ref = Atheist)", 
#                                       "Latent class - Moderately religious (ref = Atheist)", 
#                                       "Latent class - Highly religious (ref = Atheist)"))) %>%
#   mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
#   mutate(y = recode(y, "otherAction" = "Avoid fossil fuel orgs")) %>%
#   mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
#                           "ClimateOther" = "for climate & other")) %>%
#   mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
#   mutate(out = factor(out, levels = c("Avoid fossil fuel orgs - for climate (ref = No)", 
#                                       "Avoid fossil fuel orgs - for other (ref = No)",
#                                       "Avoid fossil fuel orgs - for climate & other (ref = No)")))
# res
# 
# (plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
#     geom_hline(yintercept = 1, lty = 2) +
#     geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
#     geom_point(size = 2, position = position_dodge(width = 0.75)) +
#     geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
#     scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
#     scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
#     scale_x_discrete(limits = rev) +
#     labs(x = "", y = "Relative risk ratio") +
#     scale_y_continuous(trans = "log", breaks = c(0.2, 0.33, 0.5, 0.75, 1, 2, 3)) +
#     coord_flip() +
#     theme_bw() +
#     facet_wrap(out ~ ., ncol = 1) +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12), strip.text = element_text(size = 12)))
# 
# # Save this plot
# pdf("./Results_Partners/otherAction.pdf", height = 8, width = 10)
# plot
# dev.off()
# 
# 
# ## Also make a combined plot of all the predicted probabilities
# grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
# 
# pdf("./Results_Partners/otherAction_preds.pdf", height = 10, width = 18)
# grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
# dev.off()
# 
# # And save all the predicted probabilities results too
# names(pred1)[names(pred1) == "belief"] <- "exposure"
# names(pred2)[names(pred2) == "identity"] <- "exposure"
# names(pred3)[names(pred3) == "attend"] <- "exposure"
# names(pred4)[names(pred4) == "lca"] <- "exposure"
# 
# (pred <- rbind(pred1, pred2, pred3, pred4))
# 
# # Save output
# write_csv(pred, file = "./Results_Partners/otherAction_preds.csv")


# ### And run and store results for religious identity split by denomination
# 
# ## Religious affiliation (split by denomination)
# 
# table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$otherAction[data_partner$cca_combined == 1])
# 
# # Unadjusted multinomial model
# mod_unadj <- multinom(otherAction ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
# summary(mod_unadj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_unadj_base <- multinom(otherAction ~ 1, data = data_partner, 
#                            subset = cca_confounds == TRUE & !is.na(identity_denom))
# 
# lrtest(mod_unadj_base, mod_unadj)
# 
# # Extract results
# (c_denom <- cbind(y = rep("otherAction", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#                   x = rep("Identity (denom)", 9), 
#                   x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
#                   mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                         !is.na(data_partner$otherAction) &
#                                                         !is.na(data_partner$identity_denom)),
#                   RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
#                           exp(coef(mod_unadj))[1:3, 4]), 
#                   LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
#                           exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
#                           exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
#                           exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
#                           exp(confint(mod_unadj))[4, 1, 3]), 
#                   UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
#                           exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
#                           exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
#                           exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
#                           exp(confint(mod_unadj))[4, 2, 3]),
#                   p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
#                                        coef(mod_unadj)[1:3, 4]) / 
#                                        c(summary(mod_unadj)$standard.errors[1:3, 2], 
#                                          summary(mod_unadj)$standard.errors[1:3, 3],
#                                          summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#                   p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))
# 
# 
# # Adjusted multinomial model
# mod_adj <- multinom(otherAction ~ identity_denom + ageAtQ + marital + rural + edu +
#                       occClass + income + imd + home, data = data_partner)
# summary(mod_adj)
# 
# # Check for overall association between exposure and outcome (using LR test)
# mod_adj_base <- multinom(otherAction ~ ageAtQ + marital + rural + edu +
#                            occClass + income + imd + home, data = data_partner, 
#                          subset = !is.na(identity_denom))
# 
# lrtest(mod_adj_base, mod_adj)
# 
# # Extract results
# (d_denom <- cbind(y = rep("otherAction", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
#                   x = rep("Identity (denom)", 9), 
#                   x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
#                   mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
#                                                       !is.na(data_partner$otherAction) &
#                                                       !is.na(data_partner$identity_denom)),
#                   RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
#                           exp(coef(mod_adj))[1:3, 4]), 
#                   LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
#                           exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
#                           exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
#                           exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
#                           exp(confint(mod_adj))[4, 1, 3]), 
#                   UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
#                           exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
#                           exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
#                           exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
#                           exp(confint(mod_adj))[4, 2, 3]),
#                   p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
#                                        coef(mod_adj)[1:3, 4]) / 
#                                        c(summary(mod_adj)$standard.errors[1:3, 2], 
#                                          summary(mod_adj)$standard.errors[1:3, 3],
#                                          summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
#                   p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))
# 
# 
# ## Combining results together
# res <- as_tibble(rbind(c_denom, d_denom))
# res <- res %>%
#   mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
#          UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
# res
# 
# # Save output
# write_csv(res, file = "./Results_Partners/otherAction_denom.csv")
# 
# 
# ## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
# (pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
# pred2_denom <- pred2_denom %>%
#   mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
#                         "ClimateOther" = "for climate & other")) %>%
#   mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))
# 
# (pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
#     geom_bar(position = "stack", stat = "identity") +
#     labs(x = "Identity", y = "Predicted probability of outcome") +
#     scale_fill_discrete(name = "") +
#     theme_bw() +
#     theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
#           legend.text = element_text(size = 12)))
# 
# # Save plot and results
# pdf("./Results_Partners/otherAction_denom.pdf", height = 5, width = 8)
# pred_identity_denom
# dev.off()
# 
# names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
# write_csv(pred2_denom, file = "./Results_Partners/otherAction_preds_denom.csv")



#########################
### Reduced meat/dairy

table(data_partner$meatDairy[data_partner$cca_combined == 1])
table(data_partner$belief[data_partner$cca_combined == 1], 
      data_partner$meatDairy[data_partner$cca_combined == 1])
table(data_partner$identity[data_partner$cca_combined == 1], 
      data_partner$meatDairy[data_partner$cca_combined == 1])
table(data_partner$attend[data_partner$cca_combined == 1],
      data_partner$meatDairy[data_partner$cca_combined == 1])
table(data_partner$lca[data_partner$cca_combined == 1], 
      data_partner$meatDairy[data_partner$cca_combined == 1])

## Religious belief

# Unadjusted multinomial model
mod_unadj <- multinom(meatDairy ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(meatDairy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(belief))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(a <- cbind(y = rep("meatDairy", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Unadjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$meatDairy) &
                                                  !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(meatDairy ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(meatDairy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(belief))

lrtest(mod_adj_base, mod_adj)

# Extract results
(b <- cbind(y = rep("meatDairy", 6), y_level = rep(c("Climate", "Other", "ClimateOther"), 2), 
            x = rep("Belief", 6), x_level = rep(c("Not sure", "Yes"), each = 3),
            mod = rep("Adjusted", 6), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$meatDairy) &
                                                !is.na(data_partner$belief)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))
pred1 <- pred1 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_belief <- ggplot(data = pred1, aes(fill = group, y = estimate * 100, x = belief)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Belief", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious affiliation

# Unadjusted multinomial model
mod_unadj <- multinom(meatDairy ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(meatDairy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c <- cbind(y = rep("meatDairy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$meatDairy) &
                                                  !is.na(data_partner$identity)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(meatDairy ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(meatDairy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d <- cbind(y = rep("meatDairy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Identity", 3), x_level = c("Christian", "Christian", "Christian"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$meatDairy) &
                                                !is.na(data_partner$identity)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))
pred2 <- pred2 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity <- ggplot(data = pred2, aes(fill = group, y = estimate * 100, x = identity)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious attendance

# Unadjusted multinomial model
mod_unadj <- multinom(meatDairy ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(meatDairy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(attend))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(e <- cbind(y = rep("meatDairy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$meatDairy) &
                                                  !is.na(data_partner$attend)),
            RRR = exp(coef(mod_unadj))[1:3, 2], 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2],
                    exp(confint(mod_unadj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2],
                    exp(confint(mod_unadj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_unadj)[1:3, 2] / 
                                 summary(mod_unadj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(meatDairy ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(meatDairy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(attend))

lrtest(mod_adj_base, mod_adj)

# Extract results
(f <- cbind(y = rep("meatDairy", 3), y_level = c("Climate", "Other", "ClimateOther"), 
            x = rep("Attendance", 3), x_level = c("Regular", "Regular", "Regular"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$meatDairy) &
                                                !is.na(data_partner$attend)),
            RRR = exp(coef(mod_adj))[1:3, 2], 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2],
                    exp(confint(mod_adj))[2, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2],
                    exp(confint(mod_adj))[2, 2, 3]),
            p = (1 - pnorm(abs(coef(mod_adj)[1:3, 2] / 
                                 summary(mod_adj)$standard.errors[1:3, 2]), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))
pred3 <- pred3 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_attend <- ggplot(data = pred3, aes(fill = group, y = estimate * 100, x = attend)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Attendance", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


## Religious LCAs

# Unadjusted multinomial model
mod_unadj <- multinom(meatDairy ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(meatDairy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(lca))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(g <- cbind(y = rep("meatDairy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$meatDairy) &
                                                  !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                    exp(coef(mod_unadj))[1:3, 4]), 
            LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                    exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                    exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                    exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                    exp(confint(mod_unadj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                    exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                    exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                    exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                    exp(confint(mod_unadj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                 coef(mod_unadj)[1:3, 4]) / 
                                 c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                   summary(mod_unadj)$standard.errors[1:3, 3],
                                   summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))



# Adjusted multinomial model
mod_adj <- multinom(meatDairy ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(meatDairy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(lca))

lrtest(mod_adj_base, mod_adj)

# Extract results
(h <- cbind(y = rep("meatDairy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
            x = rep("Latent class", 9), 
            x_level = rep(c("Agnostic", "Moderately religious", "Highly religious"), each = 3),
            mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$meatDairy) &
                                                !is.na(data_partner$lca)),
            RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                    exp(coef(mod_adj))[1:3, 4]), 
            LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                    exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                    exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                    exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                    exp(confint(mod_adj))[4, 1, 3]), 
            UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                    exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                    exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                    exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                    exp(confint(mod_adj))[4, 2, 3]),
            p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                 coef(mod_adj)[1:3, 4]) / 
                                 c(summary(mod_adj)$standard.errors[1:3, 2], 
                                   summary(mod_adj)$standard.errors[1:3, 3],
                                   summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
            p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))
pred4 <- pred4 %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_lca <- ggplot(data = pred4, aes(fill = group, y = estimate * 100, x = lca)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Latent classes", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/meatDairy.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "meatDairy" = "Reduced meat/dairy")) %>%
  mutate(y_level = recode(y_level, "Climate" = "for climate", "Other" = "for other",
                          "ClimateOther" = "for climate & other")) %>%
  mutate(out = paste0(y, " - ", y_level, " (ref = No)")) %>%
  mutate(out = factor(out, levels = c("Reduced meat/dairy - for climate (ref = No)", 
                                      "Reduced meat/dairy - for other (ref = No)",
                                      "Reduced meat/dairy - for climate & other (ref = No)")))
res

(plot <- ggplot(res, aes(x = exp, y = RRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Relative risk ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1, 2, 4)) +
    coord_flip() +
    theme_bw() +
    facet_wrap(out ~ ., ncol = 1) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12), strip.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/meatDairy.pdf", height = 8, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/meatDairy_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/meatDairy_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

table(data_partner$identity_denom[data_partner$cca_combined == 1], data_partner$meatDairy[data_partner$cca_combined == 1])

# Unadjusted multinomial model
mod_unadj <- multinom(meatDairy ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome (using LR test)
mod_unadj_base <- multinom(meatDairy ~ 1, data = data_partner, 
                           subset = cca_confounds == TRUE & !is.na(identity_denom))

lrtest(mod_unadj_base, mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("meatDairy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Unadjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$meatDairy) &
                                                        !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_unadj))[1:3, 2], exp(coef(mod_unadj))[1:3, 3],
                          exp(coef(mod_unadj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_unadj))[2, 1, 1], exp(confint(mod_unadj))[2, 1, 2], 
                          exp(confint(mod_unadj))[2, 1, 3], exp(confint(mod_unadj))[3, 1, 1], 
                          exp(confint(mod_unadj))[3, 1, 2], exp(confint(mod_unadj))[3, 1, 3],
                          exp(confint(mod_unadj))[4, 1, 1], exp(confint(mod_unadj))[4, 1, 2],
                          exp(confint(mod_unadj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_unadj))[2, 2, 1], exp(confint(mod_unadj))[2, 2, 2], 
                          exp(confint(mod_unadj))[2, 2, 3], exp(confint(mod_unadj))[3, 2, 1], 
                          exp(confint(mod_unadj))[3, 2, 2], exp(confint(mod_unadj))[3, 2, 3],
                          exp(confint(mod_unadj))[4, 2, 1], exp(confint(mod_unadj))[4, 2, 2],
                          exp(confint(mod_unadj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_unadj)[1:3, 2], coef(mod_unadj)[1:3, 3],
                                       coef(mod_unadj)[1:3, 4]) / 
                                       c(summary(mod_unadj)$standard.errors[1:3, 2], 
                                         summary(mod_unadj)$standard.errors[1:3, 3],
                                         summary(mod_unadj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_unadj_base, mod_unadj)$`Pr(>Chisq)`[2]))


# Adjusted multinomial model
mod_adj <- multinom(meatDairy ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner)
summary(mod_adj)

# Check for overall association between exposure and outcome (using LR test)
mod_adj_base <- multinom(meatDairy ~ ageAtQ + marital + rural + edu +
                           occClass + income + imd + home, data = data_partner, 
                         subset = !is.na(identity_denom))

lrtest(mod_adj_base, mod_adj)

# Extract results
(d_denom <- cbind(y = rep("meatDairy", 9), y_level = rep(c("Climate", "Other", "ClimateOther"), 3), 
                  x = rep("Identity (denom)", 9), 
                  x_level = rep(c("C of E", "Catholic", "Other"), each = 3),
                  mod = rep("Adjusted", 9), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$meatDairy) &
                                                      !is.na(data_partner$identity_denom)),
                  RRR = c(exp(coef(mod_adj))[1:3, 2], exp(coef(mod_adj))[1:3, 3],
                          exp(coef(mod_adj))[1:3, 4]), 
                  LCI = c(exp(confint(mod_adj))[2, 1, 1], exp(confint(mod_adj))[2, 1, 2], 
                          exp(confint(mod_adj))[2, 1, 3], exp(confint(mod_adj))[3, 1, 1], 
                          exp(confint(mod_adj))[3, 1, 2], exp(confint(mod_adj))[3, 1, 3],
                          exp(confint(mod_adj))[4, 1, 1], exp(confint(mod_adj))[4, 1, 2],
                          exp(confint(mod_adj))[4, 1, 3]), 
                  UCI = c(exp(confint(mod_adj))[2, 2, 1], exp(confint(mod_adj))[2, 2, 2], 
                          exp(confint(mod_adj))[2, 2, 3], exp(confint(mod_adj))[3, 2, 1], 
                          exp(confint(mod_adj))[3, 2, 2], exp(confint(mod_adj))[3, 2, 3],
                          exp(confint(mod_adj))[4, 2, 1], exp(confint(mod_adj))[4, 2, 2],
                          exp(confint(mod_adj))[4, 2, 3]),
                  p = (1 - pnorm(abs(c(coef(mod_adj)[1:3, 2], coef(mod_adj)[1:3, 3],
                                       coef(mod_adj)[1:3, 4]) / 
                                       c(summary(mod_adj)$standard.errors[1:3, 2], 
                                         summary(mod_adj)$standard.errors[1:3, 3],
                                         summary(mod_adj)$standard.errors[1:3, 4])), 0, 1)) * 2,
                  p_total = lrtest(mod_adj_base, mod_adj)$`Pr(>Chisq)`[2]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), RRR = as.numeric(RRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/meatDairy_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))
pred2_denom <- pred2_denom %>%
  mutate(group = recode(group, "Climate" = "for climate", "Other" = "for other",
                        "ClimateOther" = "for climate & other")) %>%
  mutate(group = factor(group, levels = c("No", "for climate", "for other", "for climate & other")))

(pred_identity_denom <- ggplot(data = pred2_denom, aes(fill = group, y = estimate * 100, x = identity_denom)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Identity", y = "Predicted probability of outcome") +
    scale_fill_discrete(name = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save plot and results
pdf("./Results_Partners/meatDairy_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/meatDairy_preds_denom.csv")



########################################################################################
#########################
### Total number of climate actions (excluding 'children' and 'other actions')

summary(data_partner$totalActions[data_partner$cca_combined == 1])

ggplot(subset(data_partner, cca_combined == 1), aes(x = totalActions)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

by(data_partner$totalActions[data_partner$cca_combined == 1], 
   data_partner$belief[data_partner$cca_combined == 1], summary)
by(data_partner$totalActions[data_partner$cca_combined == 1], 
   data_partner$identity[data_partner$cca_combined == 1], summary)
by(data_partner$totalActions[data_partner$cca_combined == 1], 
   data_partner$attend[data_partner$cca_combined == 1], summary)
by(data_partner$totalActions[data_partner$cca_combined == 1], 
   data_partner$lca[data_partner$cca_combined == 1], summary)


## Religious belief

# Unadjusted linear model
mod_unadj <- lm(totalActions ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "belief")

# Extract results
(a <- cbind(y = rep("totalActions", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = nobs(mod_unadj),
            b = coef(mod_unadj)[2:3], LCI = confint(mod_unadj)[2:3, 1],
            UCI = confint(mod_unadj)[2:3, 2], p = coef(summary(mod_unadj))[2:3, 4],
            p_total = hypotheses(mod_unadj, joint = "belief")$p))



# Adjusted linear model
mod_adj <- lm(totalActions ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "belief")

# Extract results
(b <- cbind(y = rep("totalActions", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = nobs(mod_adj),
            b = coef(mod_adj)[2:3], LCI = confint(mod_adj)[2:3, 1],
            UCI = confint(mod_adj)[2:3, 2], p = coef(summary(mod_adj))[2:3, 4],
            p_total = hypotheses(mod_adj, joint = "belief")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_belief <- ggplot(data = na.omit(data_partner), aes(x = belief, y = fitted, fill = belief)) +
  geom_boxplot() +
  labs(x = "Belief", y = "Predicted values of outcome") +
  scale_fill_discrete(guide = "none") +
  theme_bw() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious affiliation

# Unadjusted linear model
mod_unadj <- lm(totalActions ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c <- cbind(y = "totalActions", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = nobs(mod_unadj),
            b = coef(mod_unadj)[2], LCI = confint(mod_unadj)[2, 1],
            UCI = confint(mod_unadj)[2, 2], p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted linear model
mod_adj <- lm(totalActions ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d <- cbind(y = "totalActions", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = nobs(mod_adj),
            b = coef(mod_adj)[2], LCI = confint(mod_adj)[2, 1],
            UCI = confint(mod_adj)[2, 2], p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity <- ggplot(data = na.omit(data_partner), aes(x = identity, y = fitted, fill = identity)) +
    geom_boxplot() +
    labs(x = "Identity", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious attendance

# Unadjusted linear model
mod_unadj <- lm(totalActions ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(e <- cbind(y = "totalActions", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = nobs(mod_unadj),
            b = coef(mod_unadj)[2], LCI = confint(mod_unadj)[2, 1],
            UCI = confint(mod_unadj)[2, 2], p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted linear model
mod_adj <- lm(totalActions ~ attend + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(f <- cbind(y = "totalActions", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = nobs(mod_adj),
            b = coef(mod_adj)[2], LCI = confint(mod_adj)[2, 1],
            UCI = confint(mod_adj)[2, 2], p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_attend <- ggplot(data = na.omit(data_partner), aes(x = attend, y = fitted, fill = attend)) +
    geom_boxplot() +
    labs(x = "Attendance", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious LCAs

# Unadjusted linear model
mod_unadj <- lm(totalActions ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "lca")

# Extract results
(g <- cbind(y = rep("totalActions", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
            b = coef(mod_unadj)[2:4], LCI = confint(mod_unadj)[2:4, 1],
            UCI = confint(mod_unadj)[2:4, 2], p = coef(summary(mod_unadj))[2:4, 4],
            p_total = hypotheses(mod_unadj, joint = "lca")$p))



# Adjusted linear model
mod_adj <- lm(totalActions ~ lca + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "lca")

# Extract results
(h <- cbind(y = rep("totalActions", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = nobs(mod_adj),
            b = coef(mod_adj)[2:4], LCI = confint(mod_adj)[2:4, 1],
            UCI = confint(mod_adj)[2:4, 2], p = coef(summary(mod_adj))[2:4, 4],
            p_total = hypotheses(mod_adj, joint = "lca")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_lca <- ggplot(data = na.omit(data_partner), aes(x = lca, y = fitted, fill = lca)) +
    geom_boxplot() +
    labs(x = "Latent classes", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), b = as.numeric(b), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "totalActions" = "Total number of actions"))
res

(plot <- ggplot(res, aes(x = exp, y = b, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Mean difference") +
    scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/totalActions.pdf", height = 5, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/totalActions_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/totalActions_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

by(data_partner$totalActions[data_partner$cca_combined == 1], data_partner$identity_denom[data_partner$cca_combined == 1], summary)

# Unadjusted linear model
mod_unadj <- lm(totalActions ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "identity_denom")

# Extract results
(c_denom <- cbind(y = rep("totalActions", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
                  b = coef(mod_unadj)[2:4], LCI = confint(mod_unadj)[2:4, 1],
                  UCI = confint(mod_unadj)[2:4, 2], p = coef(summary(mod_unadj))[2:4, 4],
                  p_total = hypotheses(mod_unadj, joint = "identity_denom")$p))


# Adjusted linear model
mod_adj <- lm(totalActions ~ identity_denom + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "identity_denom")

# Extract results
(d_denom <- cbind(y = rep("totalActions", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = nobs(mod_adj),
                  b = coef(mod_adj)[2:4], LCI = confint(mod_adj)[2:4, 1],
                  UCI = confint(mod_adj)[2:4, 2], p = coef(summary(mod_adj))[2:4, 4],
                  p_total = hypotheses(mod_adj, joint = "identity_denom")$p))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), b = as.numeric(b), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity_denom <- ggplot(data = na.omit(data_partner), aes(x = identity_denom, y = fitted, fill = identity_denom)) +
    geom_boxplot() +
    labs(x = "Identity (denom)", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))

# Save plot and results
pdf("./Results_Partners/totalActions_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/totalActions_preds_denom.csv")



#########################
### Total number of climate actions (excluding 'children' and 'other actions') - Using Poisson model

## Religious belief

# Unadjusted Poisson model
mod_unadj <- glm(totalActions ~ belief, family = "poisson", 
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "belief")

# Extract results
(a <- cbind(y = rep("totalActions", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2:3]), LCI = exp(confint.default(mod_unadj)[2:3, 1]),
            UCI = exp(confint.default(mod_unadj)[2:3, 2]), p = coef(summary(mod_unadj))[2:3, 4],
            p_total = hypotheses(mod_unadj, joint = "belief")$p))



# Adjusted Poisson model
mod_adj <- glm(totalActions ~ belief + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "belief")

# Extract results
(b <- cbind(y = rep("totalActions", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2:3]), LCI = exp(confint.default(mod_adj)[2:3, 1]),
            UCI = exp(confint.default(mod_adj)[2:3, 2]), p = coef(summary(mod_adj))[2:3, 4],
            p_total = hypotheses(mod_adj, joint = "belief")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_belief <- ggplot(data = na.omit(data_partner), aes(x = belief, y = fitted, fill = belief)) +
    geom_boxplot() +
    labs(x = "Belief", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious affiliation

# Unadjusted Poisson model
mod_unadj <- glm(totalActions ~ identity, family = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c <- cbind(y = "totalActions", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted Poisson model
mod_adj <- glm(totalActions ~ identity + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d <- cbind(y = "totalActions", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity <- ggplot(data = na.omit(data_partner), aes(x = identity, y = fitted, fill = identity)) +
    geom_boxplot() +
    labs(x = "Identity", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious attendance

# Unadjusted Poisson model
mod_unadj <- glm(totalActions ~ attend, family = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(e <- cbind(y = "totalActions", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted Poisson model
mod_adj <- glm(totalActions ~ attend + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(f <- cbind(y = "totalActions", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_attend <- ggplot(data = na.omit(data_partner), aes(x = attend, y = fitted, fill = attend)) +
    geom_boxplot() +
    labs(x = "Attendance", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious LCAs

# Unadjusted Poisson model
mod_unadj <- glm(totalActions ~ lca, family = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "lca")

# Extract results
(g <- cbind(y = rep("totalActions", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
            UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))[2:4, 4],
            p_total = hypotheses(mod_unadj, joint = "lca")$p))



# Adjusted Poisson model
mod_adj <- glm(totalActions ~ lca + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "lca")

# Extract results
(h <- cbind(y = rep("totalActions", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
            UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))[2:4, 4],
            p_total = hypotheses(mod_adj, joint = "lca")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_lca <- ggplot(data = na.omit(data_partner), aes(x = lca, y = fitted, fill = lca)) +
    geom_boxplot() +
    labs(x = "Latent classes", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_p.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "totalActions" = "Total number of actions"))
res

(plot <- ggplot(res, aes(x = exp, y = IRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Incidence rate ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.3)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/totalActions_p.pdf", height = 5, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/totalActions_p_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/totalActions_p_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

# Unadjusted Poisson model
mod_unadj <- glm(totalActions ~ identity_denom, family = "poisson", 
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "identity_denom")

# Extract results
(c_denom <- cbind(y = rep("totalActions", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
                  IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
                  UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))[2:4, 4],
                  p_total = hypotheses(mod_unadj, joint = "identity_denom")$p))


# Adjusted Poisson model
mod_adj <- glm(totalActions ~ identity_denom + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "identity_denom")

# Extract results
(d_denom <- cbind(y = rep("totalActions", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = nobs(mod_unadj),
                  IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
                  UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))[2:4, 4],
                  p_total = hypotheses(mod_adj, joint = "identity_denom")$p))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_p_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity_denom <- ggplot(data = na.omit(data_partner), aes(x = identity_denom, y = fitted, fill = identity_denom)) +
    geom_boxplot() +
    labs(x = "Identity (denom)", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))

# Save plot and results
pdf("./Results_Partners/totalActions_p_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/totalActions_preds_p_denom.csv")



#########################
### Total number of climate actions (excluding 'children' and 'other actions') - Using zero-inflated Poisson model to account for excess zeros in the outcome

## Religious belief

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions ~ belief | belief, dist = "poisson", 
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(a <- cbind(y = rep("totalActions", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$totalActions) &
                                                  !is.na(data_partner$belief)),
            IRR = exp(coef(mod_unadj)[2:3]), LCI = exp(confint.default(mod_unadj)[2:3, 1]),
            UCI = exp(confint.default(mod_unadj)[2:3, 2]), p = coef(summary(mod_unadj))$count[2:3, 4],
            zi_OR = exp(coef(mod_unadj)[5:6]), zi_LCI = exp(confint.default(mod_unadj)[5:6, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[5:6, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2:3, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home | belief + ageAtQ + marital + 
                      rural + edu + occClass + income + imd + home, dist = "poisson",
                    data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(b <- cbind(y = rep("totalActions", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$totalActions) &
                                                  !is.na(data_partner$belief)),
            IRR = exp(coef(mod_adj)[2:3]), LCI = exp(confint.default(mod_adj)[2:3, 1]),
            UCI = exp(confint.default(mod_adj)[2:3, 2]), p = coef(summary(mod_adj))$count[2:3, 4],
            zi_OR = exp(coef(mod_adj)[28:29]), zi_LCI = exp(confint.default(mod_adj)[28:29, 1]),
            zi_UCI = exp(confint.default(mod_adj)[28:29, 2]),
            zi_p = coef(summary(mod_adj))$zero[2:3, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions) & !is.na(belief)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_belief <- ggplot(data = data_partner2, aes(x = belief, y = fitted, fill = belief)) +
    geom_boxplot() +
    labs(x = "Belief", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious affiliation

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions ~ identity | identity, dist = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c <- cbind(y = "totalActions", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                          !is.na(data_partner$totalActions) &
                                          !is.na(data_partner$identity)),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))$count[2, 4],
            zi_OR = exp(coef(mod_unadj)[4]), zi_LCI = exp(confint.default(mod_unadj)[4, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[4, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions ~ identity + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home | identity + ageAtQ + marital + rural + edu +
                   occClass + income + imd + home, dist = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d <- cbind(y = "totalActions", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                        !is.na(data_partner$totalActions) &
                                        !is.na(data_partner$identity)),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))$count[2, 4],
            zi_OR = exp(coef(mod_adj)[27]), zi_LCI = exp(confint.default(mod_adj)[27, 1]),
            zi_UCI = exp(confint.default(mod_adj)[27, 2]),
            zi_p = coef(summary(mod_adj))$zero[2, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions) & !is.na(identity)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity <- ggplot(data = data_partner2, aes(x = identity, y = fitted, fill = identity)) +
    geom_boxplot() +
    labs(x = "Identity", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious attendance

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions ~ attend | attend, dist = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(e <- cbind(y = "totalActions", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                          !is.na(data_partner$totalActions) &
                                          !is.na(data_partner$attend)),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))$count[2, 4],
            zi_OR = exp(coef(mod_unadj)[4]), zi_LCI = exp(confint.default(mod_unadj)[4, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[4, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions ~ attend + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home | attend + ageAtQ + marital + rural + edu +
                   occClass + income + imd + home, dist = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(f <- cbind(y = "totalActions", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                        !is.na(data_partner$totalActions) &
                                        !is.na(data_partner$attend)),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))$count[2, 4],
            zi_OR = exp(coef(mod_adj)[27]), zi_LCI = exp(confint.default(mod_adj)[27, 1]),
            zi_UCI = exp(confint.default(mod_adj)[27, 2]),
            zi_p = coef(summary(mod_adj))$zero[2, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions) & !is.na(attend)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_attend <- ggplot(data = data_partner2, aes(x = attend, y = fitted, fill = attend)) +
    geom_boxplot() +
    labs(x = "Attendance", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious LCAs

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions ~ lca | lca, dist = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(g <- cbind(y = rep("totalActions", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$totalActions) &
                                                  !is.na(data_partner$lca)),
            IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
            UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))$count[2:4, 4],
            zi_OR = exp(coef(mod_unadj)[6:8]), zi_LCI = exp(confint.default(mod_unadj)[6:8, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[6:8, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2:4, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions ~ lca + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home | lca + ageAtQ + marital + rural + edu +
                   occClass + income + imd + home, dist = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(h <- cbind(y = rep("totalActions", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$totalActions) &
                                                !is.na(data_partner$lca)),
            IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
            UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))$count[2:4, 4],
            zi_OR = exp(coef(mod_adj)[29:31]), zi_LCI = exp(confint.default(mod_adj)[29:31, 1]),
            zi_UCI = exp(confint.default(mod_adj)[29:31, 2]),
            zi_p = coef(summary(mod_adj))$zero[2:4, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions) & !is.na(lca)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_lca <- ggplot(data = data_partner2, aes(x = lca, y = fitted, fill = lca)) +
    geom_boxplot() +
    labs(x = "Latent classes", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), zi_OR = as.numeric(zi_OR),
         zi_LCI = as.numeric(zi_LCI), zi_UCI = as.numeric(zi_UCI), zi_p = as.numeric(zi_p))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_p_zi.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "totalActions" = "Total number of actions"))
res

(plot1 <- ggplot(res, aes(x = exp, y = IRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Incidence rate ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.9, 1, 1.1, 1.2)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

(plot2 <- ggplot(res, aes(x = exp, y = zi_OR, ymin = zi_LCI, ymax = zi_UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Odds ratio (zero-inflated)") +
    scale_y_continuous(trans = "log", breaks = c(0.33, 0.5, 0.7, 1, 1.5)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


grid.arrange(plot1, plot2, ncol = 1)

# Save this plot
pdf("./Results_Partners/totalActions_p_zi.pdf", height = 8, width = 10)
grid.arrange(plot1, plot2, ncol = 1)
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/totalActions_p_zi_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/totalActions_p_zi_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions ~ identity_denom | identity_denom, dist = "poisson",
                      data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("totalActions", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$totalActions) &
                                                        !is.na(data_partner$identity_denom)),
                  IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
                  UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))$count[2:4, 4],
                  zi_OR = exp(coef(mod_unadj)[6:8]), zi_LCI = exp(confint.default(mod_unadj)[6:8, 1]),
                  zi_UCI = exp(confint.default(mod_unadj)[6:8, 2]),
                  zi_p = coef(summary(mod_unadj))$zero[2:4, 4]))


# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home | identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, dist = "poisson",
                    data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d_denom <- cbind(y = rep("totalActions", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$totalActions) &
                                                      !is.na(data_partner$identity_denom)),
                  IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
                  UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))$count[2:4, 4],
                  zi_OR = exp(coef(mod_adj)[6:8]), zi_LCI = exp(confint.default(mod_adj)[6:8, 1]),
                  zi_UCI = exp(confint.default(mod_adj)[6:8, 2]),
                  zi_p = coef(summary(mod_adj))$zero[2:4, 4]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), zi_OR = as.numeric(zi_OR),
         zi_LCI = as.numeric(zi_LCI), zi_UCI = as.numeric(zi_UCI), zi_p = as.numeric(zi_p))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_zi_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions) & !is.na(identity_denom)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity_denom <- ggplot(data = na.omit(data_partner2), aes(x = identity_denom, y = fitted, fill = identity_denom)) +
    geom_boxplot() +
    labs(x = "Identity (denom)", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))

# Save plot and results
pdf("./Results_Partners/totalActions_zi_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/totalActions_preds_zi_denom.csv")



######################################################################
### Total number of climate actions (excluding 'children' and 'other actions' and ones which may be prohibitively costly)

summary(data_partner$totalActions_reduced[data_partner$cca_combined == 1])

ggplot(subset(data_partner, cca_combined == 1), aes(x = totalActions_reduced)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

by(data_partner$totalActions_reduced[data_partner$cca_combined == 1], 
   data_partner$belief[data_partner$cca_combined == 1], summary)
by(data_partner$totalActions_reduced[data_partner$cca_combined == 1], 
   data_partner$identity[data_partner$cca_combined == 1], summary)
by(data_partner$totalActions_reduced[data_partner$cca_combined == 1], 
   data_partner$attend[data_partner$cca_combined == 1], summary)
by(data_partner$totalActions_reduced[data_partner$cca_combined == 1], 
   data_partner$lca[data_partner$cca_combined == 1], summary)


## Religious belief

# Unadjusted linear model
mod_unadj <- lm(totalActions_reduced ~ belief, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "belief")

# Extract results
(a <- cbind(y = rep("totalActions_reduced", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = nobs(mod_unadj),
            b = coef(mod_unadj)[2:3], LCI = confint(mod_unadj)[2:3, 1],
            UCI = confint(mod_unadj)[2:3, 2], p = coef(summary(mod_unadj))[2:3, 4],
            p_total = hypotheses(mod_unadj, joint = "belief")$p))



# Adjusted linear model
mod_adj <- lm(totalActions_reduced ~ belief + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "belief")

# Extract results
(b <- cbind(y = rep("totalActions_reduced", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = nobs(mod_adj),
            b = coef(mod_adj)[2:3], LCI = confint(mod_adj)[2:3, 1],
            UCI = confint(mod_adj)[2:3, 2], p = coef(summary(mod_adj))[2:3, 4],
            p_total = hypotheses(mod_adj, joint = "belief")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_belief <- ggplot(data = na.omit(data_partner), aes(x = belief, y = fitted, fill = belief)) +
    geom_boxplot() +
    labs(x = "Belief", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious affiliation

# Unadjusted linear model
mod_unadj <- lm(totalActions_reduced ~ identity, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c <- cbind(y = "totalActions_reduced", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = nobs(mod_unadj),
            b = coef(mod_unadj)[2], LCI = confint(mod_unadj)[2, 1],
            UCI = confint(mod_unadj)[2, 2], p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted linear model
mod_adj <- lm(totalActions_reduced ~ identity + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d <- cbind(y = "totalActions_reduced", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = nobs(mod_adj),
            b = coef(mod_adj)[2], LCI = confint(mod_adj)[2, 1],
            UCI = confint(mod_adj)[2, 2], p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity <- ggplot(data = na.omit(data_partner), aes(x = identity, y = fitted, fill = identity)) +
    geom_boxplot() +
    labs(x = "Identity", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious attendance

# Unadjusted linear model
mod_unadj <- lm(totalActions_reduced ~ attend, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(e <- cbind(y = "totalActions_reduced", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = nobs(mod_unadj),
            b = coef(mod_unadj)[2], LCI = confint(mod_unadj)[2, 1],
            UCI = confint(mod_unadj)[2, 2], p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted linear model
mod_adj <- lm(totalActions_reduced ~ attend + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(f <- cbind(y = "totalActions_reduced", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = nobs(mod_adj),
            b = coef(mod_adj)[2], LCI = confint(mod_adj)[2, 1],
            UCI = confint(mod_adj)[2, 2], p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_attend <- ggplot(data = na.omit(data_partner), aes(x = attend, y = fitted, fill = attend)) +
    geom_boxplot() +
    labs(x = "Attendance", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious LCAs

# Unadjusted linear model
mod_unadj <- lm(totalActions_reduced ~ lca, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "lca")

# Extract results
(g <- cbind(y = rep("totalActions_reduced", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
            b = coef(mod_unadj)[2:4], LCI = confint(mod_unadj)[2:4, 1],
            UCI = confint(mod_unadj)[2:4, 2], p = coef(summary(mod_unadj))[2:4, 4],
            p_total = hypotheses(mod_unadj, joint = "lca")$p))



# Adjusted linear model
mod_adj <- lm(totalActions_reduced ~ lca + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "lca")

# Extract results
(h <- cbind(y = rep("totalActions_reduced", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = nobs(mod_adj),
            b = coef(mod_adj)[2:4], LCI = confint(mod_adj)[2:4, 1],
            UCI = confint(mod_adj)[2:4, 2], p = coef(summary(mod_adj))[2:4, 4],
            p_total = hypotheses(mod_adj, joint = "lca")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_lca <- ggplot(data = na.omit(data_partner), aes(x = lca, y = fitted, fill = lca)) +
    geom_boxplot() +
    labs(x = "Latent classes", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), b = as.numeric(b), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_reduced.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "totalActions_reduced" = "Total number of actions"))
res

(plot <- ggplot(res, aes(x = exp, y = b, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Mean difference") +
    scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/totalActions_reduced.pdf", height = 5, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/totalActions_reduced_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/totalActions_reduced_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

by(data_partner$totalActions_reduced[data_partner$cca_combined == 1], data_partner$identity_denom[data_partner$cca_combined == 1], summary)

# Unadjusted linear model
mod_unadj <- lm(totalActions_reduced ~ identity_denom, data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "identity_denom")

# Extract results
(c_denom <- cbind(y = rep("totalActions_reduced", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
                  b = coef(mod_unadj)[2:4], LCI = confint(mod_unadj)[2:4, 1],
                  UCI = confint(mod_unadj)[2:4, 2], p = coef(summary(mod_unadj))[2:4, 4],
                  p_total = hypotheses(mod_unadj, joint = "identity_denom")$p))


# Adjusted linear model
mod_adj <- lm(totalActions_reduced ~ identity_denom + ageAtQ + marital + rural + edu +
                occClass + income + imd + home, data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "identity_denom")

# Extract results
(d_denom <- cbind(y = rep("totalActions_reduced", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = nobs(mod_adj),
                  b = coef(mod_adj)[2:4], LCI = confint(mod_adj)[2:4, 1],
                  UCI = confint(mod_adj)[2:4, 2], p = coef(summary(mod_adj))[2:4, 4],
                  p_total = hypotheses(mod_adj, joint = "identity_denom")$p))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), b = as.numeric(b), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_reduced_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity_denom <- ggplot(data = na.omit(data_partner), aes(x = identity_denom, y = fitted, fill = identity_denom)) +
    geom_boxplot() +
    labs(x = "Identity (denom)", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))

# Save plot and results
pdf("./Results_Partners/totalActions_reduced_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/totalActions_reduced_preds_denom.csv")



#########################
### Total number of climate actions (excluding 'children' and 'other actions' and ones which may be prohibitively costly) - Using Poisson model

## Religious belief

# Unadjusted Poisson model
mod_unadj <- glm(totalActions_reduced ~ belief, family = "poisson", 
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "belief")

# Extract results
(a <- cbind(y = rep("totalActions_reduced", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2:3]), LCI = exp(confint.default(mod_unadj)[2:3, 1]),
            UCI = exp(confint.default(mod_unadj)[2:3, 2]), p = coef(summary(mod_unadj))[2:3, 4],
            p_total = hypotheses(mod_unadj, joint = "belief")$p))



# Adjusted Poisson model
mod_adj <- glm(totalActions_reduced ~ belief + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "belief")

# Extract results
(b <- cbind(y = rep("totalActions_reduced", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2:3]), LCI = exp(confint.default(mod_adj)[2:3, 1]),
            UCI = exp(confint.default(mod_adj)[2:3, 2]), p = coef(summary(mod_adj))[2:3, 4],
            p_total = hypotheses(mod_adj, joint = "belief")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_belief <- ggplot(data = na.omit(data_partner), aes(x = belief, y = fitted, fill = belief)) +
    geom_boxplot() +
    labs(x = "Belief", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious affiliation

# Unadjusted Poisson model
mod_unadj <- glm(totalActions_reduced ~ identity, family = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c <- cbind(y = "totalActions_reduced", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted Poisson model
mod_adj <- glm(totalActions_reduced ~ identity + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d <- cbind(y = "totalActions_reduced", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity <- ggplot(data = na.omit(data_partner), aes(x = identity, y = fitted, fill = identity)) +
    geom_boxplot() +
    labs(x = "Identity", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious attendance

# Unadjusted Poisson model
mod_unadj <- glm(totalActions_reduced ~ attend, family = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(e <- cbind(y = "totalActions_reduced", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))[2, 4],
            p_total = NA))



# Adjusted Poisson model
mod_adj <- glm(totalActions_reduced ~ attend + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(f <- cbind(y = "totalActions_reduced", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))[2, 4],
            p_total = NA))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_attend <- ggplot(data = na.omit(data_partner), aes(x = attend, y = fitted, fill = attend)) +
    geom_boxplot() +
    labs(x = "Attendance", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious LCAs

# Unadjusted Poisson model
mod_unadj <- glm(totalActions_reduced ~ lca, family = "poisson",
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "lca")

# Extract results
(g <- cbind(y = rep("totalActions_reduced", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
            IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
            UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))[2:4, 4],
            p_total = hypotheses(mod_unadj, joint = "lca")$p))



# Adjusted Poisson model
mod_adj <- glm(totalActions_reduced ~ lca + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "lca")

# Extract results
(h <- cbind(y = rep("totalActions_reduced", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = nobs(mod_adj),
            IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
            UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))[2:4, 4],
            p_total = hypotheses(mod_adj, joint = "lca")$p))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_lca <- ggplot(data = na.omit(data_partner), aes(x = lca, y = fitted, fill = lca)) +
    geom_boxplot() +
    labs(x = "Latent classes", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_reduced_p.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "totalActions_reduced" = "Total number of actions"))
res

(plot <- ggplot(res, aes(x = exp, y = IRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Incidence rate ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.9, 1, 1.1, 1.2)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

# Save this plot
pdf("./Results_Partners/totalActions_reduced_p.pdf", height = 5, width = 10)
plot
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/totalActions_reduced_p_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/totalActions_reduced_p_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

# Unadjusted Poisson model
mod_unadj <- glm(totalActions_reduced ~ identity_denom, family = "poisson", 
                 data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Check for overall association between exposure and outcome
hypotheses(mod_unadj, joint = "identity_denom")

# Extract results
(c_denom <- cbind(y = rep("totalActions_reduced", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = nobs(mod_unadj),
                  IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
                  UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))[2:4, 4],
                  p_total = hypotheses(mod_unadj, joint = "identity_denom")$p))


# Adjusted Poisson model
mod_adj <- glm(totalActions_reduced ~ identity_denom + ageAtQ + marital + rural + edu +
                 occClass + income + imd + home, family = "poisson",
               data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Check for overall association between exposure and outcome
hypotheses(mod_adj, joint = "identity_denom")

# Extract results
(d_denom <- cbind(y = rep("totalActions_reduced", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = nobs(mod_unadj),
                  IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
                  UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))[2:4, 4],
                  p_total = hypotheses(mod_adj, joint = "identity_denom")$p))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), p_total = as.numeric(p_total))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_reduced_p_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))

data_partner <- data_partner %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity_denom <- ggplot(data = na.omit(data_partner), aes(x = identity_denom, y = fitted, fill = identity_denom)) +
    geom_boxplot() +
    labs(x = "Identity (denom)", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))

# Save plot and results
pdf("./Results_Partners/totalActions_reduced_p_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/totalActions_reduced_preds_p_denom.csv")



#########################
### Total number of climate actions (excluding 'children' and 'other actions' and ones which may be prohibitively costly) - Using zero-inflated Poisson model to account for excess zeros in the outcome

## Religious belief

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions_reduced ~ belief | belief, dist = "poisson", 
                      data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(a <- cbind(y = rep("totalActions_reduced", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Unadjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$totalActions_reduced) &
                                                  !is.na(data_partner$belief)),
            IRR = exp(coef(mod_unadj)[2:3]), LCI = exp(confint.default(mod_unadj)[2:3, 1]),
            UCI = exp(confint.default(mod_unadj)[2:3, 2]), p = coef(summary(mod_unadj))$count[2:3, 4],
            zi_OR = exp(coef(mod_unadj)[5:6]), zi_LCI = exp(confint.default(mod_unadj)[5:6, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[5:6, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2:3, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions_reduced ~ belief + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home | belief + ageAtQ + marital + 
                      rural + edu + occClass + income + imd + home, dist = "poisson",
                    data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(b <- cbind(y = rep("totalActions_reduced", 2), x = rep("Belief", 2), x_level = c("Not sure", "Yes"),
            mod = rep("Adjusted", 2), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$totalActions_reduced) &
                                                !is.na(data_partner$belief)),
            IRR = exp(coef(mod_adj)[2:3]), LCI = exp(confint.default(mod_adj)[2:3, 1]),
            UCI = exp(confint.default(mod_adj)[2:3, 2]), p = coef(summary(mod_adj))$count[2:3, 4],
            zi_OR = exp(coef(mod_adj)[28:29]), zi_LCI = exp(confint.default(mod_adj)[28:29, 1]),
            zi_UCI = exp(confint.default(mod_adj)[28:29, 2]),
            zi_p = coef(summary(mod_adj))$zero[2:3, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred1 <- avg_predictions(mod_adj, variable = "belief"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions_reduced) & !is.na(belief)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_belief <- ggplot(data = data_partner2, aes(x = belief, y = fitted, fill = belief)) +
    geom_boxplot() +
    labs(x = "Belief", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious affiliation

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions_reduced ~ identity | identity, dist = "poisson",
                      data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c <- cbind(y = "totalActions_reduced", x = "Identity", x_level = "Christian",
            mod = "Unadjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                          !is.na(data_partner$totalActions_reduced) &
                                          !is.na(data_partner$identity)),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))$count[2, 4],
            zi_OR = exp(coef(mod_unadj)[4]), zi_LCI = exp(confint.default(mod_unadj)[4, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[4, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions_reduced ~ identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home | identity + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, dist = "poisson",
                    data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d <- cbind(y = "totalActions_reduced", x = "Identity", x_level = "Christian",
            mod = "Adjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                        !is.na(data_partner$totalActions_reduced) &
                                        !is.na(data_partner$identity)),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))$count[2, 4],
            zi_OR = exp(coef(mod_adj)[27]), zi_LCI = exp(confint.default(mod_adj)[27, 1]),
            zi_UCI = exp(confint.default(mod_adj)[27, 2]),
            zi_p = coef(summary(mod_adj))$zero[2, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred2 <- avg_predictions(mod_adj, variable = "identity"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions_reduced) & !is.na(identity)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity <- ggplot(data = data_partner2, aes(x = identity, y = fitted, fill = identity)) +
    geom_boxplot() +
    labs(x = "Identity", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious attendance

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions_reduced ~ attend | attend, dist = "poisson",
                      data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(e <- cbind(y = "totalActions_reduced", x = "Attendance", x_level = "Regular",
            mod = "Unadjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                          !is.na(data_partner$totalActions_reduced) &
                                          !is.na(data_partner$attend)),
            IRR = exp(coef(mod_unadj)[2]), LCI = exp(confint.default(mod_unadj)[2, 1]),
            UCI = exp(confint.default(mod_unadj)[2, 2]), p = coef(summary(mod_unadj))$count[2, 4],
            zi_OR = exp(coef(mod_unadj)[4]), zi_LCI = exp(confint.default(mod_unadj)[4, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[4, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions_reduced ~ attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home | attend + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, dist = "poisson",
                    data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(f <- cbind(y = "totalActions_reduced", x = "Attendance", x_level = "Regular",
            mod = "Adjusted", n = sum(data_partner$cca_confounds == TRUE & 
                                        !is.na(data_partner$totalActions_reduced) &
                                        !is.na(data_partner$attend)),
            IRR = exp(coef(mod_adj)[2]), LCI = exp(confint.default(mod_adj)[2, 1]),
            UCI = exp(confint.default(mod_adj)[2, 2]), p = coef(summary(mod_adj))$count[2, 4],
            zi_OR = exp(coef(mod_adj)[27]), zi_LCI = exp(confint.default(mod_adj)[27, 1]),
            zi_UCI = exp(confint.default(mod_adj)[27, 2]),
            zi_p = coef(summary(mod_adj))$zero[2, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred3 <- avg_predictions(mod_adj, variable = "attend"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions_reduced) & !is.na(attend)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_attend <- ggplot(data = data_partner2, aes(x = attend, y = fitted, fill = attend)) +
    geom_boxplot() +
    labs(x = "Attendance", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))


## Religious LCAs

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions_reduced ~ lca | lca, dist = "poisson",
                      data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(g <- cbind(y = rep("totalActions_reduced", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                  !is.na(data_partner$totalActions_reduced) &
                                                  !is.na(data_partner$lca)),
            IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
            UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))$count[2:4, 4],
            zi_OR = exp(coef(mod_unadj)[6:8]), zi_LCI = exp(confint.default(mod_unadj)[6:8, 1]),
            zi_UCI = exp(confint.default(mod_unadj)[6:8, 2]),
            zi_p = coef(summary(mod_unadj))$zero[2:4, 4]))



# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions_reduced ~ lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home | lca + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, dist = "poisson",
                    data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(h <- cbind(y = rep("totalActions_reduced", 3), x = rep("Latent class", 3), 
            x_level = c("Agnostic", "Moderately religious", "Highly religious"),
            mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                !is.na(data_partner$totalActions_reduced) &
                                                !is.na(data_partner$lca)),
            IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
            UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))$count[2:4, 4],
            zi_OR = exp(coef(mod_adj)[29:31]), zi_LCI = exp(confint.default(mod_adj)[29:31, 1]),
            zi_UCI = exp(confint.default(mod_adj)[29:31, 2]),
            zi_p = coef(summary(mod_adj))$zero[2:4, 4]))


## Make plot of predicted values from adjusted model, to help make the results more interpretable
(pred4 <- avg_predictions(mod_adj, variable = "lca"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions_reduced) & !is.na(lca)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_lca <- ggplot(data = data_partner2, aes(x = lca, y = fitted, fill = lca)) +
    geom_boxplot() +
    labs(x = "Latent classes", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))



## Combining all these results together
res <- as_tibble(rbind(a, b, c, d, e, f, g, h))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), zi_OR = as.numeric(zi_OR),
         zi_LCI = as.numeric(zi_LCI), zi_UCI = as.numeric(zi_UCI), zi_p = as.numeric(zi_p))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_reduced_p_zi.csv")


## Make a nice plot of these results
res <- res %>%
  mutate(exp = paste0(x, " - ", x_level)) %>%
  mutate(exp = ifelse(x == "Belief", paste0(exp, " (ref = No)"),
                      ifelse(x == "Identity", paste0(exp, " (ref = None)"),
                             ifelse(x == "Attendance", paste0(exp, " (ref = Never)"),
                                    paste0(exp, " (ref = Atheist)"))))) %>%
  mutate(exp = factor(exp, levels = c("Belief - Not sure (ref = No)", "Belief - Yes (ref = No)",
                                      "Identity - Christian (ref = None)",
                                      "Attendance - Regular (ref = Never)",
                                      "Latent class - Agnostic (ref = Atheist)", 
                                      "Latent class - Moderately religious (ref = Atheist)", 
                                      "Latent class - Highly religious (ref = Atheist)"))) %>%
  mutate(mod = factor(mod, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(y = recode(y, "totalActions_reduced" = "Total number of actions"))
res

(plot1 <- ggplot(res, aes(x = exp, y = IRR, ymin = LCI, ymax = UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Incidence rate ratio") +
    scale_y_continuous(trans = "log", breaks = c(0.9, 1, 1.1, 1.2)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))

(plot2 <- ggplot(res, aes(x = exp, y = zi_OR, ymin = zi_LCI, ymax = zi_UCI, col = mod, fill = mod)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_vline(xintercept = c(3.5, 4.5, 5.5), lty = 3, color = "grey40") +
    scale_fill_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_color_manual(values = c("black", "red"), guide = guide_legend(reverse = TRUE), name = "") +
    scale_x_discrete(limits = rev) +
    labs(x = "", y = "Odds ratio (zero-inflated)") +
    scale_y_continuous(trans = "log", breaks = c(0.33, 0.5, 0.7, 1, 1.5)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
          legend.text = element_text(size = 12)))


grid.arrange(plot1, plot2, ncol = 1)

# Save this plot
pdf("./Results_Partners/totalActions_reduced_p_zi.pdf", height = 8, width = 10)
grid.arrange(plot1, plot2, ncol = 1)
dev.off()


## Also make a combined plot of all the predicted probabilities
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)

pdf("./Results_Partners/totalActions_reduced_p_zi_preds.pdf", height = 10, width = 18)
grid.arrange(pred_belief, pred_identity, pred_attend, pred_lca, ncol = 2)
dev.off()

# And save all the predicted probabilities results too
names(pred1)[names(pred1) == "belief"] <- "exposure"
names(pred2)[names(pred2) == "identity"] <- "exposure"
names(pred3)[names(pred3) == "attend"] <- "exposure"
names(pred4)[names(pred4) == "lca"] <- "exposure"

(pred <- rbind(pred1, pred2, pred3, pred4))

# Save output
write_csv(pred, file = "./Results_Partners/totalActions_reduced_p_zi_preds.csv")


### And run and store results for religious identity split by denomination

## Religious affiliation (split by denomination)

# Unadjusted zero-inflated Poisson model
mod_unadj <- zeroinfl(totalActions_reduced ~ identity_denom | identity_denom, dist = "poisson",
                      data = data_partner, subset = cca_confounds == TRUE)
summary(mod_unadj)

# Extract results
(c_denom <- cbind(y = rep("totalActions_reduced", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Unadjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                        !is.na(data_partner$totalActions_reduced) &
                                                        !is.na(data_partner$identity_denom)),
                  IRR = exp(coef(mod_unadj)[2:4]), LCI = exp(confint.default(mod_unadj)[2:4, 1]),
                  UCI = exp(confint.default(mod_unadj)[2:4, 2]), p = coef(summary(mod_unadj))$count[2:4, 4],
                  zi_OR = exp(coef(mod_unadj)[6:8]), zi_LCI = exp(confint.default(mod_unadj)[6:8, 1]),
                  zi_UCI = exp(confint.default(mod_unadj)[6:8, 2]),
                  zi_p = coef(summary(mod_unadj))$zero[2:4, 4]))


# Adjusted zero-inflated Poisson model
mod_adj <- zeroinfl(totalActions_reduced ~ identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home | identity_denom + ageAtQ + marital + rural + edu +
                      occClass + income + imd + home, dist = "poisson",
                    data = data_partner, na.action = na.exclude)
summary(mod_adj)

# Extract results
(d_denom <- cbind(y = rep("totalActions_reduced", 3), x = rep("Identity (denom)", 3), 
                  x_level = c("C of E", "Catholic", "Other"),
                  mod = rep("Adjusted", 3), n = sum(data_partner$cca_confounds == TRUE & 
                                                      !is.na(data_partner$totalActions_reduced) &
                                                      !is.na(data_partner$identity_denom)),
                  IRR = exp(coef(mod_adj)[2:4]), LCI = exp(confint.default(mod_adj)[2:4, 1]),
                  UCI = exp(confint.default(mod_adj)[2:4, 2]), p = coef(summary(mod_adj))$count[2:4, 4],
                  zi_OR = exp(coef(mod_adj)[6:8]), zi_LCI = exp(confint.default(mod_adj)[6:8, 1]),
                  zi_UCI = exp(confint.default(mod_adj)[6:8, 2]),
                  zi_p = coef(summary(mod_adj))$zero[2:4, 4]))


## Combining results together
res <- as_tibble(rbind(c_denom, d_denom))
res <- res %>%
  mutate(n = as.numeric(n), IRR = as.numeric(IRR), LCI = as.numeric(LCI), 
         UCI = as.numeric(UCI), p = as.numeric(p), zi_OR = as.numeric(zi_OR),
         zi_LCI = as.numeric(zi_LCI), zi_UCI = as.numeric(zi_UCI), zi_p = as.numeric(zi_p))
res

# Save output
write_csv(res, file = "./Results_Partners/totalActions_reduced_zi_denom.csv")


## Make plot of predicted probabilities from adjusted model, to help make the results more interpretable
(pred2_denom <- avg_predictions(mod_adj, variable = "identity_denom"))

data_partner2 <- data_partner %>%
  filter(cca_confounds == TRUE & !is.na(totalActions_reduced) & !is.na(identity_denom)) %>%
  mutate(fitted = fitted.values(mod_adj))

(pred_identity_denom <- ggplot(data = na.omit(data_partner2), aes(x = identity_denom, y = fitted, fill = identity_denom)) +
    geom_boxplot() +
    labs(x = "Identity (denom)", y = "Predicted values of outcome") +
    scale_fill_discrete(guide = "none") +
    theme_bw() +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)))

# Save plot and results
pdf("./Results_Partners/totalActions_reduced_zi_denom.pdf", height = 5, width = 8)
pred_identity_denom
dev.off()

names(pred2_denom)[names(pred2_denom) == "identity_denom"] <- "exposure"
write_csv(pred2_denom, file = "./Results_Partners/totalActions_reduced_preds_zi_denom.csv")


