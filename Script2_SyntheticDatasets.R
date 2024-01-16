### Script for paper 'Associations between religiosity and climate change beliefs and behaviours in the Avon Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4123
### Script 2: Generating synthetic datasets
### Created 6/11/2023 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://osf.io/p5vjz/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4123 - RSBB and Climate Change")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("synthpop")
library(synthpop)

#install.packages("haven")
library(haven)



##########################################################################################
#### Read in the mother's processed data, reduce down to complete-case observations, and generate synthetic datasets using 'synthpop' (https://www.synthpop.org.uk/get-started.html)

load("data_mum_processed_B4123.RData")

# Check data
head(data_mum)
summary(data_mum)
glimpse(data_mum)

# Drop the ALN identifier variable
data_mum <- data_mum %>%
  select(-aln)

# Also drop the binary 'religious identity' variable, to add back in later
data_mum <- data_mum %>%
  select(-identity)

# Reduce down to those with complete confounder data, any exposure data, and any outcome data
data_mum <- data_mum %>%
  mutate(cca_confounds = complete.cases(ageAtBirth, ageAtQ, ethnicity, marital, rural, edu, occClass,
                                        income, imd, home)) %>%
  mutate(cca_exposures = ifelse((!is.na(belief) | !is.na(identity_denom) | !is.na(attend) | !is.na(lca)), 1, 0)) %>%
  mutate(cca_outcomes = ifelse((!is.na(climateChanging) | !is.na(climateConcern) | !is.na(climateHumans)
                                | !is.na(climateAction) | !is.na(travel) | !is.na(waste) | !is.na(energy) 
                                | !is.na(buy) | !is.na(airTravel)  | !is.na(elecCar) | !is.na(localFood) 
                                | !is.na(recycle) | !is.na(plastic) | !is.na(sustainable) | !is.na(insulation)
                                | !is.na(solar) | !is.na(veg) | !is.na(trees) | !is.na(avoidFossil) 
                                | !is.na(children) | !is.na(otherAction) | !is.na(meatDairy)), 1, 0)) %>%
  filter(cca_confounds == TRUE & cca_exposures == 1 & cca_outcomes == 1) %>%
  select(-c(cca_confounds, cca_exposures, cca_outcomes))


# Get information about variables in the dataset
codebook.syn(data_mum)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_mum_syn <- syn(data_mum, seed = 182)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 0 observations have been dropped (0.0% of data)
replicated.uniques(data_mum_syn, data_mum)
data_mum_syn <- sdc(data_mum_syn, data_mum, rm.replicated.uniques = TRUE)


## Perform a manual check on a handful of cases to check this

# Create a dataset of unique observed individuals
dat_unique <- data_mum[!(duplicated(data_mum) | duplicated(data_mum, fromLast = TRUE)), ]

# Create a dataset of unique synthetic individuals
syn_unique <- data_mum_syn$syn[!(duplicated(data_mum_syn$syn) | duplicated(data_mum_syn$syn, 
                                                                           fromLast = TRUE)), ] 

# Select 10 rows at random from the unique observed dataset
row_unique <- dat_unique[sample(nrow(dat_unique), 10), ] 

# Check there are no duplicated observations (this should equal ‘0’)
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

rm(dat_unique)
rm(syn_unique)


### Explore this synthetic dataset
data_mum_syn
summary(data_mum_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is fairly good). Save this as a PDF
compare(data_mum_syn, data_mum, stat = "counts", nrow = 6, ncol = 6)

pdf("./Results_SynthPop/ComparingDescStats_mothers.pdf", height = 15, width = 15)
compare(data_mum_syn, data_mum, stat = "counts", nrow = 6, ncol = 6)
dev.off()


## Univariable analysis with reducing meat/dairy consumption as outcome and religious belief as exposure to show that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.syn <- multinom.synds(meatDairy ~ belief, data = data_mum_syn)
summary(model.syn)

# Get comparable pattern of results (and store as PDF)
compare(model.syn, data_mum)

pdf("./Results_SynthPop/ComparingUnadjustedModel_mothers.pdf", height = 8, width = 12)
compare(model.syn, data_mum)
dev.off()


## Next compare results of multivariable analyses
model.syn2 <- multinom.synds(meatDairy ~ belief + ageAtQ + ethnicity + marital + rural + edu + 
                               occClass + income + imd + home, data = data_mum_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_mum)

pdf("./Results_SynthPop/ComparingAdjustedModel_mothers.pdf", height = 12, width = 16)
compare(model.syn2, data_mum)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
data_mum_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(data_mum_syn$syn)), data_mum_syn$syn)
summary(data_mum_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
data_mum_syn_df <- data_mum_syn$syn
head(data_mum_syn_df)
glimpse(data_mum_syn_df)
summary(data_mum_syn_df)


## Add in  a 'total number of climate actions' variable
data_mum_syn_df <- data_mum_syn_df %>%
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

table(data_mum_syn_df$totalActions)
summary(data_mum_syn_df$totalActions)
ggplot(data_mum_syn_df, aes(x = totalActions)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

table(data_mum_syn_df$totalActions_reduced)
summary(data_mum_syn_df$totalActions_reduced)
ggplot(data_mum_syn_df, aes(x = totalActions_reduced)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Also add back in a binary 'religious identity' variable
data_mum_syn_df <- data_mum_syn_df %>%
  mutate(identity = recode(identity_denom, "CofE" = "Christian", "Catholic" = "Christian", 
                           "Other" = "Christian")) %>%
  mutate(identity = factor(identity, levels = c("None", "Christian"))) %>%
  relocate(FALSE_DATA:belief, identity)

table(data_mum_syn_df$identity, useNA = "ifany")
table(data_mum_syn_df$identity_denom, useNA = "ifany")


### Store the synthetic dataset for others to use
save(data_mum_syn_df, file = "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_mum_B4123.RData")
write_csv(data_mum_syn_df, file = "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_mum_B4123.csv")
write_dta(data_mum_syn_df, "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_mum_B4123.dta")



##########################################################################################
#### Read in the partner's processed data and repeat

rm(list = ls())

load("data_partner_processed_B4123.RData")

# Check data
head(data_partner)
summary(data_partner)
glimpse(data_partner)

# Drop the ALN identifier variable
data_partner <- data_partner %>%
  select(-aln)

# Also drop the binary 'religious identity' variable, to add back in later
data_partner <- data_partner %>%
  select(-identity)

# Reduce down to those with complete confounder data, any exposure data, and any outcome data
data_partner <- data_partner %>%
  mutate(cca_confounds = complete.cases(ageAtBirth, ageAtQ, ethnicity, marital, rural, edu, occClass,
                                        income, imd, home)) %>%
  mutate(cca_exposures = ifelse((!is.na(belief) | !is.na(identity_denom) | !is.na(attend) | !is.na(lca)), 1, 0)) %>%
  mutate(cca_outcomes = ifelse((!is.na(climateChanging) | !is.na(climateConcern) | !is.na(climateHumans)
                                | !is.na(climateAction) | !is.na(travel) | !is.na(waste) | !is.na(energy) 
                                | !is.na(buy) | !is.na(airTravel)  | !is.na(elecCar) | !is.na(localFood) 
                                | !is.na(recycle) | !is.na(plastic) | !is.na(sustainable) | !is.na(insulation)
                                | !is.na(solar) | !is.na(veg) | !is.na(trees) | !is.na(avoidFossil) 
                                | !is.na(children) | !is.na(otherAction) | !is.na(meatDairy)), 1, 0)) %>%
  filter(cca_confounds == TRUE & cca_exposures == 1 & cca_outcomes == 1) %>%
  select(-c(cca_confounds, cca_exposures, cca_outcomes))


# Get information about variables in the dataset
codebook.syn(data_partner)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_partner_syn <- syn(data_partner, seed = 41)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 0 observations have been dropped (0.0% of data)
replicated.uniques(data_partner_syn, data_partner)
data_partner_syn <- sdc(data_partner_syn, data_partner, rm.replicated.uniques = TRUE)


## Perform a manual check on a handful of cases to check this

# Create a dataset of unique observed individuals
dat_unique <- data_partner[!(duplicated(data_partner) | duplicated(data_partner, fromLast = TRUE)), ]

# Create a dataset of unique synthetic individuals
syn_unique <- data_partner_syn$syn[!(duplicated(data_partner_syn$syn) | duplicated(data_partner_syn$syn, 
                                                                           fromLast = TRUE)), ] 

# Select 10 rows at random from the unique observed dataset
row_unique <- dat_unique[sample(nrow(dat_unique), 10), ] 

# Check there are no duplicated observations (this should equal ‘0’)
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

rm(dat_unique)
rm(syn_unique)


### Explore this synthetic dataset
data_partner_syn
summary(data_partner_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is fairly good). Save this as a PDF
compare(data_partner_syn, data_partner, stat = "counts", nrow = 6, ncol = 6)

pdf("./Results_SynthPop/ComparingDescStats_partners.pdf", height = 15, width = 15)
compare(data_partner_syn, data_partner, stat = "counts", nrow = 6, ncol = 6)
dev.off()


## Univariable analysis with reducing meat/dairy consumption as outcome and religious belief as exposure to show that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.syn <- multinom.synds(meatDairy ~ belief, data = data_partner_syn)
summary(model.syn)

# Get comparable pattern of results (and store as PDF)
compare(model.syn, data_partner)

pdf("./Results_SynthPop/ComparingUnadjustedModel_partners.pdf", height = 8, width = 12)
compare(model.syn, data_partner)
dev.off()


## Next compare results of multivariable analyses (excluded occupational social class and home ownership as no observations)
model.syn2 <- multinom.synds(meatDairy ~ belief + ageAtQ + ethnicity + marital + rural + edu + 
                               income + imd, data = data_partner_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_partner)

pdf("./Results_SynthPop/ComparingAdjustedModel_partners.pdf", height = 12, width = 16)
compare(model.syn2, data_partner)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
data_partner_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(data_partner_syn$syn)), data_partner_syn$syn)
summary(data_partner_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
data_partner_syn_df <- data_partner_syn$syn
head(data_partner_syn_df)
glimpse(data_partner_syn_df)
summary(data_partner_syn_df)


## Add in  a 'total number of climate actions' variable (both for all variables, and excluding some likely to be more economically and socially-patterned)
data_partner_syn_df <- data_partner_syn_df %>%
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

table(data_partner_syn_df$totalActions)
summary(data_partner_syn_df$totalActions)
ggplot(data_partner_syn_df, aes(x = totalActions)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

table(data_partner_syn_df$totalActions_reduced)
summary(data_partner_syn_df$totalActions_reduced)
ggplot(data_partner_syn_df, aes(x = totalActions_reduced)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Also add back in a binary 'religious identity' variable
data_partner_syn_df <- data_partner_syn_df %>%
  mutate(identity = recode(identity_denom, "CofE" = "Christian", "Catholic" = "Christian", 
                           "Other" = "Christian")) %>%
  mutate(identity = factor(identity, levels = c("None", "Christian"))) %>%
  relocate(FALSE_DATA:belief, identity)

table(data_partner_syn_df$identity, useNA = "ifany")
table(data_partner_syn_df$identity_denom, useNA = "ifany")


### Store the synthetic dataset for others to use
save(data_partner_syn_df, file = 
       "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_partner_B4123.RData")
write_csv(data_partner_syn_df, file = 
            "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_partner_B4123.csv")
write_dta(data_partner_syn_df, 
          "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_partner_B4123.dta")



##########################################################################################
#### Read in the offspring's processed data and repeat

rm(list = ls())

load("data_offspring_processed_B4123.RData")

# Check data
head(data_offspring)
summary(data_offspring)
glimpse(data_offspring)

# Drop the ALN and QLET identifier variables
data_offspring <- data_offspring %>%
  select(-aln, -qlet)

# Also drop the binary 'religious identity' variable, to add back in later
data_offspring <- data_offspring %>%
  select(-identity)

# Reduce down to those with complete confounder data, any exposure data, and any outcome data
data_offspring <- data_offspring %>%
  mutate(cca_confounds = complete.cases(ageAtQ, sex, ethnicity, relationship, rural, edu, occClass,
                                        income, imd, home, politics_PCA1, politics_PCA2)) %>%
  mutate(cca_exposures = ifelse((!is.na(belief) | !is.na(identity_denom) | !is.na(attend) | !is.na(lca)), 1, 0)) %>%
  mutate(cca_outcomes = ifelse((!is.na(climateChanging) | !is.na(climateConcern) | !is.na(climateHumans)
                                | !is.na(climateAction) | !is.na(travel) | !is.na(waste) | !is.na(energy) 
                                | !is.na(buy) | !is.na(airTravel)  | !is.na(elecCar) | !is.na(localFood) 
                                | !is.na(recycle) | !is.na(plastic) | !is.na(sustainable) | !is.na(insulation)
                                | !is.na(solar) | !is.na(veg) | !is.na(trees) | !is.na(avoidFossil) 
                                | !is.na(children) | !is.na(otherAction) | !is.na(meatDairy)), 1, 0)) %>%
  filter(cca_confounds == TRUE & cca_exposures == 1 & cca_outcomes == 1) %>%
  select(-c(cca_confounds, cca_exposures, cca_outcomes))


# Get information about variables in the dataset
codebook.syn(data_offspring)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_offspring_syn <- syn(data_offspring, seed = 75)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 0 observations have been dropped (0.0% of data)
replicated.uniques(data_offspring_syn, data_offspring)
data_offspring_syn <- sdc(data_offspring_syn, data_offspring, rm.replicated.uniques = TRUE)


## Perform a manual check on a handful of cases to check this

# Create a dataset of unique observed individuals
dat_unique <- data_offspring[!(duplicated(data_offspring) | duplicated(data_offspring, fromLast = TRUE)), ]

# Create a dataset of unique synthetic individuals
syn_unique <- data_offspring_syn$syn[!(duplicated(data_offspring_syn$syn) | duplicated(data_offspring_syn$syn, 
                                                                                   fromLast = TRUE)), ] 

# Select 10 rows at random from the unique observed dataset
row_unique <- dat_unique[sample(nrow(dat_unique), 10), ] 

# Check there are no duplicated observations (this should equal ‘0’)
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

rm(dat_unique)
rm(syn_unique)


### Explore this synthetic dataset
data_offspring_syn
summary(data_offspring_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is fairly good). Save this as a PDF
compare(data_offspring_syn, data_offspring, stat = "counts", nrow = 7, ncol = 6)

pdf("./Results_SynthPop/ComparingDescStats_offspring.pdf", height = 15, width = 15)
compare(data_offspring_syn, data_offspring, stat = "counts", nrow = 7, ncol = 6)
dev.off()


## Univariable analysis with reducing meat/dairy consumption as outcome and religious belief as exposure to show that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.syn <- multinom.synds(meatDairy ~ belief, data = data_offspring_syn)
summary(model.syn)

# Get comparable pattern of results (and store as PDF)
compare(model.syn, data_offspring)

pdf("./Results_SynthPop/ComparingUnadjustedModel_offspring.pdf", height = 8, width = 12)
compare(model.syn, data_offspring)
dev.off()


## Next compare results of multivariable analyses
model.syn2 <- multinom.synds(meatDairy ~ belief + ageAtQ + ethnicity + relationship + rural + edu + 
                              occClass + income + imd + home, data = data_offspring_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_offspring)

pdf("./Results_SynthPop/ComparingAdjustedModel_offspring.pdf", height = 12, width = 16)
compare(model.syn2, data_offspring)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
data_offspring_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(data_offspring_syn$syn)), data_offspring_syn$syn)
summary(data_offspring_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
data_offspring_syn_df <- data_offspring_syn$syn
head(data_offspring_syn_df)
glimpse(data_offspring_syn_df)
summary(data_offspring_syn_df)


## Add in  a 'total number of climate actions' variable (both for all variables, and excluding some likely to be more economically and socially-patterned)
data_offspring_syn_df <- data_offspring_syn_df %>%
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
  mutate(children_bin = ifelse(is.na(children), NA,
                             ifelse(children == "Climate" | children == "ClimateOther", 1, 0))) %>%
  #mutate(otherAction_bin = ifelse(is.na(otherAction), NA,
  #                           ifelse(otherAction == "Climate" | otherAction == "ClimateOther", 1, 0))) %>%
  mutate(meatDairy_bin = ifelse(is.na(meatDairy), NA,
                                ifelse(meatDairy == "Climate" | meatDairy == "ClimateOther", 1, 0))) %>%
  rowwise() %>%
  mutate(totalActions = sum(c_across(travel_bin:meatDairy_bin))) %>%
  mutate(totalActions_reduced = sum(travel_bin, waste_bin, energy_bin, buy_bin, localFood_bin, 
                                    recycle_bin, plastic_bin, sustainable_bin, trees_bin,
                                    avoidFossil_bin, children_bin, meatDairy_bin)) %>%
  ungroup() %>%
  select(-c(travel_bin:meatDairy_bin))

table(data_offspring_syn_df$totalActions)
summary(data_offspring_syn_df$totalActions)
ggplot(data_offspring_syn_df, aes(x = totalActions)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()

table(data_offspring_syn_df$totalActions_reduced)
summary(data_offspring_syn_df$totalActions_reduced)
ggplot(data_offspring_syn_df, aes(x = totalActions_reduced)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  theme_bw()


## Also add back in a binary 'religious identity' variable
data_offspring_syn_df <- data_offspring_syn_df %>%
  mutate(identity = recode(identity_denom, "CofE" = "Christian", "Catholic" = "Christian", 
                           "Other" = "Christian")) %>%
  mutate(identity = factor(identity, levels = c("None", "Christian"))) %>%
  relocate(FALSE_DATA:belief, identity)

table(data_offspring_syn_df$identity, useNA = "ifany")
table(data_offspring_syn_df$identity_denom, useNA = "ifany")


### Store the synthetic dataset for others to use
save(data_offspring_syn_df, file = 
       "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_offspring_B4123.RData")
write_csv(data_offspring_syn_df, file = 
            "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_offspring_B4123.csv")
write_dta(data_offspring_syn_df, 
          "./AnalysisCode_RSBBClimate_B4123/SyntheticData/syntheticData_offspring_B4123.dta")





