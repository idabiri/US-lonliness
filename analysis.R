# Title:
# Author: Iyanu Dabiri
# Research Question:

# How do differences in social connection and emotional support help explain
# variation in loneliness across gender, racial/ethnic, and regional groups
# in the U.S.?


# ------------ File Import ------------ 
install.packages("survey")
library(readxl)
library(dplyr)
library(survey) # for the weights used for Race
library(ggplot2)
atp <- read.csv("ATP W154.csv")


# ------------ Data Cleaning ------------

# Subset Dataset to Include Needed (Renamed) Variables for Analysis
atp_final <- atp |> 
  select(F_AGECAT, F_RACETHNMOD, F_GENDER, F_CREGION, WEIGHT_W154, FEELMOD_a_W154, 
         CLOSEFRI_W154, COMMFRI_c_W154, EMOSUPPFAM_a_W154, EMOSUPPFAM_b_W154, EMOSUPPFAM_c_W154, 
         EMOSUPPFAM_d_W154, EMOSUPPOTH_a_W154, EMOSUPPOTH_c_W154) |> 
  rename(FEEL_LONELY = FEELMOD_a_W154,
         CLOSE_FRIEND = CLOSEFRI_W154,
         COMMFRI_IRL = COMMFRI_c_W154,
         EMOSUPP_SPOUSE = EMOSUPPFAM_a_W154,
         EMOSUPP_MOM = EMOSUPPFAM_b_W154,
         EMOSUPP_DAD = EMOSUPPFAM_c_W154,
         EMOSUPP_OTHFAM = EMOSUPPFAM_d_W154,
         EMOSUPP_FRIEND = EMOSUPPOTH_a_W154,
         EMOSUPP_PROF = EMOSUPPOTH_c_W154) |> 
  glimpse()


# Recode 99s to NA
atp_final <- atp_final |> 
  mutate(across(c(F_AGECAT, F_RACETHNMOD, F_GENDER, F_CREGION,
                  FEEL_LONELY, CLOSE_FRIEND, COMMFRI_IRL, EMOSUPP_SPOUSE, EMOSUPP_MOM, 
                  EMOSUPP_DAD, EMOSUPP_OTHFAM, EMOSUPP_FRIEND, EMOSUPP_PROF), 
                ~ na_if(.x, 99)))

# Rename demographic names
# Create a index for mom, dad, and other family members. If they
# got support from any of these (answered 1-2),  +1, else 0. Make the variable 
# EMOSUPP_FAM,as general family support variable.

atp_final <- atp_final |> 
  mutate(
    EMOSUPP_FAM = (EMOSUPP_MOM %in% 1:2) +
      (EMOSUPP_DAD %in% 1:2) + (EMOSUPP_OTHFAM %in% 1:2),
    
    F_AGECAT = case_when(F_AGECAT == 1 ~ "18-29",
                         F_AGECAT == 2 ~ "30-49",
                         F_AGECAT == 3 ~ "50-64",
                         F_AGECAT == 4 ~ "65+"),
    
    F_RACETHNMOD = case_when(F_RACETHNMOD == 1 ~ "White",
                             F_RACETHNMOD == 2 ~ "Black",
                             F_RACETHNMOD == 3 ~ "Hispanic",
                             F_RACETHNMOD == 4 ~ "Other",
                             F_RACETHNMOD == 5 ~ "Asian"),
    
    F_GENDER = case_when(F_GENDER == 1 ~ "Man",
                         F_GENDER == 2 ~ "Woman",
                         F_GENDER == 3 ~ "Other"),
    
    F_CREGION = case_when(F_CREGION == 1 ~ "Northeast",
                          F_CREGION == 2 ~ "Midwest",
                          F_CREGION == 3 ~ "South",
                          F_CREGION == 4 ~ "West")
  )


# Import the weights that will be used throughout analysis
weight <- svydesign(ids = ~1, data = atp_final, weights = ~WEIGHT_W154)


# ------------ Descriptive Statistics ------------

# Check frequencies of variables
table(atp_final$F_AGECAT, useNA = "always")
table(atp_final$F_RACETHNMOD, useNA = "always")
table(atp_final$F_GENDER, useNA = "always")
table(atp_final$F_CREGION, useNA = "always")
table(atp_final$FEEL_LONELY, useNA = "always")
table(atp_final$CLOSE_FRIEND, useNA = "always") 
table(atp_final$COMMFRI_IRL, useNA = "always")
table(atp_final$EMOSUPP_SPOUSE, useNA = "always")
table(atp_final$EMOSUPP_MOM, useNA = "always") 
table(atp_final$EMOSUPP_DAD, useNA = "always")
table(atp_final$EMOSUPP_OTHFAM, useNA = "always")
table(atp_final$EMOSUPP_FRIEND, useNA = "always")
table(atp_final$EMOSUPP_PROF, useNA = "always")

# Check proportions with weights
round(svytable(~F_AGECAT, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~F_RACETHNMOD, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~F_GENDER, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~F_CREGION, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~FEEL_LONELY, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~CLOSE_FRIEND, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~COMMFRI_IRL, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~EMOSUPP_SPOUSE, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~EMOSUPP_MOM, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~EMOSUPP_DAD, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~EMOSUPP_OTHFAM, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~EMOSUPP_FRIEND, design = weight) |> prop.table() * 100, digits = 1)
round(svytable(~EMOSUPP_PROF, design = weight) |> prop.table() * 100, digits = 1)

# Check weighted loneliness by demographics
round(svytable(~FEEL_LONELY + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
  # 6.5% of 18-29, 4.8% of 30-49, 1.2% of 50-64, 0.7% 65+ always feel lonely
  # 6.9% of 18-29, 13.4% of 30-49, 23.7% of 50-64, 29.4% 65+ never feel lonely

round(svytable(~FEEL_LONELY + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
  # 2.3% of White, 6.6% of Black, 3.0% of Asian, 4.6% of Hispanic, 4.6% of Other always feel lonely
  # 19.0% of White, 20.5% of Black, 11.8% of Asian, 16.7% of Hispanic, 15.4% of Other never feel lonely

round(svytable(~FEEL_LONELY + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
  # 3.5% of men, 3.1% of women, 8.3% of other always feel lonely
  # 20.2% of men, 16.7% of women, 0% of other never feel lonely
round(svytable(~FEEL_LONELY + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
  # 3.1% of NE, 2.7% of MW, 3.9% of S, 3.3% of W always feel lonely
  # 18.4% of NE, 19.9% of MW, 18.8% of S, 15.8% of W never feel lonely

# In-person contact by demographics
round(svytable(~COMMFRI_IRL + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~COMMFRI_IRL + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~COMMFRI_IRL + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~COMMFRI_IRL + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Support from professional by demographics
round(svytable(~EMOSUPP_PROF + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_PROF + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_PROF + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_PROF + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Support from friend by demographics
round(svytable(~EMOSUPP_FRIEND + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_FRIEND + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_FRIEND + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_FRIEND + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Support from mom by demographics
round(svytable(~EMOSUPP_MOM + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_MOM + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_MOM + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_MOM + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Support from dad by demographics
round(svytable(~EMOSUPP_DAD + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_DAD + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_DAD + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_DAD + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Support from other family members by demographics
round(svytable(~EMOSUPP_OTHFAM + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_OTHFAM + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_OTHFAM + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_OTHFAM + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Support from spouse/partner by demographics
round(svytable(~EMOSUPP_SPOUSE + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_SPOUSE + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_SPOUSE + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_SPOUSE + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)




# ------------ Descriptive Visualizations ------------
