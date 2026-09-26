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
  select(F_AGECAT, F_RACECMB, F_GENDER, F_CREGION, WEIGHT_W154, FEELMOD_a_W154, 
         CLOSEFRI_W154, COMMFRI_c_W154, EMOSUPPFAM_b_W154, EMOSUPPFAM_c_W154, 
         EMOSUPPFAM_d_W154, EMOSUPPOTH_a_W154, EMOSUPPOTH_c_W154) |> 
  rename(FEEL_LONELY = FEELMOD_a_W154,
         CLOSE_FRIEND = CLOSEFRI_W154,
         COMMFRI_IRL = COMMFRI_c_W154,
         EMOSUPP_MOM = EMOSUPPFAM_b_W154,
         EMOSUPP_DAD = EMOSUPPFAM_c_W154,
         EMOSUPP_OTHFAM = EMOSUPPFAM_d_W154,
         EMOSUPP_FRIEND = EMOSUPPOTH_a_W154,
         EMOSUPP_PROF = EMOSUPPOTH_c_W154) |> 
  glimpse()

# Import the weights that will be used throughout analysis
weight <- svydesign(ids = ~1, data = atp_final, weights = ~WEIGHT_W154)

# Recode 99s to NA
atp_final <- atp_final |> 
  mutate(across(c(F_AGECAT, F_RACECMB, F_GENDER, F_CREGION,
                  FEEL_LONELY, CLOSE_FRIEND, COMMFRI_IRL, EMOSUPP_MOM, 
                  EMOSUPP_DAD, EMOSUPP_OTHFAM, EMOSUPP_FRIEND, EMOSUPP_PROF), 
                ~ na_if(.x, 99)))

# ------------ Descriptive Statistics ------------

# Check frequencies of variables
table(atp_final$F_AGECAT, useNA = "always")
table(atp_final$F_RACECMB, useNA = "always")
table(atp_final$F_GENDER, useNA = "always")
table(atp_final$F_CREGION, useNA = "always")
table(atp_final$FEEL_LONELY, useNA = "always")
table(atp_final$CLOSE_FRIEND, useNA = "always") 
table(atp_final$COMMFRI_IRL, useNA = "always")
table(atp_final$EMOSUPP_MOM, useNA = "always") 
table(atp_final$EMOSUPP_DAD, useNA = "always")
table(atp_final$EMOSUPP_OTHFAM, useNA = "always")
table(atp_final$EMOSUPP_FRIEND, useNA = "always")
table(atp_final$EMOSUPP_PROF, useNA = "always") 

# Check proportions with weights
svytable(~F_AGECAT, design = weight) |> prop.table() * 100
svytable(~F_RACECMB, design = weight) |> prop.table() * 100
svytable(~F_GENDER, design = weight) |> prop.table() * 100
svytable(~F_CREGION, design = weight) |> prop.table() * 100
svytable(~FEEL_LONELY, design = weight) |> prop.table() * 100
svytable(~CLOSE_FRIEND, design = weight) |> prop.table() * 100
svytable(~COMMFRI_IRL, design = weight) |> prop.table() * 100
svytable(~EMOSUPP_MOM, design = weight) |> prop.table() * 100
svytable(~EMOSUPP_DAD, design = weight) |> prop.table() * 100
svytable(~EMOSUPP_OTHFAM, design = weight) |> prop.table() * 100
svytable(~EMOSUPP_FRIEND, design = weight) |> prop.table() * 100
svytable(~EMOSUPP_PROF, design = weight) |> prop.table() * 100

# Check weighted loneliness by demographics
svytable(~FEEL_LONELY + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100
  # 6.5% of 18-29, 4.8% of 30-49, 1.2% of 50-64, 0.7% 65+ always feel lonely
  # 6.9% of 18-29, 13.4% of 30-49, 23.7% of 50-64, 29.4% 65+ never feel lonely

svytable(~FEEL_LONELY + F_RACECMB, design = weight) |> prop.table(margin = 2) * 100
  # 2.4% of White, 6.6% of Black, 3.0% of Asian, 6.1% of Mixed, 6.3% of Other always feel lonely
  # 18.6% of White, 20.0% of Black, 11.6% of Asian, 14.9% of Mixed, 20.6% of Other never feel lonely

svytable(~FEEL_LONELY + F_GENDER, design = weight) |> prop.table(margin = 2) * 100
  # 3.5% of men, 3.1% of women, 8.3% of other always feel lonely
  # 20.2% of men, 16.7% of women, 0% of other never feel lonely
svytable(~FEEL_LONELY + F_CREGION, design = weight) |> prop.table(margin = 2) * 100
  # 3.1% of NE, 2.7% of MW, 3.9% of S, 3.3% of W always feel lonely
  # 18.4% of NE, 19.9% of MW, 18.8% of S, 15.8% of W never feel lonely

# In-person contact by demographics
svytable(~COMMFRI_IRL + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100
svytable(~COMMFRI_IRL + F_RACECMB, design = weight) |> prop.table(margin = 2) * 100
svytable(~COMMFRI_IRL + F_GENDER, design = weight) |> prop.table(margin = 2) * 100
svytable(~COMMFRI_IRL + F_CREGION, design = weight) |> prop.table(margin = 2) * 100

# Support from professional by demographics
svytable(~EMOSUPP_PROF + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_PROF + F_RACECMB, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_PROF + F_GENDER, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_PROF + F_CREGION, design = weight) |> prop.table(margin = 2) * 100

# Support from friend by demographics
svytable(~EMOSUPP_FRIEND + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_FRIEND + F_RACECMB, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_FRIEND + F_CREGION, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_FRIEND + F_GENDER, design = weight) |> prop.table(margin = 2) * 100

# Support from mom by demographics
svytable(~EMOSUPP_MOM + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_MOM + F_RACECMB, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_MOM + F_CREGION, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_MOM + F_GENDER, design = weight) |> prop.table(margin = 2) * 100

# Support from dad by demographics
svytable(~EMOSUPP_DAD + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_DAD + F_RACECMB, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_DAD + F_CREGION, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_DAD + F_GENDER, design = weight) |> prop.table(margin = 2) * 100

# Support from other family members by demographics
svytable(~EMOSUPP_OTHFAM + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_OTHFAM + F_RACECMB, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_OTHFAM + F_CREGION, design = weight) |> prop.table(margin = 2) * 100
svytable(~EMOSUPP_OTHFAM + F_GENDER, design = weight) |> prop.table(margin = 2) * 100

#Questions - can we combine emosupp_fam?
