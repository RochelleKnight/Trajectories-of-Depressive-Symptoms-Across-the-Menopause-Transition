rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")
library(R2MLwiN)
library(dplyr)
library(rms)
library(splines)
library(ggplot2)
library(lme4)
library(gridExtra)

options(MLwiN_path = "C:/Program Files/MLwiN v3.13/mlwin.exe")

# Read in df
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
length(which(!is.na(df$epds_prorated)))

df$age_sq <- df$age^2
df$centered_age_sq <- df$centered_age^2

model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age
                        + centered_age + centered_age_sq 
                        + ethnicity + social_class + education + age_menarche
                        + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                        + (1 + centered_age | aln) + (1 + centered_age | time_point),
                        D = "Normal",
                        data = df,
                        estoptions = list(EstM = 0)
)

model_repro_no_age <- runMLwiN(epds_prorated ~ 1 + repro_age
                        + ethnicity + social_class + education + age_menarche
                        + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                        + (1 + repro_age | aln) + (1 + repro_age | time_point),
                        D = "Normal",
                        data = df,
                        estoptions = list(EstM = 0)
)

model_age <- runMLwiN(epds_prorated ~ 1 + age + age_sq
                      + ethnicity + social_class + education + age_menarche
                      + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                      + (1 + age| aln) + (1 + age | time_point),
                      D = "Normal",
                      data = df)

# Predict values for paper text
repro_seq <- seq(-20,20,5)

pred_model_repro <- data.frame(
  repro_age = seq(-20,20,5),
  centered_age = 0,
  centered_age_sq = 0,
  
  # Convert factor variables to factors with the same levels as in df
  ethnicity = factor("White", levels = levels(df$ethnicity)),
  social_class = factor("4", levels = levels(df$social_class)),
  education = factor("A-level_or_greater", levels = levels(df$education)),
  age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
  smoking_status = factor("Never", levels = levels(df$smoking_status)), 
  alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
  
  # Use mean values for continuous variables
  material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
  social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
  bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
)

# Predict values
predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
predicted_model_repro_no_age <- predict(model_repro_no_age, newdata = pred_model_repro, type = "link",se.fit = T)

# Prediction df for plotting
pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                  predicted = predicted_model_repro$fit,
                                  CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                  CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)

pred_df_model_repro_no_age <- data.frame(repro_age = pred_model_repro$repro_age,
                                  predicted = predicted_model_repro_no_age$fit,
                                  CI_low = predicted_model_repro_no_age$fit - 1.96 * predicted_model_repro_no_age$se.fit,
                                  CI_high = predicted_model_repro_no_age$fit + 1.96 * predicted_model_repro_no_age$se.fit)


age_seq <- seq(30,70,5)

pred_model_age <- data.frame(
  age = seq(30,70,5),
  repro_age = 0,
  
  # Convert factor variables to factors with the same levels as in df
  ethnicity = factor("White", levels = levels(df$ethnicity)),
  social_class = factor("4", levels = levels(df$social_class)),
  education = factor("A-level_or_greater", levels = levels(df$education)),
  age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
  smoking_status = factor("Never", levels = levels(df$smoking_status)), 
  alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
  
  # Use mean values for continuous variables
  material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
  social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
  bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
)

pred_model_age$age_sq <- pred_model_age$age^2

# Predict values
predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model_age <- predicted_model_age$fit - 1.96 * predicted_model_age$se.fit
CI_high_model_age <- predicted_model_age$fit + 1.96 * predicted_model_age$se.fit

# Combine results into a data frame
pred_df_model_age <- data.frame(age = pred_model_age$age,
                                  predicted = predicted_model_age$fit,
                                  CI_low = CI_low_model_age,
                                  CI_high = CI_high_model_age)

# Format results
pred_df_model_repro$predicted <- round(pred_df_model_repro$predicted, 2)
pred_df_model_repro$CI_low <- round(pred_df_model_repro$CI_low, 2)
pred_df_model_repro$CI_high <- round(pred_df_model_repro$CI_high, 2)
pred_df_model_repro$estimate_repro <- paste0(pred_df_model_repro$predicted, " (",pred_df_model_repro$CI_low,", ",pred_df_model_repro$CI_high,")")
pred_df_model_repro <- pred_df_model_repro %>% select(repro_age,estimate_repro)

pred_df_model_repro_no_age$predicted <- round(pred_df_model_repro_no_age$predicted, 2)
pred_df_model_repro_no_age$CI_low <- round(pred_df_model_repro_no_age$CI_low, 2)
pred_df_model_repro_no_age$CI_high <- round(pred_df_model_repro_no_age$CI_high, 2)
pred_df_model_repro_no_age$estimate_repro_no_age <- paste0(pred_df_model_repro_no_age$predicted, " (",pred_df_model_repro_no_age$CI_low,", ",pred_df_model_repro_no_age$CI_high,")")
pred_df_model_repro_no_age <- pred_df_model_repro_no_age %>% select(repro_age,estimate_repro_no_age)

pred_df_model_repro <- pred_df_model_repro %>% left_join(pred_df_model_repro_no_age)


pred_df_model_age$predicted <- round(pred_df_model_age$predicted, 2)
pred_df_model_age$CI_low <- round(pred_df_model_age$CI_low, 2)
pred_df_model_age$CI_high <- round(pred_df_model_age$CI_high, 2)
pred_df_model_age$estimate_age <- paste0(pred_df_model_age$predicted, " (",pred_df_model_age$CI_low,", ",pred_df_model_age$CI_high,")")
pred_df_model_age <- pred_df_model_age %>% select(age, estimate_age)

write.csv(pred_df_model_repro, "tables/repro_age_predictions.csv",row.names = F)
#write.csv(pred_df_model_repro_no_age, "tables/repro_age_predictions_chrono_unadjusted.csv",row.names = F)
write.csv(pred_df_model_age, "tables/chrono_age_predictions.csv",row.names = F)
