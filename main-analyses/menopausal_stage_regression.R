rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Timing-of-menopause-and-risk-of-depression/age menopause analysis/observational-analysis")

library(dplyr)
library(tidyr)
library(data.table)
library(nlme)
library(broom.mixed)
library(R2MLwiN)

options(MLwiN_path = "C:/Program Files/MLwiN v3.13/mlwin.exe")

df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))

df <- df %>% 
  select(aln,time_point,date,age,epds_prorated,epds_binary,epds_binary_with_meds,epds_binary_with_meds_broad,ethnicity,age_menarche,
         social_class,education,material_hardship,social_support,smoking_status,bmi,alcohol_intake,history_depression_d,stage)

# Date variables
df$date <- as.Date(df$date)

# Numeric variables
df$epds_prorated <- as.numeric(df$epds_prorated)

df$material_hardship <- as.numeric(df$material_hardship)
df$social_support <- as.numeric(df$social_support)
df$bmi <- as.numeric(df$bmi)

# Factor variables#

df$epds_binary_with_meds <- factor(df$epds_binary_with_meds,
                   levels = c(0,1))

df$epds_binary <- factor(df$epds_binary,
                                   levels = c(0,1))

df$stage <- factor(df$stage,
                        levels = c("reproductive","menopause_transition","post_menopause"))

df$ethnicity <- factor(df$ethnicity,
                            levels = c("White","Non_white"))

df$social_class <- factor(df$social_class,
                               levels = c("1","2","3","4","5","6"))

df$education <- factor(df$education,
                            levels = c("CSE_vocational_or_less","O_level","A-level_or_greater"))

df$age_menarche <- factor(df$age_menarche,
                               levels = c("8_11_yrs","12_14_yrs","15_older_yrs"))

df$smoking_status <- factor(df$smoking_status,
                                 levels = c("Never","Ever","Current"))

df$alcohol_intake <- factor(df$alcohol_intake,
                                 levels = c("Never or ≤4 times/month","2–3 times/week","≥ 4 times/week"))

df$history_depression_d <- factor(df$history_depression_d,
                                levels = c(0,1))


results <- as.data.frame(matrix(ncol = 5,nrow = 0))
colnames(results) <- c("model", "effect","ci","p","n")



model_unadj <- runMLwiN(logit(epds_binary) ~ 1 + stage
                        + (1 | aln),
                        D = "Binomial",
                        data = df[complete.cases(df),])

model_unadj
exp(model_unadj@FP)

coefs <- model_unadj@FP[2]
se <- sqrt(diag(model_unadj@FP.cov))[2]
z_values <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_values)))

ci_lower <- round(exp(as.numeric(coefs - 1.96 * se)),2)
ci_upper <- round(exp(as.numeric(coefs + 1.96 * se)),2)
ci <- paste0(ci_lower,", ",ci_upper)


results <- rbind(results, data.frame(
  model = "unadjusted - perimenopause",
  effect = as.numeric(round(exp(coefs),2)),
  ci = ci,
  p = as.numeric(round(p_values,3)),
  n = model_unadj@Hierarchy[5]
))

coefs <- model_unadj@FP[3]
se <- sqrt(diag(model_unadj@FP.cov))[3]
z_values <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_values)))

ci_lower <- round(exp(as.numeric(coefs - 1.96 * se)),2)
ci_upper <- round(exp(as.numeric(coefs + 1.96 * se)),2)
ci <- paste0(ci_lower,", ",ci_upper)


results <- rbind(results, data.frame(
  model = "unadjusted - postmenopause",
  effect = as.numeric(round(exp(coefs),2)),
  ci = ci,
  p = as.numeric(round(p_values,3)),
  n = model_unadj@Hierarchy[5]
))

df$age_sq <- df$age^2
model <- runMLwiN(logit(epds_binary) ~ 1 + stage + age
                  + ethnicity + social_class + education + age_menarche
                  + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                  + (1 | aln),
                  D = "Binomial",
                  data = df)

model
exp(model@FP)

coefs <- model@FP[2]
se <- sqrt(diag(model@FP.cov))[2]
z_values <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_values)))

ci_lower <- round(exp(as.numeric(coefs - 1.96 * se)),2)
ci_upper <- round(exp(as.numeric(coefs + 1.96 * se)),2)
ci <- paste0(ci_lower,", ",ci_upper)


results <- rbind(results, data.frame(
  model = "adjusted - perimenopause",
  effect = as.numeric(round(exp(coefs),2)),
  ci = ci,
  p = as.numeric(round(p_values,3)),
  n = model@Hierarchy[5]
))

coefs <- model@FP[3]
se <- sqrt(diag(model@FP.cov))[3]
z_values <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_values)))

ci_lower <- round(exp(as.numeric(coefs - 1.96 * se)),2)
ci_upper <- round(exp(as.numeric(coefs + 1.96 * se)),2)
ci <- paste0(ci_lower,", ",ci_upper)


results <- rbind(results, data.frame(
  model = "adjusted - postmenopause",
  effect = as.numeric(round(exp(coefs),2)),
  ci = ci,
  p = as.numeric(round(p_values,3)),
  n = model@Hierarchy[5]
))

# Add in history of depression confounder
model <- runMLwiN(logit(epds_binary) ~ 1 + stage + age
                  + ethnicity + social_class + education + age_menarche
                  + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                  + history_depression_d
                  + (1 | aln),
                  D = "Binomial",
                  data = df)

model
exp(model@FP)

coefs <- model@FP[2]
se <- sqrt(diag(model@FP.cov))[2]
z_values <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_values)))

ci_lower <- round(exp(as.numeric(coefs - 1.96 * se)),2)
ci_upper <- round(exp(as.numeric(coefs + 1.96 * se)),2)
ci <- paste0(ci_lower,", ",ci_upper)


results <- rbind(results, data.frame(
  model = "adjusted with history depression - perimenopause",
  effect = as.numeric(round(exp(coefs),2)),
  ci = ci,
  p = as.numeric(round(p_values,3)),
  n = model@Hierarchy[5]
))

coefs <- model@FP[3]
se <- sqrt(diag(model@FP.cov))[3]
z_values <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_values)))

ci_lower <- round(exp(as.numeric(coefs - 1.96 * se)),2)
ci_upper <- round(exp(as.numeric(coefs + 1.96 * se)),2)
ci <- paste0(ci_lower,", ",ci_upper)


results <- rbind(results, data.frame(
  model = "adjusted with history depression - postmenopause",
  effect = as.numeric(round(exp(coefs),2)),
  ci = ci,
  p = as.numeric(round(p_values,3)),
  n = model@Hierarchy[5]
))

