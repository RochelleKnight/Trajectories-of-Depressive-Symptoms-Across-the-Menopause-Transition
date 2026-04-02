rm(list = setdiff(ls(), c("p1_chrono", "p1_repro")))
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
#df <- readRDS(paste0(filestore,"analysis_df_full.rds"))
length(which(!is.na(df$epds_prorated)))

df$age_sq <- df$age^2
df$centered_age_sq <- df$centered_age^2

chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))

df$history_depression_d <- factor(df$history_depression_d,
                                  levels = c("0","1"))

# Model depression over chronological age
model_age <- runMLwiN(logit(epds_binary) ~ 1 + age + age_sq
                        #+ repro_age
                        + ethnicity + social_class + education + age_menarche
                        + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                        + history_depression_d
                        + (1 + age| aln),
                        D = "Binomial",
                        data = df)

model_age

# Plot model chronological age
# Predict values
age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)

pred_model_age <- data.frame(
  age = seq(from = chrono_age_lower, to = chrono_age_upper, length.out = 100),
  repro_age = 0,
  
  # Convert factor variables to factors with the same levels as in df
  ethnicity = factor("White", levels = levels(df$ethnicity)),
  social_class = factor("4", levels = levels(df$social_class)),
  education = factor("A-level_or_greater", levels = levels(df$education)),
  age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
  smoking_status = factor("Never", levels = levels(df$smoking_status)), 
  alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
  history_depression_d = factor("0", levels = levels((df$history_depression_d))),
  
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
                                predicted = plogis(predicted_model_age$fit),
                                CI_low = plogis(CI_low_model_age),
                                CI_high = plogis(CI_high_model_age))

# Plot model
p1_chrono <- ggplot(pred_df_model_age, aes(x = age, y = predicted)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
  geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
  geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
  #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  labs(
    #title = "Trajectory of Depression Over Reproductive Age",
    x = "Age (years)",
    y = "Predicted Probability of Depression"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = seq(30,60,
                                  by = 5),limits = c(30,70))+
  scale_y_continuous(breaks = seq(0.05,0.2,
                                  by = 0.05),limits = c(0.05,0.2))


