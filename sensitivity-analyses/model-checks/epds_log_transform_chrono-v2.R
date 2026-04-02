#rm(list = ls())
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

chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))

# Model depression over chronological age
model_age <- runMLwiN(epds_prorated ~ 1 + age + age_sq
                      + ethnicity + social_class + education + age_menarche
                      + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                      + (1 + age + age_sq | aln) + (1 + age | time_point),
                      D = "Normal",
                      data = df,
                      estoptions = list(EstM = 0,resi.store = T))

df$epds_prorated_log <- log(df$epds_prorated + 1)

# Model depression over chronological age
model_age_log <- runMLwiN(epds_prorated_log ~ 1 + age + age_sq
                          + ethnicity + social_class + education + age_menarche
                          + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                          + (1 + age + age_sq | aln) + (1 + age | time_point),
                          D = "Normal",
                          data = df,
                          estoptions = list(EstM = 0,resi.store = T))

# Plot model chronological age
# Predict values
age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)

pred_model_age <- data.frame(
  age = seq(from = chrono_age_lower, to = chrono_age_upper, length.out = 100),
  
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
predicted_model_log <- predict(model_age_log, newdata = pred_model_age, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model_age <- predicted_model_age$fit - 1.96 * predicted_model_age$se.fit
CI_high_model_age <- predicted_model_age$fit + 1.96 * predicted_model_age$se.fit

CI_low_model_log <- predicted_model_log$fit - 1.96 * predicted_model_log$se.fit
CI_high_model_log <- predicted_model_log$fit + 1.96 * predicted_model_log$se.fit

# Combine results into a data frame
pred_df_model_age <- data.frame(age = pred_model_age$age,
                                predicted = predicted_model_age$fit,
                                CI_low = CI_low_model_age,
                                CI_high = CI_high_model_age,
                                model = "EPDS original scale")

pred_df_model_log <- data.frame(age = pred_model_age$age,
                                predicted = predicted_model_log$fit,
                                CI_low = CI_low_model_log,
                                CI_high = CI_high_model_log,
                                model = "EPDS log transformed")

# Back-transform from log(EPDS + 1) to EPDS
pred_df_model_log <- pred_df_model_log %>%
  mutate(
    predicted = exp(predicted) - 1,
    CI_low = exp(CI_low) - 1,
    CI_high = exp(CI_high) - 1)

pred_df_model_age <- rbind(pred_df_model_age, pred_df_model_log)

pred_df_model_age$model <- factor(pred_df_model_age$model,
                                    levels = c("EPDS original scale",
                                               "EPDS log transformed"))

# Plot model
p1_chrono <- ggplot(pred_df_model_age, aes(x = age, y = predicted, colour = model, fill = model)) +
  geom_line(linewidth = 1.5) +
  geom_line(aes(y = CI_low), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = CI_high), linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
  #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  labs(
    #title = "Trajectory of Depression Over Reproductive Age",
    x = "Age (years)",
    y = "Predicted Depressive Symptoms Score",
    colour = NULL,  # Remove legend title
    fill = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.margin = margin(3, 3, 3, 3),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "lines")) +
  scale_x_continuous(breaks = seq(30,70,
                                  by = 5),limits = c(30,70))+
  #scale_y_continuous(breaks = seq(3,6,
  #                                by = 1),limits = c(3,6.5))+
  annotate("text", 
           x = 31.5, 
           y = 8, 
           label = "b",
           hjust = 1, vjust = 1, size = 18, color = "black",
           family = "serif", fontface = "bold"
  )+
  scale_colour_manual(values = c(
    "EPDS original scale" = "blue",
    "EPDS log transformed" = "#999999"
  )) +
  scale_fill_manual(values = c(
    "EPDS original scale" = "blue",
    "EPDS log transformed" = "#999999"
  ))

p1_chrono
