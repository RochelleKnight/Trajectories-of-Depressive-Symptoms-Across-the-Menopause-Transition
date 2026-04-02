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
df$ever_hrt_binary <- 0

replace <- which(df$ever_hrt == T)
df$ever_hrt_binary[replace] <- 1

df$ever_hrt_binary <- factor(df$ever_hrt_binary,
                              levels = c(0,1))

df$age_sq <- df$age^2
df$centered_age_sq <- df$centered_age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025, na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975, na.rm = T))

model <- runMLwiN(epds_prorated ~ 1 + repro_age*ever_hrt_binary 
                  + centered_age + centered_age_sq 
                  + ethnicity + social_class + education + age_menarche
                  + material_hardship + social_support + smoking_status + bmi + alcohol_intake + ever_hrt_binary
                  + (1 + centered_age + centered_age_sq | aln) + (1 + centered_age | time_point),
                  D = "Normal",
                  data = df,
                  estoptions = list(EstM = 0)
)

model

# Predict values
repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)

pred_model_hrt <- data.frame(
  repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
  
  centered_age = 0,
  centered_age_sq = 0,
  
  ever_hrt_binary = factor("1", levels = levels(df$ever_hrt_binary)),
  
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

pred_model_no_hrt <- data.frame(
  repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
  
  centered_age = 0,
  centered_age_sq = 0,
  
  ever_hrt_binary = factor("0", levels = levels(df$ever_hrt_binary)),
  
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
predicted_model_hrt <- predict(model, newdata = pred_model_hrt, type = "link",se.fit = T)
predicted_model_no_hrt <- predict(model, newdata = pred_model_no_hrt, type = "link",se.fit = T)


# Prediction df for plotting
# Combine results into a data frame
pred_df_model <- data.frame(
  repro_age = c(pred_model_no_hrt$repro_age, pred_model_hrt$repro_age),
  predicted = c(predicted_model_no_hrt$fit,predicted_model_hrt$fit),
  CI_low = c(predicted_model_no_hrt$fit - 1.96 * predicted_model_no_hrt$se.fit, predicted_model_hrt$fit - 1.96 * predicted_model_hrt$se.fit),
  CI_high = c(predicted_model_no_hrt$fit + 1.96 * predicted_model_no_hrt$se.fit,predicted_model_hrt$fit + 1.96 * predicted_model_hrt$se.fit),
  hrt_cat = factor(c(rep("Never HRT use", 100), rep("Ever HRT use", 100)),
                        levels = c("Ever HRT use","Never HRT use"))
)

# Plot without confidence intervals
ggplot(pred_df_model, aes(x = repro_age, y = predicted, colour = hrt_cat, fill = hrt_cat )) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",linewidth = 1) +
  labs(
    x = "Years around final menstrual period (FMP)",
    y = "Predicted Depressive Symptoms Score",
    colour = NULL,
    fill = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 26),
    axis.title = element_text(size = 26),
    legend.position = c(0.95, 0.05),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    #legend.box.margin = margin(5, 5, 5, 5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.key.size = unit(2, "lines")
  ) +
  scale_x_continuous(breaks = seq(-20, 20, by = 5))+
  scale_colour_manual(values = c(
    "Ever HRT use" = "#F781BF",
    "Never HRT use" = "black"
  )) +
  scale_fill_manual(values = c(
    "Ever HRT use" = "#F781BF",
    "Never HRT use" = "black"
  ))



ggsave("Final plots/sensitivity/HRT interaction - repro age.png", width = 11.69, height = 8.27,bg = "white")



