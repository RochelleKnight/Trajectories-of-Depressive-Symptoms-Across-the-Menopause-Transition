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
                      + repro_age
                      + ethnicity + social_class + education + age_menarche
                      + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                      + (1 + age + age_sq | aln) + (1 + age | time_point),
                      D = "Normal",
                      data = df,
                      estoptions = list(EstM = 0,resi.store = T))

# Plot level 1 residuals 
hist(model_age@residual$lev_1_resi_est_Intercept, main = "Histogram of level-1 residuals")

resid_df <- data.frame(
  residuals = model_age@residual$lev_1_resi_est_Intercept
)

resid_3 <- ggplot(resid_df, aes(sample = residuals)) +
  stat_qq(size = 1, alpha = 0.7) +
  stat_qq_line(linewidth = 0.6, color = "red") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    title = "Q–Q Plot of Level-1 Residuals"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )+
  annotate("text", 
           x = -3.5, 
           y = 20, 
           label = "c",
           hjust = 1, vjust = 1, size = 15, color = "black",
           family = "serif", fontface = "bold"
  )


ggsave("Final plots/sensitivity/residual_plot_chrono.png",resid_3, height = 8.27, width = 11.69,bg = "white")

# Read in df
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
length(which(!is.na(df$epds_prorated)))

df$age_sq <- df$age^2
df$centered_age_sq <- df$centered_age^2

chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))

df$epds_prorated_log <- log(df$epds_prorated + 1)

# Model depression over chronological age
model_age_log <- runMLwiN(epds_prorated_log ~ 1 + age + age_sq
                      + ethnicity + social_class + education + age_menarche
                      + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                      + (1 + age + age_sq | aln) + (1 + age | time_point),
                      D = "Normal",
                      data = df,
                      estoptions = list(EstM = 0,resi.store = T))

# Plot level 1 residuals 
hist(model_age_log@residual$lev_1_resi_est_Intercept, main = "Histogram of level-1 residuals")

resid_df <- data.frame(
  residuals = model_age_log@residual$lev_1_resi_est_Intercept
)

resid_4 <- ggplot(resid_df, aes(sample = residuals)) +
  stat_qq(size = 1, alpha = 0.7) +
  stat_qq_line(linewidth = 0.6, color = "red") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    title = "Q–Q Plot of Level-1 Residuals"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )+
  annotate("text", 
           x = -3.5, 
           y = 3, 
           label = "d",
           hjust = 1, vjust = 1, size = 15, color = "black",
           family = "serif", fontface = "bold"
  )

ggsave("Final plots/sensitivity/residual_plot_chrono_log.png",resid_4, height = 8.27, width = 11.69,bg = "white")

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
  
  # Use mean values for continuous variables
  material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
  social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
  bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
)

pred_model_age$age_sq <- pred_model_age$age^2

# Predict values
predicted_model_age <- predict(model_age_log, newdata = pred_model_age, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model_age <- predicted_model_age$fit - 1.96 * predicted_model_age$se.fit
CI_high_model_age <- predicted_model_age$fit + 1.96 * predicted_model_age$se.fit

# Combine results into a data frame
pred_df_model_age <- data.frame(age = pred_model_age$age,
                                predicted = predicted_model_age$fit,
                                CI_low = CI_low_model_age,
                                CI_high = CI_high_model_age)

# Back-transform from log(EPDS + 1) to EPDS
pred_df_model_age <- pred_df_model_age %>%
  mutate(
    predicted_raw = exp(predicted) - 1,
    CI_low_raw = exp(CI_low) - 1,
    CI_high_raw = exp(CI_high) - 1)

# Plot model
p1_chrono <- ggplot(pred_df_model_age, aes(x = age, y = predicted_raw)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_line(aes(y = CI_low_raw), linetype = "dashed", color = "blue", linewidth = 1.5) +
  geom_line(aes(y = CI_high_raw), linetype = "dashed", color = "blue", linewidth = 1.5) +
  geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
  #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  labs(
    #title = "Trajectory of Depression Over Reproductive Age",
    x = "Age (years)",
    y = "Predicted Depressive Symptoms Score"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(3,6,
                                  by = 1),limits = c(3,6.5))+
  annotate("text", 
           x = 31.5, 
           y = 6.5, 
           label = "b",
           hjust = 1, vjust = 1, size = 18, color = "black",
           family = "serif", fontface = "bold"
  )

p1_chrono
