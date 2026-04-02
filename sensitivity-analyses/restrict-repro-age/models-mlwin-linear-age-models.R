rm(list = setdiff(ls(), c("p1_chrono", "p2_chrono", "p1_repro","p2_repro")))
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

chrono_age_lower <- as.numeric(quantile(df$age, 0.05,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.95,na.rm = T))

df <- df %>% filter(age >= chrono_age_lower
                    & age <= chrono_age_upper)

chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))

# Model depression over chronological age
model_age <- runMLwiN(epds_prorated ~ 1 + age + age_sq
                      + ethnicity + social_class + education + age_menarche
                      + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                      + (1 + age| aln) + (1 + age | time_point),
                      D = "Normal",
                      data = df)

model_age_2 <- runMLwiN(epds_prorated ~ 1 + age + age_sq
                        + repro_age
                        + ethnicity + social_class + education + age_menarche
                        + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                        + (1 + age | aln) + (1 + age | time_point),
                        D = "Normal",
                        data = df)

                      
AIC(model_age)
AIC(model_age_2)
model_age_2

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
predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link", se.fit = T)
predicted_model_age_2 <- predict(model_age_2, newdata = pred_model_age, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model_age <- predicted_model_age$fit - 1.96 * predicted_model_age$se.fit
CI_high_model_age <- predicted_model_age$fit + 1.96 * predicted_model_age$se.fit

CI_low_model_age_2 <- predicted_model_age_2$fit - 1.96 * predicted_model_age_2$se.fit
CI_high_model_age_2 <- predicted_model_age_2$fit + 1.96 * predicted_model_age_2$se.fit

# Combine results into a data frame
pred_df_model_age <- data.frame(age = pred_model_age$age,
                                predicted = predicted_model_age$fit,
                                CI_low = CI_low_model_age,
                                CI_high = CI_high_model_age)

pred_df_model_age_2 <- data.frame(age = pred_model_age$age,
                                predicted = predicted_model_age_2$fit,
                                CI_low = CI_low_model_age_2,
                                CI_high = CI_high_model_age_2)
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
    y = "Predicted Depressive Symptoms Score"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(5,9,
                                  by = 1),limits = c(4.4,9))+
  annotate("text", 
           x = 31.5, 
           y = 9, 
           label = "b",
           hjust = 1, vjust = 1, size = 18, color = "black",
           family = "serif", fontface = "bold"
  )

p1_chrono

p2_chrono <- ggplot(pred_df_model_age_2, aes(x = age, y = predicted)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
  geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
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
  scale_y_continuous(breaks = seq(5,9,
                                  by = 1),limits = c(4.4,9))+
  annotate("text", 
           x = 31.5, 
           y = 9, 
           label = "a",
           hjust = 1, vjust = 1, size = 18, color = "black",
           family = "serif", fontface = "bold"
  )

p2_chrono
#grid_arrange_plot <- grid.arrange(p1_chrono, p2_chrono, ncol = 1)
#ggsave("Final plots/combined_plot_age.png", grid_arrange_plot, width = 8.27, height = 11.69,bg = "white")


# Predict values for paper text
# Plot model chronological age
# Predict values
age_seq <- seq(30,70,1)


pred_model_age <- data.frame(
  age = seq(30,70,1),
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
predicted_model_age_2 <- predict(model_age, newdata = pred_model_age, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model_age_2 <- predicted_model_age_2$fit - 1.96 * predicted_model_age_2$se.fit
CI_high_model_age_2 <- predicted_model_age_2$fit + 1.96 * predicted_model_age_2$se.fit

# Combine results into a data frame
pred_df_model_age_2 <- data.frame(age = pred_model_age$age,
                                  predicted = predicted_model_age_2$fit,
                                  CI_low = CI_low_model_age_2,
                                  CI_high = CI_high_model_age_2)

View(pred_df_model_age_2)
