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
table(df$repro_age)
length(which(!is.na(df$epds_prorated)))

df$centered_age <- df$age - 50
df$centered_age_sq <- df$centered_age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025,na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975,na.rm = T))

df$history_depression_d <- factor(df$history_depression_d,
                                  levels = c("0","1"))

model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age
                    + centered_age + centered_age_sq 
                    + ethnicity + social_class + education + age_menarche
                    + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                    + history_depression_d
                    + (1 + centered_age | aln) + (1 + centered_age | time_point),
                    D = "Normal",
                    data = df,
                    estoptions = list(EstM = 0,resi.store = T)
                    )

model_repro


model_repro_no_age <- runMLwiN(epds_prorated ~ 1 + repro_age
                        + ethnicity + social_class + education + age_menarche
                        + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                        + history_depression_d
                        + (1 + repro_age | aln) + (1 + repro_age | time_point),
                        D = "Normal",
                        data = df,
                        estoptions = list(EstM = 0)
)
model_repro_no_age
AIC(model_repro)
AIC(model_repro_no_age)

# Predict values
repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)

pred_model_repro <- data.frame(
  repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
  centered_age = 0,
  centered_age_sq = 0,
  
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

# Plot with confidence intervals
p1_repro <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
  geom_line(color = "blue",linewidth = 1.5) +
  geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
  geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
  #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
  #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  labs(
    #title = "Trajectory of Depression Over Reproductive Age",
    x = "Years around final menstrual period (FMP)",
    y = "Predicted Depressive Symptoms Score"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = seq(-20,20,
                                  by = 5),limits = c(-20,20))+
 scale_y_continuous(breaks = seq(5,9,
                                  by = 1),limits = c(4.5,9))+
  annotate("text",
           x = 32-50,
           y = 9,
           label = "a",
           hjust = 1, vjust = 1, size = 18, color = "black",
           family = "serif", fontface = "bold"
  )

p1_repro
#ggsave("Final plots/plot_repro.png", height = 8.27, width = 11.69,bg = "white")

p2_repro <- ggplot(pred_df_model_repro_no_age, aes(x = repro_age, y = predicted)) +
  geom_line(color = "blue",linewidth = 1.5) +
  geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
  geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
  #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
  #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  labs(
    #title = "Trajectory of Depression Over Reproductive Age",
    x = "Years around final menstrual period (FMP)",
    y = "Predicted Depressive Symptoms Score"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))+
  scale_x_continuous(breaks = seq(-20,20,
                                  by = 5),limits = c(-20,20))+
  scale_y_continuous(breaks = seq(5,9,
                                  by = 1),limits = c(5,9))+
  annotate("text",
           x = 32-50,
           y = 9,
           label = "b",
           hjust = 1, vjust = 1, size = 18, color = "black",
           family = "serif", fontface = "bold"
  )

p2_repro

#grid_arrange_plot <- grid.arrange(p1_repro, p2_repro, ncol = 1)
#ggsave("Final plots/combined_plot_repro.png", grid_arrange_plot, width = 8.27, height = 11.69,bg = "white")
#grid.arrange(p1_repro, p2_repro, ncol = 1)

