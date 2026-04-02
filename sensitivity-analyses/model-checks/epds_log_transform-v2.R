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

df$centered_age_sq <- df$centered_age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025,na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975,na.rm = T))

df$epds_prorated_log <- log(df$epds_prorated + 1)

model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age
                        + centered_age + centered_age_sq 
                        + ethnicity + social_class + education + age_menarche
                        + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                        + (1 + centered_age | aln) + (1 + centered_age | time_point),
                        D = "Normal",
                        data = df,
                        estoptions = list(EstM = 0,resi.store = T)
)

model_repro_log <- runMLwiN(epds_prorated_log ~ 1 + repro_age
                            + centered_age + centered_age_sq 
                            + ethnicity + social_class + education + age_menarche
                            + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                            + (1 + centered_age | aln) + (1 + centered_age | time_point),
                            D = "Normal",
                            data = df,
                            estoptions = list(EstM = 0,resi.store = T)
)

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
  
  # Use mean values for continuous variables
  material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
  social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
  bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
)

# Predict values
predicted_model_log <- predict(model_repro_log, newdata = pred_model_repro, type = "link",se.fit = T)
predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)

# Prediction df for plotting
pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                  predicted = predicted_model_repro$fit,
                                  CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                  CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit,
                                  model = "EPDS original scale")

pred_df_model_log <- data.frame(repro_age = pred_model_repro$repro_age,
                                  predicted = predicted_model_log$fit,
                                  CI_low = predicted_model_log$fit - 1.96 * predicted_model_log$se.fit,
                                  CI_high = predicted_model_log$fit + 1.96 * predicted_model_log$se.fit,
                                  model = "EPDS log transformed")

# Back-transform from log(EPDS + 1) to EPDS
pred_df_model_log <- pred_df_model_log %>%
  mutate(
    predicted = exp(predicted) - 1,
    CI_low = exp(CI_low) - 1,
    CI_high = exp(CI_high) - 1
  )

pred_df_model_repro <- rbind(pred_df_model_repro, pred_df_model_log)

pred_df_model_repro$model <- factor(pred_df_model_repro$model,
                                    levels = c("EPDS original scale",
                                               "EPDS log transformed"))

# Plot with confidence intervals
p1_repro <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted, colour = model, fill = model)) +
  geom_line(linewidth = 1.5) +
  geom_line(aes(y = CI_low), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = CI_high), linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
  labs(
    #title = "Trajectory of Depression Over Reproductive Age",
    x = "Years around final menstrual period (FMP)",
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
        legend.key.size = unit(1, "lines"))+
  scale_x_continuous(breaks = seq(-20,20,
                                  by = 5),limits = c(-20,20))+
  #scale_y_continuous(breaks = seq(3,6,
  #                                by = 1),limits = c(3,6.5))+
  annotate("text", 
           x = -18, 
           y = 9, 
           label = "a",
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

p1_repro

source("sensitivity-analyses/model-checks/epds_log_transform_chrono-v2.R ")
#ggsave("Final plots/sensitivity/repro_trajec_log_epds.png", height = 8.27, width = 11.69,bg = "white")

grid_arrange_plot <- grid.arrange(p1_repro, p1_chrono, ncol = 1)
ggsave("Final plots/sensitivity/combined_plot_log_epds.png", grid_arrange_plot, width = 8.27, height = 11.69,bg = "white")

