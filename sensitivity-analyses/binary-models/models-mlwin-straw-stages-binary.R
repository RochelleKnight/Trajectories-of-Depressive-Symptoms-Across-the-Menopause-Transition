rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")
library(R2MLwiN)
library(dplyr)
library(rms)
library(splines)
library(ggplot2)

options(MLwiN_path = "C:/Program Files/MLwiN v3.13/mlwin.exe")

# Read in df
df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df$centered_age <- df$age-50
df$age_sq <- df$age^2

# Model STRAW stages over age
model1 <- runMLwiN(logit(epds_binary) ~ 1 + age*stage
                   + ethnicity + social_class + education + age_menarche
                   + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                   + (1 + age | aln),
                   D = "Binomial",
                   data = df)
model1

quantiles <- df %>% filter(!is.na(stage)) %>%
  group_by(stage) %>%
  dplyr::summarize(lower_bound = quantile(age, 0.025),
            upper_bound = quantile(age, 0.975))

# Plot figure
pred_model1_pre <- data.frame(
  age = seq(from = quantiles$lower_bound[which(quantiles$stage == "reproductive")], to = quantiles$upper_bound[which(quantiles$stage == "reproductive")], length.out = 100),
  age_sq = seq(from = min(df$age[which(df$stage == "reproductive")]), to = max(df$age[which(df$stage == "reproductive")]), length.out = 100)^2,
  stage = factor("reproductive", levels = levels(df$stage)),
  
  # Convert factor variables to factors with the same levels as in df
  ethnicity = factor("White", levels = levels(df$ethnicity)),
  social_class = factor("1", levels = levels(df$social_class)),
  education = factor("A-level_or_greater", levels = levels(df$education)),
  age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
  smoking_status = factor("Never", levels = levels(df$smoking_status)),
  alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
  
  # Use mean values for continuous variables
  material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
  social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
  bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
)

pred_model1_peri <- data.frame(
  age = seq(from = quantiles$lower_bound[which(quantiles$stage == "menopause_transition")], to = quantiles$upper_bound[which(quantiles$stage == "menopause_transition")], length.out = 100),
  age_sq = seq(from = min(df$age[which(df$stage == "menopause_transition")]), to = max(df$age[which(df$stage == "menopause_transition")]), length.out = 100)^2,
  stage = factor("menopause_transition", levels = levels(df$stage)),
  
  # Convert factor variables to factors with the same levels as in df
  ethnicity = factor("White", levels = levels(df$ethnicity)),
  social_class = factor("1", levels = levels(df$social_class)),
  education = factor("A-level_or_greater", levels = levels(df$education)),
  age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
  smoking_status = factor("Never", levels = levels(df$smoking_status)),
  alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
  
  # Use mean values for continuous variables
  material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
  social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
  bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
)

pred_model1_post <- data.frame(
  age = seq(from = quantiles$lower_bound[which(quantiles$stage == "post_menopause")], to = quantiles$upper_bound[which(quantiles$stage == "post_menopause")], length.out = 100),
  age_sq = seq(from = min(df$age[which(df$stage == "post_menopause")]), to = max(df$age[which(df$stage == "post_menopause")]), length.out = 100)^2,
  stage = factor("post_menopause", levels = levels(df$stage)),
  
  # Convert factor variables to factors with the same levels as in df
  ethnicity = factor("White", levels = levels(df$ethnicity)),
  social_class = factor("1", levels = levels(df$social_class)),
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
predicted_model1_pre <- predict(model1, newdata = pred_model1_pre, type = "link", se.fit = T)
predicted_model1_peri <- predict(model1, newdata = pred_model1_peri, type = "link", se.fit = T)
predicted_model1_post <- predict(model1, newdata = pred_model1_post, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model1_pre <- plogis(predicted_model1_pre$fit - 1.96 * predicted_model1_pre$se.fit)
CI_high_model1_pre <- plogis(predicted_model1_pre$fit + 1.96 * predicted_model1_pre$se.fit)

CI_low_model1_peri <- plogis(predicted_model1_peri$fit - 1.96 * predicted_model1_peri$se.fit)
CI_high_model1_peri <- plogis(predicted_model1_peri$fit + 1.96 * predicted_model1_peri$se.fit)

CI_low_model1_post <- plogis(predicted_model1_post$fit - 1.96 * predicted_model1_post$se.fit)
CI_high_model1_post <- plogis(predicted_model1_post$fit + 1.96 * predicted_model1_post$se.fit)


# Combine predictions into one data frame
pred_df <- data.frame(
  age = c(pred_model1_pre$age, pred_model1_peri$age, pred_model1_post$age),
  predicted = c(plogis(predicted_model1_pre$fit), plogis(predicted_model1_peri$fit), plogis(predicted_model1_post$fit)),
  CI_low = c(CI_low_model1_pre, CI_low_model1_peri, CI_low_model1_post),
  CI_high = c(CI_high_model1_pre, CI_high_model1_peri, CI_high_model1_post),
  stage = factor(c(rep("Reproductive", 100), rep("Perimenopause", 100), rep("Postmenopause", 100)))
)
pred_df$stage <- factor(pred_df$stage,
                        levels = c("Reproductive","Perimenopause","Postmenopause"))

# Plot the predictions with confidence intervals
ggplot(pred_df, aes(x = age, y = predicted, colour = stage, fill = stage)) +
  geom_line(aes(linetype = stage), linewidth = 1.5) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.1, linewidth = 1) +
  labs(
    #title = "Predicted Depression Scores Across Menopausal Stages",
    x = "Age (Years)",
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
    legend.box.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 18),
    legend.key.size = unit(2, "lines")
  ) +
  scale_x_continuous(breaks = seq(25, 70, by = 5))+
  scale_colour_manual(values = c(
    "Reproductive" = "#E69F00",
    "Perimenopause" = "#56B4E9",
    "Postmenopause" = "#009E73"
  )) +
  scale_fill_manual(values = c(
    "Reproductive" = "#E69F00",
    "Perimenopause" = "#56B4E9",
    "Postmenopause" = "#009E73"
  ))

ggsave("Final plots/binary/Trajectory of Depression Over Chronological Age (straw stages).png", width = 11.69, height = 8.27, units = "in",bg="white")

