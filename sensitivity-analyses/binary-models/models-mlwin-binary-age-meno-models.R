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
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
df$age_sq <- df$age^2

df$age_meno_cat <- NA_character_
df$age_meno_cat[which(df$age_menopause < 45)] <- "Under 45"
df$age_meno_cat[which(df$age_menopause >= 45)] <- "Over 45"
df$age_meno_cat <- as.factor(df$age_meno_cat)
table(df$age_meno_cat)

quantiles <- df %>% 
  group_by(age_meno_cat) %>%
  dplyr::summarize(lower_bound = quantile(age, 0.025, na.rm=T),
                   upper_bound = quantile(age, 0.975, na.rm=T))

chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))

# Model depression over chronological age
model1 <- runMLwiN(logit(epds_binary) ~ 1 + age*age_menopause + age_sq*age_menopause
                   + ethnicity + social_class + education + age_menarche
                   + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                   + (1 + age | aln),
                   D = "Binomial",
                   data = df)

model1

# Plot model chronological age
# Predict values
age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)

pred_model1 <- data.frame(
  age = rep(seq(from = chrono_age_lower, to = chrono_age_upper, length.out = 100),3),
  
  age_menopause = c(rep(45,100),rep(50,100),rep(55,100)),
  
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
pred_model1$age_sq <- pred_model1$age^2

# Predict values
predicted_model1 <- predict(model1, newdata = pred_model1, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model1 <- predicted_model1$fit - 1.96 * predicted_model1$se.fit
CI_high_model1 <- predicted_model1$fit + 1.96 * predicted_model1$se.fit

# Combine results into a data frame
pred_df_model1 <- data.frame(age = pred_model1$age,
                             age_menopause = pred_model1$age_menopause,
                             predicted = plogis(predicted_model1$fit),
                             CI_low = plogis(CI_low_model1),
                             CI_high = plogis(CI_high_model1))

# Plot without confidence intervals
ggplot(pred_df_model1, aes(x = age, y = predicted, colour = factor(age_menopause))) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
  labs(
    x = "Age (years)",
    y = "Predicted Probability of Depression",
    colour = "Age at Menopause (Years)",
    colour = NULL,  # Remove legend title
    fill = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    legend.position = c(0.95, 0.05),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 12),
    legend.key.size = unit(2, "lines")
  ) +
  scale_x_continuous(breaks = seq(25, 70, by = 5))

ggsave("Final plots/binary/Age at menopause continuous.png", width = 11.69, height = 8.27,bg = "white")


# Plot with confidence intervals
ggplot(pred_df_model1, aes(x = age, y = predicted, colour = factor(age_menopause), fill = factor(age_menopause))) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
  labs(
    x = "Age (years)",
    y = "Predicted Probability of Depression",
    colour = "Age at Menopause (Years)",
    fill = "Age at Menopause (Years)"
    #colour = NULL,  # Remove legend title
    #fill = NULL,
    #linetype = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    legend.position = c(0.95, 0.05),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 12),
    legend.key.size = unit(2, "lines")
  ) +
  scale_x_continuous(breaks = seq(25, 70, by = 5))


ggsave("Final plots/binary/Age at menopause continuous with CIs.png", width = 11.69, height = 8.27, units = "in", bg = "white")


model2 <- runMLwiN(logit(epds_binary) ~ 1 + age*age_meno_cat + age_sq*age_meno_cat                    
                   + ethnicity + social_class + education + age_menarche
                   + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                   + (1 + age| aln),
                   D = "Binomial",
                   data = df)

model2

# Plot figure
pred_model2_under_48 <- data.frame(
  age = seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Under 45")]),
            to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Under 45")]), length.out = 100),
  
  age_meno_cat = factor("Under 45", levels = levels(df$age_meno_cat)),
  
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
pred_model2_under_48$age_sq <- pred_model2_under_48$age^2

pred_model2_over_48 <- data.frame(
  age = seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Over 45")]),
            to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Over 45")]), length.out = 100),
  
  age_meno_cat = factor("Over 45", levels = levels(df$age_meno_cat)),
  
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
pred_model2_over_48$age_sq <- pred_model2_over_48$age^2

# Predict values
predicted_model2_under48 <- predict(model2, newdata = pred_model2_under_48, type = "link", se.fit = T)
predicted_model2_over48 <- predict(model2, newdata = pred_model2_over_48, type = "link", se.fit = T)

# Calculate the confidence intervals
CI_low_model2_under48 <- predicted_model2_under48$fit - 1.96 * predicted_model2_under48$se.fit
CI_high_model2_under48 <- predicted_model2_under48$fit + 1.96 * predicted_model2_under48$se.fit

CI_low_model2_over48 <- predicted_model2_over48$fit - 1.96 * predicted_model2_over48$se.fit
CI_high_model2_over48 <- predicted_model2_over48$fit + 1.96 * predicted_model2_over48$se.fit

# Combine results into a data frame
pred_df_model2 <- data.frame(
  age = c(pred_model2_under_48$age, pred_model2_over_48$age),
  predicted = plogis(c(predicted_model2_under48$fit,predicted_model2_over48$fit)),
  CI_low = plogis(c(CI_low_model2_under48, CI_low_model2_over48)),
  CI_high = plogis(c(CI_high_model2_under48,CI_high_model2_over48)),
  age_meno_cat = factor(c(rep("Age at menopause aged 40 - 45", 100), rep("Age at menopause 45 or above", 100)),
                        levels = c("Age at menopause aged 40 - 45", "Age at menopause 45 or above"))
)


# Plot without confidence intervals
ggplot(pred_df_model2, aes(x = age, y = predicted, colour = age_meno_cat)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
  labs(
    x = "Age (Years)",
    y = "Predicted Probability of Depression",
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
  scale_x_continuous(breaks = seq(25, 70, by = 5))


ggsave("Final plots/binary/Age at menopause categorical.png", width = 11.69, height = 8.27, units = "in", bg = "white")


# Plot with confidence intervals
ggplot(pred_df_model2, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2) +
  geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
  labs(
    x = "Age (Years)",
    y = "Predicted Probability of Depression",
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
  scale_x_continuous(breaks = seq(25, 70, by = 5))+
  scale_colour_manual(values = c(
    "Age at menopause aged 40 - 45" = "#D55E00",
    "Age at menopause 45 or above" = "#0072B2"
  )) +
  scale_fill_manual(values = c(
    "Age at menopause aged 40 - 45" = "#D55E00",
    "Age at menopause 45 or above" = "#0072B2"
  ))


ggsave("Final plots/binary/Age at menopause categorical with CIs.png", width = 11.69, height = 8.27, units = "in", bg = "white")
