rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")

library(dplyr)
library(haven)

# Ethnicity
# C
#N


# Social class
# C


# Age at menarche
# D

# Education
# C, K, N, R

combine_mean_sd <- function(n1, mean1, sd1, n2, mean2, sd2) {
  # Combined mean
  mean_total <- (n1 * mean1 + n2 * mean2) / (n1 + n2)
  
  # Combined variance using pooled formula
  pooled_variance <- (
    (n1 - 1) * sd1^2 +
      (n2 - 1) * sd2^2 +
      (n1 * n2) / (n1 + n2) * (mean1 - mean2)^2
  ) / (n1 + n2 - 1)
  
  sd_total <- sqrt(pooled_variance)
  
  return(list(mean = mean_total, sd = sd_total))
}

combine_mean_sd(n1 = 11923, mean1 = 28.63, sd1 = 4.863,
                n2 = 12131, mean2 = 28.57, sd2 = 4.856)




df <- read_dta(paste0(filestore,"cohort.dta"))
df <- as.data.frame(df)

unique(df$k9996a)
df$k9996a[which(df$k9996a < 0)] <- NA_integer_
mean(as.numeric(df$k9996a), na.rm = T)

unique(df$n9992)
df$n9992[which(df$n9992 < 0)] <- NA_integer_
mean(as.numeric(df$n9992), na.rm = T)

unique(df$r9996a)
df$r9996a[which(df$r9996a < 0)] <- NA_integer_
mean(as.numeric(df$r9996a), na.rm = T)

# Exclusions
df_long <- readRDS(paste0(filestore,"analysis_df_full.rds"))
length(unique(df_long$aln[which(df_long$age_menopause < 40 | df_long$age_menopause >= 60)]))

# Complete case counts
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
df <- df %>% select(aln,date,age_menopause,repro_age,epds_prorated,
                      ethnicity,social_class,education,age_menarche,
                      material_hardship,social_support,smoking_status,bmi,alcohol_intake)
df <- df[complete.cases(df),]
length(unique(df$aln))
nrow(df)

df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df <- df %>% select(aln,date,stage,epds_prorated,
                    ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake)
df <- df[complete.cases(df),]
length(unique(df$aln))
nrow(df)

#Median age at menopause
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
df <- df %>% select(aln,age,time_point,epds_prorated,age_menopause,repro_age,ethnicity,social_class,
                    education,age_menarche,material_hardship,social_support,smoking_status,bmi,
                    alcohol_intake) 
df <- df[complete.cases(df),]
df_meno <- df %>% select(aln,age_menopause) %>% distinct()

quantile(df_meno$age_menopause, probs = c(0.25, 0.5, 0.75))

# Ages at first EPDS timepoint
df1 <- df %>% filter(time_point == 1)
range(df1$age,na.rm = T)

df12 <- df %>% filter(time_point == 11)
range(df12$age,na.rm = T)

# Number with early menopause
df_early <- df %>% filter(age_menopause >=40 & age_menopause <45)
length(unique(df_early$aln))
(length(unique(df_early$aln))/2036)*100

min(df$repro_age)
max(df$repro_age)

# Number with POI
df_long <- readRDS(paste0(filestore,"analysis_df_full.rds"))
length(unique(df_long$aln[which(df_long$age_menopause < 40)]))
length(unique(df_long$aln[which(df_long$age_menopause >= 60)]))

# STRAW stage counts
df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df <- df %>% select(aln,age,stage,epds_prorated,ethnicity,social_class,
                    education,age_menarche,material_hardship,social_support,smoking_status,bmi,
                    alcohol_intake) 
df <- df[complete.cases(df),]
unique(df$stage)
length(unique(df$aln))
quantile(df$age, probs = c(0.25, 0.5, 0.75))

length(which(df$stage == "reproductive"))
(length(which(df$stage == "reproductive"))/nrow(df))*100
quantile(df$age[df$stage == "reproductive"], probs = c(0.25, 0.5, 0.75))

length(which(df$stage == "menopause_transition"))
(length(which(df$stage == "menopause_transition"))/nrow(df))*100
quantile(df$age[df$stage == "menopause_transition"], probs = c(0.25, 0.5, 0.75))

length(which(df$stage == "post_menopause"))
(length(which(df$stage == "post_menopause"))/nrow(df))*100
quantile(df$age[df$stage == "post_menopause"], probs = c(0.25, 0.5, 0.75))



