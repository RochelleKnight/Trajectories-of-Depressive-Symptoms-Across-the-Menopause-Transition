rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")

library(dplyr)
library(tidyr)
library(data.table)

epds_colnames <- c("f","g","h","k","l","n","r","t","v","y","covid_v","mb")
epds <- read.csv(paste0(filestore,"epds_prorated_scores.csv"))
epds <- epds %>% select(aln,paste0("epds_prorated_",epds_colnames))

# Replace -1 with NA across all columns
epds <- epds %>%
  mutate(across(paste0("epds_prorated_",epds_colnames), ~ replace(., . == -1, NA)))

epds_long <- epds %>%
  pivot_longer(
    cols = starts_with("epds_prorated_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "epds_prorated"  
  )%>%
  mutate(time_point = (gsub("epds_prorated_", "", time_point)))


date_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))

date <- date_ages %>% select(aln,paste0("date_",epds_colnames))
date_long <- date %>%
  pivot_longer(
    cols = starts_with("date_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "date"  
  )%>%
  mutate(time_point = (gsub("date_", "", time_point)))

age <- date_ages %>% select(aln,paste0("age_",epds_colnames))
age_long <- age %>%
  pivot_longer(
    cols = starts_with("age_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "age"  
  )%>%
  mutate(time_point = (gsub("age_", "", time_point)))


df_long <- date_long %>% left_join(age_long)
df_long <- df_long %>% left_join(epds_long)

ids <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
ids <- ids %>% select(aln, time_point, date,age, ethnicity, social_class, education, age_menarche,
                      material_hardship,social_support,smoking_status,bmi,alcohol_intake)

ids <- ids[complete.cases(ids),]
ids <- unique(unlist(ids$aln))

df_long <- df_long %>% filter(aln %in% ids) %>%
  filter(!is.na(epds_prorated))

df_summary <- df_long %>%
  group_by(time_point) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE),
            median_epds = median(epds_prorated, na.rm = TRUE),
            q1_epds = quantile(epds_prorated, probs = c(0.25), na.rm = TRUE),
            q3_epds = quantile(epds_prorated, probs = c(0.75), na.rm = TRUE),
            min_date = min(date, na.rm = TRUE),
            max_date = max(date, na.rm = TRUE),
            median_date = median(date, na.rm = TRUE),
            n = n())

df_summary$time_point <- factor(df_summary$time_point,
                                levels = c("f","g","h","k","l","n","r","t","v","y","covid_v","mb"))

df_summary <- df_summary[order(df_summary$time_point),]

df_summary$mean_age <- round(df_summary$mean_age,1)
df_summary$sd_age <- round(df_summary$sd_age,1)
df_summary$summary <- paste0(df_summary$mean_age, " (SD = ",df_summary$sd_age,", range: ",df_summary$min_age,"-",df_summary$max_age,")")
df_summary$median_epds <- paste0(df_summary$median_epds," (",df_summary$q1_epds," - ",df_summary$q3_epds,")")

df_summary[,c("mean_age","sd_age","min_age","max_age")] <- NULL
df_summary <- df_summary[,c("time_point","summary","median_epds","n")]

write.csv(df_summary, "tables/epds_timepoints.csv",row.names = F)

# Full sample
df_long <- date_long %>% left_join(age_long)
df_long <- df_long %>% left_join(epds_long)

df_long <- df_long %>% filter(!is.na(epds_prorated))

df_summary <- df_long %>%
  group_by(time_point) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE),
            median_epds = median(epds_prorated, na.rm = TRUE),
            q1_epds = quantile(epds_prorated, probs = c(0.25), na.rm = TRUE),
            q3_epds = quantile(epds_prorated, probs = c(0.75), na.rm = TRUE),
            min_date = min(date, na.rm = TRUE),
            max_date = max(date, na.rm = TRUE),
            median_date = median(date, na.rm = TRUE),
            n = n())

df_summary$time_point <- factor(df_summary$time_point,
                                levels = c("f","g","h","k","l","n","r","t","v","y","covid_v","mb"))

df_summary <- df_summary[order(df_summary$time_point),]

df_summary$mean_age <- round(df_summary$mean_age,1)
df_summary$sd_age <- round(df_summary$sd_age,1)
df_summary$summary <- paste0(df_summary$mean_age, " (SD = ",df_summary$sd_age,", range: ",df_summary$min_age,"-",df_summary$max_age,")")
df_summary$median_epds <- paste0(df_summary$median_epds," (",df_summary$q1_epds," - ",df_summary$q3_epds,")")

df_summary[,c("mean_age","sd_age","min_age","max_age")] <- NULL
df_summary <- df_summary[,c("time_point","summary","median_epds","n")]

write.csv(df_summary, "tables/epds_timepoints_full_sample.csv",row.names = F)


