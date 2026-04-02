rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")

library(tableone)
library(dplyr)

# Read in df
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))

df <- df %>% select(aln,date,age,age_menopause,repro_age,epds_prorated,ethnicity,social_class,education,age_menarche,
                     material_hardship,social_support,smoking_status,bmi,alcohol_intake)

df <- df[complete.cases(df), ]
length(unique(df$aln))
ids <- unique(df$aln)

cor(df$repro_age,df$age)
mean(df$age_menopause)
median(df$age_menopause)
IQR(df$age_menopause)

dup_baseline_dates <- df %>%
  group_by(aln) %>%
  mutate(first_date = min(date, na.rm = TRUE)) %>%
  filter(date == first_date) %>%      # keep only baseline records
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

dup_rows <- df %>%
  group_by(aln) %>%
  mutate(first_date = min(date, na.rm = TRUE)) %>%
  filter(date == first_date) %>%
  filter(n() > 1) %>%
  arrange(aln, date)

df_baseline <- df %>% 
  group_by(aln) %>% 
  slice_min(date)  

df_baseline <- df_baseline %>% select(aln,age_menopause,ethnicity,social_class,education,age_menarche,
                             material_hardship,social_support,smoking_status,bmi,alcohol_intake)

cat_vars <- c("ethnicity", "social_class", "education","age_menarche","smoking_status","alcohol_intake")


# Create Table 1 (one row per id assumed)
table1 <- CreateTableOne(vars = c("ethnicity", "social_class", "age_menarche", "education",
                                  "material_hardship","social_support","smoking_status","bmi","alcohol_intake"),
                         data = df_baseline,
                         factorVars = cat_vars)

print(table1, showAllLevels = TRUE)
write.csv(print(table1, showAllLevels = TRUE), "tables/table1.csv")

# STRAW population
df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))

df <- df %>% select(aln,date,age,stage,epds_prorated,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake)

df <- df[complete.cases(df), ]

dup_baseline_dates <- df %>%
  group_by(aln) %>%
  mutate(first_date = min(date, na.rm = TRUE)) %>%
  filter(date == first_date) %>%      # keep only baseline records
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

dup_rows <- df %>%
  group_by(aln) %>%
  mutate(first_date = min(date, na.rm = TRUE)) %>%
  filter(date == first_date) %>%
  filter(n() > 1) %>%
  arrange(aln, date)

df_baseline <- df %>% 
  group_by(aln) %>% 
  slice_min(date)


df_baseline <- df_baseline %>% select(aln,ethnicity,social_class,education,age_menarche,
                                      material_hardship,social_support,smoking_status,bmi,alcohol_intake)

cat_vars <- c("ethnicity", "social_class", "education","age_menarche","smoking_status","alcohol_intake")

# Create Table 1 (one row per id assumed)
table1 <- CreateTableOne(vars = c("ethnicity", "social_class", "age_menarche", "education",
                                  "material_hardship","social_support","smoking_status","bmi","alcohol_intake"),
                         data = df_baseline,
                         factorVars = cat_vars)

print(table1, showAllLevels = TRUE)
write.csv(print(table1, showAllLevels = TRUE), "tables/table1_straw.csv")

# Full sample
df <- readRDS(paste0(filestore,"analysis_df_full.rds"))

# Order variables
df$social_class <- factor(df$social_class,
                          levels = c("1","2","3","4","5","6"))

df$age_menarche <- factor(df$age_menarche,
                          levels = c("8_11_yrs","12_14_yrs","15_older_yrs"))


df$education <- factor(df$education,
                       levels = c("CSE_vocational_or_less","O_level","A-level_or_greater"))
df$anm_available <- 0
#df$anm_available[which(!is.na(df$age_menopause))] <- 1
df$anm_available[which(df$aln %in% ids)] <- 1

df <- df %>% select(aln,date,age,epds_prorated,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake,anm_available)

df <- df[complete.cases(df), ]
length(unique(df$aln))

dup_baseline_dates <- df %>%
  group_by(aln) %>%
  mutate(first_date = min(date, na.rm = TRUE)) %>%
  filter(date == first_date) %>%      # keep only baseline records
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

dup_rows <- df %>%
  group_by(aln) %>%
  mutate(first_date = min(date, na.rm = TRUE)) %>%
  filter(date == first_date) %>%
  filter(n() > 1) %>%
  arrange(aln, date)

df_baseline <- df %>% 
  group_by(aln) %>% 
  slice_min(date)

df_baseline <- df_baseline %>% select(aln,ethnicity,social_class,education,age_menarche,
                                      material_hardship,social_support,smoking_status,bmi,alcohol_intake,anm_available)

cat_vars <- c("ethnicity", "social_class", "education","age_menarche","smoking_status","alcohol_intake")

# Create Table 1 (one row per id assumed)
table1 <- CreateTableOne(vars = c("ethnicity", "social_class", "age_menarche", "education",
                                  "material_hardship","social_support","smoking_status","bmi","alcohol_intake"),
                         data = df_baseline,
                         factorVars = cat_vars,
                         strata = "anm_available")

print(table1, showAllLevels = TRUE)
write.csv(print(table1, showAllLevels = TRUE), "tables/table1_full.csv")

