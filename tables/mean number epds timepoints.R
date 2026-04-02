rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")
library(dplyr)

df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
df <- df %>% select(aln,epds_prorated,repro_age,centered_age,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake)

df_complete <- df[complete.cases(df), ]

df_complete %>%
  count(aln) %>%                     # count rows per ID
  summarise(
    mean_rows = mean(n),
    sd_rows = sd(n),
    median_rows = median(n),
    min_rows = min(n),
    max_row = max(n),
    iqr_rows = IQR(n),
    q1 = quantile(n, 0.25),
    q3 = quantile(n, 0.75)
  )

min(df_complete$repro_age)
max(df_complete$repro_age)

df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df <- df %>% select(aln,epds_prorated,centered_age,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake,stage)

df <- df %>% filter(stage == "menopause_transition")

df_complete <- df[complete.cases(df), ]

df_complete %>%
  count(aln) %>%                     # count rows per ID
  summarise(
    mean_rows = mean(n),
    sd_rows = sd(n)
  )


df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df <- df %>% select(aln,epds_prorated,centered_age,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake,stage)

df <- df %>% filter(stage == "post_menopause")

df_complete <- df[complete.cases(df), ] 

df_complete %>%
  count(aln) %>%                     # count rows per ID
  summarise(
    mean_rows = mean(n),
    sd_rows = sd(n)
  )

df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df <- df %>% select(aln,epds_prorated,centered_age,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake,stage)

df <- df %>% filter(stage == "reproductive")

df_complete <- df[complete.cases(df), ]

df_complete %>%
  count(aln) %>%                     # count rows per ID
  summarise(
    mean_rows = mean(n),
    sd_rows = sd(n)
  )

df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df <- df %>% select(aln,epds_prorated,age,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake,stage)

df_complete <- df[complete.cases(df), ]
prop.table(table(df_complete$stage))*100

median(df_complete$age[df_complete$stage == "reproductive"])
median(df_complete$age[df_complete$stage == "menopause_transition"])
median(df_complete$age[df_complete$stage == "post_menopause"])

# Age at menopause categories
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))

df <- df %>% select(aln,epds_prorated,age_menopause,age,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake)

df_complete <- df[complete.cases(df), ]

df_complete <- df_complete %>% mutate(age_meno_cat = case_when(age_menopause < 45 ~ "early",
                                                               age_menopause >=45 & age_menopause <=55 ~ "normal",
                                                               age_menopause > 55 ~ "late"))

table(df_complete$age_meno_cat)

tmp <- df_complete %>% select(aln,age_meno_cat) %>% distinct()
table(tmp$age_meno_cat)

# Stage - age correlation
df <- readRDS(paste0(filestore,"analysis_df_with_straw_epds.rds"))
df <- df %>% select(aln,epds_prorated,age,ethnicity,social_class,education,age_menarche,
                    material_hardship,social_support,smoking_status,bmi,alcohol_intake,stage)
df <- df[complete.cases(df),]

ggplot(df) + geom_boxplot(aes(x = factor(stage), y = age))  

kruskal.test(df$age~df$stage)
