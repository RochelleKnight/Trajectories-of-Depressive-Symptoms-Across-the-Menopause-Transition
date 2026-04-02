# Add in STRAW categories
rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

source("create-analysis-df/03-02-surgical-menopause.R")
source("create-analysis-df/03-03-straw-stages-for-lmp-timepoints.R")

setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")
source("create-analysis-df/03-04-add-straw-to-all-timepoints.R")


library(lubridate)
library(dplyr)
library(R2MLwiN)
library(data.table)
library(haven)

options(MLwiN_path = "C:/Program Files/MLwiN v3.13/mlwin.exe")

# Read in df with repeat EPDS measures
df <- readRDS(paste0(filestore,"analysis_df_full.rds"))
length(unique(df$aln))
table(df$age_menopause)

# Censor those who have had surgical menopause
surgical_menopause <- readRDS(paste0(filestore,"whole_cohort_surgical_menopause.rds"))

df <- df %>% left_join(surgical_menopause)
unique(df$exclude_participant)

# Exclude those participants who reported surgical menopause but gave no date or age at surgery
df <- df %>% filter(exclude_participant == 0)

# Censor all timepoints that follow surgical menopause
replace <- which(df$date >= df$periods_stopped_surgery_date)
colnames <- colnames(df)
colnames <- colnames[-which(colnames %in% c("aln","time_point","age_menopause","fmp","ethnicity","social_class","education","age_menarche","ever_hrt",
                                            "periods_stopped_surgery","periods_stopped_surgery_date","periods_stopped_surgery_age"))]

df[replace,colnames] <- NA

replace <- which(df$age >= df$periods_stopped_surgery_age)
df[replace,colnames] <- NA

df[,c("periods_stopped_surgery","periods_stopped_surgery_date","periods_stopped_surgery_age","exclude_participant")] <- NULL

length(unique(df$aln))
nrow(df)/11
table(df$time_point)

# Read in STRAW df (based on LMP algorithm timepoints)
straw_long <- readRDS(paste0(filestore,"all_timepoints_with_straw.rds"))
table(straw_long$stage)

straw_long <- straw_long %>% select(aln,time_point,stage) %>%
  rename(timepoint_name = time_point)

df <- df %>% left_join(straw_long)
saveRDS(df, paste0(filestore,"df_with_straw_full.rds"))

table(df$stage)

valid_ids <- df %>%
  group_by(aln) %>%
  summarise(valid_count = sum(!is.na(epds_prorated) & !is.na(stage)), .groups = "drop") %>%
  filter(valid_count >= 1) %>%
  pull(aln)

tmp <- df %>% filter(aln %in% valid_ids)

length(which(!is.na(tmp$epds_prorated) & !is.na(tmp$stage))) / length(which(!is.na(tmp$epds_prorated)))

length(unique(tmp$aln))

table(tmp$stage)
df_long <- tmp

# Date variables
df_long$date <- as.Date(df_long$date)
df_long$fmp <- as.Date(df_long$fmp)

# Numeric variables
df_long$epds_prorated <- as.numeric(df_long$epds_prorated)
df_long$epds_binary <- as.numeric(df_long$epds_binary)
df_long$epds_binary_with_meds <- as.numeric(df_long$epds_binary_with_meds)
df_long$epds_binary_with_meds_broad <- as.numeric(df_long$epds_binary_with_meds_broad)

df_long$repro_age <- as.numeric(df_long$repro_age)
df_long$age_menopause <- as.numeric(df_long$age_menopause)
df_long$material_hardship <- as.numeric(df_long$material_hardship)
df_long$social_support <- as.numeric(df_long$social_support)
df_long$bmi <- as.numeric(df_long$bmi)

# Factor variables#
df_long$stage <- factor(df_long$stage,
                        levels = c("reproductive","menopause_transition","post_menopause"))

df_long$ethnicity <- factor(df_long$ethnicity,
                            levels = c("White","Non_white"))

df_long$social_class <- factor(df_long$social_class,
                               levels = c("1","2","3","4","5","6"))

df_long$education <- factor(df_long$education,
                            levels = c("CSE_vocational_or_less","O_level","A-level_or_greater"))

df_long$age_menarche <- factor(df_long$age_menarche,
                               levels = c("8_11_yrs","12_14_yrs","15_older_yrs"))

df_long$smoking_status <- factor(df_long$smoking_status,
                                 levels = c("Never","Ever","Current"))


df_long$alcohol_intake <- factor(df_long$alcohol_intake,
                                 levels = c("Never or ≤4 times/month","2–3 times/week","≥ 4 times/week"))

df_long$hrt <- factor(df_long$hrt,
                      levels = c("Never","Ever","Current"))


df_long$history_reproductive_depression <- factor(df_long$history_reproductive_depression,
                                                         levels = c("0","1"))

df_long$history_reproductive_antidepressants <- factor(df_long$history_reproductive_antidepressants,
                                                              levels = c("0","1"))

saveRDS(df_long, paste0(filestore,"analysis_df_with_straw_epds.rds"))
