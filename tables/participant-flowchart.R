rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")

library(dplyr)
library(tidyr)
library(data.table)

# Has at least one EPDS response
epds_colnames <- c("g","h","k","l","n","r","t","v","y","covid_v","mb")
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

epds_long <- epds_long %>% filter(!is.na(epds_prorated))
length(unique(epds_long$aln))

# Count number of EPDS scores per person
epds_counts <- epds_long %>%
  group_by(aln) %>%
  summarise(n_epds = n())

# Number of people with at least 3 scores
sum(epds_counts$n_epds >= 3)
sum(epds_counts$n_epds >= 11)

# Has an age at menopause
fmp <- readRDS(paste0(filestore,"fmp_df.rds"))
fmp <- fmp %>% filter(!is.na(age_menopause))
length(fmp$aln)

id1 <- unique(epds_long$aln)
id2 <- fmp$aln
length(intersect(id1,id2))
 
tmp <- fmp %>% filter(aln %in% intersect(id1,id2))
length(which(tmp$age_menopause <40))
length(which(tmp$age_menopause >=60))

# STRAW analysis
straw1 <- readRDS(paste0(filestore,"straw_stages.rds"))
straw2 <- readRDS(paste0(filestore,"all_timepoints_with_straw_non_censored.rds"))
straw2 <- straw2 %>% filter(!is.na(stage))
straw_ids <- unique(c(straw1$aln,straw2$aln))
length(straw_ids)

# Those with both stage and EPDS
straw2 <- straw2 %>% filter(time_point %in% epds_colnames)
straw2 <- straw2 %>% left_join(epds_long) %>% filter(!is.na(epds_prorated))
length(unique(straw2$aln))
ids <- unique(straw2$aln)

# Read in df with repeat EPDS measures
df <- readRDS(paste0(filestore,"analysis_df_full.rds"))
straw2 <- readRDS(paste0(filestore,"all_timepoints_with_straw_non_censored.rds"))
straw2 <- straw2 %>% select(aln,time_point,stage)%>% rename(timepoint_name = time_point)
df <- df %>% filter(aln %in% ids) %>% left_join(straw2)


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

df <- df %>% filter(!is.na(epds_prorated) & !is.na(stage))
length(unique(df$aln))
5825-length(unique(df$aln))

# Missing confounder data
5825-119-4946
