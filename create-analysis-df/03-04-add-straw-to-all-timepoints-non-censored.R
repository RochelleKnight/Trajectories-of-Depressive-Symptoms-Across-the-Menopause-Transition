# Add in STRAW categories
rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

library(lubridate)
library(dplyr)
library(R2MLwiN)
library(data.table)
library(haven)

# Read in date and age variables
date_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))

date <- date_ages %>% select(aln,starts_with("date_"))

date_long <- date %>%
  pivot_longer(
    cols = starts_with("date_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "date"  
  )%>%
  mutate(time_point = (gsub("date_", "", time_point)))

age <- date_ages %>% select(aln,starts_with("age_"))
age_long <- age %>%
  pivot_longer(
    cols = starts_with("age_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "age"  
  )%>%
  mutate(time_point = (gsub("age_", "", time_point)))

df <- date_long %>% left_join(age_long)

df <- df %>% filter(!is.na(date))

# Add in age at menopause
fmp <- readRDS(paste0(filestore,"fmp_df.rds"))
fmp <- fmp %>% select(aln, age_menopause, fmp)

df <- df %>% left_join(fmp)

# Read in STRAW df (based on LMP algorithm timepoints)
straw_long <- readRDS(paste0(filestore,"straw_stages.rds"))
table(straw_long$stage)

df$stage <- NA_character_

# Assign straw stages
id <- 32840
for (id in unique(df$aln)) {
  tmp <- df %>% filter(aln == id) %>% select(aln,date,age,age_menopause)
  tmp$stage <- NA_character_
  age_menopause <- unique(tmp$age_menopause)
  
  stage <- straw_long %>% filter(aln == id)
  
  reproductive <- which(stage$stage == "reproductive")
  menopause_transition <- which(stage$stage == "menopause_transition")
  post_menopause <- which(stage$stage == "post_menopause")
  
  if(length(reproductive) > 0){
    date_repro <- max(stage$date[reproductive])
    #date_repro <- date_repro %m+% months(12)
    tmp$stage[which(tmp$date <= date_repro)] <- "reproductive"
  }else{
    date_repro <- NA_Date_
  }
  
  if(length(menopause_transition) == 1){
    date_mt <- stage$date[menopause_transition]
    date_mt_min <- date_mt %m-% months(6)
    date_mt_max <- date_mt %m+% months(6)
    tmp$stage[which(tmp$date >= date_mt_min & tmp$date <= date_mt_max)] <- "menopause_transition"
  } 
  
  if(length(menopause_transition) > 1){
    date_mt_min <- min(stage$date[menopause_transition])
    #date_mt_min <- date_mt_min %m-% months(12)
    
    date_mt_max <- max(stage$date[menopause_transition])
    #date_mt_max <- date_mt_max %m+% months(12)
    
    tmp$stage[which(tmp$date >= date_mt_min & tmp$date <= date_mt_max)] <- "menopause_transition"
  }
  
  if(length(menopause_transition) == 0){
    date_mt_min <- NA_Date_
    date_mt_max <- NA_Date_
  }
  
  if(length(menopause_transition) > 0 & !is.na(age_menopause)){
    date_mt_min <- min(stage$date[menopause_transition])
    #date_mt_min <- date_mt_min %m-% months(12)
    tmp$stage[which(tmp$date >= date_mt_min & tmp$age <= (age_menopause+ 1))] <- "menopause_transition"
  }
  
  if(length(post_menopause) > 0){
    date_post <- min(stage$date[post_menopause])
    #date_post <- date_post %m-% months(12)
    tmp$stage[which(tmp$date >= date_post)] <- "post_menopause"
  }else{
    date_post <- NA_Date_
  }
  
  if(!is.na(age_menopause)){
    tmp$stage[which(tmp$age > (age_menopause +1))] <- "post_menopause"
  }
  
  if(!is.na(age_menopause)){
    replace <- which(is.na(tmp$stage) 
                     & tmp$age >= (age_menopause -3) 
                     & tmp$age <= (age_menopause +1))
    tmp$stage[replace] <- "menopause_transition"
  }
  
  df$stage[df$aln == id] <- tmp$stage
  
  # dates <- c(date_repro,date_mt_min,date_mt_max,date_post)
  # dates <- dates[!is.na(dates)]
  # dates
  # dates_ordered <- dates[order(dates)]
  # if(identical(dates, dates_ordered) == F){
  #   print(id)
  # }
  
}

table(df$stage)
saveRDS(df, paste0(filestore,"all_timepoints_with_straw_non_censored.rds"))

