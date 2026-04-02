rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

library(dplyr)
library(tidyr)
library(data.table)

# Get depression outcomes
source("02-02-depression-outcomes.R")

colnames <- c("g","h","j","k","l","n","p","r","s","t","v","y","covid_v","mb")

epds_long <- readRDS(paste0(filestore,"depression_outcomes.rds"))
epds_long <- epds_long %>% filter(time_point %in% colnames)

epds_long[,c("antidepressant","antidepressant_broad")] <- NULL

date_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))

date <- date_ages %>% select(aln,paste0("date_",colnames))

date_long <- date %>%
  pivot_longer(
    cols = starts_with("date_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "date"  
  )%>%
  mutate(time_point = (gsub("date_", "", time_point)))

age <- date_ages %>% select(aln,paste0("age_",colnames))
age_long <- age %>%
  pivot_longer(
    cols = starts_with("age_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "age"  
  )%>%
  mutate(time_point = (gsub("age_", "", time_point)))


df_long <- date_long %>% left_join(age_long)
df_long <- df_long %>% left_join(epds_long)

# Add in age at menopause and FMP variables

fmp <- readRDS(paste0(filestore,"fmp_df.rds"))
fmp <- fmp %>% select(aln, age_menopause, fmp)

df_long <- df_long %>% left_join(fmp)

# Read in confounders
confounders <- readRDS(paste0(filestore,"confounders.rds"))
date_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))

# Baseline confounders
baseline_confounders <- confounders %>% select(aln,ethnicity,social_class,education,age_menarche,history_depression_d,ever_hrt)

df_long <- df_long %>% left_join(baseline_confounders)

# Time-varying confounders
setDT(df_long)

#### Material hardship
material_hardship <- confounders %>% select(aln,paste0("material_hardship_",c("c","f","g","h","k","m","t"))) %>%
  left_join(date_ages %>% select(aln,paste0("date_",c("c","f","g","h","k","m","t"))))

material_hardship <- material_hardship %>%
  mutate(across(
    all_of(paste0("material_hardship_", c("c", "f", "g", "h", "k", "m", "t"))),
    ~ as.numeric(haven::zap_labels(.))
  ))

# Material hardship timevarying
material_hardship_long <- material_hardship %>%
  pivot_longer(
    cols = starts_with(c("material_hardship_", "date_")),
    names_to = c(".value", "time_point"),
    names_pattern = "^(.*)_(.*)$"
  )

rm(material_hardship)

# Convert to data.table for speed
setDT(material_hardship_long)

# Perform a rolling join to find the closest date <= df_long$date
# Ensure `material_hardship_long` is sorted by `aln` and `date` for the join
material_hardship_long <- material_hardship_long[which(!is.na(material_hardship_long$date) & !is.na(material_hardship_long$material_hardship)),]
setkey(material_hardship_long, aln, date)

# Perform a rolling join
df_long[, material_hardship := material_hardship_long[df_long, 
                                                      on = .(aln, date <= date), 
                                                      mult = "last", 
                                                      x.material_hardship]]

which(!is.na(df_long$date) & is.na(df_long$material_hardship))

#### Social support
social_support_cols <- colnames(confounders)[grep("social_support",colnames(confounders))]
social_support_cols <- gsub("social_support_","",social_support_cols)

social_support <- confounders %>% select(aln,paste0("social_support_",social_support_cols)) %>%
  left_join(date_ages %>% select(aln,paste0("date_",social_support_cols)))

social_support <- social_support %>%
  mutate(across(
    all_of(paste0("social_support_", social_support_cols)),
    ~ as.numeric(haven::zap_labels(.))
  ))

# Social support time-varying
social_support_long <- social_support %>%
  pivot_longer(
    cols = starts_with(c("social_support_", "date_")),
    names_to = c(".value", "time_point"),
    names_pattern = "^(.*)_(.*)$"
  )

rm(social_support)

# Convert to data.table for speed
setDT(social_support_long)

# Perform a rolling join to find the closest date <= df_long$date
# Ensure `social_support_long` is sorted by `aln` and `date` for the join
social_support_long <- social_support_long[which(!is.na(social_support_long$date) & !is.na(social_support_long$social_support)),]
setkey(social_support_long, aln, date)

# Perform a rolling join
df_long[, social_support := social_support_long[df_long, 
                                                on = .(aln, date <= date), 
                                                mult = "last", 
                                                social_support]]

which(!is.na(df_long$date) & is.na(df_long$social_support))

#### Smoking status
smoking_status_cols <- colnames(confounders)[grep("smoking_status_(?!current|ever|never)", colnames(confounders), perl = TRUE)]
smoking_status_cols <- smoking_status_cols[-which(smoking_status_cols == "smoking_status_l_weekday" | smoking_status_cols == "smoking_status_l_weekend")]
smoking_status_cols <- gsub("smoking_status_","",smoking_status_cols)

smoking_status <- confounders %>% select(aln,paste0("smoking_status_",smoking_status_cols)) %>%
  left_join(date_ages %>% select(aln,paste0("date_",smoking_status_cols)))

# Smoking status time-varying
smoking_status_long <- smoking_status %>%
  pivot_longer(
    cols = starts_with(c("smoking_status_", "date_")),
    names_to = c(".value", "time_point"),
    names_pattern = "^(.*)_(.*)$"
  )

rm(smoking_status)

# Sense check smoking status
for(i in unique(smoking_status_long$aln)){
  smoking_min <- smoking_status_long$date[which(smoking_status_long$aln == i
                       & smoking_status_long$smoking_status %in% c("Current","Ever"))]
  
  if(length(smoking_min) > 0 ){
    smoking_min <- min(smoking_min, na.rm = T)
    replace <- which(smoking_status_long$aln == i
                     & smoking_status_long$smoking_status == "Never"
                     & smoking_status_long$date > smoking_min)
    
    smoking_status_long$smoking_status[replace] <- "Ever"
    
    replace <- which(smoking_status_long$aln == i
                     & is.na(smoking_status_long$smoking_status)
                     & smoking_status_long$date > smoking_min)
    
    smoking_status_long$smoking_status[replace] <- "Ever"
  }
}

# Convert to data.table for speed
setDT(smoking_status_long)

# Perform a rolling join to find the closest date <= df_long$date
# Ensure `smoking_status_long` is sorted by `aln` and `date` for the join
smoking_status_long <- smoking_status_long[which(!is.na(smoking_status_long$date) & !is.na(smoking_status_long$smoking_status)),]
setkey(smoking_status_long, aln, date)

# Perform a rolling join
df_long[, smoking_status := smoking_status_long[df_long, 
                                                on = .(aln, date <= date), 
                                                mult = "last", 
                                                smoking_status]]

which(!is.na(df_long$date) & is.na(df_long$smoking_status))

#### BMI
bmi_cols <- colnames(confounders)[grep("bmi_", colnames(confounders))]
bmi_cols
bmi_cols <- gsub("bmi_","",bmi_cols)

bmi <- confounders %>% select(aln,paste0("bmi_",bmi_cols)) %>%
  left_join(date_ages %>% select(aln,paste0("date_",bmi_cols)))

# BMI time-varying
bmi_long <- bmi %>%
  pivot_longer(
    cols = starts_with(c("bmi_", "date_")),
    names_to = c(".value", "time_point"),
    names_pattern = "^(.*)_(.*)$"
  )

rm(bmi)

# Convert to data.table for speed
setDT(bmi_long)

# Perform a rolling join to find the closest date <= df_long$date
# Ensure `bmi_long` is sorted by `aln` and `date` for the join
bmi_long <- bmi_long[which(!is.na(bmi_long$date) & !is.na(bmi_long$bmi)),]
setkey(bmi_long, aln, date)

# Perform a rolling join
df_long[, bmi := bmi_long[df_long,
                          on = .(aln, date <= date), 
                          mult = "last", 
                          bmi]]
                                               
which(!is.na(df_long$date) & is.na(df_long$bmi))

#### Alcohol intake
alcohol_intake_cols <- colnames(confounders)[grep("alcohol_intake_", colnames(confounders))]
alcohol_intake_cols
alcohol_intake_cols <- gsub("alcohol_intake_","",alcohol_intake_cols)

alcohol_intake <- confounders %>% select(aln,paste0("alcohol_intake_",alcohol_intake_cols)) %>%
  left_join(date_ages %>% select(aln,paste0("date_",alcohol_intake_cols)))


# Alcohol intake time-varying
alcohol_intake_long <- alcohol_intake %>%
  pivot_longer(
    cols = starts_with(c("alcohol_intake_", "date_")),
    names_to = c(".value", "time_point"),
    names_pattern = "^(.*)_(.*)$"
  )

rm(alcohol_intake)

# Convert to data.table for speed
setDT(alcohol_intake_long)

# Perform a rolling join to find the closest date <= df_long$date
# Ensure `alcohol_intake_long` is sorted by `aln` and `date` for the join
alcohol_intake_long <- alcohol_intake_long[which(!is.na(alcohol_intake_long$date) & !is.na(alcohol_intake_long$alcohol_intake)),]
setkey(alcohol_intake_long, aln, date)

# Perform a rolling join
df_long[, alcohol_intake := alcohol_intake_long[df_long,
                                               on = .(aln, date <= date), 
                                               mult = "last", 
                                               alcohol_intake]]

which(!is.na(df_long$date) & is.na(df_long$alcohol_intake))

#### HRT
hrt_cols <- colnames(confounders)[grep("hrt_", colnames(confounders))]
hrt_cols
hrt_cols <- gsub("hrt_","",hrt_cols)

hrt <- confounders %>% select(aln,paste0("hrt_",hrt_cols)) %>%
  left_join(date_ages %>% select(aln,paste0("date_",hrt_cols)))

hrt_long <- hrt %>%
  pivot_longer(
    cols = starts_with(c("hrt_", "date_")),
    names_to = c(".value", "time_point"),
    names_pattern = "^(.*)_(.*)$"
  )

rm(hrt)

# Convert to data.table for speed
setDT(hrt_long)

# Perform a rolling join to find the closest date <= df_long$date
# Ensure `hrt_long` is sorted by `aln` and `date` for the join
hrt_long <- hrt_long[-which(is.na(hrt_long$date)),]
setkey(hrt_long, aln, date)

# Perform a rolling join
df_long[, hrt := hrt_long[df_long,
                          on = .(aln, date <= date), 
                          mult = "last", 
                          hrt]]

which(!is.na(df_long$date) & is.na(df_long$hrt))

saveRDS(hrt_long, paste0(filestore,"hrt_long_format.rds"))

saveRDS(df_long, paste0(filestore,"df_full_all_depression.rds"))

# Analysis df for trajectory analysis
df_long <- readRDS(paste0(filestore,"df_full_all_depression.rds"))
df_long_trajec <- df_long %>% filter(time_point %in% c("g","h","k","l","n","r","t","v","y","covid_v","mb")) %>% 
  rename(timepoint_name =  time_point)

# Fix timepoints
# Number timepoints from 1 - 12 and ensure they are date ordered
tmp <- df_long_trajec %>% filter(!is.na(date))

tmp_ordered <- tmp %>% arrange(aln,date)

violating_ids <- unique(tmp$aln[which(tmp$time_point != tmp_ordered$time_point)])


df_long_trajec$time_point <- rep(1:11,length(unique(df_long_trajec$aln)))

for(i in violating_ids){
  tmp <- df_long_trajec %>% filter(aln == i & !is.na(date)) %>% arrange(date)
  
  replace <- sort(tmp$time_point,decreasing = FALSE)
  tmp$time_point <- replace
  
  df_long_trajec <- df_long_trajec[-which(df_long_trajec$aln == i & df_long_trajec$time_point %in% replace),]
  
  df_long_trajec <- rbind(df_long_trajec,tmp)
}

df_long_trajec <- df_long_trajec %>% arrange(aln,time_point)%>%
  relocate(time_point, .after = aln)

# Add in reproductive age
df_long_trajec$repro_age <- df_long_trajec$age - df_long_trajec$age_menopause
length(which(!is.na(df_long_trajec$repro_age)))
length(which(!is.na(df_long_trajec$age_menopause)))

df_long_trajec <- df_long_trajec %>%
  relocate(repro_age, .after = epds_binary_with_meds_broad)

# Format all variables
colnames(df_long_trajec)

# Date variables
df_long_trajec$date <- as.Date(df_long_trajec$date)
df_long_trajec$fmp <- as.Date(df_long_trajec$fmp)

# Numeric variables
df_long_trajec$epds_prorated <- as.numeric(df_long_trajec$epds_prorated)
df_long_trajec$epds_binary <- as.numeric(df_long_trajec$epds_binary)
df_long_trajec$epds_binary_with_meds <- as.numeric(df_long_trajec$epds_binary_with_meds)
df_long_trajec$epds_binary_with_meds_broad <- as.numeric(df_long_trajec$epds_binary_with_meds_broad)

df_long_trajec$repro_age <- as.numeric(df_long_trajec$repro_age)
df_long_trajec$age_menopause <- as.numeric(df_long_trajec$age_menopause)
df_long_trajec$material_hardship <- as.numeric(df_long_trajec$material_hardship)
df_long_trajec$social_support <- as.numeric(df_long_trajec$social_support)
df_long_trajec$bmi <- as.numeric(df_long_trajec$bmi)

# Factor variables
df_long_trajec$ethnicity <- factor(df_long_trajec$ethnicity,
                                   levels = c("White","Non_white"))

df_long_trajec$social_class <- factor(df_long_trajec$social_class,
                                      levels = c("4","1","2","3","5","6"))

df_long_trajec$education <- factor(df_long_trajec$education,
                                   levels = c("A-level_or_greater","CSE_vocational_or_less","O_level"))

df_long_trajec$age_menarche <- factor(df_long_trajec$age_menarche,
                                      levels = c("12_14_yrs","8_11_yrs","15_older_yrs"))

df_long_trajec$smoking_status <- factor(df_long_trajec$smoking_status,
                                        levels = c("Never","Ever","Current"))

df_long_trajec$alcohol_intake <- factor(df_long_trajec$alcohol_intake,
                                        levels = c("Never or ≤4 times/month","2–3 times/week","≥ 4 times/week"))

df_long_trajec$hrt <- factor(df_long_trajec$hrt,
                             levels = c("Never","Ever","Current"))

df_long_trajec$history_depression_d <- factor(df_long_trajec$history_depression_d,
                                              levels = c("0","1"))

df_long_trajec$history_reproductive_depression <- factor(df_long_trajec$history_reproductive_depression,
                                              levels = c("0","1"))

df_long_trajec$history_reproductive_antidepressants <- factor(df_long_trajec$history_reproductive_antidepressants,
                                                         levels = c("0","1"))

# Center age
df_long_trajec$centered_age <- df_long_trajec$age - 50

saveRDS(df_long_trajec, paste0(filestore,"analysis_df_full.rds"))
#df_long_trajec <- readRDS(paste0(filestore,"analysis_df_full.rds"))

# Age at menopause analysis
# Has an age at menopause
df_long_age_meno <- df_long_trajec %>% filter(!is.na(age_menopause) & age_menopause >=40 & age_menopause < 60)

nrow(df_long_age_meno)/11
table(df_long_age_meno$time_point)

# Has at least one EPDS score reported
ids_with_epds <- df_long_age_meno %>%
  group_by(aln) %>%
  summarise(has_non_na_epds = any(!is.na(epds_prorated)), .groups = "drop") %>%
  filter(has_non_na_epds) %>%
  pull(aln) # Extract the IDs

df_long_age_meno <- df_long_age_meno %>% filter(aln %in% ids_with_epds)

length(unique(df_long_age_meno$aln))
nrow(df_long_age_meno)/11
table(df_long_age_meno$time_point)

saveRDS(df_long_age_meno, paste0(filestore,"analysis_df_complete_case_epds.rds"))
