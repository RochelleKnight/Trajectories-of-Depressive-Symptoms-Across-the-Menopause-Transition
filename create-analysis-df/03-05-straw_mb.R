# Questionnaire MB STRAW stages

source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

library(haven)
library(dplyr)
library(lubridate)   
library(tidyr)

df_mb <- readRDS(paste0(filestore,"cohort_negatives_set_to_na.rds"))

# MB4600: When participant had their last menstrual period
# 1: In the past 3 months
# 2: 4-12 months ago
# 3: More than 12 months ago

# Period in the last 3 months
unique(df_mb$MB4600)
df_mb <- df_mb %>% mutate(period_history_mb = case_when(MB4600 == 1 ~ "last_3_months",
                                                  MB4600 == 2 ~ "4_12_months_ago",
                                                  MB4600 == 3 ~ "more_12_months",
                                                  TRUE ~ NA_character_))
table(df_mb$period_history_mb)

# Exclude timepoint
# MB4610: Participant's menstrual periods stopped as they had hysterectomy
# MB4660: Participant's menstrual periods stopped for other reason
# MB4670: Participant's menstrual periods stopped for unknown reason

unique(df_mb$MB4610)
unique(df_mb$MB4660)
unique(df_mb$MB4670)
unique(df_mb$MB4650)

df_mb <- df_mb %>% mutate(exclude_mb = case_when(MB4610 == 1 | MB4660 == 1 | MB4670 == 1 | MB4620 == 1 | MB4630 == 1 ~ 1,
                                           TRUE~ NA_integer_))
table(df_mb$exclude_mb)

# Pregnant or breast-feeding
# MB4620: Participant's menstrual periods stopped as they were/are pregnant
# MB4630: Participant's menstrual periods stopped as they were/are breastfeeding
tmp <- df_mb %>% filter(MB4620 == 1 | MB4630 == 1) %>% select(MB4680, age_mb)

df_mb <- df_mb %>% mutate(prenant_breastfeeding_mb = case_when(MB4620 == 1 | MB4630 == 1 ~ 1,
                                                         TRUE ~ NA_integer_))
table(df_mb$prenant_breastfeeding_mb)

# Currently using HRT
#MB4790: Participant uses HRT nowadays
#MB4800: Participant uses HRT tablets
#MB4810: Participant uses HRT patches
#MB4820: Participant uses HRT cream

df_mb <- df_mb %>% mutate(hrt_mb = case_when(MB4790 == 1 | MB4800 == 1 | MB4810 == 1 | MB4820 == 1 ~ 1,
                                       TRUE ~ NA_integer_))
table(df_mb$hrt_mb)

# Currenlty using hormonal coil
# MB4640: Participant's menstrual periods stopped as they had a hormonal Coil
unique(df_mb$MB4640)
df_mb <- df_mb %>% mutate(contracept_mb = case_when(MB4640 == 1 ~ 1,
                                              TRUE ~ NA_integer_))
table(df_mb$contracept_mb)

# Menopausal status
# MB4680: Best description of participant's current menopause status
unique(df_mb$MB4680)

df_mb <- df_mb %>% mutate(menopause_status = case_when(MB4680 == 1 ~ "post_menopause",
                                                 MB4680 == 2 ~ "menopause_transition",
                                                 MB4680 == 3 ~ "reproductive",
                                                 TRUE ~ NA_character_))
table(df_mb$menopause_status,df_mb$period_history_mb)

df_mb <- df_mb %>% mutate(straw_mb = case_when(menopause_status == "reproductive" & period_history_mb == "last_3_months" ~ "reproductive",
                                         menopause_status == "reproductive" & period_history_mb  %in% c("4_12_months_ago","more_12_months") ~ "menopause_transition",
                                         menopause_status == "reproductive" & is.na(period_history_mb) ~ "reproductive",
                                         menopause_status == "menopause_transition" & period_history_mb %in% c("last_3_months","4_12_months_ago") ~ "menopause_transition",
                                         menopause_status == "menopause_transition" & is.na(period_history_mb) ~ "menopause_transition",
                                         menopause_status == "menopause_transition" & period_history_mb == "more_12_months" ~ "menopause_transition",
                                         menopause_status == "post_menopause" & period_history_mb == "more_12_months" ~ "post_menopause",
                                         menopause_status == "post_menopause" & is.na(period_history_mb) ~ "post_menopause",
                                         TRUE ~ NA_character_
                                         ))


table(df_mb$straw_mb)
table(df_mb$menopause_status,df_mb$period_history_mb)

df_mb$straw_mb[which(df_mb$exclude_mb == 1)] <- NA_character_

# Change for those reporting HRT or contraceptives
df_mb$straw_mb[which(df_mb$hrt_mb == 1)] <- df_mb$menopause_status[which(df_mb$hrt_mb == 1)]
df_mb$straw_mb[which(df_mb$contracept_mb == 1)] <- df_mb$menopause_status[which(df_mb$contracept_mb == 1)]

table(df_mb$straw_mb)

straw_mb <- df_mb %>% select(aln,straw_mb,date_mb,age_mb)
