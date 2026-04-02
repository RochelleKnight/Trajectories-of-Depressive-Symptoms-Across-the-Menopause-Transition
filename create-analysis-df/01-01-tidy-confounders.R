rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

source("01-02-social-support-function.R")
library(haven)
library(dplyr)
 
# Load cohort data set
df <- read_dta(paste0(filestore,"cohort.dta"))
df <- as.data.frame(df)

# Add in EPDS variables
epds_colnames <- c("g","h","k","l","n","r","t","v","y","covid_v","mb")
epds <- read.csv(paste0(filestore,"epds_prorated_scores.csv"))
epds <- epds %>% select(aln,paste0("epds_prorated_",epds_colnames))

# Replace -1 with NA across all columns
epds <- epds %>%
  mutate(across(paste0("epds_prorated_",epds_colnames), ~ replace(., . == -1, NA)))

df <- df %>% left_join(epds)

# Add in date columns
date_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))

date <- date_ages %>% select(aln,paste0("date_",epds_colnames))

df <- df %>% left_join(date)
rm(date_ages, date, epds)
########################## Baseline confounders ################################

#### Ethnicity
# Questionnaire C
unique(df$c800)
table(df$c800)
df <- df %>% mutate(ethnicity_c = case_when(c800 == 1 ~ "White",
                                             c800 > 1 & c800 <=9 ~ "Non_white",
                                             TRUE ~ NA))
table(df$ethnicity_c, useNA = "always")

# Questionnaire N
unique(df$n4180)
table(df$n4180)
df <- df %>% mutate(ethnicity_n = case_when(n4180 == 1 ~ "White",
                                            n4180 > 1 & n4180 <=9 ~ "Non_white",
                                            TRUE ~ NA))
table(df$ethnicity_n, useNA = "always")

#which(!is.na(df$ethnicity_c) & !is.na(df$ethnicity_n) & df$ethnicity_c != df$ethnicity_n)
#df$c800[which(!is.na(df$ethnicity_c) & !is.na(df$ethnicity_n) & df$ethnicity_c != df$ethnicity_n)]
#df$n4180[which(!is.na(df$ethnicity_c) & !is.na(df$ethnicity_n) & df$ethnicity_c != df$ethnicity_n)]

df$ethnicity <- ifelse(!is.na(df$ethnicity_c), df$ethnicity_c, df$ethnicity_n)

df <- df %>% mutate(ethnicity = factor(ethnicity,
                                         levels = c("White","Non_white")))
table(df$ethnicity, useNA = "always")

### Social class
unique(df$c755)
table(df$c755)
unique(df$c765)
table(df$c765)

df <- df %>%
  mutate(
    social_class_maternal = case_when(
      c755 < 0 | c755 > 6 ~ NA_integer_,
      TRUE ~ as.numeric(c755)
    ),
    social_class_paternal = case_when(
      c765 < 0 | c765 > 6 ~ NA_integer_,
      TRUE ~ as.numeric(c765)
    ),
    social_class = case_when(
      !is.na(social_class_maternal) | !is.na(social_class_paternal) ~ pmax(social_class_maternal,social_class_paternal,na.rm = T),
      TRUE ~ NA_integer_
    )
  )


table(df$social_class, useNA = "always")

### Education
unique(df$c645a)
table(df$c645a)
df$c645a <- as.numeric(df$c645a)

source("01-02-recode-education-confounder.R")

df <- df %>% mutate(education = case_when(is.na(c645a) & is.na(education_k) & is.na(education_n) ~ NA_integer_,
                                          TRUE ~ pmax(c645a,education_k,education_n,na.rm = T))) %>%
  mutate(education = case_when(education %in% c("4","5", "6") ~ "A-level_or_greater",
                               education == "3" ~ "O_level",
                               education %in% c("0", "1", "2") ~ "CSE_vocational_or_less",
                               TRUE ~ NA_character_)) %>%
  mutate(education = factor(education, 
                            levels = c("A-level_or_greater","O_level","CSE_vocational_or_less")))

table(df$education)

### Age at menarche
# Questionnaire D
table(df$d010a)
df <- df %>% 
  mutate(d010a = as.character(d010a)) %>%
  mutate(age_menarche_d = case_when(d010a == "1" ~ "8_11_yrs",
                                  d010a == "2" ~ "12_14_yrs",
                                  d010a == "3" ~ "15_older_yrs",
                                  TRUE ~ NA_character_)) %>%
  mutate(age_menarche_d = factor(age_menarche_d,
                               levels = c("8_11_yrs","12_14_yrs","15_older_yrs")))

table(df$age_menarche_d)

## Questionnaire N
table(df$n1120)
df <- df %>% 
  mutate(age_menarche_n = case_when(n1120 >= 8 & n1120 <= 11 ~ "8_11_yrs",
                                    n1120 >= 12 & n1120 <= 14 ~ "12_14_yrs",
                                    n1120 >= 15 & n1120 < 99  ~ "15_older_yrs",
                                    TRUE ~ NA_character_)) %>%
  mutate(age_menarche_n = factor(age_menarche_n,
                                 levels = c("8_11_yrs","12_14_yrs","15_older_yrs")))

table(df$age_menarche_n, useNA = "always")

## Questionnaire R
table(df$r2080)
df <- df %>% 
  mutate(age_menarche_r = case_when(r2080 >= 8 & r2080 <= 11 ~ "8_11_yrs",
                                    r2080 >= 12 & r2080 <= 14 ~ "12_14_yrs",
                                    r2080 >= 15 & r2080 < 99  ~ "15_older_yrs",
                                    TRUE ~ NA_character_)) %>%
  mutate(age_menarche_r = factor(age_menarche_r,
                                 levels = c("8_11_yrs","12_14_yrs","15_older_yrs")))

table(df$age_menarche_r, useNA = "always")

# Overall
df <- df %>%
  mutate(age_menarche = coalesce(age_menarche_d, age_menarche_n, age_menarche_r))

table(df$age_menarche, useNA = "always")

### History of depression
# Questionnaire D
table(df$d171)
unique(df$d171)

df <- df %>% mutate(history_depression_d = case_when(d171 %in% c(1,2) ~ 1,
                                                   d171 == -1 ~ NA_integer_,
                                                   TRUE ~ 0))

table(df$history_depression_d, useNA = "always")

### Material hardship
# Questionnaire C
table(df$c525, useNA = "always")

df <- df %>% mutate(material_hardship_c = case_when(c525 < 0 | c525 >15 ~ NA,
                                                    TRUE ~ c525))

table(df$material_hardship_c, useNA = "always")

# Questionnaire F
table(df$f805)
df <- df %>% mutate(material_hardship_f = case_when(f805 < 0 | f805 >15 ~ NA,
                                                    TRUE ~ f805))

table(df$material_hardship_f, useNA = "always")

# Questionnaire G
table(df$g840,useNA = "always")
table(df$g835,useNA = "always")
table(df$g836,useNA = "always")
table(df$g837,useNA = "always")
table(df$g838,useNA = "always")
table(df$g839,useNA = "always")

df <- df %>% mutate(across(c("g835", "g836", "g837", "g838", "g839"), 
                           ~ case_when(
                             . < 0 ~ NA_real_,        
                             . == 5 ~ 4,              
                             TRUE ~ .             
                           )))

df$material_hardship_g <- 20 - df$g839 -df$g838 - df$g837 - df$g836 - df$g835

rows_with_na <- which(rowSums(is.na(select(df, g835, g836, g837, g838, g839))) > 0)
df$material_hardship_g[rows_with_na] <- NA

table(df$material_hardship_g, useNA = "always")

# Questionnaire H
table(df$h735,useNA = "always")

df <- df %>% mutate(material_hardship_h = case_when(h735 < 0 | h735 >15 ~ NA,
                                                    TRUE ~ h735))

table(df$material_hardship_h, useNA = "always")

# Questionnaire K
table(df$k6200,useNA = "always")
table(df$k6201,useNA = "always")
table(df$k6202,useNA = "always")
table(df$k6203,useNA = "always")
table(df$k6204,useNA = "always")

df <- df %>% mutate(across(c("k6200", "k6201", "k6202", "k6203", "k6204"), 
                           ~ case_when(
                             . < 0 ~ NA_real_,        
                             . == 5 ~ 4,              
                             TRUE ~ .             
                           )))

df$material_hardship_k <- 20 - df$k6200 -df$k6201 - df$k6202 - df$k6203 - df$k6204

rows_with_na <- which(rowSums(is.na(select(df, k6200, k6201, k6202, k6203, k6204))) > 0)
df$material_hardship_k[rows_with_na] <- NA

table(df$material_hardship_k, useNA = "always")

# Questionnaire M
table(df$m5170,useNA = "always")
table(df$m5171,useNA = "always")
table(df$m5172,useNA = "always")
table(df$m5173,useNA = "always")
table(df$m5174,useNA = "always")

df <- df %>% mutate(across(c("m5170", "m5171", "m5172", "m5173", "m5174"), 
                           ~ case_when(
                             . < 0 ~ NA_real_,        
                             . == 5 ~ 4,              
                             TRUE ~ .             
                           )))

df$material_hardship_m <- 20 - df$m5170 -df$m5171 - df$m5172 - df$m5173 - df$m5174

rows_with_na <- which(rowSums(is.na(select(df, m5170, m5171, m5172, m5173, m5174))) > 0)
df$material_hardship_m[rows_with_na] <- NA

table(df$material_hardship_m, useNA = "always")

# Questionnaire T
table(df$t1360,useNA = "always")
table(df$t1361,useNA = "always")
table(df$t1362,useNA = "always")
table(df$t1363,useNA = "always")
table(df$t1364,useNA = "always")

df <- df %>% mutate(across(c("t1360", "t1361", "t1362", "t1363", "t1364"), 
                           ~ case_when(
                             . < 0 ~ NA_real_,        
                             . >= 5 ~ 4,              
                             TRUE ~ .             
                           )))

df$material_hardship_t <- 20 - df$t1360 -df$t1361 - df$t1362 - df$t1363 - df$t1364

rows_with_na <- which(rowSums(is.na(select(df, t1360, t1361, t1362, t1363, t1364))) > 0)
df$material_hardship_t[rows_with_na] <- NA

table(df$material_hardship_t, useNA = "always")

### Social support

# Questionnaire D
table(df$d801)

df <- df %>% mutate(social_support_d = case_when(d801 < 0 ~ NA_real_,
                                                 TRUE ~ d801))

table(df$social_support_d, useNA = "always")

# Questionnaire E
table(df$e612)
df <- df %>% mutate(social_support_e = case_when(e612 < 0 ~ NA_real_,
                                                 TRUE ~ e612))
table(df$social_support_e, useNA = "always")    

# Questionnaire F
table(df$f922)
df <- df %>% mutate(social_support_f = case_when(f922 < 0 ~ NA_real_,
                                                 TRUE ~ f922))
table(df$social_support_f, useNA = "always")  

# Questionnaire G
unique(df$g227)
table(df$g227)

df <- df %>% mutate(social_support_g = case_when(g227 < 0 ~ NA_real_,
                                                 TRUE ~ g227))
table(df$social_support_g, useNA = "always")  

#Questionnaire K
unique(df$k8020)
unique(df$k8021)
unique(df$k8022)
unique(df$k8023)
unique(df$k8024)
unique(df$k8025)
unique(df$k8026)
unique(df$k8027)
unique(df$k8028)
unique(df$k8029)

social_support_k <- social_support_score(df,"k8020","k8021","k8022","k8023","k8024","k8025","k8026","k8027","k8028","k8029")
df <- df %>% left_join(social_support_k %>% rename(social_support_k = social_support))

table(df$social_support_k,useNA = "always")
hist(df$social_support_k)

# Questionnaire L
unique(df$l7020)
unique(df$l7021)
unique(df$l7022)
unique(df$l7023)
unique(df$l7024)
unique(df$l7025)
unique(df$l7026)
unique(df$l7027)
unique(df$l7028)
unique(df$l7029)

social_support_l <- social_support_score(df,"l7020","l7021","l7022","l7023","l7024","l7025","l7026","l7027","l7028","l7029")
df <- df %>% left_join(social_support_l %>% rename(social_support_l = social_support))

table(df$social_support_l,useNA = "always")
hist(df$social_support_l)

# Questionnaire P
unique(df$p4020)
unique(df$p4021)
unique(df$p4022)
unique(df$p4023)
unique(df$p4024)
unique(df$p4025)
unique(df$p4026)
unique(df$p4027)
unique(df$p4028)
unique(df$p4029)

social_support <- social_support_score(df,"p4020","p4021","p4022","p4023","p4024","p4025","p4026","p4027","p4028","p4029")
df <- df %>% left_join(social_support %>% rename(social_support_p = social_support))

table(df$social_support_p,useNA = "always")
hist(df$social_support_p)

# Questionnaire S
unique(df$s6020)
unique(df$s6021)
unique(df$s6022)
unique(df$s6023)
unique(df$s6024)
unique(df$s6025)
unique(df$s6026)
unique(df$s6027)
unique(df$s6028)
unique(df$s6029)

social_support <- social_support_score(df,"s6020","s6021","s6022","s6023","s6024","s6025","s6026","s6027","s6028","s6029")
df <- df %>% left_join(social_support %>% rename(social_support_s = social_support))

table(df$social_support_s,useNA = "always")
hist(df$social_support_s)

#### BMI

# Calculate mean height across all responses
height <- df %>% select(aln,dw021,m4221,n1145,p1291,s1291,V4400,XB070,MB4540,fm1ms100,fm2ms100,fm3ms100,fm4ms100)

colnames <- colnames(height)[colnames(height) != "aln"]

height <- height %>% mutate(across(all_of(colnames), ~ case_when(. <0 ~ NA_real_,
                                                                 TRUE ~ .)))

# Check the biggest difference in height across all responses

height$row_diff <- apply(height %>% select(colnames), 1, function(row) max(row, na.rm = T) - min(row, na.rm = T))

height$height_mean <- apply(height %>% select(colnames), 1, function(row) {
  if (all(is.na(row))) {
    NA  
  } else {
    mean(row, na.rm = T)
  }
})

height$height_mean <- height$height_mean/100
df <- df %>% left_join(height %>% select(aln,height_mean))

# Remove all weight response < 0
df <- df %>% mutate(across(all_of(c("dw002", "m4220","n1140","p1290","s1290","V4410","XB080","MB4580","fm1ms110",
                                    "fm2ms110","fm3ms110","fm4ms110")), ~ case_when(. <= 0 ~ NA_real_,
                                                         TRUE ~ .)))

# Questionnaire D
max(df$dw002,na.rm = T)
min(df$dw002,na.rm = T)

df$bmi_d <-df$dw002/df$height_mean^2
hist(df$bmi_d)


dates_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))
df <- df %>% left_join(dates_ages)

which(df$bmi_d == min(df$bmi_d, na.rm = T))
df$age_d[which(df$bmi_d == min(df$bmi_d, na.rm = T))]



# Questionnaire M
max(df$m4220,na.rm = T)
min(df$m4220,na.rm = T)

df$bmi_m <-df$m4220/df$height_mean^2
hist(df$bmi_m)

# Questionnaire N
max(df$n1140,na.rm = T)
min(df$n1140,na.rm = T)

df$bmi_n <-df$n1140/df$height_mean^2
hist(df$bmi_n)

# Questionnaire P
max(df$p1290,na.rm = T)
min(df$p1290,na.rm = T)

df$bmi_p <-df$p1290/df$height_mean^2
hist(df$bmi_p)

# Questionnaire S
max(df$s1290,na.rm = T)
min(df$s1290,na.rm = T)

df$bmi_s <-df$s1290/df$height_mean^2
hist(df$bmi_s)

# Questionnaire V
max(df$V4410,na.rm = T)
min(df$V4410,na.rm = T)

df$bmi_v <-df$V4410/df$height_mean^2
hist(df$bmi_v)

# Questionnaire XB
max(df$XB080,na.rm = T)
min(df$XB080,na.rm = T)

df$bmi_xb <-df$XB080/df$height_mean^2
hist(df$bmi_xb)

# Questionnaire MB
max(df$MB4580,na.rm = T)
min(df$MB4580,na.rm = T)

df$bmi_mb <-df$MB4580/df$height_mean^2
hist(df$bmi_mb)

# FoM 1
max(df$fm1ms110,na.rm = T)
min(df$fm1ms110,na.rm = T)

df$bmi_1 <-df$fm1ms110/df$height_mean^2
hist(df$bmi_1)

# FoM 2
max(df$fm2ms110,na.rm = T)
min(df$fm2ms110,na.rm = T)

df$bmi_2 <-df$fm2ms110/df$height_mean^2
hist(df$bmi_2)

# FoM 3
max(df$fm3ms110,na.rm = T)
min(df$fm3ms110,na.rm = T)

df$bmi_3 <-df$fm3ms110/df$height_mean^2
hist(df$bmi_3)

# FoM 4
max(df$fm4ms110,na.rm = T)
min(df$fm4ms110,na.rm = T)

df$bmi_4 <-df$fm4ms110/df$height_mean^2
hist(df$bmi_4)

### HRT

# Questionnaire P
unique(df$p1055)
table(df$p1055,useNA = "always")

df <- df %>% mutate(hrt_p = case_when(p1055 <= 0 ~ NA_character_,
                                      p1055 == 1 ~ "Current",
                                      p1055 == 2 | p1055 == 3 ~ "Ever",
                                      p1055 == 4 | p1055 == 5 ~ "Never",
                                      TRUE ~ NA_character_))

table(df$hrt_p, useNA = "always")

# Questionnaire Q
unique(df$q4290)
unique(df$q4293)
unique(df$q4295)

table(df$q4290)
table(df$q4293)
table(df$q4295)

df <- df %>% mutate(hrt_q = case_when(q4290 == 1 ~ "Current",
                                      TRUE ~ NA_character_))

table(df$hrt_q, useNA = "always")


# Questionnaire S
unique(df$s4290)
unique(df$s4293)
unique(df$s4295)

table(df$s4290)
table(df$s4293)
table(df$s4295)

df <- df %>% mutate(hrt_s = case_when(s4290 == 1 ~ "Current",
                                      TRUE ~ NA_character_))

table(df$hrt_s, useNA = "always")

# Questionnaire T
unique(df$t4920)
unique(df$t4961)
unique(df$t4960)

table(df$t4920)
table(df$t4961)
table(df$t4960)


df <- df %>% mutate(hrt_t = case_when(t4961 == 1 ~ "Current",
                                      t4920 == 1 | t4960 == 1 ~ "Ever",
                                      t4920 == 2 | t4961 == -2 | t4960 == -2 ~ "Never",
                                      TRUE ~ NA_character_))

table(df$hrt_t, useNA = "always")

# Questionnaire V
unique(df$V4920)
unique(df$V4955)
unique(df$V4954)

table(df$V4920)
table(df$V4955)
table(df$V4954)


df <- df %>% mutate(hrt_v = case_when(V4955 == 1 ~ "Current",
                                      V4920 == 1 | V4954 == 1 ~ "Ever",
                                      V4920 == 2 | V4955 == -2 | V4954 == -2 ~ "Never",
                                      TRUE ~ NA_character_))

table(df$hrt_v, useNA = "always")

# Questionnaire Y
unique(df$Y5130)
unique(df$Y5140)

df <- df %>% mutate(hrt_y = case_when(Y5140 == 1 ~ "Current",
                                      Y5130 == 1 ~ "Ever",
                                      Y5130 == 0 | Y5140 == -2 ~ "Never",
                                      TRUE ~ NA_character_))

table(df$hrt_y, useNA = "always")

# Questionnaire MB
unique(df$MB4790)
unique(df$MB4800)
unique(df$MB4810)
unique(df$MB4820)

df <- df %>% mutate(hrt_mb = case_when(MB4790 == 1 | MB4800 == 1 | MB4810 == 1 | MB4820 == 1 ~ "Current",
                                      TRUE ~ NA_character_))

table(df$hrt_mb, useNA = "always")


# FoM 1
unique(df$fm1sa206)
unique(df$fm1ob110)

df <- df %>% mutate(hrt_1 = case_when(fm1sa206 == 1 | fm1ob110 == 1 ~ "Current",
                                      TRUE ~ NA_character_))

table(df$hrt_1, useNA = "always")

# FoM 2
unique(df$fm2ob110a)
unique(df$fm2ob110b)
unique(df$fm2ob110c)

df <- df %>% mutate(hrt_2 = case_when(fm2ob110a == 1 | fm2ob110b == 1 | fm2ob110c == 1  ~ "Current",
                                      TRUE ~ NA_character_))

table(df$hrt_2, useNA = "always")

# FoM 3
unique(df$fm3ob110a)
unique(df$fm3ob110b)
unique(df$fm3ob110c)

df <- df %>% mutate(hrt_3 = case_when(fm3ob110a == 1 | fm3ob110b == 1 | fm3ob110c == 1  ~ "Current",
                                      TRUE ~ NA_character_))

table(df$hrt_3, useNA = "always")

# FoM 4
unique(df$fm4ob110a)
unique(df$fm4ob110b)
unique(df$fm4ob110c)

df <- df %>% mutate(hrt_4 = case_when(fm4ob110a == 1 | fm4ob110b == 1 | fm4ob110c == 1  ~ "Current",
                                      TRUE ~ NA_character_))

table(df$hrt_4, useNA = "always")

# Get ever/never hrt users
hrt <- df %>% select(aln,contains("hrt"))
hrt$ever_hrt <- apply(hrt, 1, function(row) any(grepl("Current|Ever", row)))

# Left join onto df
hrt <- hrt %>% select(aln, ever_hrt)
df <- df %>% left_join(hrt)

### Smoking status
# Questionnaire B
unique(df$b650)

df <- df %>% mutate(smoking_status_b = case_when(b650 == 1 ~ "Ever",
                                                 b650 == 2 ~ "Never",
                                                 TRUE ~ NA_character_ ))

table(df$smoking_status_b, useNA = "always")

# Questionnaire F
unique(df$f621)
table(df$f621)

df <- df %>% mutate(smoking_status_f = case_when(f621 == 0 ~ "Never",
                                                 f621 %in% c(1,2,3) ~ "Current",
                                                 TRUE ~ NA_character_ ))

table(df$smoking_status_f, useNA = "always")

# Questionnaire G
unique(df$g820)
table(df$g820)

df <- df %>% mutate(smoking_status_g = case_when(g820 < 0 ~ NA_character_,
                                                 is.na(g820) ~ NA_character_,
                                                 g820 == 0 ~ "Never",
                                                 TRUE ~ "Current" ))

table(df$smoking_status_g, useNA = "always")

# Questionnaire H
unique(df$h720)
table(df$h720)

df <- df %>% mutate(smoking_status_h = case_when(h720 == -2 ~ "Current", 
                                                 h720 < 0 ~ NA_character_,
                                                 is.na(h720) ~ NA_character_,
                                                 h720 == 0 ~ "Never",
                                                 TRUE ~ "Current" ))

table(df$smoking_status_h, useNA = "always")

# Questionnaire J
unique(df$j735)
table(df$j735)

df <- df %>% mutate(smoking_status_j = case_when(j735 < 0 ~ NA_character_,
                                                 is.na(j735) ~ NA_character_,
                                                 j735 == 0 ~ "Never",
                                                 TRUE ~ "Current" ))

table(df$smoking_status_j, useNA = "always")

# Questionnaire K
unique(df$k6180)
table(df$k6180)

df <- df %>% mutate(smoking_status_k = case_when(k6180 < 0 ~ NA_character_,
                                                 k6180 == 99 ~ NA_character_,
                                                 is.na(k6180) ~ NA_character_,
                                                 k6180 == 0 ~ "Never",
                                                 k6180 == 97 ~ "Ever",
                                                 TRUE ~ "Current" ))

table(df$smoking_status_k, useNA = "always")

# Questionnaire L
unique(df$l5050)
table(df$l5050)

unique(df$l5051)
table(df$l5051)

df <- df %>% mutate(smoking_status_l_weekday = case_when(l5050 < 0 ~ NA_character_,
                                                         l5050 == 99 ~ NA_character_,
                                                         is.na(l5050) ~ NA_character_,
                                                         l5050 == 0 ~ "Never",
                                                         l5050 == 97 ~ "Ever",
                                                         TRUE ~ "Current" ))

df <- df %>% mutate(smoking_status_l_weekend = case_when(l5051 < 0 ~ NA_character_,
                                                         l5051 == 99 ~ NA_character_,
                                                         is.na(l5051) ~ NA_character_,
                                                         l5051 == 0 ~ "Never",
                                                         l5051 == 97 ~ "Ever",
                                                         TRUE ~ "Current" ))

table(df$smoking_status_l_weekday,df$smoking_status_l_weekend, useNA = "always")

df <- df %>% mutate(smoking_status_l = case_when(is.na(smoking_status_l_weekday) & is.na(smoking_status_l_weekend) ~ NA_character_,
                                                 smoking_status_l_weekday == "Current" | smoking_status_l_weekend == "Current" ~ "Current",
                                                 smoking_status_l_weekday == "Ever" | smoking_status_l_weekend == "Ever" ~ "Ever",
                                                 TRUE ~ "Never" ))
table(df$smoking_status_l, useNA = "always")

# Questionnaire M
unique(df$m5160)
table(df$m5160)

df <- df %>% mutate(smoking_status_m = case_when(m5160 < 0 ~ NA_character_,
                                                 m5160 == 99 ~ NA_character_,
                                                 is.na(m5160) ~ NA_character_,
                                                 m5160 == 0 ~ "Never",
                                                 m5160 == 97 ~ "Ever",
                                                 TRUE ~ "Current" ))

table(df$smoking_status_m, useNA = "always")

#Questionnaire N
unique(df$n5002)
unique(df$n5003)
unique(df$n5004)
unique(df$n5005)
unique(df$n5008)
unique(df$n5010)
unique(df$n5000)

df <- df %>% mutate(smoking_status_current_n = case_when(n5008 %in% c(2,3,4,5) ~ "Current",
                                                       n5010 > 0 & n5010 <= 30 ~ "Current",
                                                       TRUE ~ NA_character_))

df <- df %>% mutate(smoking_status_ever_n = case_when(n5000 == 1 | n5002 == 1 | n5003 == 1 | n5004 == 1 | n5005 == 1 ~ "Ever",
                                                    n5010 == 97 ~ "Ever",
                                                    TRUE ~ NA_character_))

df <- df %>% mutate(smoking_status_never_n = case_when(n5000 == 2 | n5002 == 2 | n5003 == 2 | n5004 == 2 | n5005 == 2 ~ "Never",
                                                     n5008 == 1 ~ "Never",
                                                     n5010 == 0 ~ "Never",
                                                     TRUE ~ NA_character_))

df <- df %>% mutate(smoking_status_n = case_when(smoking_status_current_n == "Current" ~ "Current",
                                                 smoking_status_ever_n == "Ever" ~ "Ever",
                                                 smoking_status_never_n == "Never" ~ "Never",
                                                 TRUE ~ NA_character_))

table(df$smoking_status_n, useNA = "always")

# Questionnaire R
unique(df$r6010)
unique(df$r6012)
unique(df$r6013)
unique(df$r6014)
unique(df$r6015)
unique(df$r6018)
unique(df$r6020)

df <- df %>% mutate(smoking_status_current_r = case_when(r6018 %in% c(2,3,4,5) ~ "Current",
                                                         r6020 > 0 & r6020 <= 30 ~ "Current",
                                                         TRUE ~ NA_character_))

df <- df %>% mutate(smoking_status_ever_r = case_when(r6010 == 2 | r6012 == 1 | r6013 == 1 | r6014 == 1 | r6015 == 1 ~ "Ever",
                                                      r6020 == 97 ~ "Ever",
                                                      TRUE ~ NA_character_))

df <- df %>% mutate(smoking_status_never_r = case_when(r6010 == 1 ~ "Never",
                                                       r6018 == 1 ~ "Never",
                                                       r6020 == 0 ~ "Never",
                                                       TRUE ~ NA_character_))

df <- df %>% mutate(smoking_status_r = case_when(smoking_status_current_r == "Current" ~ "Current",
                                                 smoking_status_ever_r == "Ever" ~ "Ever",
                                                 smoking_status_never_r == "Never" ~ "Never",
                                                 TRUE ~ NA_character_))

table(df$smoking_status_r, useNA = "always")

# Questionnaire S 
unique(df$s1300)
unique(df$s1301)
unique(df$s1302)
unique(df$s1303)

table(df$s1300)
table(df$s1301)
table(df$s1302)
table(df$s1303)

df <- df %>% mutate(smoking_status_s = case_when(s1300 > 0 & s1300 != 999 ~ "Current",
                                                 s1301 > 0 & s1301 != 999 ~ "Current",
                                                 s1302 %in% c(1,2) ~ "Current",
                                                 s1303 %in% c(1,2) ~ "Current",
                                                 s1300 == 0 | s1301 == 0 | s1302 == 3 | s1303 == 3 ~ "Never",
                                                 TRUE ~ NA_character_))

table(df$smoking_status_s, useNA = "always")

# Questionnaire T
unique(df$t5520)
unique(df$t5521)
unique(df$t5526)
unique(df$t5560)

table(df$t5520)
table(df$t5521)
table(df$t5526)
table(df$t5560)

df <- df %>% mutate(smoking_status_t = case_when(t5520 == 1 ~ "Current",
                                                 t5521 == 1 ~ "Current",
                                                 t5526 > 0 ~ "Current",
                                                 t5560 == 1 ~ "Ever",
                                                 t5520 == 2 ~ "Never",
                                                 t5521 == -2 ~ "Never",
                                                 t5526 == 0 ~ "Never",
                                                 t5560 == 2 ~ "Never",
                                                 TRUE ~ NA_character_))

table(df$smoking_status_t, useNA = "always")

# Questionnaire V
unique(df$V5520)
unique(df$V5521)
unique(df$V5526)

table(df$V5520)
table(df$V5521)
table(df$V5526)

df <- df %>% mutate(smoking_status_v = case_when(V5520 == 1 | V5521 == 1 ~ "Current",
                                                 V5526 > 0 ~ "Current",
                                                 V5520 == 2 | V5521 == -2 ~ "Never",
                                                 V5526 == 0 ~ "Never",
                                                 TRUE ~ NA_character_))

table(df$smoking_status_v, useNA = "always")

# Questionnaire MA
unique(df$MA8010)
unique(df$MA8030)
unique(df$MA8040)
unique(df$MA8060)
unique(df$MA8070)
unique(df$MA8080)

df <- df %>% mutate(smoking_status_ma = case_when(MA8040 == 1 ~ "Current",
                                                  MA8060 >0 & MA8060 <= 6 ~ "Current",
                                                  MA8080 == 1 ~ "Current",
                                                  MA8010 == 1 ~ "Ever",
                                                  MA8030 > 0 & MA8030 <= 6 ~ "Ever",
                                                  MA8070 == 1 ~ "Ever",
                                                  MA8010 == 0 | MA8030 == -2 | MA8040 == -2 | MA8060 == -2 | MA8080 == -2 ~ "Never",
                                                  TRUE ~ NA_character_))                                                
                                                 
table(df$smoking_status_ma, useNA = "always")

### Alcohol intake

# Questionnaire F
unique(df$f626)
table(df$f626)

df <- df %>% mutate(alcohol_intake_f = case_when(f626 %in% c(4,5,6) ~ "Never or ≤4 times/month",
                                                 f626 == 3 ~ "2–3 times/week",
                                                 f626 %in% c(1,2) ~ "≥ 4 times/week",
                                                 TRUE ~ NA))
prop.table(table(df$alcohol_intake_f, useNA = "always"))

# Questionnaire G
unique(df$g825)
table(df$g825)

df <- df %>% mutate(alcohol_intake_g = case_when(g825 %in% c(4,5,6) ~ "Never or ≤4 times/month",
                                                 g825 == 3 ~ "2–3 times/week",
                                                 g825 %in% c(1,2) ~ "≥ 4 times/week",
                                                 TRUE ~ NA))
table(df$alcohol_intake_g, useNA = "always")
prop.table(table(df$alcohol_intake_g))

# Questionnaire H
unique(df$h724)
table(df$h724)

df <- df %>% mutate(alcohol_intake_h = case_when(h724 %in% c(4,5,6) ~ "Never or ≤4 times/month",
                                                 h724 == 3 ~ "2–3 times/week",
                                                 h724 %in% c(1,2) ~ "≥ 4 times/week",
                                                 TRUE ~ NA))
table(df$alcohol_intake_h, useNA = "always")

# Questionnaire K
unique(df$k6191)
table(df$k6191)

df <- df %>% mutate(alcohol_intake_k = case_when(k6191 %in% c(4,5,6) ~ "Never or ≤4 times/month",
                                                 k6191 == 3 ~ "2–3 times/week",
                                                 k6191 %in% c(1,2) ~ "≥ 4 times/week",
                                                 TRUE ~ NA))
table(df$alcohol_intake_k, useNA = "always")

# Questionnaire T
unique(df$t5500)
table(df$t5500)

df <- df %>% mutate(alcohol_intake_t = case_when(t5500 %in% c(1,2,3) ~ "Never or ≤4 times/month",
                                                 t5500 == 4 ~ "2–3 times/week",
                                                 t5500 == 5 ~ "≥ 4 times/week",
                                                 TRUE ~ NA))
table(df$alcohol_intake_t, useNA = "always")

# Questionnaire V
unique(df$V5500)
table(df$V5500)

df <- df %>% mutate(alcohol_intake_v = case_when(V5500 %in% c(1,2,3) ~ "Never or ≤4 times/month",
                                                 V5500 == 4 ~ "2–3 times/week",
                                                 V5500 == 5 ~ "≥ 4 times/week",
                                                 TRUE ~ NA))
table(df$alcohol_intake_v, useNA = "always")

# Questionnaire MA
unique(df$MA8220)
table(df$MA8220)

df <- df %>% mutate(alcohol_intake_ma = case_when(MA8220 %in% c(0,1,2) ~ "Never or ≤4 times/month",
                                                 MA8220 == 3 ~ "2–3 times/week",
                                                 MA8220 == 4 ~ "≥ 4 times/week",
                                                 TRUE ~ NA))
table(df$alcohol_intake_ma, useNA = "always")

# Save dataset
df_1 <- df %>% select(aln,ethnicity,social_class,education,age_menarche,history_depression_d,contains("material_hardship"),contains("social_support"),
                      contains("smoking_status"),contains("bmi"),contains("alcohol_intake"),contains("hrt"))

saveRDS(df_1, file = paste0(filestore,"confounders.rds"))
