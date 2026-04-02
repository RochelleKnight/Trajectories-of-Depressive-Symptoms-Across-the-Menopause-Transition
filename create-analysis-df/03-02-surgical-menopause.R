############################ SURGICAL MENOPAUSE ################################

# Determine those who's periods have stopped due to surgical reasons
# Determine those women who had had one of the following surgeries:
# Removal of uterus (womb) and both ovaries
# Removal of both ovaries only
# Removal of uterus (womb) 
# Removal of uterus (womb) and one ovary
# All of these surgeries will result in menstruation stopping hence we cannot determine age at menopause 
# as we base this on menstruation patterns (unless they have gone through the menopause before surgery)

rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

library(haven)
library(lubridate)
df <- read_dta(paste0(filestore,"cohort.dta"))
df <- as.data.frame(df)

# Add in date columns
date_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))
df <- df %>% left_join(date_ages)
rm(date_ages)

# Questionnaire T

#t4700: Respondent has ever had an operation for removal of uterus (womb) and both ovaries#
#t4701: Date of operation for removal of uterus (womb) and both ovaries: month
#t4702: Date of operation for removal of uterus (womb) and both ovaries: year
#t4703: Age when operated on for removal of uterus (womb) and both ovaries
#t4730: Respondent has ever had operation for removal of both ovaries only
#t4731: Date of operation for removal of both ovaries only: month
#t4732: Date of operation for removal of both ovaries only: year
#t4733: Age when operated on for removal of both ovaries only
#t4710: Respondent has ever had operation for removal of uterus (womb) only
#t4711: Date of operation for removal of uterus (womb) only: month
#t4712: Date of operation for removal of uterus (womb) only: year
#t4713: Age when operated on for removal of uterus (womb) only
#t4720: Respondent has ever had operation for removal of uterus (womb) and one ovary
#t4721: Date of operation for removal of uterus (womb) and one ovary: month
#t4722: Date of operation for removal of uterus (womb) and one ovary: year
#t4723: Age when operated on for removal of uterus (womb) and one ovary

# Ensure all month responses are valid
unique(df$t4701)
unique(df$t4731)
unique(df$t4711)
unique(df$t4721)

df$t4701 <- ifelse(df$t4701 < 1 |df$t4701 > 12, NA, df$t4701)
df$t4731 <- ifelse(df$t4731 < 1 |df$t4731 > 12, NA, df$t4731)
df$t4711 <- ifelse(df$t4711 < 1 |df$t4711 > 12, NA, df$t4711)
df$t4721 <- ifelse(df$t4721 < 1 |df$t4721 > 12, NA, df$t4721)

unique(df$t4701)
unique(df$t4731)
unique(df$t4711)
unique(df$t4721)

# Ensure all year responses are valid
unique(df$t4702)
unique(df$t4732)
unique(df$t4712)
unique(df$t4722)

df$t4702 <- ifelse(df$t4702 <0, NA, df$t4702)
df$t4732 <- ifelse(df$t4732 <0, NA, df$t4732)
df$t4712 <- ifelse(df$t4712 <0, NA, df$t4712)
df$t4722 <- ifelse(df$t4722 <0, NA, df$t4722)

unique(df$t4702)
unique(df$t4732)
unique(df$t4712)
unique(df$t4722)

# Ensure all age responses are valid
unique(df$t4703)
unique(df$t4733)
unique(df$t4713)
unique(df$t4723)

df$t4703 <- ifelse(df$t4703 < 0, NA, df$t4703)
df$t4733 <- ifelse(df$t4733 < 0, NA, df$t4733)
df$t4713 <- ifelse(df$t4713 < 0, NA, df$t4713)
df$t4723 <- ifelse(df$t4723 < 0, NA, df$t4723)

unique(df$t4700)
unique(df$t4730)
unique(df$t4710)
unique(df$t4720)

df <- df %>% mutate(periods_stopped_surgery_t = case_when(t4700 == 1 | t4730 == 1 | t4710 == 1 | t4720 == 1 ~ 1,
                                                          TRUE ~ 0))

table(df$periods_stopped_surgery_t )

# Date of surgical menopause
df$periods_stopped_surgery_type_1_date_t <- paste0(df$t4701,"/15/",df$t4702)
df$periods_stopped_surgery_type_1_date_t <- as.Date(df$periods_stopped_surgery_type_1_date_t, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_1_date_t)

df$periods_stopped_surgery_type_2_date_t <- paste0(df$t4731,"/15/",df$t4732)
df$periods_stopped_surgery_type_2_date_t <- as.Date(df$periods_stopped_surgery_type_2_date_t, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_2_date_t)

df$periods_stopped_surgery_type_3_date_t <- paste0(df$t4711,"/15/",df$t4712)
df$periods_stopped_surgery_type_3_date_t <- as.Date(df$periods_stopped_surgery_type_3_date_t, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_3_date_t)

df$periods_stopped_surgery_type_4_date_t <- paste0(df$t4721,"/15/",df$t4722)
df$periods_stopped_surgery_type_4_date_t <- as.Date(df$periods_stopped_surgery_type_4_date_t, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_4_date_t)

df$periods_stopped_surgery_date_t <- apply(df[,c("periods_stopped_surgery_type_1_date_t","periods_stopped_surgery_type_2_date_t",
                                                 "periods_stopped_surgery_type_3_date_t","periods_stopped_surgery_type_4_date_t")], 1, function(row) min(row, na.rm = TRUE))

df$periods_stopped_surgery_date_t <- as.Date(df$periods_stopped_surgery_date_t, origin = "1970-01-01")

# Age at surgical menopause
df$periods_stopped_surgery_age_t <- apply(df[,c("t4703","t4733","t4713","t4723")], 1, function(row) {
  min_val <- min(row, na.rm = TRUE)
  if (all(is.na(row))) {
    return(NA)
  } else {
    return(min_val)
  }
})

# Estimate age at menopause using date of surgery
df$dif <- NA
df$dif[which(!is.na(df$periods_stopped_surgery_date_t))] <- floor(time_length(difftime(df$date_t[which(!is.na(df$periods_stopped_surgery_date_t))], df$periods_stopped_surgery_date_t[which(!is.na(df$periods_stopped_surgery_date_t))]), "years"))

df$periods_stopped_surgery_age_est_t <- df$age_t - df$dif

length(which(is.na(df$periods_stopped_surgery_age_t) & !is.na(df$periods_stopped_surgery_age_est_t)))
df$periods_stopped_surgery_age_t <- ifelse(is.na(df$periods_stopped_surgery_age_t),df$periods_stopped_surgery_age_est_t,df$periods_stopped_surgery_age_t)
table(df$periods_stopped_surgery_age_t)

# Anyone without an age at  surgery 
nrow(df %>% filter(!is.na(periods_stopped_surgery_date_t) & periods_stopped_surgery_t == 1 & is.na(periods_stopped_surgery_age_t)) )

# Questionnaire V

#V4700: Respondent has ever had an operation for removal of uterus (womb) and both ovaries#
#V4701: Date of operation for removal of uterus (womb) and both ovaries: month
#V4702: Date of operation for removal of uterus (womb) and both ovaries: year
#V4703: Age when operated on for removal of uterus (womb) and both ovaries
#V4730: Respondent has ever had operation for removal of both ovaries only
#V4731: Date of operation for removal of both ovaries only: month
#V4732: Date of operation for removal of both ovaries only: year
#V4733: Age when operated on for removal of both ovaries only
#V4710: Respondent had uterus removed
#V4711: Respondent had  uterus removed - Month
#V4712: Respondent had uterus removed - Year
#V4713: Respondent had uterus removed - Age
#V4720: Respondent had uterus and one ovary removed
#V4721: Respondent had uterus & one ovary removed – Month
#V4722: Respondent had uterus & one ovary removed– Year
#V4723: Respondent had uterus & one ovary removed - age

# Ensure all month responses are valid
unique(df$V4701)
unique(df$V4731)
unique(df$V4711)
unique(df$V4721)

df$V4701 <- ifelse(df$V4701 < 1 |df$V4701 > 12, NA, df$V4701)
df$V4731 <- ifelse(df$V4731 < 1 |df$V4731 > 12, NA, df$V4731)
df$V4711 <- ifelse(df$V4711 < 1 |df$V4711 > 12, NA, df$V4711)
df$V4721 <- ifelse(df$V4721 < 1 |df$V4721 > 12, NA, df$V4721)

unique(df$V4701)
unique(df$V4731)
unique(df$V4711)
unique(df$V4721)

# Ensure all year responses are valid
unique(df$V4702)
unique(df$V4732)
unique(df$V4712)
unique(df$V4722)

df$V4702 <- ifelse(df$V4702 <0, NA, df$V4702)
df$V4732 <- ifelse(df$V4732 <0, NA, df$V4732)
df$V4712 <- ifelse(df$V4712 <0, NA, df$V4712)
df$V4722 <- ifelse(df$V4722 <0, NA, df$V4722)

unique(df$V4702)
unique(df$V4732)
unique(df$V4712)
unique(df$V4722)

# Ensure all age responses are valid
unique(df$V4703)
unique(df$V4733)
unique(df$V4713)
unique(df$V4723)

df$V4703 <- ifelse(df$V4703 < 0, NA, df$V4703)
df$V4733 <- ifelse(df$V4733 < 0, NA, df$V4733)
df$V4713 <- ifelse(df$V4713 < 0, NA, df$V4713)
df$V4723 <- ifelse(df$V4723 < 0, NA, df$V4723)

unique(df$V4700)
unique(df$V4730)
unique(df$V4710)
unique(df$V4720)

df <- df %>% mutate(periods_stopped_surgery_v = case_when(V4700 == 1 | V4730 == 1 | V4710 == 1 | V4720 == 1 ~ 1,
                                                          TRUE ~ 0))

table(df$periods_stopped_surgery_v)

# Date of surgical menopause
df$periods_stopped_surgery_type_1_date_v <- paste0(df$V4701,"/15/",df$V4702)
df$periods_stopped_surgery_type_1_date_v <- as.Date(df$periods_stopped_surgery_type_1_date_v, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_1_date_v)

df$periods_stopped_surgery_type_2_date_v <- paste0(df$V4731,"/15/",df$V4732)
df$periods_stopped_surgery_type_2_date_v <- as.Date(df$periods_stopped_surgery_type_2_date_v, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_2_date_v)

df$periods_stopped_surgery_type_3_date_v <- paste0(df$V4711,"/15/",df$V4712)
df$periods_stopped_surgery_type_3_date_v <- as.Date(df$periods_stopped_surgery_type_3_date_v, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_3_date_v)

df$periods_stopped_surgery_type_4_date_v <- paste0(df$V4721,"/15/",df$V4722)
df$periods_stopped_surgery_type_4_date_v <- as.Date(df$periods_stopped_surgery_type_4_date_v, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_4_date_v)

df$periods_stopped_surgery_date_v <- apply(df[,c("periods_stopped_surgery_type_1_date_v","periods_stopped_surgery_type_2_date_v",
                                                 "periods_stopped_surgery_type_3_date_v","periods_stopped_surgery_type_4_date_v")], 1, function(row) min(row, na.rm = TRUE))

df$periods_stopped_surgery_date_v <- as.Date(df$periods_stopped_surgery_date_v, origin = "1970-01-01")

# Age at surgical menopause
df$periods_stopped_surgery_age_v <- apply(df[,c("V4703","V4733","V4713","V4723")], 1, function(row) {
  min_val <- min(row, na.rm = TRUE)
  if (all(is.na(row))) {
    return(NA)
  } else {
    return(min_val)
  }
})

# Estimate age at menopause using date of surgery
df$dif <- NA
df$dif[which(!is.na(df$periods_stopped_surgery_date_v))] <- floor(time_length(difftime(df$date_v[which(!is.na(df$periods_stopped_surgery_date_v))], df$periods_stopped_surgery_date_v[which(!is.na(df$periods_stopped_surgery_date_v))]), "years"))

df$periods_stopped_surgery_age_est_v <- df$age_v - df$dif

length(which(is.na(df$periods_stopped_surgery_age_v) & !is.na(df$periods_stopped_surgery_age_est_v)))
df$periods_stopped_surgery_age_v <- ifelse(is.na(df$periods_stopped_surgery_age_v),df$periods_stopped_surgery_age_est_v,df$periods_stopped_surgery_age_v)

# Anyone without an age at  surgery 
nrow(df %>% filter(!is.na(periods_stopped_surgery_date_v) & periods_stopped_surgery_v == 1 & is.na(periods_stopped_surgery_age_v)))

# Questionnaire Y

#Y5020: Respondent has ever had an operation for removal of uterus (womb) and both ovaries#
#Y5021: Date of operation for removal of uterus (womb) and both ovaries: month
#Y5022: Date of operation for removal of uterus (womb) and both ovaries: year
#Y5023: Age when operated on for removal of uterus (womb) and both ovaries
#Y5050: Respondent has ever had operation for removal of both ovaries only
#Y5051: Date of operation for removal of both ovaries only: month
#Y5052: Date of operation for removal of both ovaries only: year
#Y5053: Age when operated on for removal of both ovaries only
#Y5040: Respondent has ever had operation for removal of uterus (womb) only
#Y5041: Date of operation for removal of uterus (womb) only: month
#Y5042: Date of operation for removal of uterus (womb) only: year
#Y5043: Age when operated on for removal of uterus (womb) only
#Y5030: Respondent has ever had operation for removal of uterus (womb) and one ovary
#Y5031: Date of operation for removal of uterus (womb) and one ovary: month
#Y5032: Date of operation for removal of uterus (womb) and one ovary: year
#Y5033: Age when operated on for removal of uterus (womb) and one ovary

# Ensure all month responses are valid
unique(df$Y5021)
table(df$Y5021, useNA = "always")
unique(df$Y5051)
table(df$Y5021, useNA = "always")
unique(df$Y5041)
table(df$Y5041, useNA = "always")
unique(df$Y5031)
table(df$Y5041, useNA = "always")

df$Y5021 <- ifelse(df$Y5021 < 1 |df$Y5021 > 12, NA, df$Y5021)
df$Y5051 <- ifelse(df$Y5051 < 1 |df$Y5051 > 12, NA, df$Y5051)
df$Y5041 <- ifelse(df$Y5041 < 1 |df$Y5041 > 12, NA, df$Y5041)
df$Y5031 <- ifelse(df$Y5031 < 1 |df$Y5031 > 12, NA, df$Y5031)

unique(df$Y5021)
table(df$Y5021, useNA = "always")
unique(df$Y5051)
table(df$Y5021, useNA = "always")
unique(df$Y5041)
table(df$Y5041, useNA = "always")
unique(df$Y5031)
table(df$Y5041, useNA = "always")

# Ensure all year responses are valid
unique(df$Y5022)
unique(df$Y5052)
unique(df$Y5042)
unique(df$Y5032)

df$Y5022 <- ifelse(df$Y5022 <0, NA, df$Y5022)
df$Y5052 <- ifelse(df$Y5052 <0, NA, df$Y5052)
df$Y5042 <- ifelse(df$Y5042 <0, NA, df$Y5042)
df$Y5032 <- ifelse(df$Y5032 <0, NA, df$Y5032)

unique(df$Y5022)
unique(df$Y5052)
unique(df$Y5042)
unique(df$Y5032)

# Ensure all age responses are valid
unique(df$Y5023)
unique(df$Y5053)
unique(df$Y5043)
unique(df$Y5033)

df$Y5023 <- ifelse(df$Y5023 < 0, NA, df$Y5023)
df$Y5053 <- ifelse(df$Y5053 < 0, NA, df$Y5053)
df$Y5043 <- ifelse(df$Y5043 < 0, NA, df$Y5043)
df$Y5033 <- ifelse(df$Y5033 < 0, NA, df$Y5033)

unique(df$Y5020)
unique(df$Y5050)
unique(df$Y5040)
unique(df$Y5030)

df <- df %>% mutate(periods_stopped_surgery_y = case_when(Y5020 == 1 | Y5050 == 1 | Y5040 == 1 | Y5030 == 1 ~ 1,
                                                          TRUE ~ 0))

table(df$periods_stopped_surgery_y)

# Date of surgical menopause
df$periods_stopped_surgery_type_1_date_y <- paste0(df$Y5021,"/15/",df$Y5022)
df$periods_stopped_surgery_type_1_date_y <- as.Date(df$periods_stopped_surgery_type_1_date_y, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_1_date_y)

df$periods_stopped_surgery_type_2_date_y <- paste0(df$Y5051,"/15/",df$Y5052)
df$periods_stopped_surgery_type_2_date_y <- as.Date(df$periods_stopped_surgery_type_2_date_y, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_2_date_y)

df$periods_stopped_surgery_type_3_date_y <- paste0(df$Y5041,"/15/",df$Y5042)
df$periods_stopped_surgery_type_3_date_y <- as.Date(df$periods_stopped_surgery_type_3_date_y, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_3_date_y)

df$periods_stopped_surgery_type_4_date_y <- paste0(df$Y5031,"/15/",df$Y5032)
df$periods_stopped_surgery_type_4_date_y <- as.Date(df$periods_stopped_surgery_type_4_date_y, format = "%m/%d/%Y")
unique(df$periods_stopped_surgery_type_4_date_y)

df$periods_stopped_surgery_date_y <- apply(df[,c("periods_stopped_surgery_type_1_date_y","periods_stopped_surgery_type_2_date_y",
                                                 "periods_stopped_surgery_type_3_date_y","periods_stopped_surgery_type_4_date_y")], 1, function(row) min(row, na.rm = TRUE))

df$periods_stopped_surgery_date_y <- as.Date(df$periods_stopped_surgery_date_y, origin = "1970-01-01")

# Age at surgical menopause
df$periods_stopped_surgery_age_y <- apply(df[,c("Y5023","Y5053","Y5043","Y5033")], 1, function(row) {
  min_val <- min(row, na.rm = TRUE)
  if (all(is.na(row))) {
    return(NA)
  } else {
    return(min_val)
  }
})

# Estimate age at menopause using date of surgery
df$dif <- NA
df$dif[which(!is.na(df$periods_stopped_surgery_date_y))] <- floor(time_length(difftime(df$date_y[which(!is.na(df$periods_stopped_surgery_date_y))], df$periods_stopped_surgery_date_y[which(!is.na(df$periods_stopped_surgery_date_y))]), "years"))
df$periods_stopped_surgery_age_est_y <- df$age_y - df$dif

length(which(is.na(df$periods_stopped_surgery_age_y) & !is.na(df$periods_stopped_surgery_age_est_y)))
df$periods_stopped_surgery_age_y <- ifelse(is.na(df$periods_stopped_surgery_age_y),df$periods_stopped_surgery_age_est_y,df$periods_stopped_surgery_age_y)

# Anyone without an age at  surgery 
nrow(df %>% filter(!is.na(periods_stopped_surgery_date_y) & periods_stopped_surgery_y == 1 & is.na(periods_stopped_surgery_age_y)))

# Overall surgical menopause
df <- df %>% mutate(periods_stopped_surgery = case_when(periods_stopped_surgery_t == 1 | periods_stopped_surgery_v == 1 | periods_stopped_surgery_y == 1 ~ 1,
                                                        TRUE ~ 0))

table(df$periods_stopped_surgery)

# Overall date surgical menopause
df$periods_stopped_surgery_date <- apply(df[,c("periods_stopped_surgery_date_t","periods_stopped_surgery_date_v","periods_stopped_surgery_date_y")], 1, function(row) min(row, na.rm = TRUE))
df$periods_stopped_surgery_date <- as.Date(df$periods_stopped_surgery_date, origin = "1970-01-01")


# Overall age surgical menopause
df$periods_stopped_surgery_age <- apply(df[,c("periods_stopped_surgery_age_t","periods_stopped_surgery_age_v","periods_stopped_surgery_age_y")], 1, function(row) {
  min_val <- min(row, na.rm = TRUE)
  if (all(is.na(row))) {
    return(NA)
  } else {
    return(min_val)
  }
})

# Number of those with no age at surgery - to be excluded
nrow(df%>% filter(periods_stopped_surgery == 1 & is.na(periods_stopped_surgery_age) & is.na(periods_stopped_surgery_date))) 

df$exclude_participant <- 0
replace <- which(df$periods_stopped_surgery == 1 & is.na(df$periods_stopped_surgery_age) & is.na(df$periods_stopped_surgery_date))
df$exclude_participant[replace] <- 1

# Number of those with age of surgery but did not report having had surgery
nrow(df %>% filter(periods_stopped_surgery == 0 & !is.na(periods_stopped_surgery_age)))
length(which(df$periods_stopped_surgery == 0 & (!is.na(df$periods_stopped_surgery_age) | !is.na(df$periods_stopped_surgery_date))))
df$periods_stopped_surgery[which(df$periods_stopped_surgery == 0 & (!is.na(df$periods_stopped_surgery_age) | !is.na(df$periods_stopped_surgery_date)))] <- 1

length(which(df$periods_stopped_surgery==1))

###################### Periods stopped by surgery ##############################

# The women are asked at each time point if their periods have stopped what is the cause of this
# They are able to respond in variable ways at each time point but we are able to determine if the cause is due to surgery
# We take the date of surgery as the date of attendance

# FoM1

# Determine those who have reported a hysterectomy
unique(df$fm1ob121)
df$hysterectomy_surgery_1 <- ifelse(!is.na(df$fm1ob121) & (df$fm1ob121 == 1 | df$fm1ob121 == 3) , 1, 0)
table(df$hysterectomy_surgery_1, useNA = "always")

df$hysterectomy_surgery_date_1 <- as.Date(NA)
df$hysterectomy_surgery_date_1[which(df$hysterectomy_surgery_1 == 1)] <- df$date_1[which(df$hysterectomy_surgery_1 == 1)]
unique(df$hysterectomy_surgery_date_1)

df$hysterectomy_surgery_age_1 <- NA
df$hysterectomy_surgery_age_1[which(df$hysterectomy_surgery_1 == 1)] <- df$age_1[which(df$hysterectomy_surgery_1 == 1)]
unique(df$hysterectomy_surgery_age_1)

# FoM2

# Determine those who have reported a hysterectomy
unique(df$fm2ob121)
df$hysterectomy_surgery_2 <- ifelse(!is.na(df$fm2ob121) & df$fm2ob121 == 1, 1, 0)
table(df$hysterectomy_surgery_2, useNA = "always")

df$hysterectomy_surgery_date_2 <- as.Date(NA)
df$hysterectomy_surgery_date_2[which(df$hysterectomy_surgery_2 == 1)] <- df$date_2[which(df$hysterectomy_surgery_2 == 1)]
unique(df$hysterectomy_surgery_date_2)

df$hysterectomy_surgery_age_2 <- NA
df$hysterectomy_surgery_age_2[which(df$hysterectomy_surgery_2 == 1)] <- df$age_2[which(df$hysterectomy_surgery_2 == 1)]
unique(df$hysterectomy_surgery_age_2)

# FoM3

# Determine those who have reported a hysterectomy
unique(df$fm3ob121)
df$hysterectomy_surgery_3 <- ifelse(!is.na(df$fm3ob121) & df$fm3ob121 == 1, 1, 0)
table(df$hysterectomy_surgery_3, useNA = "always")

df$hysterectomy_surgery_date_3 <- as.Date(NA)
df$hysterectomy_surgery_date_3[which(df$hysterectomy_surgery_3 == 1)] <- df$date_3[which(df$hysterectomy_surgery_3 == 1)]
unique(df$hysterectomy_surgery_date_3)

df$hysterectomy_surgery_age_3 <- NA
df$hysterectomy_surgery_age_3[which(df$hysterectomy_surgery_3 == 1)] <- df$age_3[which(df$hysterectomy_surgery_3 == 1)]
unique(df$hysterectomy_surgery_age_3)

# FoM4

# Determine those who have reported a hysterectomy
unique(df$fm4ob121)
df$hysterectomy_surgery_4 <- ifelse(!is.na(df$fm4ob121) & df$fm4ob121 == 1, 1, 0)
table(df$hysterectomy_surgery_4, useNA = "always")

df$hysterectomy_surgery_date_4 <- as.Date(NA)
df$hysterectomy_surgery_date_4[which(df$hysterectomy_surgery_4 == 1)] <- df$date_4[which(df$hysterectomy_surgery_4 == 1)]
unique(df$hysterectomy_surgery_date_4)

df$hysterectomy_surgery_age_4 <- NA
df$hysterectomy_surgery_age_4[which(df$hysterectomy_surgery_4 == 1)] <- df$age_4[which(df$hysterectomy_surgery_4 == 1)]
unique(df$hysterectomy_surgery_age_4)

# Questionnaire T

# Determine those who have reported a period stopped by surgery
unique(df$t4801)
df$hysterectomy_surgery_t <- ifelse(!is.na(df$t4801) & df$t4801 == 1, 1, 0)
table(df$hysterectomy_surgery_t, useNA = "always")

df$hysterectomy_surgery_date_t <- as.Date(NA)
df$hysterectomy_surgery_date_t[which(df$hysterectomy_surgery_t == 1)] <- df$date_t[which(df$hysterectomy_surgery_t == 1)]
unique(df$hysterectomy_surgery_date_t)

df$hysterectomy_surgery_age_t <- NA
df$hysterectomy_surgery_age_t[which(df$hysterectomy_surgery_t == 1)] <- df$age_t[which(df$hysterectomy_surgery_t == 1)]
unique(df$hysterectomy_surgery_age_t)

# Questionnaire V

# Determine those who have reported a period stopped by surgery
unique(df$V4801)
df$hysterectomy_surgery_v <- ifelse(!is.na(df$V4801) & df$V4801 == 1, 1, 0)
table(df$hysterectomy_surgery_v, useNA = "always")

df$hysterectomy_surgery_date_v <- as.Date(NA)
df$hysterectomy_surgery_date_v[which(df$hysterectomy_surgery_v == 1)] <- df$date_v[which(df$hysterectomy_surgery_v == 1)]
unique(df$hysterectomy_surgery_date_v)

df$hysterectomy_surgery_age_v <- NA
df$hysterectomy_surgery_age_v[which(df$hysterectomy_surgery_v == 1)] <- df$age_v[which(df$hysterectomy_surgery_v == 1)]
unique(df$hysterectomy_surgery_age_v)

# Questionnaire Y

# Determine those who have reported a period stopped by surgery
unique(df$Y5080)
df$hysterectomy_surgery_y <- ifelse(!is.na(df$Y5080) & df$Y5080 == 1, 1, 0)
table(df$hysterectomy_surgery_y, useNA = "always")

df$hysterectomy_surgery_date_y <- as.Date(NA)
df$hysterectomy_surgery_date_y[which(df$hysterectomy_surgery_y == 1)] <- df$date_y[which(df$hysterectomy_surgery_y == 1)]
unique(df$hysterectomy_surgery_date_y)

df$hysterectomy_surgery_age_y <- NA
df$hysterectomy_surgery_age_y[which(df$hysterectomy_surgery_y == 1)] <- df$age_y[which(df$hysterectomy_surgery_y == 1)]
unique(df$hysterectomy_surgery_age_y)

# Questionnaire MB

# Determine those who have reported a period stopped by surgery
unique(df$MB4610)
df$hysterectomy_surgery_mb <- ifelse(!is.na(df$MB4610) & df$MB4610 == 1, 1, 0)
table(df$hysterectomy_surgery_mb, useNA = "always")

df$hysterectomy_surgery_date_mb <- NA_Date_
df$hysterectomy_surgery_date_mb[which(df$hysterectomy_surgery_mb == 1)] <- df$date_mb[which(df$hysterectomy_surgery_mb == 1)]
unique(df$hysterectomy_surgery_date_mb)

df$hysterectomy_surgery_age_mb <- NA_integer_
df$hysterectomy_surgery_age_mb[which(df$hysterectomy_surgery_mb == 1)] <- df$age_y[which(df$hysterectomy_surgery_mb == 1)]
unique(df$hysterectomy_surgery_age_mb)

# Determine earliest report of hysterectomy/period stopped by surgery 
df$hysterectomy_surgery_date <- apply(df[,c("hysterectomy_surgery_date_1","hysterectomy_surgery_date_2",
                                            "hysterectomy_surgery_date_3","hysterectomy_surgery_date_4",
                                            "hysterectomy_surgery_date_t","hysterectomy_surgery_date_v",
                                            "hysterectomy_surgery_date_y","hysterectomy_surgery_date_mb")], 1, function(row) min(row, na.rm = TRUE))


df$hysterectomy_surgery_date <- as.Date(df$hysterectomy_surgery_date, origin = "1970-01-01")

unique(df$hysterectomy_surgery_date)
class(df$hysterectomy_surgery_date)

# Overall age surgical menopause
df$hysterectomy_surgery_age <- apply(df[,c("hysterectomy_surgery_age_1","hysterectomy_surgery_age_2","hysterectomy_surgery_age_3","hysterectomy_surgery_age_4",
                                           "hysterectomy_surgery_age_t","hysterectomy_surgery_age_v","hysterectomy_surgery_age_y","hysterectomy_surgery_age_mb")], 1, function(row) {
                                             min_val <- min(row, na.rm = TRUE)
                                             if (all(is.na(row))) {
                                               return(NA)
                                             } else {
                                               return(min_val)
                                             }
                                           })

# Add any hysterectomy_surgery_date to periods_stopped_surgery_date so that we have a complete list of women who's periods have
# been stopped by surgery

replace <- which(is.na(df$periods_stopped_surgery_date) & is.na(df$periods_stopped_surgery_age))

df$periods_stopped_surgery_date[replace] <- df$hysterectomy_surgery_date[replace]
df$periods_stopped_surgery_age[replace] <- df$hysterectomy_surgery_age[replace]

replace <- which(!is.na(df$periods_stopped_surgery_age) & !is.na(df$hysterectomy_surgery_age) 
                 & df$hysterectomy_surgery_age < df$periods_stopped_surgery_age)

df$periods_stopped_surgery_date[replace] <- df$hysterectomy_surgery_date[replace]
df$periods_stopped_surgery_age[replace] <- df$hysterectomy_surgery_age[replace]

replace <- which(!is.na(df$periods_stopped_surgery_date) & !is.na(df$hysterectomy_surgery_date) 
                 & df$hysterectomy_surgery_date < df$periods_stopped_surgery_date)

df$periods_stopped_surgery_date[replace] <- df$hysterectomy_surgery_date[replace]
df$periods_stopped_surgery_age[replace] <- df$hysterectomy_surgery_age[replace]

# Update periods_stopped_surgery indicator variable

df$periods_stopped_surgery <- 0
replace <- which(!is.na(df$periods_stopped_surgery_date) | !is.na(df$periods_stopped_surgery_age))
df$periods_stopped_surgery[replace] <- 1

length(which(!is.na(df$periods_stopped_surgery_date) & is.na(df$periods_stopped_surgery_age)))

cols_remove <- grep("periods_stopped", names(df), value = TRUE)
cols_remove <- cols_remove[!cols_remove %in% c("periods_stopped_surgery","periods_stopped_surgery_age","periods_stopped_surgery_date")]
df[,cols_remove] <- NULL

cols_remove <- grep("hysterectomy_surgery", names(df), value = TRUE)
df[,cols_remove] <- NULL

df <- df %>% select(aln,periods_stopped_surgery,periods_stopped_surgery_date,
                    periods_stopped_surgery_age, exclude_participant)

# Save data frame
saveRDS(df, file = paste0(filestore,"whole_cohort_surgical_menopause.rds"))
