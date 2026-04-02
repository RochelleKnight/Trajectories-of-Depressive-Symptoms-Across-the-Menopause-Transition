rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

library(haven)
library(dplyr)
library(tidyr)
library(data.table)

# Load cohort data set
df <- read_dta(paste0(filestore,"cohort.dta"))
df <- as.data.frame(df)

# Depression medication
# B
table(df$b122)
unique(df$b122)

table(df$b123)
unique(df$b123)

x1 <- df$aln[which(df$b123 ==1)]
x2 <- df$aln[which(df$b122 %in% c(1,2,3))]
which(!x1 %in% x2)

df <- df %>% mutate(antidepressant_b = case_when(b122 %in% c(1,2,3) ~ 1,
                                                 b122 %in% c(4) ~ 0,
                                                 TRUE ~ NA_integer_))

df$antidepressant_broad_b <- df$antidepressant_b

# C
table(df$c101)
unique(df$c101)

df <- df %>% mutate(antidepressant_c = case_when(c101 == 1 ~ 1,
                                                 c101 == 2 ~ 0,
                                                 TRUE ~ NA_integer_))

df$antidepressant_broad_c <- df$antidepressant_c

# E
table(df$e326)
unique(df$e326)

df <- df %>% mutate(antidepressant_e = case_when(e326 %in% c(1,2) ~ 1,
                                                 e326 == 3 ~ 0,
                                                 TRUE ~ NA_integer_))

df$antidepressant_broad_e <- df$antidepressant_e


# F
table(df$f063)
unique(df$f063)
df <- df %>% mutate(antidepressant_f = case_when(f063 %in% c(1,2) ~ 1,
                                                 f063 %in% c(3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_f = case_when(f063 %in% c(1,2,3) ~ 1,
                                                 f063 %in% c(4) ~ 0,
                                                 TRUE ~ NA_integer_))
table(df$antidepressant_f)
table(df$antidepressant_broad_f)

# G
table(df$g049)
unique(df$g049)
df <- df %>% mutate(antidepressant_g = case_when(g049 %in% c(1,2) ~ 1,
                                                 g049 %in% c(3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_g = case_when(g049 %in% c(1,2,3) ~ 1,
                                                       g049 %in% c(4) ~ 0,
                                                       TRUE ~ NA_integer_))
table(df$antidepressant_g)
table(df$antidepressant_broad_g)

# H
table(df$h039)
unique(df$h039)
df <- df %>% mutate(antidepressant_h = case_when(h039 %in% c(1,2) ~ 1,
                                                 h039 %in% c(3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_h = case_when(h039 %in% c(1,2,3) ~ 1,
                                                       h039 %in% c(4) ~ 0,
                                                       TRUE ~ NA_integer_))
table(df$antidepressant_h)
table(df$antidepressant_broad_h)

# J
table(df$j044)
unique(df$j044)
df <- df %>% mutate(antidepressant_j = case_when(j044 %in% c(1,2) ~ 1,
                                                 j044 %in% c(3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_j = case_when(j044 %in% c(1,2,3) ~ 1,
                                                       j044 %in% c(4) ~ 0,
                                                       TRUE ~ NA_integer_))
table(df$antidepressant_j)
table(df$antidepressant_broad_j)

# K
table(df$k1044)
unique(df$k1044)
df <- df %>% mutate(antidepressant_k = case_when(k1044 %in% c(1,2) ~ 1,
                                                 k1044 %in% c(3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_k = case_when(k1044 %in% c(1,2,3) ~ 1,
                                                       k1044 %in% c(4) ~ 0,
                                                       TRUE ~ NA_integer_))
table(df$antidepressant_k)
table(df$antidepressant_broad_k)

# L
table(df$l3044)
unique(df$l3044)
df <- df %>% mutate(antidepressant_l = case_when(l3044 %in% c(1,2) ~ 1,
                                                 l3044 %in% c(3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_l = case_when(l3044 %in% c(1,2,3) ~ 1,
                                                       l3044 %in% c(4) ~ 0,
                                                       TRUE ~ NA_integer_))
table(df$antidepressant_l)
table(df$antidepressant_broad_l)

# P
table(df$p1054)
unique(df$p1054)
df <- df %>% mutate(antidepressant_p = case_when(p1054 %in% c(1,2) ~ 1,
                                                 p1054 %in% c(3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_p = case_when(p1054 %in% c(1,2,3) ~ 1,
                                                       p1054 %in% c(4) ~ 0,
                                                       TRUE ~ NA_integer_))
table(df$antidepressant_p)
table(df$antidepressant_broad_p)

# S
table(df$s4103)
unique(df$s4103)
df <- df %>% mutate(antidepressant_s = case_when(s4103 %in% c(1,2) ~ 1,
                                                 s4103 %in% c(-1,3,4) ~ 0,
                                                 TRUE ~ NA_integer_))

df <- df %>% mutate(antidepressant_broad_s = case_when(s4103 %in% c(0,1,2,3) ~ 1,
                                                       s4103 %in% c(-1,4) ~ 0,
                                                       TRUE ~ NA_integer_))
table(df$antidepressant_s)
table(df$antidepressant_broad_s)

tmp <- df %>% select(aln,contains("antidepressant_broad"))

tmp_long <- tmp %>%
  pivot_longer(
    cols = starts_with("antidepressant_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "antidepressant_broad"  
  )%>%
  mutate(time_point = (gsub("antidepressant_broad_", "", time_point)))

tmp1 <- df %>% select(aln,contains("antidepressant")) %>%
  select(aln,!contains("antidepressant_broad_"))

tmp1_long <- tmp1 %>%
  pivot_longer(
    cols = starts_with("antidepressant_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "antidepressant"  
  )%>%
  mutate(time_point = (gsub("antidepressant_", "", time_point)))

tmp1_long <- tmp1_long %>% left_join(tmp_long)

# EPDS variables
epds_colnames <- c("b","c","e","f","g","h","k","l","n","r","t","v","y","covid_v","mb")
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

epds_long$epds_binary <- NA

epds_long$epds_binary[which(epds_long$epds_prorated > 12 )] <- 1 
epds_long$epds_binary[which(epds_long$epds_prorated <= 12 )] <- 0 

# Combine EPDS and antidepressant use
time_point <- c("b","c","e","f","g","h","j","k","l","n","p","r","s","t","v","y","covid_v","mb")
aln <- unique(c(unique(epds_long$aln),unique(tmp1_long$aln)))
depression <- as.data.frame(crossing(aln,time_point))

depression <- depression %>% left_join(epds_long, by = c("aln" = "aln",
                                                         "time_point" = "time_point"))


depression <- depression %>% left_join(tmp1_long, by = c("aln" = "aln",
                                                         "time_point" = "time_point"))


depression <- depression %>% mutate(epds_binary_with_meds = case_when(antidepressant == 1 ~ 1,
                                                                    TRUE ~ epds_binary),
                                  epds_binary_with_meds_broad = case_when(antidepressant_broad == 1 ~ 1,
                                                                          TRUE ~ epds_binary))

saveRDS(depression, paste0(filestore,"depression_outcomes.rds"))

