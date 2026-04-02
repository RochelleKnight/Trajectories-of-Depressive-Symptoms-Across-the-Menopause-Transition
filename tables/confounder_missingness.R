rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")

# Read in df
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
length(which(is.na(df$epds_prorated)))/nrow(df)

# Missing baseline confounders
df1 <- df %>% 
  filter(!is.na(date) & !is.na(epds_prorated)) %>%
  select(aln,ethnicity,social_class,education,age_menarche) %>% distinct()

length(unique(df1$aln))

missing_pct <- sapply(df1, function(x) mean(is.na(x)) * 100)
missing_df <- data.frame(
  Variable = names(missing_pct),
  Missing_Percentage = round(missing_pct, 2)
)

row.names(missing_df) <- NULL

# Missing timevarying confounders
df2 <- df %>% 
  filter(!is.na(date) & !is.na(epds_prorated)) %>%
  select(epds_prorated,material_hardship,social_support,smoking_status,bmi,alcohol_intake) 

missing_pct_2 <- sapply(df2, function(x) mean(is.na(x)) * 100)
missing_df_2 <- data.frame(
  Variable = names(missing_pct_2),
  Missing_Percentage = round(missing_pct_2, 2)
)

row.names(missing_df_2) <- NULL

missing_df <- rbind(missing_df,missing_df_2)

# Number removed due to missingness
df3 <- df %>% 
  filter(!is.na(date))

df4 <- df3 %>% select(aln,date,age_menopause,repro_age,epds_prorated,
                      ethnicity,social_class,education,age_menarche,
                      material_hardship,social_support,smoking_status,bmi,alcohol_intake)

df4 <- df4[complete.cases(df4),]

length(unique(df3$aln))
nrow(df3)

length(unique(df4$aln))
nrow(df4)

write.csv(missing_df, "tables/confounder_missingness.csv", row.names = F)

