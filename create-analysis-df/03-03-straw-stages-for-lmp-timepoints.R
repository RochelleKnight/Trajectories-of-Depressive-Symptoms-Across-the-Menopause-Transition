rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition/create-analysis-df")

library(haven)
library(dplyr)
library(lubridate)   
library(tidyr)

# STRAW
# Reproductive: if periods are regular, had menstrual periods in last 12 months and recent periods

# Menopause transition: if periods are regular, had menstrual periods in the last 12 months, and 2 or more months since last period
# Post menopause: more than 365 days since LMP Straw 

# Additional criteria 
# if periods are not regular, period in last 12 months, period in last 3 months and age above 46 -> menopause transition

# if periods are not regular, period in last 12 months, period in last 3 months and age equal or below 46 -> reproductive
# if no period in last 3 months, period in last 12 months and age above 46 -> menopause transition
# if no period in last 3 months, period in last 12 months and age below 46

fmp_df <- readRDS(file = paste0(filestore,"fmp_df.rds"))

df <- readRDS(paste0(filestore,"cohort_lmp_variables.rds"))


straw <- fmp_df %>% select(aln,age_menopause,fmp,paste0("date_",1:8),paste0("age_",1:8),paste0("datelmp_",1:8),paste0("period3_",1:8),
                           paste0("period12_",1:8),paste0("regperiod_",1:8),paste0("use_straw_",1:8))

# Create Stage variable
for (i in 1:8) {
  # Create stage variable
  straw[,paste0("stage_",i)] <- NA_character_
  
  # Create post-menopause indicator variable
  straw[,"post_menopause"] <- NULL
  straw[,"post_menopause"] <- NA_integer_
  
  # Post-menopause using age at menopause
  replace <- which(straw[,paste0("age_",i)] > (straw[,"age_menopause"] + 1))
  
  straw[replace,"post_menopause"] <- 1
  
  # Create menopause transition indicator variable
  straw[,"menopause_transition"] <- NULL
  straw[,"menopause_transition"] <- NA_integer_
  
  # Menopause transition using age at menopause
  replace <- which(straw[,paste0("age_",i)] >= (straw[,"age_menopause"] ) & 
                     straw[,paste0("age_",i)] <= (straw[,"age_menopause"] + 1))
  
  straw[replace,"menopause_transition"] <- 1
  
  # Reproductive: periods are regular and had recent period
  # Recent period: Had period in the last 3 months (based on period3 == 1) and no date of LMP or
  # date of LMP is within the last 60 days
  replace <- which(!is.na(straw[,paste0("date_",i)])
                   & is.na(straw[,"post_menopause"])
                   & is.na(straw[,"menopause_transition"])
                   & straw[,paste0("regperiod_",i)] == 1 
                   & (straw[,paste0("date_",i)] - straw[,paste0("datelmp_",i)] < 60 ))
  
  
  straw[replace,paste0("stage_",i)] <- "reproductive"
  
  # Reproductive: period regularity is missing but had recent period
  
  # replace <- which(!is.na(straw[,paste0("date_",i)])
  #                  & is.na(straw[,"post_menopause"])
  #                  & is.na(straw[,paste0("regperiod_",i)])
  #                  & ((straw[,paste0("period3_",i)] == 1 & is.na(straw[,paste0("datelmp_",i)]))
  #                     | (straw[,paste0("date_",i)] - straw[,paste0("datelmp_",i)] < 60 )))
  # 
  # straw[replace,paste0("stage_",i)] <- "reproductive"

  # Menopause transition: periods are irregular and had recent period
  replace <- which(!is.na(straw[,paste0("date_",i)])
                   & is.na(straw[,"post_menopause"])
                   & straw[,paste0("regperiod_",i)] == 0
                   & (straw[,paste0("date_",i)] - straw[,paste0("datelmp_",i)] < 60 ))
  
  straw[replace,paste0("stage_",i)] <- "menopause_transition"
  
  # Menopause transition: Not had recent period (regardless of period regularity)
  # No recent period: Not had period in the last 3 months (based on period3 == 0 & period12 == 1) and no date of LMP or
  # date of LMP is more than 60 days ago but within the last 365 days
  replace <- which(!is.na(straw[,paste0("date_",i)])
                   & is.na(straw[,"post_menopause"])
                   & (straw[,paste0("date_",i)] - straw[,paste0("datelmp_",i)] >= 60 & straw[,paste0("date_",i)] - straw[,paste0("datelmp_",i)] <= 365))
  
  straw[replace,paste0("stage_",i)] <- "menopause_transition"
  
  # Menopause transition: If age at attendance is less than 3 years prior to age at menopause
  replace <- which(is.na(straw[,paste0("stage_",i)])
                   & straw[,paste0("age_",i)] >= (straw[,"age_menopause"]-3) 
                   & straw[,paste0("age_",i)] <= (straw[,"age_menopause"] + 1))
  
  straw[replace,paste0("stage_",i)] <- "menopause_transition"
  
  # Post menopause: If age at attendance is greater than age at menopause
  replace <- which(!is.na(straw[,paste0("date_",i)])
                   & straw[,paste0("age_",i)] > (straw[,"age_menopause"] + 1))

  straw[replace,paste0("stage_",i)] <- "post_menopause"
  
  # Post menopause: If age at menopause is missing, but not had a period in the last 12 months
  replace <- which(!is.na(straw[,paste0("date_",i)])
                   & is.na(straw[,"fmp"])
                   & is.na(straw[,"age_menopause"])
                   & is.na(straw[,paste0("datelmp_",i)])
                   & straw[,paste0("period12_",i)] == 0)
  
  straw[replace,paste0("stage_",i)] <- "post_menopause"
  
  
  replace <- which(straw[,paste0("use_straw_",i)] == 0)
  straw[replace,paste0("stage_",i)] <- NA_character_
  
  counts <- table(straw[,paste0("stage_",i)])
  print(counts)
  print(prop.table(counts) * 100)
}

# Determine the max timepoint that has a STRAW stage
straw$max_non_na_index <- apply(straw[, paste0("stage_", 1:8)], 1, function(row) {
  non_na_indices <- which(!is.na(row))
  if (length(non_na_indices) == 0) {
    return(NA) # Return NA if all values are NA
  } else {
    return(max(non_na_indices)) # Otherwise, return the max index
  }
})


straw$max_non_na_index[which(is.na(straw$max_non_na_index))] <- 0

# Add in menopausal status using questionnaire MB
source("straw_mb.R")
straw <- straw %>% left_join(straw_mb)

# Post-menopause based on age at menopause
replace <- which(straw$age_mb > straw$age_menopause + 1)
straw$straw_mb[replace] <- "post_menopause"

# Menopause transition based on age at menopause
replace <- which(is.na(straw$straw_mb)
                 & straw$age_mb >= (straw$age_menopause -3) 
                 & straw$age_mb <= (straw$age_menopause + 1))
straw$straw_mb[replace] <- "menopause_transition"

straw$stage_9 <- NA_character_

for (i in 1:nrow(straw)) {
  index <- straw$max_non_na_index[i] + 1
  straw[i,paste0("stage_",index)] <- straw$straw_mb[i]
  straw[i,paste0("date_",index)] <- straw$date_mb[i]
  straw[i,paste0("age_",index)] <- straw$age_mb[i]
}

straw <- straw %>% select(aln,age_menopause,fmp,
                          paste0("date_",1:9),paste0("age_",1:9),paste0("stage_",1:9))

num_rows_with_non_na <- sum(rowSums(!is.na(straw[,paste0("stage_",1:9)])) > 0)

table(unlist(straw[,paste0("stage_",1:9)]))


# Sense check stages i.e. check that menopause transition does not occur before reproductive
for (i in 1:9) {
  straw[,paste0("stage_check_",i)] <- NA_integer_
  
  replace <- which(is.na(straw[,paste0("stage_",i)]))
  straw[replace,paste0("stage_check_",i)] <- 0
  
  replace <- which(straw[,paste0("stage_",i)] == "reproductive")
  straw[replace,paste0("stage_check_",i)] <- 1
  
  replace <- which(straw[,paste0("stage_",i)] == "menopause_transition")
  straw[replace,paste0("stage_check_",i)] <- 2
  
  replace <- which(straw[,paste0("stage_",i)] == "post_menopause")
  straw[replace,paste0("stage_check_",i)] <- 3
  
}


# Function to validate the order in a row
validate_order <- function(row) {
  # Convert stages to numeric values
  # Remove 0s (corresponding to NA)
  numeric_row <- row[row > 0]
  # Check if the numeric values are non-decreasing
  is_sorted <- all(diff(numeric_row) >= 0)
  return(is_sorted)
}

# Apply the validation function row-wise
straw$valid_order <- apply(straw[,paste0("stage_check_",1:9)], 1, validate_order)

tmp <- straw %>% 
  filter(valid_order == F) %>%
  select(aln,age_menopause, paste0("stage_",1:9),paste0("age_",1:9))
length(which(straw$valid_order == FALSE))


# Sanity checks
# A woman is classified as being in the menopause transition, then classified as reproductive at the next timepoint. 
# If, for at least the two subsequent timepoints, she is classified as either in the menopause transition or postmenopause 
# (with no further classification as reproductive), her earlier classification of reproductive should be changed to menopause transition.
for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(menopause_transition) > 0 & length(reproductive) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(reproductive)) {
        index <- reproductive[j]
        
        # if((index-1) %in% menopause_transition & index >= max(reproductive)
        #    & max(c(menopause_transition,post_menopause),na.rm = T) - index >=2){
        #   straw[i,paste0("stage_",index)] <- "menopause_transition"
        # }
        
        if((index-1) %in% menopause_transition
           & ((index + 1) %in% menopause_transition | (index + 1) %in% post_menopause)
           & ((index + 2) %in% menopause_transition | (index + 2) %in% post_menopause)){
          straw[i,paste0("stage_",index)] <- "menopause_transition"
        }
      }
    }
  }
}


for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(menopause_transition) > 0 & length(post_menopause) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(post_menopause)) {
        index <- post_menopause[j]
        
        if((index + 1) %in% menopause_transition & (index + 2) %in% menopause_transition ){
          straw[i,paste0("stage_",index)] <- "menopause_transition"
        }
      }
    }
  }
}

for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(reproductive) > 0 & length(post_menopause) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(post_menopause)) {
        index <- post_menopause[j]
        
        if((index + 1) %in% reproductive & (index + 2) %in% reproductive ){
          straw[i,paste0("stage_",index)] <- "reproductive"
        }
      }
    }
  }
}

for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(menopause_transition) > 0 & length(reproductive) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(reproductive)) {
        index <- reproductive[j]
        
        if(index == max(reproductive)
           & (index-1) %in% menopause_transition & (index-2) %in% menopause_transition
           & ((index + 1) %in% menopause_transition | (index + 1) %in% post_menopause | index > max(c(menopause_transition,post_menopause),na.rm = T))){
          straw[i,paste0("stage_",index)] <- "menopause_transition"
        }
      }
    }
  }
}


for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(menopause_transition) > 0 & length(reproductive) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(menopause_transition)) {
        index <- menopause_transition[j]
        
        if(index == min(menopause_transition) & (index +1) %in% reproductive & (index +2) %in% reproductive){
          straw[i,paste0("stage_",index)] <- "reproductive"
        }
      }
    }
  }
}

for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(menopause_transition) > 0 & length(reproductive) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(reproductive)) {
        index <- reproductive[j]
        
        # Remove any previous timepoints that are NA from the previous index vector
        if(index-2 >= 1){
          previous <- seq(1:(index-2))
          
          if(length(which(!previous %in% c(reproductive,menopause_transition,post_menopause))) > 0){
            previous[-which(!previous %in% c(reproductive,menopause_transition,post_menopause))]
          }
        }else{
          previous <- NA_integer_ 
        }
        
        date_repro <- straw[i,paste0("date_",index)]
        age_repro <- straw[i,paste0("age_",index)]
        fmp <- straw$fmp[i]
        age_menopause <- straw$age_menopause[i]
        avg_age_menopause <- 49.4
        date_diff <- ifelse(!is.na(fmp),as.numeric(fmp - date_repro)/365.25,
                            ifelse(!is.na(age_menopause),age_menopause - age_repro, avg_age_menopause - age_repro))
        
        if((index - 1) %in% menopause_transition
           & index == max(reproductive)
           & ((index - 1) == 1 | all(previous) %in% reproductive)
           & ((index + 1) %in% menopause_transition)
           & (index+2) > max(c(menopause_transition,post_menopause),na.rm = T)){
          
          if(date_diff <= 3){
            straw[i,paste0("stage_",index)] <- "menopause_transition"
          }else if(date_diff > 3){
            straw[i,paste0("stage_",(index-1))] <- "reproductive"
          }
        }
        
        if((index - 1) %in% menopause_transition 
           & index == max(reproductive) 
           & ((index - 1) == 1 | all(previous) %in% reproductive) 
           & ((index + 1) %in% post_menopause | index > max(c(menopause_transition,post_menopause),na.rm = T))){
          
          if(date_diff <= 3 & date_diff > 0){
            straw[i,paste0("stage_",index)] <- "menopause_transition"
          }else if(date_diff > 3){
            straw[i,paste0("stage_",(index-1))] <- "reproductive"
          }else if(date_diff <= 0){
            straw[i,paste0("stage_",index)] <- "post_menopause"
          }
        }
      }
    }
  }
}

for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(post_menopause) > 0 & length(reproductive) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(reproductive)) {
        index <- reproductive[j]
        
        date_repro <- straw[i,paste0("date_",index)]
        age_repro <- straw[i,paste0("age_",index)]
        fmp <- straw$fmp[i]
        age_menopause <- straw$age_menopause[i]
        avg_age_menopause <- 49.4
        date_diff <- ifelse(!is.na(fmp),as.numeric(fmp - date_repro)/365.25,
                            ifelse(!is.na(age_menopause),age_menopause - age_repro, avg_age_menopause - age_repro))
        
        
        if((index - 1) %in% post_menopause
           & index == max(reproductive) 
           & index > max(c(menopause_transition,post_menopause),na.rm = T)){
          
          if(date_diff > 0){
            straw[i,paste0("stage_",(index-1))] <- "reproductive"
          }else if(date_diff <= 0){
            straw[i,paste0("stage_",index)] <- "post_menopause"
          }
        }
      }
    }
  }
}

for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(post_menopause) >0 & length(reproductive) > 0){
      # Check through all reproductive timepoints
      for (j in 1:length(reproductive)) {
        index <- reproductive[j]
        
        date_repro <- straw[i,paste0("date_",index)]
        age_repro <- straw[i,paste0("age_",index)]
        fmp <- straw$fmp[i]
        age_menopause <- straw$age_menopause[i]
        avg_age_menopause <- 49.4
        date_diff <- ifelse(!is.na(fmp),as.numeric(fmp - date_repro)/365.25,
                            ifelse(!is.na(age_menopause),age_menopause - age_repro, avg_age_menopause - age_repro))
        
        
        if(!(index - 1) %in% c(reproductive,menopause_transition,post_menopause) & (index - 2) %in% post_menopause
           & index == max(reproductive) 
           & index > max(c(menopause_transition,post_menopause),na.rm = T)){
          print(straw[i,c(paste0("stage_",1:9))])
          if(date_diff > 0){
            straw[i,paste0("stage_",(index-2))] <- "reproductive"
          }else if(date_diff <= 0){
            straw[i,paste0("stage_",index)] <- "post_menopause"
          }
        }
      }
    }
  }
}

for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(post_menopause) > 0 & length(menopause_transition) > 0){
      # Check through all post_menopause timepoints
      for (j in 1:length(post_menopause)) {
        index <- post_menopause[j]
        
        age_index <- straw[i,paste0("age_",index)]
        avg_age_menopause <- 49.4
        date_diff <- avg_age_menopause - age_index
        
        if((index +1) %in% menopause_transition & (index+1) == max(c(menopause_transition,post_menopause))){
          
          if(date_diff > 0){
            straw[i,paste0("stage_",index)] <- "menopause_transition"
          }else if(date_diff <= 0){
            straw[i,paste0("stage_",(index+1))] <- "post_menopause"
          }
        }
      }
    }
  }
}


for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(menopause_transition) >0 & length(reproductive) > 0){
      # Check through all reproductive timepoints
      check <- c()
      for (j in 1:length(reproductive)) {
        index <- reproductive[j]
        
        if(index > min(menopause_transition)){
          check <- append(check,index)
        }
      }
      if(length(check) > 0){
        check <- seq(min(menopause_transition),max(check))
        
        for (j in 1:length(check)) {
          index <- check[j]
          
          date_repro <- straw[i,paste0("date_",index)]
          age_repro <- straw[i,paste0("age_",index)]
          fmp <- straw$fmp[i]
          age_menopause <- straw$age_menopause[i]
          avg_age_menopause <- 49.4
          date_diff <- ifelse(!is.na(fmp),as.numeric(fmp - date_repro)/365.25,
                              ifelse(!is.na(age_menopause),age_menopause - age_repro, avg_age_menopause - age_repro))
          
          if(date_diff > 3){
            straw[i,paste0("stage_",(index))] <- "reproductive"
          }else if(date_diff <= 3){
            straw[i,paste0("stage_",index)] <- "menopause_transition"
          }
        }
      }
    }
  }
}

for (i in 1:nrow(straw)) {
  if(!is.na(straw$date_1[i])){
    
    reproductive <- which(straw[i,paste0("stage_",1:9)] == "reproductive")
    menopause_transition <- which(straw[i,paste0("stage_",1:9)] == "menopause_transition")
    post_menopause <- which(straw[i,paste0("stage_",1:9)] == "post_menopause")
    
    if(length(menopause_transition) >0 & length(post_menopause) > 0){
      # Check through all reproductive timepoints
      check <- c()
      for (j in 1:length(menopause_transition)) {
        index <- menopause_transition[j]
        
        if(index > min(post_menopause)){
          check <- append(check,index)
        }
      }
      if(length(check) > 0){
        check <- seq(min(post_menopause),max(check))
        
        for (j in 1:length(check)) {
          index <- check[j]
          
          date_repro <- straw[i,paste0("date_",index)]
          age_repro <- straw[i,paste0("age_",index)]
          fmp <- straw$fmp[i]
          age_menopause <- straw$age_menopause[i]
          avg_age_menopause <- 49.4
          date_diff <- ifelse(!is.na(fmp),as.numeric(fmp - date_repro)/365.25,
                              ifelse(!is.na(age_menopause),age_menopause - age_repro, avg_age_menopause - age_repro))
          
          if(date_diff > 0){
            straw[i,paste0("stage_",(index))] <- "menopause_transition"
          }else if(date_diff <= 0){
            straw[i,paste0("stage_",index)] <- "post_menopause"
          }
        }
      }
    }
  }
}


# Sense check stages i.e. check that menopause transition does not occur before reproductive
for (i in 1:9) {
  straw[,paste0("stage_check_",i)] <- NA_integer_
  
  replace <- which(is.na(straw[,paste0("stage_",i)]))
  straw[replace,paste0("stage_check_",i)] <- 0
  
  replace <- which(straw[,paste0("stage_",i)] == "reproductive")
  straw[replace,paste0("stage_check_",i)] <- 1
  
  replace <- which(straw[,paste0("stage_",i)] == "menopause_transition")
  straw[replace,paste0("stage_check_",i)] <- 2
  
  replace <- which(straw[,paste0("stage_",i)] == "post_menopause")
  straw[replace,paste0("stage_check_",i)] <- 3
  
}


# Function to validate the order in a row
validate_order <- function(row) {
  # Convert stages to numeric values
  # Remove 0s (corresponding to NA)
  numeric_row <- row[row > 0]
  # Check if the numeric values are non-decreasing
  is_sorted <- all(diff(numeric_row) >= 0)
  return(is_sorted)
}

# Apply the validation function row-wise
straw$valid_order <- apply(straw[,paste0("stage_check_",1:9)], 1, validate_order)

tmp <- straw %>% 
  filter(valid_order == F) %>%
  select(aln,age_menopause, paste0("stage_",1:9),paste0("age_",1:9))
length(which(straw$valid_order == FALSE))

table(unlist(straw[,paste0("stage_",1:9)]))

straw_long <- straw %>% select(aln,paste0("date_",1:9),paste0("stage_",1:9))

straw_long <- straw_long %>%
  pivot_longer(
    cols = -aln,
    names_to = c(".value", "time_point"),
    names_sep = "_"
  )

straw_long <- straw_long %>% filter(!is.na(date)) %>% select(-time_point)

table(straw_long$stage)

# Save data set
saveRDS(straw_long, file = paste0(filestore,"straw_stages.rds"))
