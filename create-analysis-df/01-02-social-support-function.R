get_mode <- function(x) {
  unique_x <- unique(na.omit(x)) # Remove NAs and get unique values
  unique_x[which.max(tabulate(match(x, unique_x)))] # Find the most frequent value
}
# tmp <- df
# var1 <- "k8020"
# var2 <- "k8021"
# var3 <- "k8022"
# var4 <- "k8023"
# var5 <- "k8024"
# var6 <- "k8025"
# var7 <- "k8026"
# var8 <- "k8027"
# var9 <- "k8028"
# var10 <- "k8029"

social_support_score <- function(tmp,var1,var2,var3,var4,var5,var6,var7,var8,var9,var10){
  
  tmp <- tmp %>% mutate(across(all_of(c(var1,var5)),
                               ~ case_when(. <= 0 ~ NA_real_,
                                           . == 1 ~ 0,
                                           . == 2 ~ 1,
                                           . == 3 ~ 2,
                                           . == 4 ~ 3,
                                           . == 7 ~ 3,
                                           . == 9 ~ NA_real_,
                                           TRUE ~ .
                               )))
  
  tmp <- tmp %>% mutate(across(all_of(c(var2,var3,var4,var6,var7,var8,var9,var10)),
                               ~ case_when(. <= 0 ~ NA_real_,
                                           . == 1 ~ 3,
                                           . == 2 ~ 2,
                                           . == 3 ~ 1,
                                           . == 4 ~ 0,
                                           . == 7 ~ 0,
                                           . == 9 ~ NA_real_,
                                           TRUE ~ .
                               )))
  
  columns_to_check <- c(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10)
  
  # Get the row numbers where all specified columns are NA
  rows_all_na <- which(apply(tmp[columns_to_check], 1, function(x) all(is.na(x))))
  
  tmp <- tmp %>%
    mutate(across(
      all_of(c(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10)), 
      ~ ifelse(is.na(.), get_mode(.), .)
    ))
  
  tmp[rows_all_na,columns_to_check] <- NA
  
  tmp <- tmp %>%
    mutate(social_support = rowSums(across(all_of(c(var1, var2, var3, var4, var5, var6, var7, var8, var9, var10))), na.rm = FALSE))
  
  return(tmp %>% select(aln,social_support))
  
}
