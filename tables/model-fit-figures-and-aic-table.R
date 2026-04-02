# Model fit table - AIC & BIC

rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")
library(R2MLwiN)
library(dplyr)
library(rms)
library(splines)
library(ggplot2)
library(lme4)
library(gridExtra)

options(MLwiN_path = "C:/Program Files/MLwiN v3.13/mlwin.exe")

# Read in df
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))

df$age_sq <- df$age^2
df$centered_age_sq <- df$centered_age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025,na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975,na.rm = T))

chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))

df$age_meno_cat <- NA_character_
df$age_meno_cat[which(df$age_menopause < 45)] <- "Under 45"
df$age_meno_cat[which(df$age_menopause >= 45)] <- "Over 45"
df$age_meno_cat <- as.factor(df$age_meno_cat)

quantiles <- df %>% 
  group_by(age_meno_cat) %>%
  dplyr::summarize(lower_bound = quantile(age, 0.025, na.rm=T),
                   upper_bound = quantile(age, 0.975, na.rm=T))

model_fit <- as.data.frame(matrix(ncol = 6,nrow = 0))
colnames(model_fit) <- c("model","N participants","N observations","df","AIC","BIC")

# Trajectory across reproductive age
knot_numbers <- 3:6
for (k in knot_numbers) {
  if(k == 3){
    model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age
                            + centered_age + centered_age_sq 
                            + ethnicity + social_class + education + age_menarche
                            + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                            + (1 + centered_age | aln) + (1 + centered_age | time_point),
                            D = "Normal",
                            data = df,
                            estoptions = list(EstM = 0)
    )
    
    model_repro_sq <- runMLwiN(epds_prorated ~ 1 + repro_age + I(repro_age^2)
                            + centered_age + centered_age_sq 
                            + ethnicity + social_class + education + age_menarche
                            + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                            + (1 + centered_age | aln) + (1 + centered_age | time_point),
                            D = "Normal",
                            data = df,
                            estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
    model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
    model_fit[nrow(model_fit),"N participants"] <- model_repro_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro_sq)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro_sq)
    
    # Plot figure
    # Predict values
    pred_model_repro <- data.frame(
      repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
      centered_age = 0,
      centered_age_sq = 0,
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("4", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)), 
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    # Predict values
    predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
    predicted_model_repro_sq <- predict(model_repro_sq, newdata = pred_model_repro, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                      predicted = predicted_model_repro$fit,
                                      CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                      CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
    
    pred_df_model_repro_sq <- data.frame(repro_age = pred_model_repro$repro_age,
                                      predicted = predicted_model_repro_sq$fit,
                                      CI_low = predicted_model_repro_sq$fit - 1.96 * predicted_model_repro_sq$se.fit,
                                      CI_high = predicted_model_repro_sq$fit + 1.96 * predicted_model_repro_sq$se.fit)
    
    
    
    # Plot with confidence intervals
    plot_a <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 9))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(5,10,
                                      by = 1))+
      annotate("text",
               x = 32-50,
               y = 10,
               label = "a",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b <- ggplot(pred_df_model_repro_sq, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 9))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(5,10,
                                      by = 1))+
      annotate("text",
               x = 32-50,
               y = 10,
               label = "b",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
  }
  
  # 1. Create spline basis
  spline_basis_repro <- rms::rcs(df$repro_age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_repro))) {
    df[[paste0("spline", i, "_repro")]] <- as.numeric(spline_basis_repro[, i])
  }
  
  spline_names <- paste0("spline", 1:ncol(spline_basis_repro), "_repro")
  spline_terms <- paste(spline_names, collapse = " + ")
  full_formula <- as.formula(paste(
    "epds_prorated ~ 1 +",
    spline_terms,
    "+ centered_age + centered_age_sq + ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + centered_age | aln) + (1 + centered_age | time_point)"
  ))
  
  # 4. Run model
  model_repro <- runMLwiN(
    full_formula,
    D = "Normal",
    data = df,
    estoptions = list(EstM = 0)
  )
  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
  model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_repro, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
  model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
  
  # Plot figure
  # Predict values
  repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_repro,"parms")
  spline_basis_seq <- rcs(repro_seq, spline_knots)
  
  pred_model_repro <- data.frame(
    repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
    centered_age = 0,
    centered_age_sq = 0,
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("4", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)), 
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model_repro[, spline_names] <- spline_basis_seq
  
  # Predict values
  predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)

  # Prediction df for plotting
  pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                    predicted = predicted_model_repro$fit,
                                    CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                    CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
  
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
    geom_line(color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
    #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
    labs(
      #title = "Trajectory of Depression Over Reproductive Age",
      x = "Years around final menstrual period (FMP)",
      y = "Predicted Depressive Symptoms Score"
    ) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 9))+
    scale_x_continuous(breaks = seq(-20,20,
                                    by = 5),limits = c(-20,20))+
    scale_y_continuous(breaks = seq(5,10,
                                    by = 1))+
    annotate("text",
             x = 32-50,
             y = 10,
             label = plot_label,
             hjust = 1, vjust = 1, size = 16, color = "black",
             family = "serif", fontface = "bold"
    )
  
  assign(paste0("plot_",plot_label),plot)
}

grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
ggsave("Final plots/model_fit/repro_age_adjusted_for_chrono.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")

# Trajectory across chronological age
knot_numbers <- 3:6
for (k in knot_numbers) {
  
  if(k==3){
    model_age <- runMLwiN(epds_prorated ~ 1 + age 
                            + ethnicity + social_class + education + age_menarche
                            + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                            + (1 + age | aln) + (1 + age | time_point),
                            D = "Normal",
                            data = df,
                            estoptions = list(EstM = 0)
    )
    
    model_age_sq <- runMLwiN(epds_prorated ~ 1 + age + I(age^2)
                          + ethnicity + social_class + education + age_menarche
                          + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                          + (1 + age | aln) + (1 + age | time_point),
                          D = "Normal",
                          data = df,
                          estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological unadjusted for reproductive - figure 3"
    model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological unadjusted for reproductive - figure 3"
    model_fit[nrow(model_fit),"N participants"] <- model_age_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_age_sq)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_age_sq)

    pred_model_age <- data.frame(
      age = seq(chrono_age_lower,chrono_age_upper,length.out = 100),
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("4", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)), 
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    # Predict values
    predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link",se.fit = T)
    predicted_model_age_sq <- predict(model_age_sq, newdata = pred_model_age, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_age <- data.frame(age = pred_model_age$age,
                                    predicted = predicted_model_age$fit,
                                    CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
                                    CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit)
    
    pred_df_model_age_sq <- data.frame(age = pred_model_age$age,
                                    predicted = predicted_model_age_sq$fit,
                                    CI_low = predicted_model_age_sq$fit - 1.96 * predicted_model_age_sq$se.fit,
                                    CI_high = predicted_model_age_sq$fit + 1.96 * predicted_model_age_sq$se.fit)
    
    
    # Plot with confidence intervals
    plot_a <- ggplot(pred_df_model_age, aes(x = age, y = predicted)) +
      geom_line(color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Age (years)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 9)) +
      scale_y_continuous(breaks = seq(4,11,
                                      by = 1),limits = c(4,11))+
      annotate("text", 
               x = 31.5, 
               y = 10.9, 
               label = "a",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b <- ggplot(pred_df_model_age_sq, aes(x = age, y = predicted)) +
      geom_line(color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Age (years)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 9)) +
      scale_y_continuous(breaks = seq(4,11,
                                      by = 1),limits = c(4,11))+
      annotate("text", 
               x = 31.5, 
               y = 10.9, 
               label = "b",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    
  }
  
  # 1. Create spline basis
  spline_basis_age <- rms::rcs(df$age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_age))) {
    df[[paste0("spline", i, "_age")]] <- as.numeric(spline_basis_age[, i])
  }
  
  spline_names <- paste0("spline", 1:ncol(spline_basis_age), "_age")
  spline_terms <- paste(spline_names, collapse = " + ")
  full_formula <- as.formula(paste(
    "epds_prorated ~ 1 +",
    spline_terms,
    "+ ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + age | aln) + (1 + age | time_point)"
  ))
  
  # 4. Run model
  model_age <- runMLwiN(
    full_formula,
    D = "Normal",
    data = df,
    estoptions = list(EstM = 0)
  )
  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "chronological unadjusted for reproductive - figure 3"
  model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_age, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
  model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
  
  # Plot figure
  # Predict values
  age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_age,"parms")
  spline_basis_seq <- rcs(age_seq, spline_knots)
  
  pred_model_age <- data.frame(
    age = seq(chrono_age_lower,chrono_age_upper,length.out = 100),
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("4", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)), 
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model_age[, spline_names] <- spline_basis_seq
  
  # Predict values
  predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model_age <- data.frame(age = pred_model_age$age,
                                    predicted = predicted_model_age$fit,
                                    CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
                                    CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit)
  
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_age, aes(x = age, y = predicted)) +
    geom_line(color = "blue", linewidth = 1.5) +
    geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
    geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
    geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
    labs(
      #title = "Trajectory of Depression Over Reproductive Age",
      x = "Age (years)",
      y = "Predicted Depressive Symptoms Score"
    ) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 9)) +
    scale_y_continuous(breaks = seq(4,11,
                                    by = 1),limits = c(4,11))+
    annotate("text", 
             x = 31.5, 
             y = 10.9, 
             label = plot_label,
             hjust = 1, vjust = 1, size = 16, color = "black",
             family = "serif", fontface = "bold"
    )
  
  assign(paste0("plot_",plot_label),plot)
}

grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
ggsave("Final plots/model_fit/chrono_age_unadjusted_for_repro.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")

# Trajectory using age at menopause
knot_numbers <- 3:6
for (k in knot_numbers) {
  if(k==3){
    model_age <- runMLwiN(epds_prorated ~ 1 + age*age_meno_cat
                          + ethnicity + social_class + education + age_menarche
                          + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                          + (1 + age | aln) + (1 + age | time_point),
                          D = "Normal",
                          data = df,
                          estoptions = list(EstM = 0)
    )
    
    model_age_sq <- runMLwiN(epds_prorated ~ 1 + age*age_meno_cat + age_sq*age_meno_cat
                             + ethnicity + social_class + education + age_menarche
                             + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                             + (1 + age | aln) + (1 + age | time_point),
                             D = "Normal",
                             data = df,
                             estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
    model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
    model_fit[nrow(model_fit),"N participants"] <- model_age_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_age_sq)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_age_sq)
    
    pred_model <- data.frame(
      age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Under 45")]),
                    to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Under 45")]), length.out = 100),2),
      
      age_meno_cat = c(rep("Under 45",100),rep("Over 45",100)),
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("1", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)),
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    pred_model$age_sq <- pred_model$age^2
    pred_model$age_meno_cat <- factor(pred_model$age_meno_cat, levels = levels(df$age_meno_cat))
    
    # Predict values
    predicted_model_age <- predict(model_age, newdata = pred_model, type = "link",se.fit = T)
    predicted_model_age_sq <- predict(model_age_sq, newdata = pred_model, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_age <- data.frame(age = pred_model$age,
                                    predicted = predicted_model_age$fit,
                                    CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
                                    CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit,
                                    age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
                                                          levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
    
    pred_df_model_age_sq <- data.frame(age = pred_model$age,
                                    predicted = predicted_model_age_sq$fit,
                                    CI_low = predicted_model_age_sq$fit - 1.96 * predicted_model_age_sq$se.fit,
                                    CI_high = predicted_model_age_sq$fit + 1.96 * predicted_model_age_sq$se.fit,
                                    age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
                                                          levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
    
    # Plot with confidence intervals
    plot_a <- ggplot(pred_df_model_age, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
      geom_line(linewidth = 1.5) +
      labs(
        x = "Age (Years)",
        y = "Predicted Depressive Symptoms Score",
        colour = NULL,
        fill = NULL,
        linetype = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 9),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = "black"),
        #legend.box.margin = margin(5, 5, 5, 5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "lines")
      ) +
      scale_x_continuous(breaks = seq(25, 70, by = 5))+
      scale_y_continuous(breaks = seq(4,11,
                                      by = 1),limits = c(4,11))+
      annotate("text", 
               x = 28, 
               y = 11, 
               label = "a",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b <- ggplot(pred_df_model_age_sq, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
      geom_line(linewidth = 1.5) +
      labs(
        x = "Age (Years)",
        y = "Predicted Depressive Symptoms Score",
        colour = NULL,
        fill = NULL,
        linetype = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 9),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = "black"),
        #legend.box.margin = margin(5, 5, 5, 5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "lines")
      ) +
      scale_x_continuous(breaks = seq(25, 70, by = 5))+
      scale_y_continuous(breaks = seq(4,11,
                                      by = 1),limits = c(4,11))+
      annotate("text", 
               x = 28, 
               y = 11, 
               label = "b",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
  }
  
  # 1. Create spline basis
  spline_basis_age <- rms::rcs(df$age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_age))) {
    df[[paste0("spline", i, "_age")]] <- as.numeric(spline_basis_age[, i])
  }
  
  spline_names_no_interaction <- paste0("spline", 1:ncol(spline_basis_age), "_age")
  spline_names <- paste0("spline", 1:ncol(spline_basis_age), "_age*age_meno_cat")
  spline_terms <- paste(spline_names, collapse = " + ")
  
  full_formula <- as.formula(paste(
    "epds_prorated ~ 1 +",
    spline_terms,
    "+ ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + age | aln) + (1 + age | time_point)"
  ))
  
  # 4. Run model
  model_age <- runMLwiN(
    full_formula,
    D = "Normal",
    data = df,
    estoptions = list(EstM = 0)
  )
  model_age
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
  model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_age, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
  model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
  
  # Plot figure
  # Predict values
  age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_age,"parms")
  spline_basis_seq <- rcs(age_seq, spline_knots)
  
  
  pred_model <- data.frame(
    age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Under 45")]),
              to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Under 45")]), length.out = 100),2),
    
    age_meno_cat = c(rep("Under 45",100),rep("Over 45",100)),
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("1", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)),
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model[1:100, spline_names_no_interaction] <- spline_basis_seq
  pred_model[101:200, spline_names_no_interaction] <- spline_basis_seq
  
  pred_model$age_meno_cat <- factor(pred_model$age_meno_cat, levels = levels(df$age_meno_cat))

  # Predict values
  predicted_model_age <- predict(model_age, newdata = pred_model, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model_age <- data.frame(age = pred_model$age,
                                  predicted = predicted_model_age$fit,
                                  CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
                                  CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit,
                                  age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
                                         levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_age, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
    geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
    geom_line(linewidth = 1.5) +
    labs(
      x = "Age (Years)",
      y = "Predicted Depressive Symptoms Score",
      colour = NULL,
      fill = NULL,
      linetype = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 9),
      legend.position = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", color = "black"),
      #legend.box.margin = margin(5, 5, 5, 5),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1, "lines")
    ) +
    scale_x_continuous(breaks = seq(25, 70, by = 5))+
    scale_y_continuous(breaks = seq(4,11,
                                    by = 1),limits = c(4,11))+
    annotate("text", 
             x = 28, 
             y = 11, 
             label = plot_label,
             hjust = 1, vjust = 1, size = 16, color = "black",
             family = "serif", fontface = "bold"
    )
  
  assign(paste0("plot_",plot_label),plot)
}

grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
ggsave("Final plots/model_fit/chrono_age_by_menopause_categories.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")

# # Trajectory across reproductive age - 3 EPDS measures
# df <- readRDS(paste0(filestore,"analysis_df_complete_case_3_epds.rds"))
# 
# df$age_sq <- df$age^2
# df$centered_age_sq <- df$centered_age^2
# 
# repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025,na.rm = T))
# repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975,na.rm = T))
# 
# chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
# chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))
# 
# df$age_meno_cat <- NA_character_
# df$age_meno_cat[which(df$age_menopause < 45)] <- "Under 45"
# df$age_meno_cat[which(df$age_menopause >= 45)] <- "Over 45"
# df$age_meno_cat <- as.factor(df$age_meno_cat)
# 
# quantiles <- df %>% 
#   group_by(age_meno_cat) %>%
#   dplyr::summarize(lower_bound = quantile(age, 0.025, na.rm=T),
#                    upper_bound = quantile(age, 0.975, na.rm=T))
# 
# knot_numbers <- 3:6
# for (k in knot_numbers) {
#   if(k == 3){
#     model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age
#                             + centered_age + centered_age_sq 
#                             + ethnicity + social_class + education + age_menarche
#                             + material_hardship + social_support + smoking_status + bmi + alcohol_intake
#                             + (1 + centered_age | aln) + (1 + centered_age | time_point),
#                             D = "Normal",
#                             data = df,
#                             estoptions = list(EstM = 0)
#     )
#     
#     model_repro_sq <- runMLwiN(epds_prorated ~ 1 + repro_age + I(repro_age^2)
#                                + centered_age + centered_age_sq 
#                                + ethnicity + social_class + education + age_menarche
#                                + material_hardship + social_support + smoking_status + bmi + alcohol_intake
#                                + (1 + centered_age | aln) + (1 + centered_age | time_point),
#                                D = "Normal",
#                                data = df,
#                                estoptions = list(EstM = 0)
#     )
#     
#     model_fit[nrow(model_fit) + 1, ] <- NA
#     model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
#     model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
#     model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
#     model_fit[nrow(model_fit),"df"] <- "Linear"
#     model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
#     model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
#     
#     model_fit[nrow(model_fit) + 1, ] <- NA
#     model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
#     model_fit[nrow(model_fit),"N participants"] <- model_repro_sq@Hierarchy[,"N_complete"]
#     model_fit[nrow(model_fit),"N observations"] <- model_repro_sq@Nobs
#     model_fit[nrow(model_fit),"df"] <- "Quadratic"
#     model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro_sq)
#     model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro_sq)
#     
#     # Plot figure
#     # Predict values
#     pred_model_repro <- data.frame(
#       repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
#       centered_age = 0,
#       centered_age_sq = 0,
#       
#       # Convert factor variables to factors with the same levels as in df
#       ethnicity = factor("White", levels = levels(df$ethnicity)),
#       social_class = factor("4", levels = levels(df$social_class)),
#       education = factor("A-level_or_greater", levels = levels(df$education)),
#       age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
#       smoking_status = factor("Never", levels = levels(df$smoking_status)), 
#       alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
#       
#       # Use mean values for continuous variables
#       material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
#       social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
#       bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
#     )
#     
#     # Predict values
#     predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
#     predicted_model_repro_sq <- predict(model_repro_sq, newdata = pred_model_repro, type = "link",se.fit = T)
#     
#     # Prediction df for plotting
#     pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
#                                       predicted = predicted_model_repro$fit,
#                                       CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
#                                       CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
#     
#     pred_df_model_repro_sq <- data.frame(repro_age = pred_model_repro$repro_age,
#                                          predicted = predicted_model_repro_sq$fit,
#                                          CI_low = predicted_model_repro_sq$fit - 1.96 * predicted_model_repro_sq$se.fit,
#                                          CI_high = predicted_model_repro_sq$fit + 1.96 * predicted_model_repro_sq$se.fit)
#     
#     
#     
#     # Plot with confidence intervals
#     plot_a <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
#       geom_line(color = "blue",linewidth = 1.5) +
#       geom_line(aes(y = CI_low),linetype = "dashed", color = "blue",linewidth = 1.5) +
#       geom_line(aes(y = CI_high),linetype = "dashed", color = "blue",linewidth = 1.5) +
#       geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
#       #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
#       #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
#       labs(
#         #title = "Trajectory of Depression Over Reproductive Age",
#         x = "Years around final menstrual period (FMP)",
#         y = "Predicted Depressive Symptoms Score"
#       ) +
#       theme_minimal() +
#       theme(panel.grid.major.x = element_blank(),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.minor.y = element_blank(),
#             axis.line = element_line(color = "black"),
#             axis.text = element_text(size = 12),
#             axis.title.x = element_text(size = 12),
#             axis.title.y = element_text(size = 9))+
#       scale_x_continuous(breaks = seq(-20,20,
#                                       by = 5),limits = c(-20,20))+
#       scale_y_continuous(breaks = seq(5,10,
#                                       by = 1))+
#       annotate("text",
#                x = 32-50,
#                y = 10,
#                label = "a",
#                hjust = 1, vjust = 1, size = 16, color = "black",
#                family = "serif", fontface = "bold"
#       )
#     
#     plot_b <- ggplot(pred_df_model_repro_sq, aes(x = repro_age, y = predicted)) +
#       geom_line(color = "blue",linewidth = 1.5) +
#       geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
#       geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
#       geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
#       #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
#       #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
#       labs(
#         #title = "Trajectory of Depression Over Reproductive Age",
#         x = "Years around final menstrual period (FMP)",
#         y = "Predicted Depressive Symptoms Score"
#       ) +
#       theme_minimal() +
#       theme(panel.grid.major.x = element_blank(),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.minor.y = element_blank(),
#             axis.line = element_line(color = "black"),
#             axis.text = element_text(size = 12),
#             axis.title.x = element_text(size = 12),
#             axis.title.y = element_text(size = 9))+
#       scale_x_continuous(breaks = seq(-20,20,
#                                       by = 5),limits = c(-20,20))+
#       scale_y_continuous(breaks = seq(5,10,
#                                       by = 1))+
#       annotate("text",
#                x = 32-50,
#                y = 10,
#                label = "b",
#                hjust = 1, vjust = 1, size = 16, color = "black",
#                family = "serif", fontface = "bold"
#       )
#   }
#  
#   
#   # 1. Create spline basis
#   spline_basis_repro <- rms::rcs(df$repro_age, k)
#   
#   # 2. Add spline columns to df
#   for (i in 1:(ncol(spline_basis_repro))) {
#     df[[paste0("spline", i, "_repro")]] <- as.numeric(spline_basis_repro[, i])
#   }
#   
#   spline_names <- paste0("spline", 1:ncol(spline_basis_repro), "_repro")
#   spline_terms <- paste(spline_names, collapse = " + ")
#   full_formula <- as.formula(paste(
#     "epds_prorated ~ 1 +",
#     spline_terms,
#     "+ centered_age + centered_age_sq + ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + centered_age | aln) + (1 + centered_age | time_point)"
#   ))
#   
#   # 4. Run model
#   model_repro <- runMLwiN(
#     full_formula,
#     D = "Normal",
#     data = df,
#     estoptions = list(EstM = 0)
#   )
#   
#   model_fit[nrow(model_fit) + 1, ] <- NA
#   model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
#   model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
#   model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
#   model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_repro, "parms")) - 1
#   model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
#   model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
#   
#   # Plot figure
#   # Predict values
#   repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)
#   
#   # Generate restricted cubic spline terms for the sequence
#   spline_knots <- attr(spline_basis_repro,"parms")
#   spline_basis_seq <- rcs(repro_seq, spline_knots)
#   
#   pred_model_repro <- data.frame(
#     repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
#     centered_age = 0,
#     centered_age_sq = 0,
#     
#     # Convert factor variables to factors with the same levels as in df
#     ethnicity = factor("White", levels = levels(df$ethnicity)),
#     social_class = factor("4", levels = levels(df$social_class)),
#     education = factor("A-level_or_greater", levels = levels(df$education)),
#     age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
#     smoking_status = factor("Never", levels = levels(df$smoking_status)), 
#     alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
#     
#     # Use mean values for continuous variables
#     material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
#     social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
#     bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
#   )
#   
#   pred_model_repro[, spline_names] <- spline_basis_seq
#   
#   # Predict values
#   predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
#   
#   # Prediction df for plotting
#   pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
#                                     predicted = predicted_model_repro$fit,
#                                     CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
#                                     CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
#   
#   
#   # Plot with confidence intervals
#   plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
#   plot <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
#     geom_line(color = "blue",linewidth = 1.5) +
#     geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
#     geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
#     #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
#     #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
#     labs(
#       #title = "Trajectory of Depression Over Reproductive Age",
#       x = "Years around final menstrual period (FMP)",
#       y = "Predicted Depressive Symptoms Score"
#     ) +
#     theme_minimal() +
#     theme(panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           panel.grid.minor.y = element_blank(),
#           axis.line = element_line(color = "black"),
#           axis.text = element_text(size = 12),
#           axis.title.x = element_text(size = 12),
#           axis.title.y = element_text(size = 9))+
#     scale_x_continuous(breaks = seq(-20,20,
#                                     by = 5),limits = c(-20,20))+
#     scale_y_continuous(breaks = seq(5,10,
#                                     by = 1))+
#     annotate("text",
#              x = 32-50,
#              y = 10,
#              label = plot_label,
#              hjust = 1, vjust = 1, size = 16, color = "black",
#              family = "serif", fontface = "bold"
#     )
#   
#   assign(paste0("plot_",plot_label),plot)
# }
# 
# grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
# ggsave("Final plots/model_fit/repro_age_adjusted_for_chrono_3_epds.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")
# 
# # Trajectory across chronological age
# knot_numbers <- 3:6
# for (k in knot_numbers) {
#   
#   if(k==3){
#     model_age <- runMLwiN(epds_prorated ~ 1 + age 
#                           + ethnicity + social_class + education + age_menarche
#                           + material_hardship + social_support + smoking_status + bmi + alcohol_intake
#                           + (1 + age | aln) + (1 + age | time_point),
#                           D = "Normal",
#                           data = df,
#                           estoptions = list(EstM = 0)
#     )
#     
#     model_age_sq <- runMLwiN(epds_prorated ~ 1 + age + I(age^2)
#                              + ethnicity + social_class + education + age_menarche
#                              + material_hardship + social_support + smoking_status + bmi + alcohol_intake
#                              + (1 + age | aln) + (1 + age | time_point),
#                              D = "Normal",
#                              data = df,
#                              estoptions = list(EstM = 0)
#     )
#     
#     model_fit[nrow(model_fit) + 1, ] <- NA
#     model_fit[nrow(model_fit),"model"] <- "chronological unadjusted for reproductive - figure 3"
#     model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
#     model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
#     model_fit[nrow(model_fit),"df"] <- "Linear"
#     model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
#     model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
#     
#     model_fit[nrow(model_fit) + 1, ] <- NA
#     model_fit[nrow(model_fit),"model"] <- "chronological unadjusted for reproductive - figure 3"
#     model_fit[nrow(model_fit),"N participants"] <- model_age_sq@Hierarchy[,"N_complete"]
#     model_fit[nrow(model_fit),"N observations"] <- model_age_sq@Nobs
#     model_fit[nrow(model_fit),"df"] <- "Quadratic"
#     model_fit[nrow(model_fit),"AIC"] <- AIC(model_age_sq)
#     model_fit[nrow(model_fit),"BIC"] <- BIC(model_age_sq)
#     
#     pred_model_age <- data.frame(
#       age = seq(chrono_age_lower,chrono_age_upper,length.out = 100),
#       
#       # Convert factor variables to factors with the same levels as in df
#       ethnicity = factor("White", levels = levels(df$ethnicity)),
#       social_class = factor("4", levels = levels(df$social_class)),
#       education = factor("A-level_or_greater", levels = levels(df$education)),
#       age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
#       smoking_status = factor("Never", levels = levels(df$smoking_status)), 
#       alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
#       
#       # Use mean values for continuous variables
#       material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
#       social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
#       bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
#     )
#     
#     # Predict values
#     predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link",se.fit = T)
#     predicted_model_age_sq <- predict(model_age_sq, newdata = pred_model_age, type = "link",se.fit = T)
#     
#     # Prediction df for plotting
#     pred_df_model_age <- data.frame(age = pred_model_age$age,
#                                     predicted = predicted_model_age$fit,
#                                     CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
#                                     CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit)
#     
#     pred_df_model_age_sq <- data.frame(age = pred_model_age$age,
#                                        predicted = predicted_model_age_sq$fit,
#                                        CI_low = predicted_model_age_sq$fit - 1.96 * predicted_model_age_sq$se.fit,
#                                        CI_high = predicted_model_age_sq$fit + 1.96 * predicted_model_age_sq$se.fit)
#     
#     
#     # Plot with confidence intervals
#     plot_a <- ggplot(pred_df_model_age, aes(x = age, y = predicted)) +
#       geom_line(color = "blue", linewidth = 1.5) +
#       geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
#       geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
#       geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
#       #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
#       labs(
#         #title = "Trajectory of Depression Over Reproductive Age",
#         x = "Age (years)",
#         y = "Predicted Depressive Symptoms Score"
#       ) +
#       theme_minimal() +
#       theme(panel.grid.major.x = element_blank(),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.minor.y = element_blank(),
#             axis.line = element_line(color = "black"),
#             axis.text = element_text(size = 12),
#             axis.title.x = element_text(size = 12),
#             axis.title.y = element_text(size = 9)) +
#       scale_y_continuous(breaks = seq(4,10,
#                                       by = 2),limits = c(4,11))+
#       annotate("text", 
#                x = 31.5, 
#                y = 10.9, 
#                label = "a",
#                hjust = 1, vjust = 1, size = 16, color = "black",
#                family = "serif", fontface = "bold"
#       )
#     
#     plot_b <- ggplot(pred_df_model_age_sq, aes(x = age, y = predicted)) +
#       geom_line(color = "blue", linewidth = 1.5) +
#       geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
#       geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
#       geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
#       #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
#       labs(
#         #title = "Trajectory of Depression Over Reproductive Age",
#         x = "Age (years)",
#         y = "Predicted Depressive Symptoms Score"
#       ) +
#       theme_minimal() +
#       theme(panel.grid.major.x = element_blank(),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.minor.y = element_blank(),
#             axis.line = element_line(color = "black"),
#             axis.text = element_text(size = 12),
#             axis.title.x = element_text(size = 12),
#             axis.title.y = element_text(size = 9)) +
#       scale_y_continuous(breaks = seq(4,10,
#                                       by = 2),limits = c(4,11))+
#       annotate("text", 
#                x = 31.5, 
#                y = 10.9, 
#                label = "b",
#                hjust = 1, vjust = 1, size = 16, color = "black",
#                family = "serif", fontface = "bold"
#       )
#   }
#   
#   # 1. Create spline basis
#   spline_basis_age <- rms::rcs(df$age, k)
#   
#   # 2. Add spline columns to df
#   for (i in 1:(ncol(spline_basis_age))) {
#     df[[paste0("spline", i, "_age")]] <- as.numeric(spline_basis_age[, i])
#   }
#   
#   spline_names <- paste0("spline", 1:ncol(spline_basis_age), "_age")
#   spline_terms <- paste(spline_names, collapse = " + ")
#   full_formula <- as.formula(paste(
#     "epds_prorated ~ 1 +",
#     spline_terms,
#     "+ ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + age | aln) + (1 + age | time_point)"
#   ))
#   
#   # 4. Run model
#   model_age <- runMLwiN(
#     full_formula,
#     D = "Normal",
#     data = df,
#     estoptions = list(EstM = 0)
#   )
#   
#   model_fit[nrow(model_fit) + 1, ] <- NA
#   model_fit[nrow(model_fit),"model"] <- "chronological adjusted for reproductive - figure 3"
#   model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
#   model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
#   model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_age, "parms")) - 1
#   model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
#   model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
#   
#   # Plot figure
#   # Predict values
#   age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)
#   
#   # Generate restricted cubic spline terms for the sequence
#   spline_knots <- attr(spline_basis_age,"parms")
#   spline_basis_seq <- rcs(age_seq, spline_knots)
#   
#   pred_model_age <- data.frame(
#     age = seq(chrono_age_lower,chrono_age_upper,length.out = 100),
#     
#     # Convert factor variables to factors with the same levels as in df
#     ethnicity = factor("White", levels = levels(df$ethnicity)),
#     social_class = factor("4", levels = levels(df$social_class)),
#     education = factor("A-level_or_greater", levels = levels(df$education)),
#     age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
#     smoking_status = factor("Never", levels = levels(df$smoking_status)), 
#     alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
#     
#     # Use mean values for continuous variables
#     material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
#     social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
#     bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
#   )
#   
#   pred_model_age[, spline_names] <- spline_basis_seq
#   
#   # Predict values
#   predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link",se.fit = T)
#   
#   # Prediction df for plotting
#   pred_df_model_age <- data.frame(age = pred_model_age$age,
#                                   predicted = predicted_model_age$fit,
#                                   CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
#                                   CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit)
#   
#   
#   # Plot with confidence intervals
#   plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
#   plot <- ggplot(pred_df_model_age, aes(x = age, y = predicted)) +
#     geom_line(color = "blue", linewidth = 1.5) +
#     geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
#     geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
#     geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
#     #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
#     labs(
#       #title = "Trajectory of Depression Over Reproductive Age",
#       x = "Age (years)",
#       y = "Predicted Depressive Symptoms Score"
#     ) +
#     theme_minimal() +
#     theme(panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           panel.grid.minor.y = element_blank(),
#           axis.line = element_line(color = "black"),
#           axis.text = element_text(size = 12),
#           axis.title.x = element_text(size = 12),
#           axis.title.y = element_text(size = 9)) +
#     scale_y_continuous(breaks = seq(4,11,
#                                     by = 1),limits = c(4,11))+
#     annotate("text", 
#              x = 31.5, 
#              y = 10.9, 
#              label = plot_label,
#              hjust = 1, vjust = 1, size = 16, color = "black",
#              family = "serif", fontface = "bold"
#     )
#   assign(paste0("plot_",plot_label),plot)
# }
# 
# grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
# ggsave("Final plots/model_fit/chrono_age_unadjusted_for_repro_3_epds.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")
# 
# # Trajectory using age at menopause
# knot_numbers <- 3:6
# for (k in knot_numbers) {
#   if(k==3){
#     model_age <- runMLwiN(epds_prorated ~ 1 + age*age_meno_cat
#                           + ethnicity + social_class + education + age_menarche
#                           + material_hardship + social_support + smoking_status + bmi + alcohol_intake
#                           + (1 + age | aln) + (1 + age | time_point),
#                           D = "Normal",
#                           data = df,
#                           estoptions = list(EstM = 0)
#     )
#     
#     model_age_sq <- runMLwiN(epds_prorated ~ 1 + age*age_meno_cat + age_sq*age_meno_cat
#                              + ethnicity + social_class + education + age_menarche
#                              + material_hardship + social_support + smoking_status + bmi + alcohol_intake
#                              + (1 + age | aln) + (1 + age | time_point),
#                              D = "Normal",
#                              data = df,
#                              estoptions = list(EstM = 0)
#     )
#     
#     model_fit[nrow(model_fit) + 1, ] <- NA
#     model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
#     model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
#     model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
#     model_fit[nrow(model_fit),"df"] <- "Linear"
#     model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
#     model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
#     
#     model_fit[nrow(model_fit) + 1, ] <- NA
#     model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
#     model_fit[nrow(model_fit),"N participants"] <- model_age_sq@Hierarchy[,"N_complete"]
#     model_fit[nrow(model_fit),"N observations"] <- model_age_sq@Nobs
#     model_fit[nrow(model_fit),"df"] <- "Quadratic"
#     model_fit[nrow(model_fit),"AIC"] <- AIC(model_age_sq)
#     model_fit[nrow(model_fit),"BIC"] <- BIC(model_age_sq)
#     
#     pred_model <- data.frame(
#       age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Under 45")]),
#                     to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Under 45")]), length.out = 100),2),
#       
#       age_meno_cat = c(rep("Under 45",100),rep("Over 45",100)),
#       
#       # Convert factor variables to factors with the same levels as in df
#       ethnicity = factor("White", levels = levels(df$ethnicity)),
#       social_class = factor("1", levels = levels(df$social_class)),
#       education = factor("A-level_or_greater", levels = levels(df$education)),
#       age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
#       smoking_status = factor("Never", levels = levels(df$smoking_status)),
#       alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
#       
#       # Use mean values for continuous variables
#       material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
#       social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
#       bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
#     )
#     
#     pred_model$age_sq <- pred_model$age^2
#     pred_model$age_meno_cat <- factor(pred_model$age_meno_cat, levels = levels(df$age_meno_cat))
#     
#     # Predict values
#     predicted_model_age <- predict(model_age, newdata = pred_model, type = "link",se.fit = T)
#     predicted_model_age_sq <- predict(model_age_sq, newdata = pred_model, type = "link",se.fit = T)
#     
#     # Prediction df for plotting
#     pred_df_model_age <- data.frame(age = pred_model$age,
#                                     predicted = predicted_model_age$fit,
#                                     CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
#                                     CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit,
#                                     age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
#                                                           levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
#     
#     pred_df_model_age_sq <- data.frame(age = pred_model$age,
#                                        predicted = predicted_model_age_sq$fit,
#                                        CI_low = predicted_model_age_sq$fit - 1.96 * predicted_model_age_sq$se.fit,
#                                        CI_high = predicted_model_age_sq$fit + 1.96 * predicted_model_age_sq$se.fit,
#                                        age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
#                                                              levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
#     
#     # Plot with confidence intervals
#     plot_a <- ggplot(pred_df_model_age, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
#       #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
#       geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
#       geom_line(linewidth = 1.5) +
#       labs(
#         x = "Age (Years)",
#         y = "Predicted Depressive Symptoms Score",
#         colour = NULL,
#         fill = NULL,
#         linetype = NULL
#       ) +
#       theme_minimal() +
#       theme(
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.line = element_line(color = "black"),
#         axis.text = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 9),
#         legend.position = c(0.95, 0.05),
#         legend.justification = c(1, 0),
#         legend.background = element_rect(fill = "white", color = "black"),
#         #legend.box.margin = margin(5, 5, 5, 5),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 9),
#         legend.key.size = unit(1, "lines")
#       ) +
#       scale_x_continuous(breaks = seq(25, 70, by = 5))+
#       annotate("text", 
#                x = 28, 
#                y = 11, 
#                label = "a",
#                hjust = 1, vjust = 1, size = 16, color = "black",
#                family = "serif", fontface = "bold"
#       )
#     
#     plot_b <- ggplot(pred_df_model_age_sq, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
#       #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
#       geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
#       geom_line(linewidth = 1.5) +
#       labs(
#         x = "Age (Years)",
#         y = "Predicted Depressive Symptoms Score",
#         colour = NULL,
#         fill = NULL,
#         linetype = NULL
#       ) +
#       theme_minimal() +
#       theme(
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.line = element_line(color = "black"),
#         axis.text = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 9),
#         legend.position = c(0.95, 0.05),
#         legend.justification = c(1, 0),
#         legend.background = element_rect(fill = "white", color = "black"),
#         #legend.box.margin = margin(5, 5, 5, 5),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 9),
#         legend.key.size = unit(1, "lines")
#       ) +
#       scale_x_continuous(breaks = seq(25, 70, by = 5))+
#       annotate("text", 
#                x = 28, 
#                y = 11, 
#                label = "b",
#                hjust = 1, vjust = 1, size = 16, color = "black",
#                family = "serif", fontface = "bold"
#       )
#   }
#   
#   # 1. Create spline basis
#   spline_basis_age <- rms::rcs(df$age, k)
#   
#   # 2. Add spline columns to df
#   for (i in 1:(ncol(spline_basis_age))) {
#     df[[paste0("spline", i, "_age")]] <- as.numeric(spline_basis_age[, i])
#   }
#   
#   spline_names_no_interaction <- paste0("spline", 1:ncol(spline_basis_age), "_age")
#   spline_names <- paste0("spline", 1:ncol(spline_basis_age), "_age*age_meno_cat")
#   spline_terms <- paste(spline_names, collapse = " + ")
#   
#   full_formula <- as.formula(paste(
#     "epds_prorated ~ 1 +",
#     spline_terms,
#     "+ ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + age | aln) + (1 + age | time_point)"
#   ))
#   
#   # 4. Run model
#   model_age <- runMLwiN(
#     full_formula,
#     D = "Normal",
#     data = df,
#     estoptions = list(EstM = 0)
#   )
#   
#   
#   model_fit[nrow(model_fit) + 1, ] <- NA
#   model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
#   model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
#   model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
#   model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_age, "parms")) - 1
#   model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
#   model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
#   
#   # Plot figure
#   # Predict values
#   age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)
#   
#   # Generate restricted cubic spline terms for the sequence
#   spline_knots <- attr(spline_basis_age,"parms")
#   spline_basis_seq <- rcs(age_seq, spline_knots)
#   
#   
#   pred_model <- data.frame(
#     age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Under 45")]),
#                   to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Under 45")]), length.out = 100),2),
#     
#     age_meno_cat = c(rep("Under 45",100),rep("Over 45",100)),
#     
#     # Convert factor variables to factors with the same levels as in df
#     ethnicity = factor("White", levels = levels(df$ethnicity)),
#     social_class = factor("1", levels = levels(df$social_class)),
#     education = factor("A-level_or_greater", levels = levels(df$education)),
#     age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
#     smoking_status = factor("Never", levels = levels(df$smoking_status)),
#     alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
#     
#     # Use mean values for continuous variables
#     material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
#     social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
#     bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
#   )
#   
#   pred_model[1:100, spline_names_no_interaction] <- spline_basis_seq
#   pred_model[101:200, spline_names_no_interaction] <- spline_basis_seq
#   
#   pred_model$age_meno_cat <- factor(pred_model$age_meno_cat, levels = levels(df$age_meno_cat))
#   
#   # Predict values
#   predicted_model_age <- predict(model_age, newdata = pred_model, type = "link",se.fit = T)
#   
#   # Prediction df for plotting
#   pred_df_model_age <- data.frame(age = pred_model$age,
#                                   predicted = predicted_model_age$fit,
#                                   CI_low = predicted_model_age$fit - 1.96 * predicted_model_age$se.fit,
#                                   CI_high = predicted_model_age$fit + 1.96 * predicted_model_age$se.fit,
#                                   age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
#                                                         levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
#   
#   # Plot with confidence intervals
#   plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
#   plot <- ggplot(pred_df_model_age, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
#     #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
#     geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
#     geom_line(linewidth = 1.5) +
#     labs(
#       x = "Age (Years)",
#       y = "Predicted Depressive Symptoms Score",
#       colour = NULL,
#       fill = NULL,
#       linetype = NULL
#     ) +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       axis.line = element_line(color = "black"),
#       axis.text = element_text(size = 12),
#       axis.title.x = element_text(size = 12),
#       axis.title.y = element_text(size = 9),
#       legend.position = c(0.95, 0.05),
#       legend.justification = c(1, 0),
#       legend.background = element_rect(fill = "white", color = "black"),
#       #legend.box.margin = margin(5, 5, 5, 5),
#       legend.title = element_text(size = 12),
#       legend.text = element_text(size = 9),
#       legend.key.size = unit(1, "lines")
#     ) +
#     scale_x_continuous(breaks = seq(25, 70, by = 5))+
#     scale_y_continuous(breaks = seq(5,11,
#                                     by = 1),limits = c(5,11))+
#     annotate("text", 
#              x = 28, 
#              y = 11, 
#              label = plot_label,
#              hjust = 1, vjust = 1, size = 16, color = "black",
#              family = "serif", fontface = "bold"
#     )
#   
#   assign(paste0("plot_",plot_label),plot)
# }
# 
# grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
# ggsave("Final plots/model_fit/chrono_age_by_menopause_categories_3_epds.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")

# HRT stratification
tmp <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
tmp$ever_hrt_binary <- 0

replace <- which(tmp$ever_hrt == T)
tmp$ever_hrt_binary[replace] <- 1

tmp$ever_hrt_binary <- factor(tmp$ever_hrt_binary,
                              levels = c(0,1))

# Plot for those who have used HRT
df <- tmp %>% filter(ever_hrt == T)

df$age_sq <- df$age^2
df$centered_age_sq <- df$centered_age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025, na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975, na.rm = T))

knot_numbers <- 3:6
for (k in knot_numbers) {
  if(k == 3){
    model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age
                            + centered_age + centered_age_sq 
                            + ethnicity + social_class + education + age_menarche
                            + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                            + (1 + centered_age | aln) + (1 + centered_age | time_point),
                            D = "Normal",
                            data = df,
                            estoptions = list(EstM = 0)
    )
    
    model_repro_sq <- runMLwiN(epds_prorated ~ 1 + repro_age + I(repro_age^2)
                               + centered_age + centered_age_sq 
                               + ethnicity + social_class + education + age_menarche
                               + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                               + (1 + centered_age | aln) + (1 + centered_age | time_point),
                               D = "Normal",
                               data = df,
                               estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "HRT stratified - ever HRT - supp figure 6"
    model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "HRT stratified - ever HRT - supp figure 6"
    model_fit[nrow(model_fit),"N participants"] <- model_repro_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro_sq)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro_sq)
    
    # Plot figure
    # Predict values
    pred_model_repro <- data.frame(
      repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
      centered_age = 0,
      centered_age_sq = 0,
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("4", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)), 
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    # Predict values
    predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
    predicted_model_repro_sq <- predict(model_repro_sq, newdata = pred_model_repro, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                      predicted = predicted_model_repro$fit,
                                      CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                      CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
    
    pred_df_model_repro_sq <- data.frame(repro_age = pred_model_repro$repro_age,
                                         predicted = predicted_model_repro_sq$fit,
                                         CI_low = predicted_model_repro_sq$fit - 1.96 * predicted_model_repro_sq$se.fit,
                                         CI_high = predicted_model_repro_sq$fit + 1.96 * predicted_model_repro_sq$se.fit)
    
    
    
    # Plot with confidence intervals
    plot_a_1 <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 6))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(5,11,
                                      by = 1))+
      annotate("text",
               x = -17,
               y = 11,
               label = "a",
               hjust = 1, vjust = 1, size = 14, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b_1 <- ggplot(pred_df_model_repro_sq, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 6))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(5,11,
                                      by = 1))+
      annotate("text",
               x = -17,
               y = 11,
               label = "b",
               hjust = 1, vjust = 1, size = 14, color = "black",
               family = "serif", fontface = "bold"
      )
  }
  
  # 1. Create spline basis
  spline_basis_repro <- rms::rcs(df$repro_age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_repro))) {
    df[[paste0("spline", i, "_repro")]] <- as.numeric(spline_basis_repro[, i])
  }
  
  spline_names <- paste0("spline", 1:ncol(spline_basis_repro), "_repro")
  spline_terms <- paste(spline_names, collapse = " + ")
  full_formula <- as.formula(paste(
    "epds_prorated ~ 1 +",
    spline_terms,
    "+ centered_age + centered_age_sq + ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + centered_age | aln) + (1 + centered_age | time_point)"
  ))
  
  # 4. Run model
  model_repro <- runMLwiN(
    full_formula,
    D = "Normal",
    data = df,
    estoptions = list(EstM = 0)
  )
  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "HRT stratified - ever HRT - supp figure 6"
  model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_repro, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
  model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
  
  # Plot figure
  # Predict values
  repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_repro,"parms")
  spline_basis_seq <- rcs(repro_seq, spline_knots)
  
  pred_model_repro <- data.frame(
    repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
    centered_age = 0,
    centered_age_sq = 0,
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("4", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)), 
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model_repro[, spline_names] <- spline_basis_seq
  
  # Predict values
  predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                    predicted = predicted_model_repro$fit,
                                    CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                    CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
  
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
    geom_line(color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
    #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
    labs(
      #title = "Trajectory of Depression Over Reproductive Age",
      x = "Years around final menstrual period (FMP)",
      y = "Predicted Depressive Symptoms Score"
    ) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 6))+
    scale_x_continuous(breaks = seq(-20,20,
                                    by = 5),limits = c(-20,20))+
    scale_y_continuous(breaks = seq(5,11,
                                    by = 1),limits = c(5,11))+
    annotate("text",
             x = -17,
             y = 11,
             label = plot_label,
             hjust = 1, vjust = 1, size = 14, color = "black",
             family = "serif", fontface = "bold"
    )
  assign(paste0("plot_",plot_label,"_1"),plot)
}

# Plot for those who have never used HRT
df <- tmp %>% filter(ever_hrt == F)

df$age_sq <- df$age^2
df$centered_age_sq <- df$centered_age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025, na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975, na.rm = T))

knot_numbers <- 3:6
for (k in knot_numbers) {
  if(k == 3){
    model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age
                            + centered_age + centered_age_sq 
                            + ethnicity + social_class + education + age_menarche
                            + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                            + (1 + centered_age | aln) + (1 + centered_age | time_point),
                            D = "Normal",
                            data = df,
                            estoptions = list(EstM = 0)
    )
    
    model_repro_sq <- runMLwiN(epds_prorated ~ 1 + repro_age + I(repro_age^2)
                               + centered_age + centered_age_sq 
                               + ethnicity + social_class + education + age_menarche
                               + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                               + (1 + centered_age | aln) + (1 + centered_age | time_point),
                               D = "Normal",
                               data = df,
                               estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "HRT stratified - ever HRT - supp figure 6"
    model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "HRT stratified - ever HRT - supp figure 6"
    model_fit[nrow(model_fit),"N participants"] <- model_repro_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro_sq)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro_sq)
    
    # Plot figure
    # Predict values
    pred_model_repro <- data.frame(
      repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
      centered_age = 0,
      centered_age_sq = 0,
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("4", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)), 
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    # Predict values
    predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
    predicted_model_repro_sq <- predict(model_repro_sq, newdata = pred_model_repro, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                      predicted = predicted_model_repro$fit,
                                      CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                      CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
    
    pred_df_model_repro_sq <- data.frame(repro_age = pred_model_repro$repro_age,
                                         predicted = predicted_model_repro_sq$fit,
                                         CI_low = predicted_model_repro_sq$fit - 1.96 * predicted_model_repro_sq$se.fit,
                                         CI_high = predicted_model_repro_sq$fit + 1.96 * predicted_model_repro_sq$se.fit)
    
    
    
    # Plot with confidence intervals
    plot_a_2 <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 6))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(5,11,
                                      by = 1))+
      annotate("text",
               x = -17,
               y = 11,
               label = "a",
               hjust = 1, vjust = 1, size = 14, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b_2 <- ggplot(pred_df_model_repro_sq, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 6))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(5,11,
                                      by = 1),limits = c(5,11))+
      annotate("text",
               x = -17,
               y = 11,
               label = "b",
               hjust = 1, vjust = 1, size = 14, color = "black",
               family = "serif", fontface = "bold"
      )
  }
  
  # 1. Create spline basis
  spline_basis_repro <- rms::rcs(df$repro_age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_repro))) {
    df[[paste0("spline", i, "_repro")]] <- as.numeric(spline_basis_repro[, i])
  }
  
  spline_names <- paste0("spline", 1:ncol(spline_basis_repro), "_repro")
  spline_terms <- paste(spline_names, collapse = " + ")
  full_formula <- as.formula(paste(
    "epds_prorated ~ 1 +",
    spline_terms,
    "+ centered_age + centered_age_sq + ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + centered_age | aln) + (1 + centered_age | time_point)"
  ))
  
  # 4. Run model
  model_repro <- runMLwiN(
    full_formula,
    D = "Normal",
    data = df,
    estoptions = list(EstM = 0)
  )
  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "HRT stratified - never HRT - supp figure 6"
  model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_repro, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
  model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
  
  # Plot figure
  # Predict values
  repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_repro,"parms")
  spline_basis_seq <- rcs(repro_seq, spline_knots)
  
  pred_model_repro <- data.frame(
    repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
    centered_age = 0,
    centered_age_sq = 0,
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("4", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)), 
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model_repro[, spline_names] <- spline_basis_seq
  
  # Predict values
  predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                    predicted = predicted_model_repro$fit,
                                    CI_low = predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit,
                                    CI_high = predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit)
  
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
    geom_line(color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
    #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
    labs(
      #title = "Trajectory of Depression Over Reproductive Age",
      x = "Years around final menstrual period (FMP)",
      y = "Predicted Depressive Symptoms Score"
    ) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "black"),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 6),
          axis.text = element_text(size = 12))+
    scale_x_continuous(breaks = seq(-20,20,
                                    by = 5),limits = c(-20,20))+
    scale_y_continuous(breaks = seq(5,11,
                                    by = 1),limits = c(5,11))+
    annotate("text",
             x = -17,
             y = 11,
             label = plot_label,
             hjust = 1, vjust = 1, size = 14, color = "black",
             family = "serif", fontface = "bold"
    )
  assign(paste0("plot_",plot_label,"_2"),plot)
}

grid_arrange_plot <- grid.arrange(plot_a_1,plot_a_2,
                                  plot_b_1,plot_b_2,
                                  plot_c_1,plot_c_2,
                                  plot_d_1,plot_d_2,
                                  plot_e_1,plot_e_2,
                                  plot_f_1,plot_f_2,
                                  ncol = 2)  
ggsave("Final plots/model_fit/HRT_stratification.png", grid_arrange_plot, width = 8.27, height = 11.69 ,bg = "white")


# HRT interaction
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
df$ever_hrt_binary <- 0

replace <- which(df$ever_hrt == T)
df$ever_hrt_binary[replace] <- 1

df$ever_hrt_binary <- factor(df$ever_hrt_binary,
                             levels = c(0,1))

df$repro_age_sq <- df$repro_age^2
df$centered_age_sq <- df$centered_age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025, na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975, na.rm = T))

quantiles <- df %>% 
  group_by(ever_hrt_binary) %>%
  dplyr::summarize(lower_bound = quantile(repro_age, 0.025, na.rm=T),
                   upper_bound = quantile(repro_age, 0.975, na.rm=T))

knot_numbers <- 3:6
for (k in knot_numbers) {
  if(k == 3){
    model_repro <- runMLwiN(epds_prorated ~ 1 + repro_age*ever_hrt_binary
                           + centered_age + centered_age_sq
                           + ethnicity + social_class + education + age_menarche
                           + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                           + (1 + centered_age | aln) + (1 + centered_age | time_point),
                           D = "Normal",
                           data = df,
                           estoptions = list(EstM = 0)
    )
    
    model_repro_sq <- runMLwiN(epds_prorated ~ 1 + repro_age*ever_hrt_binary + repro_age_sq*ever_hrt_binary
                               + centered_age + centered_age_sq
                               + ethnicity + social_class + education + age_menarche
                               + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                               + (1 + centered_age | aln) + (1 + centered_age | time_point),
                               D = "Normal",
                               data = df,
                               estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "HRT interaction - supplementary figure 7"
    model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "HRT interaction - supplementary figure 7"
    model_fit[nrow(model_fit),"N participants"] <- model_repro_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro_sq)
    model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro_sq)
    
    # Plot figure
    # Predict values
    pred_model <- data.frame(
      repro_age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$ever_hrt_binary == "0")]),
                          to = as.numeric(quantiles$upper_bound[which(quantiles$ever_hrt_binary == "1")]), length.out = 100),2),
      centered_age = 0,
      centered_age_sq = 0,
      ever_hrt_binary = c(rep("0",100),rep("1",100)),
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("1", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)),
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    pred_model$repro_age_sq <- pred_model$repro_age^2
    pred_model$ever_hrt_binary <- factor(pred_model$ever_hrt_binary, levels = levels(df$ever_hrt_binary))
    
    # Predict values
    predicted_model <- predict(model_repro, newdata = pred_model, type = "link",se.fit = T)
    predicted_model_sq <- predict(model_repro_sq, newdata = pred_model, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model <- data.frame(repro_age = pred_model$repro_age,
                                predicted = predicted_model$fit,
                                CI_low = predicted_model$fit - 1.96 * predicted_model$se.fit,
                                CI_high = predicted_model$fit + 1.96 * predicted_model$se.fit,
                                ever_hrt_binary = factor(c(rep("Never HRT use", 100), rep("Ever HRT use", 100)),
                                                         levels = c("Ever HRT use", "Never HRT use")))
    
    pred_df_model_sq <- data.frame(repro_age = pred_model$repro_age,
                                predicted = predicted_model_sq$fit,
                                CI_low = predicted_model_sq$fit - 1.96 * predicted_model_sq$se.fit,
                                CI_high = predicted_model_sq$fit + 1.96 * predicted_model_sq$se.fit,
                                ever_hrt_binary = factor(c(rep("Never HRT use", 100), rep("Ever HRT use", 100)),
                                                         levels = c("Ever HRT use", "Never HRT use")))
    
    # Plot with confidence intervals
    plot_a <- ggplot(pred_df_model, aes(x = repro_age, y = predicted, colour = ever_hrt_binary, fill =ever_hrt_binary )) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
      geom_line(linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      labs(
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score",
        colour = NULL,
        fill = NULL,
        linetype = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 9),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = "black"),
        #legend.box.margin = margin(5, 5, 5, 5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "lines")
      ) +
      scale_x_continuous(breaks = seq(-20, 20, by = 5))+
      scale_y_continuous(breaks = seq(5,11,
                                      by = 1),limits = c(5,11))+
      annotate("text", 
               x = -17, 
               y = 11, 
               label = "a",
               hjust = 1, vjust = 1, size = 18, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b <- ggplot(pred_df_model_sq, aes(x = repro_age, y = predicted, colour = ever_hrt_binary, fill =ever_hrt_binary )) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
      geom_line(linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      labs(
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Depressive Symptoms Score",
        colour = NULL,
        fill = NULL,
        linetype = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 9),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = "black"),
        #legend.box.margin = margin(5, 5, 5, 5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "lines")
      ) +
      scale_x_continuous(breaks = seq(-20, 20, by = 5))+
      scale_y_continuous(breaks = seq(5,11,
                                      by = 1),limits = c(5,11))+
      annotate("text", 
               x = -17, 
               y = 11, 
               label = "b",
               hjust = 1, vjust = 1, size = 18, color = "black",
               family = "serif", fontface = "bold"
      )
  }
  
  # 1. Create spline basis
  spline_basis_repro <- rms::rcs(df$repro_age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_repro))) {
    df[[paste0("spline", i, "_repro")]] <- as.numeric(spline_basis_repro[, i])
  }
  
  spline_names_no_interaction <- paste0("spline", 1:ncol(spline_basis_repro), "_repro")
  
  spline_names <- paste0("spline", 1:ncol(spline_basis_repro), "_repro*ever_hrt_binary")
  spline_terms <- paste(spline_names, collapse = " + ")
  
  full_formula <- as.formula(paste(
    "epds_prorated ~ 1 +",
    spline_terms,
    "+ centered_age + centered_age_sq + ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + centered_age | aln) + (1 + centered_age | time_point)"
  ))
  
  # 4. Run model
  model_repro <- runMLwiN(
    full_formula,
    D = "Normal",
    data = df,
    estoptions = list(EstM = 0)
  )
  
  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "HRT interaction - supplementary figure 7"
  model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_repro, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
  model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
  
  # Plot figure
  # Predict values
  repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_repro,"parms")
  spline_basis_seq <- rcs(repro_seq, spline_knots)
  
  
  pred_model <- data.frame(
    repro_age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$ever_hrt_binary == "0")]),
                  to = as.numeric(quantiles$upper_bound[which(quantiles$ever_hrt_binary == "1")]), length.out = 100),2),
    
    centered_age = 0,
    centered_age_sq = 0,
    ever_hrt_binary = c(rep("0",100),rep("1",100)),
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("1", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)),
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model[1:100, spline_names_no_interaction] <- spline_basis_seq
  pred_model[101:200, spline_names_no_interaction] <- spline_basis_seq
  
  pred_model$ever_hrt_binary <- factor(pred_model$ever_hrt_binary, levels = levels(df$ever_hrt_binary))
  
  # Predict values
  predicted_model <- predict(model_repro, newdata = pred_model, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model <- data.frame(repro_age = pred_model$repro_age,
                                  predicted = predicted_model$fit,
                                  CI_low = predicted_model$fit - 1.96 * predicted_model$se.fit,
                                  CI_high = predicted_model$fit + 1.96 * predicted_model$se.fit,
                                  ever_hrt_binary = factor(c(rep("Never HRT use", 100), rep("Ever HRT use", 100)),
                                                        levels = c("Ever HRT use", "Never HRT use")))
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model, aes(x = repro_age, y = predicted, colour = ever_hrt_binary, fill =ever_hrt_binary )) +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
    geom_line(linewidth = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
    labs(
      x = "Years around final menstrual period (FMP)",
      y = "Predicted Depressive Symptoms Score",
      colour = NULL,
      fill = NULL,
      linetype = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 9),
      legend.position = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", color = "black"),
      #legend.box.margin = margin(5, 5, 5, 5),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1, "lines")
    ) +
    scale_x_continuous(breaks = seq(-20, 20, by = 5))+
    scale_y_continuous(breaks = seq(5,11,
                                    by = 1),limits = c(5,11))+
    annotate("text", 
             x = -17, 
             y = 11, 
             label = plot_label,
             hjust = 1, vjust = 1, size = 18, color = "black",
             family = "serif", fontface = "bold"
    )
  
  assign(paste0("plot_",plot_label),plot)
}

grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
ggsave("Final plots/model_fit/HRT_interaction.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")


# Binary models
df <- readRDS(paste0(filestore,"analysis_df_complete_case_epds.rds"))
df$centered_age_sq <- df$centered_age^2
df$repro_age_sq <- df$repro_age^2
df$age_sq <- df$age^2

repro_age_lower <- as.numeric(quantile(df$repro_age, 0.025,na.rm = T))
repro_age_upper <- as.numeric(quantile(df$repro_age, 0.975,na.rm = T))

chrono_age_lower <- as.numeric(quantile(df$age, 0.025,na.rm = T))
chrono_age_upper <- as.numeric(quantile(df$age, 0.975,na.rm = T))

df$age_meno_cat <- NA_character_
df$age_meno_cat[which(df$age_menopause < 45)] <- "Under 45"
df$age_meno_cat[which(df$age_menopause >= 45)] <- "Over 45"
df$age_meno_cat <- as.factor(df$age_meno_cat)

quantiles <- df %>% 
  group_by(age_meno_cat) %>%
  dplyr::summarize(lower_bound = quantile(age, 0.025, na.rm=T),
                   upper_bound = quantile(age, 0.975, na.rm=T))

# Trajectory across reproductive age
knot_numbers <- 3:6
for (k in knot_numbers) {
  if(k == 3){
    model_repro <- runMLwiN(logit(epds_binary) ~ 1 + repro_age
                            + centered_age + centered_age_sq 
                            + ethnicity + social_class + education + age_menarche
                            + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                            + (1 + centered_age | aln),
                            D = "Binomial",
                            data = df,
                            estoptions = list(EstM = 0)
    )
    
    model_repro_sq <- runMLwiN(logit(epds_binary) ~ 1 + repro_age + repro_age_sq
                               + centered_age + centered_age_sq 
                               + ethnicity + social_class + education + age_menarche
                               + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                               + (1 + centered_age | aln),
                               D = "Binomial",
                               data = df,
                               estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
    model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    #model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro)
    #model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological - figure 2"
    model_fit[nrow(model_fit),"N participants"] <- model_repro_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_repro_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    #model_fit[nrow(model_fit),"AIC"] <- AIC(model_repro_sq)
    #model_fit[nrow(model_fit),"BIC"] <- BIC(model_repro_sq)
    
    # Plot figure
    # Predict values
    pred_model_repro <- data.frame(
      repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
      centered_age = 0,
      centered_age_sq = 0,
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("4", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)), 
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    pred_model_repro$repro_age_sq <- pred_model_repro$repro_age^2
    
    # Predict values
    predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
    predicted_model_repro_sq <- predict(model_repro_sq, newdata = pred_model_repro, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                      predicted = plogis(predicted_model_repro$fit),
                                      CI_low = plogis(predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit),
                                      CI_high = plogis(predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit))
    
    pred_df_model_repro_sq <- data.frame(repro_age = pred_model_repro$repro_age,
                                         predicted = plogis(predicted_model_repro_sq$fit),
                                         CI_low = plogis(predicted_model_repro_sq$fit - 1.96 * predicted_model_repro_sq$se.fit),
                                         CI_high = plogis(predicted_model_repro_sq$fit + 1.96 * predicted_model_repro_sq$se.fit))
    
    
    
    # Plot with confidence intervals
    plot_a <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high),linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Probability of Depression"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 9))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(0.05,0.3,
                                      by = 0.05),limits = c(0.05,0.3))+
      annotate("text",
               x = 32-50,
               y = 0.3,
               label = "a",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    
    plot_b <- ggplot(pred_df_model_repro_sq, aes(x = repro_age, y = predicted)) +
      geom_line(color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Years around final menstrual period (FMP)",
        y = "Predicted Probability of Depression"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 9))+
      scale_x_continuous(breaks = seq(-20,20,
                                      by = 5),limits = c(-20,20))+
      scale_y_continuous(breaks = seq(0.05,0.3,
                                      by = 0.05),limits = c(0.05,0.3))+
      # scale_y_continuous(breaks = seq(5,10,
      #                                 by = 1))+
      annotate("text",
               x = 32-50,
               y = 0.3,
               label = "b",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
  }
  
  # 1. Create spline basis
  spline_basis_repro <- rms::rcs(df$repro_age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_repro))) {
    df[[paste0("spline", i, "_repro")]] <- as.numeric(spline_basis_repro[, i])
  }
  
  spline_names <- paste0("spline", 1:ncol(spline_basis_repro), "_repro")
  spline_terms <- paste(spline_names, collapse = " + ")
  full_formula <- as.formula(paste(
    "logit(epds_binary) ~ 1 +",
    spline_terms,
    "+ centered_age + centered_age_sq + ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + centered_age | aln)"
  ))
  
  # 4. Run model
  model_repro <- runMLwiN(
    full_formula,
    D = "Binomial",
    data = df,
    estoptions = list(EstM = 0)
  )
  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "reproductive adjusted for chronological binary - supplementary figure 7"
  model_fit[nrow(model_fit),"N participants"] <- model_repro@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_repro@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_repro, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- NA
  model_fit[nrow(model_fit),"BIC"] <- NA
  
  # Plot figure
  # Predict values
  repro_seq <- seq(repro_age_lower,repro_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_repro,"parms")
  spline_basis_seq <- rcs(repro_seq, spline_knots)
  
  pred_model_repro <- data.frame(
    repro_age = seq(repro_age_lower,repro_age_upper,length.out = 100),
    centered_age = 0,
    centered_age_sq = 0,
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("4", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)), 
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model_repro[, spline_names] <- spline_basis_seq
  
  # Predict values
  predicted_model_repro <- predict(model_repro, newdata = pred_model_repro, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model_repro <- data.frame(repro_age = pred_model_repro$repro_age,
                                    predicted = plogis(predicted_model_repro$fit),
                                    CI_low = plogis(predicted_model_repro$fit - 1.96 * predicted_model_repro$se.fit),
                                    CI_high = plogis(predicted_model_repro$fit + 1.96 * predicted_model_repro$se.fit))
  
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_repro, aes(x = repro_age, y = predicted)) +
    geom_line(color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_low), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_line(aes(y = CI_high), linetype = "dashed", color = "blue",linewidth = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red",,linewidth = 1) +
    #geom_vline(xintercept = -5, linetype = "dashed", color = "grey") +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
    labs(
      #title = "Trajectory of Depression Over Reproductive Age",
      x = "Years around final menstrual period (FMP)",
      y = "Predicted Probability of Depression"
    ) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 9))+
    scale_x_continuous(breaks = seq(-20,20,
                                    by = 5),limits = c(-20,20))+
    scale_y_continuous(breaks = seq(0.05,0.3,
                                    by = 0.05),limits = c(0.05,0.3))+
    annotate("text",
             x = -17,
             y = 0.35,
             label = plot_label,
             hjust = 1, vjust = 1, size = 18, color = "black",
             family = "serif", fontface = "bold"
    )
  
  assign(paste0("plot_",plot_label),plot)
}

grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
ggsave("Final plots/model_fit/binary_repro_age_adjusted_for_chrono.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")

# Trajectory across chronological age
knot_numbers <- 3:6
for (k in knot_numbers) {
  
  if(k==3){
    model_age <- runMLwiN(logit(epds_binary) ~ 1 + age 
                          + ethnicity + social_class + education + age_menarche
                          + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                          + (1 + age | aln),
                          D = "Binomial",
                          data = df,
                          estoptions = list(EstM = 0)
    )
    
    model_age_sq <- runMLwiN(logit(epds_binary) ~ 1 + age + I(age^2)
                             + ethnicity + social_class + education + age_menarche
                             + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                             + (1 + age | aln),
                             D = "Binomial",
                             data = df,
                             estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological unadjusted for reproductive - figure 3"
    model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    #model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
    #model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological unadjusted for reproductive - figure 3"
    model_fit[nrow(model_fit),"N participants"] <- model_age_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    #model_fit[nrow(model_fit),"AIC"] <- AIC(model_age_sq)
    #model_fit[nrow(model_fit),"BIC"] <- BIC(model_age_sq)
    
    pred_model_age <- data.frame(
      age = seq(chrono_age_lower,chrono_age_upper,length.out = 100),
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("4", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)), 
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    # Predict values
    predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link",se.fit = T)
    predicted_model_age_sq <- predict(model_age_sq, newdata = pred_model_age, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_age <- data.frame(age = pred_model_age$age,
                                    predicted = plogis(predicted_model_age$fit),
                                    CI_low = plogis(predicted_model_age$fit - 1.96 * predicted_model_age$se.fit),
                                    CI_high = plogis(predicted_model_age$fit + 1.96 * predicted_model_age$se.fit))
    
    pred_df_model_age_sq <- data.frame(age = pred_model_age$age,
                                       predicted = plogis(predicted_model_age_sq$fit),
                                       CI_low = plogis(predicted_model_age_sq$fit - 1.96 * predicted_model_age_sq$se.fit),
                                       CI_high = plogis(predicted_model_age_sq$fit + 1.96 * predicted_model_age_sq$se.fit))
    
    
    # Plot with confidence intervals
    plot_a <- ggplot(pred_df_model_age, aes(x = age, y = predicted)) +
      geom_line(color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Age (years)",
        y = "Predicted Probability of Depression"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 9)) +
      scale_y_continuous(breaks = seq(0.05,0.3,
                                      by = 0.05),limits = c(0.05,0.3))+
      #scale_y_continuous(breaks = seq(4,11,
      #                                by = 1),limits = c(4,11))+
      annotate("text", 
               x = 31.5, 
               y = 0.3, 
               label = "a",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b <- ggplot(pred_df_model_age_sq, aes(x = age, y = predicted)) +
      geom_line(color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
      labs(
        #title = "Trajectory of Depression Over Reproductive Age",
        x = "Age (years)",
        y = "Predicted Probability of Depression"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 9)) +
      scale_y_continuous(breaks = seq(0.05,0.3,
                                      by = 0.05),limits = c(0.05,0.3))+
      #scale_y_continuous(breaks = seq(4,11,
      #                                by = 1),limits = c(4,11))+
      annotate("text", 
               x = 31.5, 
               y = 0.3, 
               label = "b",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    
  }
  
  
  # 1. Create spline basis
  spline_basis_age <- rms::rcs(df$age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_age))) {
    df[[paste0("spline", i, "_age")]] <- as.numeric(spline_basis_age[, i])
  }
  
  spline_names <- paste0("spline", 1:ncol(spline_basis_age), "_age")
  spline_terms <- paste(spline_names, collapse = " + ")
  full_formula <- as.formula(paste(
    "logit(epds_binary) ~ 1 +",
    spline_terms,
    "+ ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + age | aln)"
  ))
  
  # 4. Run model
  model_age <- runMLwiN(
    full_formula,
    D = "Binomial",
    data = df,
    estoptions = list(EstM = 0)
  )

  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "chronological adjusted for reproductive binary - supplementary figure 8"
  model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_age, "parms")) - 1
  model_fit[nrow(model_fit),"AIC"] <- NA
  model_fit[nrow(model_fit),"BIC"] <- NA
  
  # Plot figure
  # Predict values
  age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_age,"parms")
  spline_basis_seq <- rcs(age_seq, spline_knots)
  
  pred_model_age <- data.frame(
    age = seq(chrono_age_lower,chrono_age_upper,length.out = 100),
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("4", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)), 
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model_age[, spline_names] <- spline_basis_seq
  
  # Predict values
  predicted_model_age <- predict(model_age, newdata = pred_model_age, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model_age <- data.frame(age = pred_model_age$age,
                                  predicted = plogis(predicted_model_age$fit),
                                  CI_low = plogis(predicted_model_age$fit - 1.96 * predicted_model_age$se.fit),
                                  CI_high = plogis(predicted_model_age$fit + 1.96 * predicted_model_age$se.fit))
  
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_age, aes(x = age, y = predicted)) +
    geom_line(color = "blue", linewidth = 1.5) +
    geom_line(aes(y = CI_low), linetype = "dashed", color = "blue", linewidth = 1.5) +
    geom_line(aes(y = CI_high), linetype = "dashed", color = "blue", linewidth = 1.5) +
    geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",,linewidth = 1) +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
    labs(
      #title = "Trajectory of Depression Over Reproductive Age",
      x = "Age (years)",
      y = "Predicted Probability of Depression"
    ) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 9)) +
    scale_y_continuous(breaks = seq(0.05,0.3,
                                    by = 0.05),limits = c(0.05,0.3))+
    annotate("text", 
             x = 31.5, 
             y = 0.3, 
             label = plot_label,
             hjust = 1, vjust = 1, size = 18, color = "black",
             family = "serif", fontface = "bold"
    )
  
  assign(paste0("plot_",plot_label),plot)
}

grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
ggsave("Final plots/model_fit/binary_chrono_age_unadjusted_for_repro.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")

# Trajectory using age at menopause
knot_numbers <- 3:6
for (k in knot_numbers) {
  
  if(k==3){
    model_age <- runMLwiN(logit(epds_binary) ~ 1 + age*age_meno_cat
                          + ethnicity + social_class + education + age_menarche
                          + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                          + (1 + age | aln),
                          D = "Binomial",
                          data = df,
                          estoptions = list(EstM = 0)
    )
    
    model_age_sq <- runMLwiN(logit(epds_binary) ~ 1 + age*age_meno_cat + age_sq*age_meno_cat
                             + ethnicity + social_class + education + age_menarche
                             + material_hardship + social_support + smoking_status + bmi + alcohol_intake
                             + (1 + age | aln),
                             D = "Binomial",
                             data = df,
                             estoptions = list(EstM = 0)
    )
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
    model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
    model_fit[nrow(model_fit),"df"] <- "Linear"
    #model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
    #model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
    
    model_fit[nrow(model_fit) + 1, ] <- NA
    model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories - figure 5"
    model_fit[nrow(model_fit),"N participants"] <- model_age_sq@Hierarchy[,"N_complete"]
    model_fit[nrow(model_fit),"N observations"] <- model_age_sq@Nobs
    model_fit[nrow(model_fit),"df"] <- "Quadratic"
    #model_fit[nrow(model_fit),"AIC"] <- AIC(model_age_sq)
    #model_fit[nrow(model_fit),"BIC"] <- BIC(model_age_sq)
    
    pred_model <- data.frame(
      age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Under 45")]),
                    to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Under 45")]), length.out = 100),2),
      
      age_meno_cat = c(rep("Under 45",100),rep("Over 45",100)),
      
      # Convert factor variables to factors with the same levels as in df
      ethnicity = factor("White", levels = levels(df$ethnicity)),
      social_class = factor("1", levels = levels(df$social_class)),
      education = factor("A-level_or_greater", levels = levels(df$education)),
      age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
      smoking_status = factor("Never", levels = levels(df$smoking_status)),
      alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
      
      # Use mean values for continuous variables
      material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
      social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
      bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
    )
    
    pred_model$age_sq <- pred_model$age^2
    pred_model$age_meno_cat <- factor(pred_model$age_meno_cat, levels = levels(df$age_meno_cat))
    
    # Predict values
    predicted_model_age <- predict(model_age, newdata = pred_model, type = "link",se.fit = T)
    predicted_model_age_sq <- predict(model_age_sq, newdata = pred_model, type = "link",se.fit = T)
    
    # Prediction df for plotting
    pred_df_model_age <- data.frame(age = pred_model$age,
                                    predicted = plogis(predicted_model_age$fit),
                                    CI_low = plogis(predicted_model_age$fit - 1.96 * predicted_model_age$se.fit),
                                    CI_high = plogis(predicted_model_age$fit + 1.96 * predicted_model_age$se.fit),
                                    age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
                                                          levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
    
    pred_df_model_age_sq <- data.frame(age = pred_model$age,
                                       predicted = plogis(predicted_model_age_sq$fit),
                                       CI_low = plogis(predicted_model_age_sq$fit - 1.96 * predicted_model_age_sq$se.fit),
                                       CI_high = plogis(predicted_model_age_sq$fit + 1.96 * predicted_model_age_sq$se.fit),
                                       age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
                                                             levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
    
    # Plot with confidence intervals
    plot_a <- ggplot(pred_df_model_age, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
      geom_line(linewidth = 1.5) +
      labs(
        x = "Age (Years)",
        y = "Predicted Probability of Depression",
        colour = NULL,
        fill = NULL,
        linetype = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 9),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = "black"),
        #legend.box.margin = margin(5, 5, 5, 5),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.7, "lines")
      ) +
      scale_x_continuous(breaks = seq(25, 70, by = 5))+
      #scale_y_continuous(breaks = seq(4,11,
      #                                by = 1),limits = c(4,11))+
      annotate("text", 
               x = 28, 
               y = 0.3, 
               label = "a",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
    
    plot_b <- ggplot(pred_df_model_age_sq, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
      #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
      geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
      geom_line(linewidth = 1.5) +
      labs(
        x = "Age (Years)",
        y = "Predicted Probability of Depression",
        colour = NULL,
        fill = NULL,
        linetype = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 9),
        legend.position = c(0.95, 0.75),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = "black"),
        #legend.box.margin = margin(5, 5, 5, 5),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.7, "lines")
      ) +
      scale_x_continuous(breaks = seq(25, 70, by = 5))+
      #scale_y_continuous(breaks = seq(4,11,
      #                                by = 1),limits = c(4,11))+
      annotate("text", 
               x = 28, 
               y = 0.3, 
               label = "b",
               hjust = 1, vjust = 1, size = 16, color = "black",
               family = "serif", fontface = "bold"
      )
  }
  
  # 1. Create spline basis
  spline_basis_age <- rms::rcs(df$age, k)
  
  # 2. Add spline columns to df
  for (i in 1:(ncol(spline_basis_age))) {
    df[[paste0("spline", i, "_age")]] <- as.numeric(spline_basis_age[, i])
  }
  
  spline_names_no_interaction <- paste0("spline", 1:ncol(spline_basis_age), "_age")
  spline_names <- paste0("spline", 1:ncol(spline_basis_age), "_age*age_meno_cat")
  spline_terms <- paste(spline_names, collapse = " + ")
  
  full_formula <- as.formula(paste(
    "logit(epds_binary) ~ 1 +",
    spline_terms,
    "+ ethnicity + social_class + education + age_menarche + material_hardship + social_support + smoking_status + bmi + alcohol_intake + (1 + age | aln)"
  ))
  
  # 4. Run model
  model_age <- runMLwiN(
    full_formula,
    D = "Binomial",
    data = df,
    estoptions = list(EstM = 0)
  )
  
  model_fit[nrow(model_fit) + 1, ] <- NA
  model_fit[nrow(model_fit),"model"] <- "chronological by menopause categories binary - supplementary figure 10"
  model_fit[nrow(model_fit),"N participants"] <- model_age@Hierarchy[,"N_complete"]
  model_fit[nrow(model_fit),"N observations"] <- model_age@Nobs
  model_fit[nrow(model_fit),"df"] <- length(attr(spline_basis_age, "parms")) - 1
  #model_fit[nrow(model_fit),"AIC"] <- AIC(model_age)
  #model_fit[nrow(model_fit),"BIC"] <- BIC(model_age)
  
  # Plot figure
  # Predict values
  age_seq <- seq(chrono_age_lower,chrono_age_upper,length.out = 100)
  
  # Generate restricted cubic spline terms for the sequence
  spline_knots <- attr(spline_basis_age,"parms")
  spline_basis_seq <- rcs(age_seq, spline_knots)
  
  
  pred_model <- data.frame(
    age = rep(seq(from = as.numeric(quantiles$lower_bound[which(quantiles$age_meno_cat == "Under 45")]),
                  to = as.numeric(quantiles$upper_bound[which(quantiles$age_meno_cat == "Under 45")]), length.out = 100),2),
    
    age_meno_cat = c(rep("Under 45",100),rep("Over 45",100)),
    
    # Convert factor variables to factors with the same levels as in df
    ethnicity = factor("White", levels = levels(df$ethnicity)),
    social_class = factor("1", levels = levels(df$social_class)),
    education = factor("A-level_or_greater", levels = levels(df$education)),
    age_menarche = factor("12_14_yrs", levels = levels(df$age_menarche)),
    smoking_status = factor("Never", levels = levels(df$smoking_status)),
    alcohol_intake = factor("Never or ≤4 times/month", levels = levels(df$alcohol_intake)),
    
    # Use mean values for continuous variables
    material_hardship = mean(df$material_hardship, na.rm = TRUE),  # Set material hardship to the mean
    social_support = mean(df$social_support, na.rm = TRUE),  # Set social support to the mean
    bmi = mean(df$bmi, na.rm = TRUE)  # Set BMI to the mean value
  )
  
  pred_model[1:100, spline_names_no_interaction] <- spline_basis_seq
  pred_model[101:200, spline_names_no_interaction] <- spline_basis_seq
  
  pred_model$age_meno_cat <- factor(pred_model$age_meno_cat, levels = levels(df$age_meno_cat))
  
  # Predict values
  predicted_model_age <- predict(model_age, newdata = pred_model, type = "link",se.fit = T)
  
  # Prediction df for plotting
  pred_df_model_age <- data.frame(age = pred_model$age,
                                  predicted = plogis(predicted_model_age$fit),
                                  CI_low = plogis(predicted_model_age$fit - 1.96 * predicted_model_age$se.fit),
                                  CI_high = plogis(predicted_model_age$fit + 1.96 * predicted_model_age$se.fit),
                                  age_meno_cat = factor(c(rep("Age at menopause under 45", 100), rep("Age at menopause 45 or above", 100)),
                                                        levels = c("Age at menopause under 45", "Age at menopause 45 or above")))
  
  # Plot with confidence intervals
  plot_label <- ifelse(k == 3, "c",ifelse(k == 4, "d", ifelse(k == 5, "e", "f")))
  plot <- ggplot(pred_df_model_age, aes(x = age, y = predicted, colour = age_meno_cat, fill =age_meno_cat )) +
    #geom_ribbon(aes(ymin = CI_low, ymax = CI_high ), alpha = 0.2,linetype = "dashed") +
    geom_vline(xintercept = 49.6, linetype = "dashed", color = "red",linewidth = 1) +
    geom_line(linewidth = 1.5) +
    labs(
      x = "Age (Years)",
      y = "Predicted Probability of Depression",
      colour = NULL,
      fill = NULL,
      linetype = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 9),
      legend.position = c(0.95, 0.05),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", color = "black"),
      #legend.box.margin = margin(5, 5, 5, 5),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.7, "lines")
    ) +
    scale_x_continuous(breaks = seq(25, 70, by = 5))+
    annotate("text", 
             x = 28, 
             y = 0.3, 
             label = plot_label,
             hjust = 1, vjust = 1, size = 18, color = "black",
             family = "serif", fontface = "bold"
    )
  
  assign(paste0("plot_",plot_label),plot)
}

grid_arrange_plot <- grid.arrange(plot_a,plot_b,plot_c,plot_d,plot_e,plot_f, ncol = 2)  
ggsave("Final plots/model_fit/binary_chrono_age_by_menopause_categories.png", grid_arrange_plot, width = 11.69 , height = 8.27 ,bg = "white")

write.csv(model_fit, "Final plots/model_fit/model_fit_aic.csv",row.names = F)





