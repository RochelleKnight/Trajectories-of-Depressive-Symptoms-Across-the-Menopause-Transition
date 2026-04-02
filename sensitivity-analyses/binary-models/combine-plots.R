rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")

source("sensitivity-analyses/binary-models/models-mlwin-binary-repro-models.R")
source("sensitivity-analyses/binary-models/models-mlwin-binary-age-models.R")

grid_arrange_plot <- grid.arrange(p1_repro, p1_chrono, ncol = 1)
ggsave("Final plots/binary/combined_plot_repro_chrono.png", grid_arrange_plot, width = 8.27, height = 11.69,bg = "white")
grid.arrange(p1_repro, p1_chrono, ncol = 1)


