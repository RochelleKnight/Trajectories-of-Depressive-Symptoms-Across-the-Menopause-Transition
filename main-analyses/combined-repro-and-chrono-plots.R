rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Trajectories-of-Depressive-Symptoms-Across-the-Menopause-Transition")

source("main-analyses/models-mlwin-linear-repro-models.R")
source("main-analyses/models-mlwin-linear-age-models.R")

grid_arrange_plot <- grid.arrange(p1_repro, p1_chrono, ncol = 1)
ggsave("Final plots/combined_plot_repro_chrono_adj.png", grid_arrange_plot, width = 8.27, height = 11.69,bg = "white")
grid.arrange(p1_repro, p1_chrono, ncol = 1)

#grid_arrange_plot <- grid.arrange(p2_repro, p2_chrono, ncol = 1)
ggsave("Final plots/combined_plot_repro_chrono_unadj.png", p2_repro, height = 8.27, width = 11.69,bg = "white")
#grid.arrange(p2_repro, p2_chrono, ncol = 1)
