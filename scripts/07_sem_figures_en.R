# ============================================================
# 07_sem_figures.R
# Final SEM figures
# ============================================================

if (!exists("PROJECT_ROOT")) {
  source("00_setup.R")
}

library(semPlot)
library(semptools)

# ------------------------------------------------------------
# Load SEM models
# ------------------------------------------------------------

sem_models_path <- file.path(DIRS$results_models, "sem_models.rds")

if (!file.exists(sem_models_path)) {
  source("06_sem_model.R")
}

sem_models <- readRDS(sem_models_path)

fit_sem_recommended_full <- sem_models$fit_sem_recommended_full
fit_sem_recommended_pars <- sem_models$fit_sem_recommended_pars

# ------------------------------------------------------------
# Function to save figure in PDF and PNG
# ------------------------------------------------------------

save_qgraph_dual <- function(qgraph_obj, stem, width = 12, height = 8) {
  pdf(file.path(DIRS$results_figures, paste0(stem, ".pdf")), width = width, height = height)
  plot(qgraph_obj)
  dev.off()
  
  png(file.path(DIRS$results_figures, paste0(stem, ".png")),
      width = width, height = height, units = "in", res = 300)
  plot(qgraph_obj)
  dev.off()
}

# ------------------------------------------------------------
# 1. Main figure: latent variables only
# ------------------------------------------------------------

latent_plot <- semPlot::semPaths(
  fit_sem_recommended_pars,
  what = "std",
  whatLabels = "std",
  structural = TRUE,
  residuals = FALSE,
  intercepts = FALSE,
  style = "ram",
  layout = "tree2",
  nCharNodes = 0,
  nCharEdges = 0,
  curvePivot = TRUE,
  node.width = 0.90,
  edge.label.cex = 0.60,
  edge.color = "black",
  sizeLat = 9,
  sizeMan = 5,
  sizeInt = 0.8,
  border.color = "black",
  label.color = "black",
  mar = c(8, 4, 8, 4)
)

latent_plot <- tryCatch(
  semptools::mark_sig(latent_plot, fit_sem_recommended_pars),
  error = function(e) latent_plot
)

save_qgraph_dual(
  latent_plot,
  stem = "fig_sem_latent_only_recommended_pars",
  width = 15,
  height = 7
)

# ------------------------------------------------------------
# 2. EE second-order model figure
# ------------------------------------------------------------

ee_plot <- semPlot::semPaths(
  fit_sem_recommended_pars,
  what = "std",
  whatLabels = "std",
  structural = FALSE,
  intercepts = FALSE,
  residuals = FALSE,
  style = "ram",
  layout = "tree2",
  nCharNodes = 0,
  nCharEdges = 0,
  edge.color = "black",
  edge.label.cex = 0.70,
  border.color = "black",
  label.color = "black",
  color = list(lat = "white", man = "white"),
  sizeLat = 9,
  sizeMan = 4,
  mar = c(6, 6, 6, 6),
  nodeLabels = c(
    "EE", "CT", "DH", "AI",
    paste0("ee_", 1:18)
  )
)

ee_plot <- tryCatch(
  semptools::mark_sig(ee_plot, fit_sem_recommended_pars),
  error = function(e) ee_plot
)

save_qgraph_dual(
  ee_plot,
  stem = "fig_ee_second_order_measurement",
  width = 11,
  height = 9
)

# ------------------------------------------------------------
# 3. Full SEM figure with indicators
# ------------------------------------------------------------

full_plot <- semPlot::semPaths(
  fit_sem_recommended_pars,
  what = "std",
  whatLabels = "std",
  structural = FALSE,
  intercepts = FALSE,
  residuals = FALSE,
  style = "ram",
  layout = "tree2",
  nCharNodes = 0,
  nCharEdges = 0,
  edge.color = "black",
  edge.label.cex = 0.45,
  border.color = "black",
  label.color = "black",
  color = list(lat = "white", man = "white"),
  sizeLat = 8,
  sizeMan = 3,
  mar = c(8, 8, 8, 8)
)

full_plot <- tryCatch(
  semptools::mark_sig(full_plot, fit_sem_recommended_pars),
  error = function(e) full_plot
)

save_qgraph_dual(
  full_plot,
  stem = "fig_sem_full_measurement_recommended_pars",
  width = 18,
  height = 12
)

message("SEM figures saved in: ", DIRS$results_figures)
