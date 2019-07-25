# Script summary
# 
# Render rmarkdown files in the documents directory
#
# Output Files
#   /documents/changepoint_results.pdf
#   /documents/qmap_adj_ecdf_plots.pdf


#-- Setup ---------------------------------------------------------------------
library(rmarkdown)

workdir <- getwd()
code_dir <- file.path(workdir, "code/ASOS")
doc_dir <- file.path(workdir, "documents")

#------------------------------------------------------------------------------

#-- Render Files --------------------------------------------------------------
# changepoint_results.Rmd
{rmd_path <- file.path(code_dir, "changepoint_results.Rmd")
render(rmd_path, output_dir = doc_dir)}

# ASOS data quantile mapping ECDF plots
# qmap_adj_ecdf_plots.Rmd
{rmd_path <- file.path(code_dir, "qmap_adj_ecdf_plots.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# ERA-Interim quantile mapping ECDF plots

# CM3 quantile mapping ECDF plots


#------------------------------------------------------------------------------
