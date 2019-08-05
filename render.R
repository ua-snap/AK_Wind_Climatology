# Script summary
#   Render RMarkdown files in the documents directory
#
# Output Files
#   /documents/changepoint_results.pdf
#   /documents/discontinuity_table.pdf
#   /documents/asos_ecdf_plots.pdf
#   /documents/era_ecdf_plots.pdf
#   /documents/cm3_ecdf_plots.pdf
#   /documents/ccsm4_ecdf_plots.pdf
#   /documents/asos_wrf_compare_events.pdf
#   /documents/model_clim_mean_quantile_differences.pdf



#-- Setup ---------------------------------------------------------------------
library(rmarkdown)

workdir <- getwd()
code_dir <- file.path(workdir, "code")
asos_dir <- file.path(code_dir, "ASOS")
wrf_dir <- file.path(code_dir, "WRF")
doc_dir <- file.path(workdir, "documents")

#------------------------------------------------------------------------------

#-- Render Files --------------------------------------------------------------
# changepoint_results.Rmd
{rmd_path <- file.path(code_dir, "changepoint_results.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# discontinuity_table.Rmd
{rmd_path <- file.path(asos_dir, "discontinuity_table.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# ASOS data quantile mapping ECDF plots
# asos_ecdf_plots.Rmd
{rmd_path <- file.path(asos_dir, "asos_ecdf_plots.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# ERA-Interim quantile mapping ECDF plots
# era_ecdf_plots.Rmd
{rmd_path <- file.path(wrf_dir, "era_ecdf_plots.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# CM3 quantile mapping ECDF plots
# cm3_ecdf_plots.Rmd
{rmd_path <- file.path(wrf_dir, "cm3_ecdf_plots.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# CCSM4 quantile mapping ECDF plots
# ccsm4_ecdf_plots.Rmd
{rmd_path <- file.path(wrf_dir, "ccsm4_ecdf_plots.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# High Wind Event Sample Comparison - All Sources
{rmd_path <- file.path(code_dir, "asos_wrf_compare_events.Rmd")
  render(rmd_path, output_dir = doc_dir)}

# High Wind Event Sample Comparison - All Sources
{rmd_path <- file.path(wrf_dir, "model_clim_mean_quantile_differences.Rmd")
  render(rmd_path, output_dir = doc_dir)}

#------------------------------------------------------------------------------
