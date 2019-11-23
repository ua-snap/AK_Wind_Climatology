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

doc_dir <- "../AK_Wind_Climatology_aux/docs"

#------------------------------------------------------------------------------

#-- Render Files --------------------------------------------------------------
# changepoint_results.Rmd
{rmd_path <- "ASOS_code/changepoint_results.Rmd"
  render(rmd_path, output_dir = doc_dir)}

# discontinuity_table.Rmd
{rmd_path <- "ASOS_code/discontinuity_table.Rmd"
  render(rmd_path, output_dir = doc_dir)}

# ASOS data quantile mapping ECDF plots
# asos_ecdf_plots.Rmd
{rmd_path <- "ASOS_code/asos_ecdf_plots.Rmd"
  render(rmd_path, output_dir = doc_dir)}

# ERA-Interim quantile mapping ECDF plots
# era_ecdf_plots.Rmd
{rmd_path <- "WRF_code/era_ecdf_plots.Rmd"
  render(rmd_path, output_dir = doc_dir)}

# CM3 quantile mapping ECDF plots
# cm3_ecdf_plots.Rmd
{rmd_path <- "WRF_code/cm3_ecdf_plots.Rmd"
  render(rmd_path, output_dir = doc_dir)}

# CCSM4 quantile mapping ECDF plots
# ccsm4_ecdf_plots.Rmd
{rmd_path <- "WRF_code/ccsm4_ecdf_plots.Rmd"
  render(rmd_path, output_dir = doc_dir)}

# High Wind Event Sample Comparison - All Sources
{rmd_path <- "asos_wrf_compare_events.Rmd"
  render(rmd_path, output_dir = doc_dir)}

# High Wind Event Sample Comparison - All Sources
{rmd_path <- "WRF_code/model_clim_mean_quantile_differences.Rmd"
  render(rmd_path, output_dir = doc_dir)}

#------------------------------------------------------------------------------
