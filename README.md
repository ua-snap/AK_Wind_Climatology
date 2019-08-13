# AK Wind Climatology
## Code
This Repo is for processing and (partial) analysis of wind data gathered by ASOS stations in Alaska, and output from WRF models. It is currently for my own version control purposes, but it may be of use to others eventually.

### Files, Directory & descriptions  
  
* **asos_wrf_compare_events.Rmd**: compare model output with observed values for select high wind events
* **asos_wrF_verification.R**: compare alignment of WRF grid with some ASOS locations
* **helpers.R**: various functions for this project
* **render.R**: Render RMarkdown files in /documents directory  
  
* **ASOS**: all scripts used in processing the ASOS data
	* **asos_download.R**: Auto download of ASOS data from IEM (not used currently)  
	* **asos_ecdf_plots.Rmd**: Assessment of Empirical Distribution Functions (ECDFs) from quantile mapping "biased" ASOS data  
	* **asos_scrap.R**: scrap script for testing code  
	* **changepoint_results.Rmd**: monthly-summarized wind speed time series with changepoints, pre- and post-qmapping  
	* **climatology_example_figures.R**: example figures for potential ACCAP website  
	* **daily_summary.R**: Summarize unadjusted wind speeds of all stations by day  
	* **discontinuity_table.Rmd**: table of detected discontinuities and mean differences for ASOS stations
	* **hourly_adjustment.R**: perform changepoint detection on monthly averages, perform quantile mapping
	* **hourly_quality_check.R**: Assessment and quality control of raw data (e.g. suspiciously high speeds)
	* **post_processing_summary.R**: final quality check and summary of post-processed data (unfinished)
	* **select_adj_summary**: summarize selected/adjusted data by day and month
	* **select_stations.R**: select stations for climatology
	* **succ_obs_summary.R**: create figure visualizing observation success of raw data
  
* **WRF**: scripts used in processing the WRF data  
	* **ccsm4_ecdf_plots.Rmd**: ECDF plots for quantile mapping of CCSM4 data
	* **cm3_ecdf_plots.Rmd**: ECDF plots for quantile mapping of CM3 data
	* **era_ecdf_plots.Rmd**: ECDF plots for quantile mapping of ERA-Interim data
	* **model_clim_mean_quantile_differences.Rmd**: generate tables of mean and quantile differences between periods for select months
	* **prelim_analysis.R**: compare means between climatology periods (historical and future)
	* **select_stations_output.R**: Extract WRF output for 
	* **wrf_adjustment.R**: quantile mapping adjustment of WRF output
	* **wrf_conversion.R**: convert velocity components from raw WRF output to ws and wd
	* **wrf_scrap.R**: scrap script for working with the WRF data
	* **wrf_summary.R**: summarize WRF data by day and by month
