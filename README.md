# AK Wind Climatology
## Code
This Repo is for processing and analyzing wind data gathered
by ASOS stations in Alaska. It is currently for my own version control purposes, 
but it may be of use to others eventually.

### Files, Directory & descriptions

* **ASOS**: all scripts used in processing the ASOS data  
	* **asos_download.R**: Auto download of ASOS data from IEM
	* **asos_qmap_ecdf_plots.Rmd**: Assessment of Empirical Distribution Functions (ECDFs) from quantile mapping "biased" ASOS data
	* **changepoint_results.Rmd**: monthly-summarized wind speed time series with changepoints, pre- and post-qmapping
	* **climatology_example_figures.R**: example figures for potential ACCAP website
	* **daily_summary.R**: Summarize unadjusted wind speeds of all stations by day
	* **hourly_adjustment.R**: perform changepoint detection on monthly averages, perform quantile mapping
	* **hourly_quality_check.R**: Assessment and quality control of raw data quality (e.g. suspiciously high speeds)
	* **post_processing_summary.R**: final quality check and summary of post-processed data
	* **render.R**: Render RMarkdown files in separate directory
	* **scrap.R**: scrap script for testing code
	* **select_adj_summary**: summarize selected/adjusted data by day and month
	* **select_stations.R**: select stations for climatology
	* **succ_obs_figure.R**: create figure visualizing observation success
  
* **Climatology_app**: Shiny app for the wind climatology of Alaska  
  
* **IEM_ASOS_daily_app**: Shiny app for exploring AK ASOS data summarized to the daily scale  
  
* **WRF**: scripts used in processing the WRF data  
	* **select_stations_output.R**: Extract WRF output for 
	* **wrf_adjustment.R**: quantile mapping adjustment of WRF output
	* **wrf_conversion.R**: convert velocity components from raw WRF output to ws and wd
	* **era_qmap_ecdf_plots.Rmd**: ECDFs from quantile mapping ERA-Interim data
	* **cm3_qmap_ecdf_plots.Rmd**: ECDFs from quantile mapping GFDL CM3 data
	* **ccsm4_qmap_ecdf_plots.Rmd**: ECDFs from quantile mapping NCAR CCSM4 data

