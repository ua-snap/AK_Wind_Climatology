# AK Wind Climatology
## Code
This Repo is for processing and analyzing wind data gathered
by ASOS stations in Alaska. It is generally for my own reference, 
but it may be of use to others as well eventually.

### Files, Directory & descriptions

* **ASOS**: all text files used in working with the ASOS data  
	* **ASOS_download.R**: Auto download of ASOS data from IEM
	* **changepoint_results.Rmd**: monthly-summarized wind speed time series with changepoints, pre- and post-qmapping
	* **climatology_example_figures.R**: example figures for potential ACCAP website
	* **daily_summary.R**: Summarize unadjusted wind speeds of all stations by day
	* **hourly_adjustment.R**: perform changepoint detection on monthly averages, perform quantile mapping
	* **hourly_quality_check.R**: Assessment and quality control of raw data quality (e.g. suspiciously high speeds)
	* **post_processing_summary.R**: final quality check and summary of post-processed data
	* **qmap_adj_ecdf_plots.Rmd**: Assessment of Empirical Distribution Functions from quantile mapping "biased" ASOS data
	* **scrap.R**: scrap script for testing code
	* **select_adj_summary**: summarize selected/adjusted data by day and month
	* **select_stations.R**: select stations for climatology
	* **succ_obs_figure.R**: create figure visualizing observation success

Climatology_app: Shiny app for the wind climatology of Alaska
IEM_ASOS_daily_app: Shiny app for exploring AK ASOS data summarized to the daily scale


