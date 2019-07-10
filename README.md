# AK Wind Climatology
## Code
This Repo is for processing and analyzing wind data gathered
by ASOS stations in Alaska. It is generally for my own reference, 
but it may be of use to others as well eventually.

### Files, Directory & descriptions

* **ASOS**: all text files used in working with the ASOS data  
	* **ASOS_download.R**: Auto download of ASOS data from IEM
	* **climatology_example_figures.R**: example figures for potential ACCAP website
	* **daily_changepoint.RMD**: Assessment of wind speed time series changepoints
	* **daily_summary.R**: Sumarize unadjusted wind speeds of all stations by day
	* **hourly_quality_check.R**: Assessment of data quality (e.g. suspiciously high speeds)
	* **qmap_adj_ecdf_plots.Rmd**: Assessment of Empirical Distribution Functions from quantile mapping "biased" ASOS data
	* **monthly_adjustment_results.Rmd**: Assessment of monthly wind speed time series post quantile mapping adjustment
	* **post_processing_summary.R**: final quality check and summary of post-processed data
	* **scrap.R**: scrap script for testing code
	* **select_adj_summary**: summarize selected/adjusted data by day and month

Climatology_app: Shiny app for the wind climatology of Alaska
IEM_ASOS_daily_app: Shiny app for exploring AK ASOS data summarized to the daily scale


