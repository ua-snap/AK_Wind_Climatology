# AK Wind Climatology
This Repo is for processing and analyzing wind data gathered
by ASOS stations in Alaska. It is generally for my own reference, 
but it may be of use to others as well eventually.

Directory & descriptions

* **ASOS**: all text files used in working with the ASOS data  
	* **ASOS_download.R**: Auto download of ASOS data from IEM 
	* **daily_changepoint.RMD**: Assessment of wind speed time series changepoints
	* **hourly_quality_check.R**: Assessment of data quality (e.g. suspiciously high speeds)
	* **hourly_qmap_adj_ecdf_plots.Rmd**: Assessment of Empirical Distribution Functions from quantile mapping "biased" ASOS data
	* **monthly_adjustment_results.Rmd**: Assessment of wind speed time series post quantile mapping adjustment
	* 

Climatology_app: Shiny app for the wind climatology of Alaska
IEM_ASOS_daily_app: Shiny app for exploring AK ASOS data summarized to the daily scale


