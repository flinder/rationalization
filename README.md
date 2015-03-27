Rationalization
===============

This repo contains all files pertaining to the preparation and analysis of my project "Misreporting of ideological self placement to rationalize party preferences". Comments Welcome

The branch 'pre_registration' is locked and contains the project outline and status before the main data collection was conducted.

Files:
------
- analysis
	+ analysis_prerun.R: Description of data collected in pre run of the experiment and some code for calculating quantities of interest for analysis of main study
	+ anes_prep.R: Prepares raw ANES data (anes_timeseries_2012_stata12.dta) as downloaded from the ANES webpage for use in ideology_prediction.R
	+ ideology_prediction.R: Fits a random forest to the ANES data to extract the ost important predictors of ideological self placement
	+ power_analysis.R: Simulation of hypothesized DGP and power analysis to determine sample size
	+ prerun_data_prep.R: Produces pre_run_clean.csv from raw output of qualtrics and Mturk (not here due to data privacy)
	+ MAIN_data_prep.R: Produces main_study_clean.csv and main_study_geo_loc.csv from raw output of qualtrics and Mturk (not here due to data privacy)
	+ analysis_main.R: Description and analysis of main study: Main results for paper
- data
	+ pre_run/pre_run_clean.csv: Cleaned and anonymized data from pre run of the experiment
	+ preparation/anes_clean.csv: cleaned ANES data for use in ideology_prediction.R
	+ main_study/main_study_clean.csv: main data file
	+ main_study/main_study_geo_loc.csv: randomly shuffled geo-locations of respondents (from Mturk accounts)
- surveys
	+ rationalization_main.txt: survey questions and flow for main study
	+ rationalization_prerun.txt: survey questions and flow for pre run
- paper: all files pertaining to the write up and presentation slides
	
The maps in 'figures/main' have been created using [CartoDB](http://cartodb.com/) and can be accessed interactively [here](http://cdb.io/1DxjEaZ).
