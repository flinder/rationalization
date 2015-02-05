Rationalization
===============

This repo contains all files pertaining to the preparation and analysis of my project "Misreporting of ideological self placement to rationalize party preferences". Comments Welcome

Files:
------
- analysis
	+ analysis_prerun.R: Description of data collected in pre run of the experiment and some code for calculating quantities of interest for analysis of main study
	+ anes_prep.R: Prepares raw ANES data (anes_timeseries_2012_stata12.dta) as downloaded from the ANES webpage for use in ideology_prediction.R
	+ ideology_prediction.R: Fits a random forest to the ANES data to extract the ost important predictors of ideological self placement
	+ power_analysis.R: Simulation of hypothesized DGP and power analysis to determine sample size
	+ prerun_data_prep.R: Produces pre_run_clean.csv from raw output of qualtrics and Mturk (not here due to data privacy)
- data
	+ pre_run/pre_run_clean.csv: Cleaned and anonymized data from pre run of the experiment
	+ preparation/anes_clean.csv: cleaned ANES data for use in ideology_prediction.R
- surveys
	+ rationalization_main.txt: survey questions and flow for main study
	+ rationalization_prerun.txt: survey questions and flow for pre run
