climate_eva_scripts repository: 

This is a collection of various in use, archived, and broken scripts which I have and am using to do
eva research. Most all heavily rely on the library extRemesby Eric Gilleland. More information can be 
found at https://cran.r-project.org/web/packages/extRemes/.

Useage of scripts:
climdiv_dataprep.R:     ACTIVE: This script is used to parse datasets from NOAA/NCDC's ClimDiv to be
		        used in R for various types of analysis. The script prompts the user for a the
		        dataset they wish to access, the states they wish to access, and if they want 
		        the divisional or state dataset. 
find_state.R:		ACTIVE: This function is used to find a state (or region) based on a code which 
			corresponds to a National Weather Service Region. 

eva_script_(broken).R:  ARCHIVED: This script may now be broken and is only to be used for reference.
			Previous versious of this were used to do a full EVA analysis of the contiguous
			US ClimDiv's (divisional dataset).
tdf_curve.R:		ARCHIVED: This script was used to develop TDF (temperature-duration-frequency)
			curves for Houston and Sacramento.
eva_script.R:		ARCHIVED: This was the first script used to do EVA analysis, utilizing the 
			library extRemes heavily. 


