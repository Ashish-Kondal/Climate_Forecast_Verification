---
title: "Heidke Skill Score Computation"
author: "Ashish Kondal"
date: "01/12/2024"
output: html_document
---

#-------------------------------------------------------------------------------
## 				CODE DESCRIPTION
#-------------------------------------------------------------------------------
The provided R code computes the Heidke Skill Score (HSS) for tercile categorization on NMME (North American Multi-Model Ensemble) ensemble mean forecasts with multiple initialization months. 
See "NMME_data_sample.JPG" for the structure of input data.
Here's a simple summary of the HSS computation process for a meterological variable (Precipitation):

1. Input Setup:
	- Define input and output directories.
	- Set the working directory for code chunks.

2.Retrieving Arguments:
	- Retrieve command line arguments from a batch script, specifically the initialization month of NMME models.
	
3. Heidke Skill Score Computation:
	- Check if the initialization month is within the desired range (November to February). If not, cancel the run. (Just added for my own problem - no need to do it if you do not need it)
	- Select the variable "Precipitation". (Arbitrarily selected one meteorlogical variable)
	- Read observation data (GridMET) and forecast data (NMME) from aggregated VIC-CropSyst outputs.
	- Restrict data to November through February.  (Specific to my problem  - skip this step if you don't need it)
	- Generate seasonal average precipitation for the specified months.
	- Join forecast and observation data, convert it into a shapefile, and perform spatial aggregation based on HUC 06 sub-basins.

4. Spatial Operations and HSS Calculation:
	- Define functions for spatial operations and calculating the Heidke Skill Score.
	- Loop through each HUC 06 sub-basin:
	- Perform spatial operations to extract data for the current sub-basin.
	- Calculate terciles for both observation and forecast data.
	- Create a contingency table and calculate HSS for each tercile category (Below 33rd, Between 33rd and 67th, Above 67th).

5. Save the HSS results to an output file.
	- The HSS results for each sub-basin are saved to output files for further analysis.

In summary, the code conducts a spatial analysis on observational and forecasted data, calculates terciles, and evaluates the Heidke Skill Score for different tercile categories. 
This process is repeated for multiple initialization months, providing a comprehensive assessment of model skill in predicting tercile-based precipitation outcomes. 
The results are saved for each HUC 06 sub-basin, allowing for further investigation and interpretation of skill scores.

#-------------------------------------------------------------------------------
## 				SETUP - STARTING WITH FRESH ENVIRONMENT AND CONSOLE
#-------------------------------------------------------------------------------

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width = 12, fig.height = 8, fig.path ='Figs/',echo = FALSE, warning = FALSE, message = FALSE)
cat("\014")     				# Clear console
rm(list = ls()) 				# Clearing the Environment and saved variable
```
#-------------------------------------------------------------------------------
## 				INSTALLING REQUIRED PACKAGES
# Skip if you already have required packages installed
#-------------------------------------------------------------------------------

#install.packages("dplyr")
#install.packages("data.table")
#install.packages("tidyverse")
#install.packages("knitr")
#install.packages("readr")
#install.packages("sf")
#install.packages("pryr")  # To check memory usage

#-------------------------------------------------------------------------------
## 				LOADING PACKAGES
#-------------------------------------------------------------------------------

```{r Loading Packages, include = FALSE, echo = FALSE, warning=FALSE}
library(readr)
library(knitr)
library(dplyr)
library(sf)
```

#-------------------------------------------------------------------------------
## 				INPUT SETUP - DEFINE VARIABLES AND DIRECTORIES
#-------------------------------------------------------------------------------

```{r Input setup}
input_dir <- paste("Path_To_Input_Directory/", sep="")  										# Setting working directory and suffix as file address
output_dir <- paste("Path_To_Output_Directory/", sep="") 													# Setting output directory to store files and final images

# Setting working directory for all code chunks
knitr::opts_knit$set(root.dir = input_dir)																															# Setting input directory as working directory: where all the input files exists.     

# Create output directory if not already exists
if (!dir.exists(paste(output_dir, sep="")))
	{ dir.create(paste(output_dir, sep="")) } 

months_list <- c("January","February","March","April","May","June","July","August","September","October","November","December")  
Seasons <- c("Summer","Winter","Fall","Spring")


# Hardcoded variables (Specific to problem)
basin <- c('pnw')
variable_list <- c("Sum_PPT","Avg_Tavg","Avg_Baseflow","Sum_Baseflow","Avg_Runoff","Sum_Runoff")																	# Variables List (name corresponding to "Aggregated_FLUX_File"). 
varDisp <- c("Accumulated_Precipitation", "Average_Temperature"," Average_Baseflow_Runoff","Accumulated_Baseflow_Runoff","Average_Runoff","Accumulated_Runoff")		# Full display name of corresponding variable list.

# Shapefiles of the study region on HUC 06 spatial aggregation
huc6_shapefile_address <- paste("Path_To_Input_Directory_of_Shapefile/huc6.shp", sep="")
huc6_shp <- st_read(huc6_shapefile_address)
```

#-------------------------------------------------------------------------------
## 				RETRIEVING ARGUMENTS FROM BATCH SCRIPT
#-------------------------------------------------------------------------------
```{r Retrieving Arguments}
args <- commandArgs(trailingOnly = TRUE)										# Retrieving argument from batch script
init <- as.numeric(args[1])														# Enforcing retrieved argument to be in numeric format
```

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
## 				HEIDKE SKILL SCORE COMPUTATION
# Heidke Skill Score of Accumulated Precipitation: November thru February
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

```{r Seasonal HSS, echo=FALSE, warning = FALSE}

# Killing Unnecssary Runs of Unwanted INITIALIZATION months - If we don't do it, we are unnecessary running these initialization from batch script.
if (isTRUE(init[!(init %in% c(6,7,8,9,10,11))] == init)){																# Check if initializtion months "init" isn't a part of list. If not, then cancel the run.
	print("Files are not generated for this initialization because it doesn't have required forecast season")
	quit()
}	

# Selecting appropriate variable - Precipitation
req_cols <- c("Lat","Lon","Year","Month",variable_list[1])																# Selecting appropriate variables/columns from the input files. See "variable_list" for the list of available variables.
disp_variable_name <- varDisp[1]																						# Storing selected variable name for later use.
cat(disp_variable_name)


#-------------------------------------------------------------------------------
## 				READING OBSERVATION DATASET
# Reading GridMET File - Aggregated VIC-CropSyst's Output
#-------------------------------------------------------------------------------	
histfile <- paste(input_dir,basin,"_FLUX_GridMET_VIC_CropSyst_Aggregated_Output_NEW1.txt",sep="") 											# Name (with fullpath) of Aggregated VIC-CropSyst Flux File generated with GridMET data.
vhist_data <- read_csv(file = histfile, col_names = TRUE, show_col_types = FALSE, col_select = all_of(req_cols))								# Reading and only selecting particular columns to save memory and faster computations.

#-------------------------------------------------------------------------------
## 				READING FORECAST DATASET
# Reading NMME File - Aggregated VIC-CropSyst's Output
#-------------------------------------------------------------------------------	
mm_inputfilename <- paste(input_dir,basin,"_FLUX_ENSMEAN_VIC_CropSyst_Aggregated_Output_InitMonth_0",init,"_NEW1.txt",sep="") 				# Name (with fullpath) of Aggregated VIC-CropSyst Flux File generated with NMME data.
mm <- read_csv(file = mm_inputfilename,col_names = TRUE, show_col_types = FALSE, col_select = all_of(req_cols))								# Reading and only selecting particular columns to save memory and faster computations.


#-------------------------------------------------------------------------------
## 			MAKE CONSISTENT TIME FRAME OF OBSERVATION AND FORECAST DATASET
# Since we are interested only in Novebmer through February precipitation, we extracted the data for these months only.
#-------------------------------------------------------------------------------	
obv_data <- filter(vhist_data,(Month >= 11 | Month <=2))
mm_data <- filter(mm,(Month >= 11 | Month <=2))
obv_data <- dplyr::arrange(obv_data,Lon,Lat,Year,Month)
colnames(obv_data) <- c("Lat","Lon","Year","Month","GridMET")
mm_data <- dplyr::arrange(mm_data,Lon,Lat,Year,Month)
colnames(mm_data) <- c("Lat","Lon","Year","Month","ENSMEAN")


#-------------------------------------------------------------------------------
## 			GENERATING SEASONAL ACCUMULATED PRECIPITATION - NOV THRU FEB
#-------------------------------------------------------------------------------	
# For ease of operations, we increase the year corresponding to November and December by 1, this way we can easily summed up Nov-Dec-Jan-Feb in one step.
mm_data <- mm_data %>% mutate(Year = ifelse(Month %in% c(11, 12), Year + 1, Year))
obv_data <- obv_data %>% mutate(Year = ifelse(Month %in% c(11, 12), Year + 1, Year))
obv_data1 <- obv_data %>% group_by(Lat,Lon,Year) %>% summarize(sum(GridMET))			# For this step, we increased year by 1
frc_data1 <- mm_data %>% group_by(Lat,Lon,Year) %>% summarize(sum(ENSMEAN))

# Ungroup the datasets to avoid unintended grouping and incompatibility issues in downstream operations	
frc_data1 <- ungroup(frc_data1)
obv_data1 <- ungroup(obv_data1)
# Reassuring that column names are still unchanged after ungrouping
colnames(obv_data1) <- c("Lat","Lon","Year","GridMET")
colnames(frc_data1) <- c("Lat","Lon","Year","ENSMEAN")

#-------------------------------------------------------------------------------
## 			JOIN FORECAST AND OBSERVATION DATA TOGETHER & CONVERT INTO SHAPEFILE
#-------------------------------------------------------------------------------
merged_data <- merge(frc_data1,obv_data1,by=c("Lat","Lon","Year"))
colnames(merged_data) <- c("Lat","Lon","Year","ENSMEAN","GridMET")
merged_data <- na.omit(merged_data)										# Removing any NaN's
sf_data = st_as_sf(merged_data, coords = c("Lon", "Lat"),crs=4326)		# Transforming merged data into shapefile for subsequent processing

#-------------------------------------------------------------------------------
## 			SPATIAL AGGREGATION - HUC 06 
#-------------------------------------------------------------------------------																					
name_huc <- huc6_shp$Name																													# Save Name column from huc_06 shapefile into a new variable.
HSS_outfilename <- paste(output_dir, "HUC06_HeidkeSkillScore_Precipitaion_InitMonth_0",init,"_NDJF.txt",sep="")		# Output File name. All seasons AC will be stored in a single file.


#-------------------------------------------------------------------------------
## 			FUNCTION FOR SPATIAL OPERATIONS
#-------------------------------------------------------------------------------

perform_spatial_operations <- function(current_huc, huc6_shp, sf_data) {
	nam1 <- subset(huc6_shp, Name == current_huc)																			# Subset the HUC06 file to extract current sub-basin's shapefile.
	shp1 = st_transform(structure(nam1, proj4string = "+init=epsg:3857"), "+init=epsg:4326")								# Transform the projection of subsetted sub-basin's shapefile
	intersect_ind =st_intersects(sf_data, shp1, sparse=FALSE)																# Finding row index of coordinates in sf_data (i.e. merged_data) which falls within sub-basin. 
	data_hucc = sf_data[intersect_ind,]																						# Extracting data from sf_data based on "intersect_ind".
	df_data_huc <- st_drop_geometry(data_hucc) %>% 																			# Here, we are just removing the "geometry" column from the dataframe. We don't need it anymore.
		group_by(Year) %>% 																									# Grouping the huc_data based on Year and then, summarize the column.
		summarize(GridMET = sum(GridMET), ENSMEAN = sum(ENSMEAN))
   	df_data_huc <- na.omit(df_data_huc)																						# Removing unintentionally introduced NA's in the data
	return(df_data_huc)
}

#-------------------------------------------------------------------------------
## 			FUNCTION FOR CALCULATING HEIDKE SKILL SCORE 
#-------------------------------------------------------------------------------

calculate_skill_metrics <- function(df_data_huc) {
	if (nrow(df_data_huc) != 0) {
		
		# Generate terciles based on observation and forecast data
		quantiles <- quantile(df_data_huc$GridMET, probs = c(1/3, 2/3))
		quantiles_f <- quantile(df_data_huc$ENSMEAN, probs = c(1/3, 2/3))

		# Create tercile categorization of observation and forecast data
		df_data_huc$Obs_Tercile <- cut(df_data_huc$GridMET, breaks = c(-Inf, quantiles[1], quantiles[2], Inf), labels = c("Below 33rd", "Between 33rd and 67th", "Above 67th"))
		df_data_huc$Forecast_Tercile <- cut(df_data_huc$ENSMEAN, breaks = c(-Inf, quantiles_f[1], quantiles_f[2], Inf), labels = c("Below 33rd", "Between 33rd and 67th", "Above 67th"))
		
		# Create a contingency table
		contingency_table <- table(df_data_huc$Obs_Tercile, df_data_huc$Forecast_Tercile)
		
		# Retrieving elements of contingency table for subsequent processing. Post scripts (1,2,3) in the variable's name corresponds to "Below 33rd", "Between 33rd and 67th", and "Above 67th" category.
		Hit1=contingency_table[1,1]
		Hit2=contingency_table[2,2]
		Hit3=contingency_table[3,3]
		CN1=contingency_table[3,2]+contingency_table[2,2]+contingency_table[2,3]+contingency_table[3,3]
		CN2=contingency_table[1,1]+contingency_table[1,3]+contingency_table[3,1]+contingency_table[3,3]
		CN3=contingency_table[1,1]+contingency_table[1,2]+contingency_table[2,2]+contingency_table[2,1]
		Miss1=contingency_table[2,1]+contingency_table[3,1]
		Miss2=contingency_table[1,2]+contingency_table[3,2]
		Miss3=contingency_table[1,3]+contingency_table[2,3]
		FA1=contingency_table[1,2]+contingency_table[1,3]
		FA2=contingency_table[2,1]+contingency_table[2,3]
		FA3=contingency_table[3,1]+contingency_table[3,2]
		
		# Computing Hedike Skill Score for Each Tercile Category Separately
		#HSS = (2*((Hit * CN)-(FA*Miss)))/(((Hit+Miss)*(Miss+CN))+((Hit+FA)*(FA+CN))) 
		pc_cond1 =((2*((as.double(Hit1)*(as.double(CN1)))-((as.double(FA1)*as.double(Miss1)))))/(((as.double(Hit1)+as.double(Miss1))*(as.double(Miss1)+as.double(CN1)))+((as.double(Hit1)+as.double(FA1))*(as.double(FA1)+as.double(CN1))))) * 100      # obs/2 is subtracted, because based on Tercile Categorization.
		pc_cond2 =((2*((as.double(Hit2)*(as.double(CN2)))-((as.double(FA2)*as.double(Miss2)))))/(((as.double(Hit2)+as.double(Miss2))*(as.double(Miss2)+as.double(CN2)))+((as.double(Hit2)+as.double(FA2))*(as.double(FA2)+as.double(CN2))))) * 100      # obs/2 is subtracted, because based on Tercile Categorization.
		pc_cond3 =((2*((as.double(Hit3)*(as.double(CN3)))-((as.double(FA3)*as.double(Miss3)))))/(((as.double(Hit3)+as.double(Miss3))*(as.double(Miss3)+as.double(CN3)))+((as.double(Hit3)+as.double(FA3))*(as.double(FA3)+as.double(CN3))))) * 100      # obs/2 is subtracted, because based on Tercile Categorization.
		HSS_data <- as.data.frame(cbind(current_huc,pc_cond1,pc_cond2,pc_cond3))
		colnames(HSS_data) <- c("HUC06_Name","Below 33rd Percentile", "Between 33rd and 67th Percentile","Above 67th Percentile")
		return(list(HSS_data = HSS_data))
	  } else {
		return(NULL)
	  }
}

#-------------------------------------------------------------------------------
## 			FUNCTION FOR SAVING HSS OUTPUT FILE
#-------------------------------------------------------------------------------

for (gg in 1:length(name_huc)) {																	# Running loop for the length of "name_huc" i.e. for number of sub-basins in particular huc shapefile.	
  current_huc <- name_huc[gg]																		# Current Sub-basin
  df_data_huc <- perform_spatial_operations(current_huc, huc6_shp, sf_data)							# Perform spatial operation on current sub-basin
  metrics_result <- calculate_skill_metrics(df_data_huc)											# Computing Heidke Skill Score for current sub-basin
  if (!is.null(metrics_result)) {
	HSS_data <- metrics_result$HSS_data
    if (file.exists(HSS_outfilename) == TRUE) {
		write_csv(x = HSS_data, file = HSS_outfilename, append = TRUE, col_names = FALSE)
	}else{
		write_csv(x = HSS_data, file = HSS_outfilename, append = FALSE, col_names = TRUE)
	}
	rm(HSS_data)
  }
}
```









