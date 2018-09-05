#Author:
#Date:
#Agency:
#Purpose:



#Load libraries
#--------------------------
#	library(foreign)
	library(reshape2)
	library(ggplot2)
	library(scales)
	library(tidycensus)
	library(acs)
	library(tictoc)
	library(dplyr)
	library(httr)

#Set script parameters
#--------------------------
	#Set working directory
	#choose.dir()
	setwd("Z:/JRoll/Workshops/TC Data Science Workshop 2018/Crash Exercise/")
	#Set cache 
	options(tigris_use_cache = TRUE)
	#Configure firewall settings
	set_config(use_proxy(url="proxynew.odot.state.or.us", port=8080)) 
		set_config( config( ssl_verifypeer = 0L ) )
	#Set Census API Key
	Developer_Key <- ""
	
#Create functions
#----------------------------	
	#Function that simplifies loading .RData objects
	assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }
	#Function that changes column headers to lower case except for first letter
	toProperName <- function(X){
		EndX <- nchar(X)
	    paste(toupper(substring(X, 1, 1)), tolower(substring(X, 2, EndX)), sep="")
     }
	
#Create working folders (Be sure to have write access to any selected directories)
#------------------
	#Choose working directory
	#setwd(choose.dir())
	
#Define work space variables
#--------------------------
	#create years vector
	Years. <- as.character(c(1975:2016))
	#Create a vector of state names - add comment
	States. <- unique(fips_codes$state_name)[c(1:51,55)]
	#Create a vector of state Census FIPS codes
	State_Fips. <- unique(fips_codes$state_code)[c(1:51,55)]
	
#Load Data
#---------------------------
	#Load FARS state fatal data
	Fars_State_Fatal.. <- assignLoad(file =  paste(getwd(), "/Data/State_Fatal_Data.RData", sep=""))
	#State_Fc_Desc_Data.. <- assignLoad(file =  paste(getwd(), "/Data/Processed_Data/State_Fatal_Fc_Desc_Data.RData", sep=""))
	
	#Load ODOT Crash data file 
	Load_Crash.. <- assignLoad(paste(getwd(),"/Data/ODOT_Crash_02_16.RData",sep=""))

#Explore data 
#------------------------
	#Data structure
	str(Load_Crash..)
	
	#number of rows
	nrow(Load_Crash..)
	
	#Look at years of data 
	unique(Load_Crash..$Crash_yr_no)
	
	#Look at top rows 
	head(Load_Crash..)
	
#Statewide Evaluation
#----------------------
	#Define metrics to summarize
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
	
	#Do simply summary of one injury severity type using just tapply()
	tapply(Load_Crash..$Tot_fatal_cnt, Load_Crash..$Crash_yr_no, sum)
		
	#Summarize using combination of reshape:melt, sapply, & tapply functions
	State_Summary.. <- melt(sapply(Metrics., function(x){tapply(Load_Crash..[,x],Load_Crash..$Crash_yr_no, sum)}),varnames = c("Year","Injury_Severity"), value.name = "Count")
	
	#Take a look at the summarize data---
	#Look at structure of data
	str(State_Summary..)
	#Number of rows
	nrow(State_Summary..)
	#look at first few rows
	head(State_Summary..)
	
	#Plot all Fatal injuries form ODOT data - 2002 - 2016
	#++++++++++++++++++++++++
	dat <- State_Summary..[State_Summary..$Injury_Severity%in%"Tot_fatal_cnt",]
	#Initialize ggplot
	ggplot(dat, aes(x = Year, y = Count)) + 
		#Add line layer
		geom_line(color = "red", size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 4) + 
		#Add custom y-axis label
		ylab("Annual Traffic Fatal Injuries")  +  
		#Add title
		ggtitle("Traffic Fatal Injuries in \nOregon") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,1),  labels = seq(2002,2016,1)) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 
		
	
	#Plot all injuries form ODOT data - 2002 - 2016
	#++++++++++++++++++++++++
	dat <- State_Summary..
	dat$Injury_Severity <- as.character(dat$Injury_Severity)
	#Change ODOT injury codes to more presentable form
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_fatal_cnt"] <- "Fatal Injuries (K)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_lvl_a_cnt"] <- "Severe Injuries (A)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_cnt"] <- "All Injuries (ABC)"
	#As character class, 'Injury_Severity' wont order properly in Facet so convert to factor
	dat$Injury_Severity <- factor(dat$Injury_Severity,  levels = c("Fatal Injuries (K)", "Severe Injuries (A)", "All Injuries (ABC)"))
	
	#Initialize ggplot
	ggplot(dat, aes(x = Year, y = Count)) + 
		#Add line layer
		geom_line(aes(color = Injury_Severity), size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 4) + 
		#Add facet 
		facet_wrap(~Injury_Severity, scales = "free") +
		#Add custom y-axis label
		ylab("Annual Traffic Injuries")  +  
		#Add title
		ggtitle("Traffic Injuries by Severity in \nOregon") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,3),  labels = seq(2002,2016,3)) +
		#Add commas 
		scale_y_continuous(labels = comma) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 
		
	
#County Evaluation
#----------------------
	#Define metrics to summarize
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
	
	#Do simply summary of one injury severity type using just tapply()
	tapply(Load_Crash..$Tot_fatal_cnt, Load_Crash..$Crash_yr_no, sum)
		
	#Summarize a for loop and melt()
	Temp.. <- Load_Crash..
	#By year and county 
	TempList_ <- list()
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
	for(metric in Metrics.){
		TempList_[[metric]] <- tapply(Temp..[,metric],list(Temp..$Crash_yr_no,Temp..$Cnty_nm), sum,na.rm=T) 
	}
	TempList_FcSv <- melt(TempList_, varnames = c("Year","County"), value.name = "Count")
	colnames(TempList_FcSv )[4] <- "Injury_Severity"
	#Rename 
	County_Summary.. <- TempList_FcSv
	#Convert conty to character from factor
	County_Summary..$County <- as.character(County_Summary..$County)
	
	#Take a look at data---
	#Look at structure of data
	str(County_Summary..)
	#Number of rows
	nrow(County_Summary..)
	#look at first few rows
	head(County_Summary..)
	
	#Plot all injuries from ODOT data for each county - 2002 - 2016
	#++++++++++++++++++++++++
	dat <- County_Summary..
	dat$Injury_Severity <- as.character(dat$Injury_Severity)
	#Change ODOT injury codes to more presentable form
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_fatal_cnt"] <- "Fatal Injuries (K)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_lvl_a_cnt"] <- "Severe Injuries (A)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_cnt"] <- "All Injuries (ABC)"
	#As character class, 'Injury_Severity' wont order properly in Facet so convert to factor
	dat$Injury_Severity <- factor(dat$Injury_Severity,  levels = c("Fatal Injuries (K)", "Severe Injuries (A)", "All Injuries (ABC)"))
	#Initialize ggplot
	ggplot(dat, aes(x = Year, y = Count)) + 
		#Add line layer
		geom_line(aes(color = County), size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 4) + 
		#Add facet 
		facet_wrap(~Injury_Severity, scales = "free") +
		#Add custom y-axis label
		ylab("Annual Traffic Injuries")  +  
		#Add title
		ggtitle("Traffic Injuries by Severity in \nOregon Counties") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,3),  labels = seq(2002,2016,3)) +
		#Add commas 
		scale_y_continuous(labels = comma) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 
		

#		
#So this is a mess and we need to be more deliberate about what data we want to show
#Lets use 2016 population to categorize the counties by size and visualize


	#Retrieve population data 
	#######################
	#Use get_acs() from tidycensus package to retrieve population data
	#The function requires a key which is free and available from US Census. 
	County_Population.. <-   as.data.frame(get_acs(geography = "county", variables = "B25038_001",
          state = "41", key = Developer_Key, year = c(2016), survey = "acs5"))
	  
	#Take a look at data---
	#Look at structure of data
	str(County_Population..)
	#Number of rows
	nrow(County_Population..)
	#look at first few rows
	head(County_Population..)
	#To make joining with the injury data possible we need to reformat the county GEOID
	County_Population..$County <- as.character(unlist(lapply(strsplit(County_Population..$NAME, split =" "), function(x)x[1])))
	
	#Now join population data to Traffic Injury data
	County_Summary..$Population <- County_Population..$estimate[match(County_Summary..$County, County_Population..$County)]
	
	#Now lets create categories for the counties---
	#Lets use the quintiles to categorize the counties
	summary(County_Population..$estimate)
	
	#Create an object that represents the quintiles
	Population_Quintiles. <- summary(County_Population..$estimate)[c(2,3,5,6)]
	
	#Use these to categorize the counties
	County_Summary..$Population_Bin <- cut(County_Summary..$Population, breaks = Population_Quintiles. , labels = c("<=7,436","7,437 - 37,240","37,241-313,200"))
	
	#look at which counties were put in which bins
	tapply(County_Summary..$County, County_Summary..$Population_Bin, unique)
	
	#Now explore the summarizing the traffic injury data by these bins
	#Sum by bin 
	Sum_County_Injury.. <- melt(tapply(County_Summary..$Count, list(County_Summary..$Year, County_Summary..$Population_Bin, County_Summary..$Injury_Severity), sum),
		varnames = c("Year","Population_Bin", "Injury_Severity"), value.name = "Count")
	
	#Now chart
	dat <- Sum_County_Injury..
	dat$Injury_Severity <- as.character(dat$Injury_Severity)
	#Change ODOT injury codes to more presentable form
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_fatal_cnt"] <- "Fatal Injuries (K)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_lvl_a_cnt"] <- "Severe Injuries (A)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_cnt"] <- "All Injuries (ABC)"
	#As character class, 'Injury_Severity' wont order properly in Facet so convert to factor
	dat$Injury_Severity <- factor(dat$Injury_Severity,  levels = c("Fatal Injuries (K)", "Severe Injuries (A)", "All Injuries (ABC)"))
	#COnvert population bin variable to factor
	dat$Population_Bin <- factor(dat$Population_Bin, levels = c("<=7,436","7,437 - 37,240","37,241-313,200"))
	#Initialize ggplot
	ggplot(dat, aes(x = Year, y = Count)) + 
		#Add line layer
		geom_line(aes(color = Population_Bin), size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 4) + 
		#Add facet 
		facet_wrap(~Injury_Severity, scales = "free") +
		#Add custom y-axis label
		ylab("Annual Traffic Injuries")  +  
		#Add title
		ggtitle("Traffic Injuries by Severity in \nOregon Counties") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,3),  labels = seq(2002,2016,3)) +
		#Add commas 
		scale_y_continuous(labels = comma) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 
		
	
	
	
	
	

	
	
	
	
	
	
	
#Download and format Census Statewide population data
#-----------------------------
	#Get population data
	Census_Data.. <-   get_acs(geography = "state", variables = "B25038_001",
          state = State_Fips., key = Developer_Key, year = 2012, survey = "acs1")
	
		  
		  

	
	
	
	
	
	

	