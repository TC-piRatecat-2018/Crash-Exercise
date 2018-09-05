#Script Description
#--------------------
#This script statewide crash data to evualate large truck crashes
#Author: Josh Roll
#Date:7/25/2016



	#Load libraries
	#-----------------------
	library(RODBC)
	library(reshape2)
	library(ggplot2)
	library(scales)
	library(cowplot)
	library(rgdal)
	library(maptools)
	library(gridExtra)
	library(tictoc)
	library(tidycensus)
	library(tictoc)
	library(dplyr)
	install.packages("TRR")

	#Construct functions to apply below
	#---------------------------
	#Proper name
	toProperName <- function(X){
          EndX <- nchar(X)
          paste(toupper(substring(X, 1, 1)), tolower(substring(X, 2, EndX)), sep="")
     }
	#For loading R data objects
	assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }
	
	
	options(scipen=999)
	options(digist = 2)

	
#Load Data
#----------------------------------------
	#Set directory 
	setwd("F:/Data/Crash/Data/ODOT/")
	
	#Load Crash data
	LoadCrash.. <- assignLoad("Processed/Crash_02_16.RData")
	#Load Vehicle Data
	LoadVhcl.. <- assignLoad("Processed/Vhcl_02_16.RData")
	#Load Participant Data
	LoadPartic.. <- assignLoad("Processed/Partic_02_15.RData")
	
	#Load VMT Data
	Load_Vmt.. <- assignLoad("F:/Data/VMT/Data/Processed/Processed_HPMS_1980_2016.RData")
	
	#Prepare Data
	#-----------------------------
	#All crashes by severity by year
	##########################
	#Define metrics to summarize
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
	#Summarize
	All_Modes_YrSv.. <- melt(sapply(Metrics., function(x){tapply(LoadCrash..[,x],list(LoadCrash..$Crash_yr_no), sum)}),varnames = c("Year","Injury_Severity"), value.name = "Count")
	
	#plot all Fatal injuries form ODOT data - 2002 - 2016
	#++++++++++++++++++++++++
	dat <- All_Modes_YrSv..[All_Modes_YrSv..$Injury_Severity%in%"Tot_fatal_cnt",]
	p <- ggplot(dat, aes(x = Year, y = Count)) + 
		geom_line(color = "red", size = 1.25) + 
		geom_point(color = "black", size = 4) + 
		ylab("Annual Traffic Fatal Injuries")  +  
		#Add title
		ggtitle("Oregon Traffic Fatal Injuries") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,1),  labels = seq(2002,2016,1)) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 
	p	
	
	#Save
	File_Location <- "Z:/JRoll/Workshops/TC Data Science Workshop 2018/Crash Exercise/Graphics/Oregon_Traffic_Fatals_2002_2016.jpeg"
	jpeg(file = File_Location,, width = 640, height = 360, quality = 100)
	p 
	dev.off()
	
	

	
#Set script parameters
#--------------------------
	#Set working directory
	#setwd(choose.dir())
	setwd("F:/Data/FARS")

		
#Define work space variables
#--------------------------
	#create years vector
	Years. <- as.character(c(1975:2016))
	#Create a vector of state names - add comment
	States. <- unique(fips_codes$state_name)[c(1:51,55)]
	
	#Create vector of codes for cities in CLMPO
	#MpoCodes. <- c("350","660","1960")
	#names(MpoCodes.) <- c("Coburg","Eugene","Springfield")
	#
	#Link to FARS Data documentation
	#https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812447
		
	
#Process data 
#Each year of data not in same format so we have to pick out data of interest and compile into a single data set
#-------------------------
	#Define data frames to store processed data below
	All_State_Data.. <- data.frame()
	#County_Data.. <- data.frame()
		
	#Use for loop to cycle through each year of data
	for(year in Years.){
		#Define flags since they change by year
		#+++++++++++++++++++++++++++++++++++++++
		#BAC levels - Set a default for years 1975 - 2014
		Bac_Levels. <- c(0,94);
		Drunk_Level <- 80
		#Alcohol test 
		#Determine for years 1975 - 1990
		if(year%in%as.character(1975:1990)){
			Alcohol_Flag <- "Test_res"}
		#Determine for years 1991 - 2016	
		if(year%in%as.character(1991:2016)){
			Alcohol_Flag <- "Alc_res";}
		#Set BAC and Drunk level for years 2015 - 2016	
		if(year%in%as.character(2015:2016)){
			Bac_Levels. <- c(0,940);
			Drunk_Level <- 800}
		#Pedestrian Flag
		if(year%in%as.character(1975:1981)){Ped_Flag <- "3"}
		if(year%in%as.character(1982:2016)){Ped_Flag <- "5"}
		#Bike Flag
		#if(year%in%as.character(1975:1981)){Bike_Flag <- "4"}
		#if(year%in%as.character(1982:2016)){Bike_Flag <- "6"}
		
		#Load data
		#++++++++++++++++++++++++++++++++++++++++++++++++
		Accident.. <- assignLoad(file = paste(getwd(),"/","Data/Data_Download/",year,"/accident.RDATA",sep=""))
		Person.. <- assignLoad(file = paste(getwd(),"/","Data/Data_Download/",year,"/person.RDATA",sep=""))
		#Format data
		colnames(Accident..) <- toProperName(colnames(Accident..))
		colnames(Person..) <- toProperName(colnames(Person..))
		
		#Prepare Data
		#++++++++++++++++++++++++++
		#Add geographic descriptives to Accident data---
		#Reform State variable to character and add leading zero if necessary
		Accident..$State <- sprintf("%02d",Accident..$State)
		#Add state names from tidycensus package
		Accident..$State_desc <- fips_codes$state_name[match(Accident..$State, fips_codes$state_code)]
		#Reform County variable to character and add leading zero if necessary
		Accident..$County <- sprintf("%03d",Accident..$County)
		#Add county names from tidycensus package
		Accident..$County_desc <- fips_codes$county[match(Accident..$County, fips_codes$county_code)]
		
		#Add geographic descriptives to Person data---
		#Reform State variable to character and add leading zero if necessary
		Person..$State <- sprintf("%02d",Person..$State)
		#Add state names from tidycensus package
		Person..$State_desc <- fips_codes$state_name[match(Person..$State, fips_codes$state_code)]
		#Reform County variable to character and add leading zero if necessary
		Person..$County <- sprintf("%03d",Person..$County)
		#Add county names from tidycensus package
		Person..$County_desc <- fips_codes$county[match(Person..$County, fips_codes$county_code)]
				
		#All fatals--
		#By state
		Temp_State. <- tapply(Accident..$Fatals, list(Accident..$State_desc) , sum)
		#Craft data frame and Store (State data frame created for first time)
		State_Data.. <- data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "All Fatals")
			
		#All alcohol involved fatals---
		#Determine Number fatalities alcohol was involved (Use 'police reported drinking involved' and measure greater than 0) ---
		Alc_Inv.. <- Person..[Person..$Drinking%in%1 | (Person..[,Alcohol_Flag] > Bac_Levels.[1] & Person..[,Alcohol_Flag]  <= Bac_Levels.[2]),]
		#Select only fatal injuries (fatal injury == 4)
		Alc_Inv.. <- Alc_Inv..[Alc_Inv..$Inj_sev%in%4,]
		#Sum by state 
		Temp_State. <- tapply(Alc_Inv..$Inj_sev, list(Alc_Inv..$State_desc) , length)
		#Craft data frame and Store with existing 
		State_Data.. <- rbind(State_Data..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Alcohol Involved Fatals"))
		
		#Pedestrian fatalities---
		#Determine person records that are pedestrians
		Ped_Fatals.. <- Person..[Person..$Per_typ%in%Ped_Flag,]
		#Select only fatal injuries (fatal injury == 4)
		Ped_Fatals.. <- Ped_Fatals..[Ped_Fatals..$Inj_sev%in%4,]
		#Sum by state 
		Temp_State. <- tapply(Ped_Fatals..$Inj_sev, list(Ped_Fatals..$State_desc) , length)
		#Craft data frame and Store
		State_Data.. <- rbind(State_Data..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Pedestrian Fatals"))
		
		#Pedestrian fatalities with BAC > 0.08 (for the pedestrian)---
		#Determine person records that are pedestrians
		Ped_Fatals.. <- Person..[Person..$Per_typ%in%Ped_Flag,]
		#Select only fatal injuries (fatal injury == 4)
		Ped_Fatals.. <- Ped_Fatals..[Ped_Fatals..$Inj_sev%in%4 & Ped_Fatals..[,Alcohol_Flag,] >= Drunk_Level,]
		#Sum by state 
		Temp_State. <- tapply(Ped_Fatals..$Inj_sev, list(Ped_Fatals..$State_desc) , length)
		#Craft data frame and Store
		State_Data.. <- rbind(State_Data..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Drunk Pedestrian Fatals"))
		
		#Process data
		#+++++++++++++++++++++++++++++++++++++
		#Calculate the proportion of total fatals that are alcohol involved
		#Create a vector for numerator
		x <-  State_Data..$Count[State_Data..$Measure%in%"Alcohol Involved Fatals" ]
		names(x) <- State_Data..$State[State_Data..$Measure%in%"Alcohol Involved Fatals" ]
		#Order according to defined vector of states to ensure proper calculation
		x <- x[order(factor(names(x), levels=States.))]
		#Create a vector for denominator
		y <-  State_Data..$Count[State_Data..$Measure%in%"All Fatals"]
		names(y) <- State_Data..$State[State_Data..$Measure%in%"All Fatals"]
		#Order according to defined vector of states to ensure proper calculation
		y <- y[order(factor(names(y), levels=States.))]
		#Calculate proportion		
		Temp_State. <- x / y[names(x)]
		#Craft data frame and Store
		State_Data.. <- rbind(State_Data..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Proportion Alcohol Involved Fatals"))
		
		#Store in master data frame
		#+++++++++++++++++++++++++++++++++++
		#Add year to current State data frame 
		State_Data..$Year <- year
		#Store
		All_State_Data.. <- rbind(All_State_Data.., State_Data..)
				
		#Provide progress
		print(year)
	}
	#Rename All_State_Data for quicker error fix and object calling in the following code
	State_Fatal.. <- All_State_Data..
	

	
#Analysis (Calculations and visualizations)
#-----------------------

	#Graph Oregon total fatals
	#+++++++++++++++++++++++++
	Select_States. <- c("Oregon")
	#Select states of interest
	Select_Fatal.. <- State_Fatal..[State_Fatal..$State%in%Select_States.,]
	#Select measures of interest
	Select_Fatal.. <- filter(Select_Fatal.., Measure == c("All Fatals"))
	#Order measures for graph
	#Select_Fatal..$Measure <- factor(Select_Fatal..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Graph data---
	p <- ggplot(Select_Fatal.., aes(x = Year, y = Count, group  = Measure)) +
		#Add line layer - define color based on measure
		geom_line(color = "red",size = 1.25 , show.legend = F) +
		#Add point layer - define point color based on measure
		geom_point(aes(fill = Measure), color = "black", size = 4, show.legend = F) +
		#Use facet command to create separate panels for each measure
		#facet_grid(~Measure) +
		#Rotate label names for x-axis
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		scale_y_continuous(breaks = seq(0,775,50),  labels = seq(0,775,50)) +
		#Customize y axis label
		ylab("Annual Traffic Fatal Injuries")  +  
		#Add title
		ggtitle("Historic Traffic Fatals in Oregon") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90,vjust = .5)) 
		
	#Save
	File_Location <- "Z:/JRoll/Workshops/TC Data Science Workshop 2018/Crash Exercise/Graphics/Oregon_Traffic_Fatals_1975_2016.jpeg"
	jpeg(file = File_Location, width = 640, height = 360, quality = 100)
	p 
	dev.off()
	
	
	


	#Summarize VMT Data for the select state and compare to national 
	#++++++++++++++++++++++++++++++++
	#Create a vector of state to examine
	Select_States. <- c("Oregon")
	
	#Prepare national Fatal Data---
	#Select measure of choice
	Select_Fatal.. <- filter(State_Fatal.., Measure == c("All Fatals"))
	#Create national fatal injury data frame total by year
	National_Fatal.. <- data.frame(Count = tapply(Select_Fatal..$Count, Select_Fatal..$Year, sum),	
		Year = names(tapply(Select_Fatal..$Count, Select_Fatal..$Year, sum)))
	National_Fatal..$State <- "US Average"
	
	#Prepare national VMT Data---
	National_Vmt.. <- Load_Vmt..[Load_Vmt..$State%in%"United States",]
	#Append US Total VMT to National Fatal Injury
	National_Fatal..$Vmt_Total <- National_Vmt..$Total[match(National_Fatal..$Year, National_Vmt..$Year)] * 1e6
	#Calculate rate per 100 million VMT
	National_Fatal..$Rate <- National_Fatal..$Count *1e8 / National_Fatal..$Vmt_Total
	
	#Prepare select state data---
	#Append VMT to state data
	Select_Fatal..$Vmt_Total <- Load_Vmt..$Total[match(paste(Select_Fatal..$State, Select_Fatal..$Year), paste(Load_Vmt..$State, Load_Vmt..$Year))] * 1e6
	#Calculate fatal injury rate per 100 million VMT
	Select_Fatal..$Rate <- Select_Fatal..$Count *1e8 / Select_Fatal..$Vmt_Total
	#Define and Select states fatal injury data of interest---
	Select_Fatal.. <- Select_Fatal..[Select_Fatal..$State%in%Select_States.,]
	
	
	#Graph data---
	#Combine select and national data
	dat <- rbind(Select_Fatal..[,colnames(National_Fatal..)], National_Fatal..)
	#Look at years with full data
	dat <- dat[dat$Year%in%1980:2016,]
	
	ggplot(dat, aes(x = Year, y = Rate, group = State)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = State),,size = 1.25) +
		#Add point layer - define point color based on measure
		geom_point(color = "black", size = 2) +
		#Use facet command to create separate panels for each measure
		#facet_grid(~State, scales = "free") +
		#Rotate label names for x-axis
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1980,2016,5),  labels = seq(1980,2016,5)) +
		scale_y_continuous(breaks = seq(0,4,.25),  labels = seq(0,4,.25)) +
		#Customize y axis label
		ylab("Annual Traffic Fatal Injury Rate \n(per 100 million VMT)")  +  
		#Add title
		ggtitle("Historic Traffic Fatal Injury Rate Comparison") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90,vjust = .5))  +
		#Add legend to bottom of graphic
		theme(legend.position="bottom")
	
	
	#Calculate the change in frequency of fatal compared to the change in fatal rate
	#+++++++++++++++++++++++++++++++++++++++++++++++
	#Make a copy of the data
	Data.. <- dat
	#Calculate the rolling averages of fatal injury counts and rates 
	#Fatal counts---
	Data.. <- Data.. %>% group_by(State) %>% mutate(Fatal_Count_Mean = runMean(Count,n = 3))
	#Fatal rates---
	Data.. <- Data.. %>% group_by(State) %>% mutate(Fatal_Rate_Mean = runMean(Rate,n = 3))
	
	#Select first and last years (1982 1st available year for beginning period)
	Data.. <- Data..[Data..$Year%in%c("1982","2016"),]
	Data.. <- Data.. %>% group_by(State) %>% mutate(Count_Change = 1 - (Fatal_Count_Mean[1] / Fatal_Count_Mean[2]))
	Data.. <- Data.. %>% group_by(State) %>% mutate(Rate_Change = 1 - (Fatal_Rate_Mean[1] / Fatal_Rate_Mean[2]))
	
	aes(x = Year, y = Rate, group = State)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = State),,size = 1.25) +
		#Add point layer - define point color based on measure
		geom_point(color = "black", size = 2) +
		#Use facet command to create separate panels for each measure
		#facet_grid(~State, scales = "free") +
		#Rotate label names for x-axis
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1980,2016,5),  labels = seq(1980,2016,5)) +
		scale_y_continuous(breaks = seq(0,4,.25),  labels = seq(0,4,.25)) +
		#Customize y axis label
		ylab("Annual Traffic Fatal Injury Rate \n(per 100 million VMT)")  +  
		#Add title
		ggtitle("Historic Traffic Fatal Injury Rate Comparison") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90,vjust = .5))  +
		#Add legend to bottom of graphic
		theme(legend.position="bottom")
	
	