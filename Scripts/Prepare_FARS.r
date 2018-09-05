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
	library(tictoc)
	library(dplyr)

#Set script parameters
#--------------------------
	#Set working directory
	#choose.dir()
	setwd("F:/Data/FARS/")

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
	#Define functional classification lookup
	#1975-1980 - no data for these years
	Fc_desc_75_80.. <- data.frame(Fc_desc = NA, Fc_code = NA)
	#1981-1986
	Fc_desc_81_86.. <- data.frame(Fc_desc = c("Interstate, principle arterial","Freeway, expressway, principle arterial","Principle arterial, other","Minor arterial","Collector","Collector","Collector",
		"Local","Unknown"),
		Fc_code = 1:9)
	#1987-2014
	Fc_desc_87_14.. <- data.frame(Fc_desc = c("Interstate, principle arterial","Interstate, principle arterial","Freeway, expressway, principle arterial","Principle arterial, other",
		"Principle arterial, other","Minor arterial","Minor arterial","Collector","Collector","Collector","Local","Local","Unknown","Unknown","Unknown"),
		Fc_code = c(1,11,12,2,13,3,14,4,5,15,6,16,9,19,99), Area_Type = c(""))
	#2015-current
	Fc_desc_15_Current.. <- data.frame(Fc_desc = c("Interstate, principle arterial","Freeway, expressway, principle arterial","Principle arterial, other","Minor arterial","Collector","Collector",
		"Local","Unknown","Unknown","Unknown"),
		Fc_code = c(1:7,96,98,99))
	#Create vector of codes for cities in CLMPO
	#MpoCodes. <- c("350","660","1960")
	#names(MpoCodes.) <- c("Coburg","Eugene","Springfield")
	#
	#Link to FARS Data documentation
	#https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812447
		
	
#Process data 
#Why are we doing this?--? Markdown
#Each year of data not in same format so we have to pick out data of interest and compile into a single data set
#-------------------------
	#Define data frames to store processed data below
	All_State_Fatal.. <- data.frame()
	#For functional classification data
	All_State_Fc_Desc_Fatal.. <- data.frame()
	#County_Data.. <- data.frame()
	tic()
	#Use for loop to cycle through each year of data
	for(year in Years.){
		#Define flags since they change by year
		#+++++++++++++++++++++++++++++++++++++++
		#Alcohol test---
		#Determine for years 1975 - 1990
		if(year%in%as.character(1975:1990)){
			Alcohol_Flag <- "Test_res"}
		#Determine for years 1991 - 2016	
		if(year%in%as.character(1991:2016)){
			Alcohol_Flag <- "Alc_res";}
		#Pedestrian Flag - The column doesn't change in but the the code does
		if(year%in%as.character(1975:1981)){Ped_Flag <- "3"}
		if(year%in%as.character(1982:2016)){Ped_Flag <- "5"}
		#Bike Flag
		#if(year%in%as.character(1975:1981)){Bike_Flag <- "4"}
		#if(year%in%as.character(1982:2016)){Bike_Flag <- "6"}
		#Functional Classification Flag---
		if(year%in%c(as.character(1975:2014)))Fc_Flag <- "Road_fnc"
		if(year%in%c(as.character(2015:2018)))Fc_Flag <- "Func_sys"		
		
		#Define measures 
		#+++++++++++++++++++++++++++++++++++++++
		#BAC levels - Set a default for years 1975 - 2014
		Bac_Levels. <- c(0,94);
		Drunk_Level <- 80
		#Set BAC and Drunk level for years 2015 - 2016	
		if(year%in%as.character(2015:2016)){
			Bac_Levels. <- c(0,940);
			Drunk_Level <- 800}
		#Define functional classification lookup table (defined above) 
		if(year%in%c(as.character(1975:1980))){Fc_desc.. <- Fc_desc_75_80..}
		if(year%in%c(as.character(1981:1986))){Fc_desc.. <- Fc_desc_81_86..}
		if(year%in%c(as.character(1987:2014))){Fc_desc.. <- Fc_desc_87_14..}
		if(year%in%c(as.character(2015:2018))){Fc_desc.. <- Fc_desc_15_Current..}
				
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
		#Decode functional classifications ---
		Accident..$Fc_desc <- Fc_desc..$Fc_desc[match(Accident..[,Fc_Flag],Fc_desc..$Fc_code)]
				
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
		State_Fatal.. <- data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "All Fatals")
		
		#All fatal injuries by functional classification, and state---
		Temp_State.. <- melt(tapply(Accident..$Fatals, list(Accident..$State_desc, Accident..$Fc_desc),sum), varnames = c("State","Fc_Desc"), value.name = "Count")
		#Only finish formatting and add to state data if fc_desc summary data exists
		if(nrow(Temp_State..) > 0){		
			#Assign a measure 
			Temp_State..$Measure <- "All Fatals"
			#Add year 
			Temp_State..$Year <- year
		}
		#Rename result 
		State_Fc_Desc_Fatal.. <- Temp_State..
		
		#All alcohol involved fatals---
		#Determine Number fatalities alcohol was involved (Use 'police reported drinking involved' and measure greater than 0) ---
		Alc_Inv.. <- Person..[Person..$Drinking%in%1 | (Person..[,Alcohol_Flag] > Bac_Levels.[1] & Person..[,Alcohol_Flag]  <= Bac_Levels.[2]),]
		#Select only fatal injuries (fatal injury == 4)
		Alc_Inv.. <- Alc_Inv..[Alc_Inv..$Inj_sev%in%4,]
		#Sum by state 
		Temp_State. <- tapply(Alc_Inv..$Inj_sev, list(Alc_Inv..$State_desc) , length)
		#Craft data frame and Store with existing 
		State_Fatal.. <- rbind(State_Fatal..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Alcohol Involved Fatals"))
		
		#Pedestrian fatalities---
		#Determine person records that are pedestrians
		Ped_Fatals.. <- Person..[Person..$Per_typ%in%Ped_Flag,]
		#Select only fatal injuries (fatal injury == 4)
		Ped_Fatals.. <- Ped_Fatals..[Ped_Fatals..$Inj_sev%in%4,]
		#Sum by state 
		Temp_State. <- tapply(Ped_Fatals..$Inj_sev, list(Ped_Fatals..$State_desc) , length)
		#Craft data frame and Store
		State_Fatal.. <- rbind(State_Fatal..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Pedestrian Fatals"))
		
		#Pedestrian fatalities with BAC > 0.08 (for the pedestrian)---
		#Determine person records that are pedestrians
		Ped_Fatals.. <- Person..[Person..$Per_typ%in%Ped_Flag,]
		#Select only fatal injuries (fatal injury == 4)
		Ped_Fatals.. <- Ped_Fatals..[Ped_Fatals..$Inj_sev%in%4 & Ped_Fatals..[,Alcohol_Flag,] >= Drunk_Level,]
		#Sum by state 
		Temp_State. <- tapply(Ped_Fatals..$Inj_sev, list(Ped_Fatals..$State_desc) , length)
		#Craft data frame and Store
		State_Fatal.. <- rbind(State_Fatal..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Drunk Pedestrian Fatals"))
		
		#Pedestrian fatalities by functional classification---
		#Determine person records that are pedestrians
		Ped_Fatals.. <- Person..[Person..$Per_typ%in%Ped_Flag,]
		#Select only fatal injuries (fatal injury == 4)
		Ped_Fatals.. <- Ped_Fatals..[Ped_Fatals..$Inj_sev%in%4,]
		#Assign function classification from accident records
		Ped_Fatals..$Fc_desc <- Accident..$Fc_desc[match(Ped_Fatals..$St_case, Accident..$St_case)]
		#Sum by state and function classification
		Temp_State.. <- melt(tapply(Ped_Fatals..$Inj_sev, list(Ped_Fatals..$State_desc, Ped_Fatals..$Fc_desc) , length), varname = c("State","Fc_Desc"),value.name = "Count")
		#Only finish formatting and add to state data if fc_desc summary data exists
		if(nrow(Temp_State..) > 0){
			#Add Measure flag
			Temp_State..$Measure <- "Pedestrian Fatals"
			#Add year 
			Temp_State..$Year <- year
			#Add to state data frame
			State_Fc_Desc_Fatal.. <- rbind(State_Fc_Desc_Fatal..,Temp_State..[,colnames(State_Fc_Desc_Fatal..)])	
		}
		#Process data - make calculations
		#+++++++++++++++++++++++++++++++++++++
		#Calculate the proportion of total fatals that are alcohol involved---
		#Create a vector for numerator
		x <-  State_Fatal..$Count[State_Fatal..$Measure%in%"Alcohol Involved Fatals" ]
		#Order according to defined vector of states to ensure proper calculation
		x <- x[order(factor(names(x), levels=States.))]
		#Create a vector for denominator
		y <-  State_Fatal..$Count[State_Fatal..$Measure%in%"All Fatals"]
		#Order according to defined vector of states to ensure proper calculation
		y <- y[order(factor(names(y), levels=States.))]
		#Calculate proportion		
		Temp_State. <- x / y[names(x)]
		#Craft data frame and Store
		State_Fatal.. <- rbind(State_Fatal..,data.frame("State" = names(Temp_State.), Year = year, "Count" = Temp_State., "Measure" = "Proportion Alcohol Involved Fatals"))
				
		#Calculate proportion of total in each functional classification---
		#Select All fatal measure
		Temp..  <- State_Fc_Desc_Fatal..[State_Fc_Desc_Fatal..$Measure%in%"All Fatals",]
		if(nrow(Temp..) > 0){
			#Append total fatals 
			Temp..$Total_Fatals <- State_Fatal..$Count[State_Fatal..$Measure%in%"Pedestrian Fatals"][match(Temp..$State, State_Fatal..$State[State_Fatal..$Measure%in%"All Fatals"])]
			#Calculate proportion of total that is in each fc desc (call this count so the data frame column names conform to the below
			Temp..$Count <- round(Temp..$Count / Temp..$Total_Fatals,4)
			#Rename the measure
			Temp..$Measure <- "Proportion Total Fatals in Fc_Desc"
			#Append
			State_Fc_Desc_Fatal.. <- rbind(State_Fc_Desc_Fatal.., Temp..[,colnames(State_Fc_Desc_Fatal..)])
		}		
		
		#Calculate proportion of pedestrian fatal total in each functional classification---
		#Select All fatal measure
		Temp..  <- State_Fc_Desc_Fatal..[State_Fc_Desc_Fatal..$Measure%in%"Pedestrian Fatals",]
		if(nrow(Temp..) > 0){
			#Append total fatals 
			Temp..$Total_Fatals <- State_Fatal..$Count[State_Fatal..$Measure%in%"Pedestrian Fatals"][match(Temp..$State, State_Fatal..$State[State_Fatal..$Measure%in%"Pedestrian Fatals"])]
			#Calculate proportion of total that is in each fc desc (call this count so the data frame column names conform to the below
			Temp..$Count <- round(Temp..$Count / Temp..$Total_Fatals,4)
			#Rename the measure
			Temp..$Measure <- "Proportion Pedestrian Fatals in Fc_Desc"
			#Append
			State_Fc_Desc_Fatal.. <- rbind(State_Fc_Desc_Fatal.., Temp..[,colnames(State_Fc_Desc_Fatal..)])
		}		
		
		#Store in master data frame
		#+++++++++++++++++++++++++++++++++++
		#Add year to current State data frame 
		State_Fatal..$Year <- year
		#Store
		All_State_Fatal.. <- rbind(All_State_Fatal.., State_Fatal..)
		All_State_Fc_Desc_Fatal.. <- rbind(All_State_Fc_Desc_Fatal.., State_Fc_Desc_Fatal..)
		
		#Provide progress
		print(year)
	}
	toc()

	#Write data to .RData object
	save(All_State_Data..,file =  paste(getwd(), "/Data/Processed_Data/State_Fatal_Data.RData", sep=""))
	save(All_State_Fc_Desc_Fatal..,file =  paste(getwd(), "/Data/Processed_Data/State_Fatal_Fc_Desc_Data.RData", sep=""))
	#Rename All_State_Data for quicker error fix
	State_Fatal.. <- All_State_Fatal..
	State_Fc_Desc_Fatal.. <- All_State_Fc_Desc_Fatal..
	
#Do some validations checks to make sure our processes are not creating errors
#----------------------------
	#Total US traffic deaths - 2016
	sum(State_Fatal..$Count[State_Fatal..$Year%in%"2016" & State_Fatal..$Measure%in%"All Fatals"])
	#Total ped traffic deaths - 2016
	sum(State_Fatal..$Count[State_Fatal..$Year%in%"2016" & State_Fatal..$Measure%in%"Pedestrian Fatals"])
	#Validation source = https://www.nhtsa.gov/press-releases/usdot-releases-2016-fatal-traffic-crash-data
	
#Exploratory Visualization
#-----------------------
	#Graph Oregon total fatals, alcohol involved fatals, and the proportion
	#+++++++++++++++++++++++++
	Select_States. <- c("Oregon")
	#Select states of interest
	Select_Data.. <- State_Fatal..[State_Fatal..$State%in%Select_States.,]
	
	#Select measures of interest
	Select_Data.. <- Select_Data..[Select_Data..$Measure%in%c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"),]
	Select_Data.. <- mutate(Select_Data.., Measure = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Order measures for graph
	Select_Data..$Measure <- factor(Select_Data..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Graph data---
	ggplot(Select_Data.., aes(x = Year, y = Count, group  = Measure)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = Measure)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = Measure)) +
		#Use facet command to create separate panels for each measure
		#facet_grid(~Measure) +
		#Rotate label names for x-axis
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		scale_y_continuous(breaks = seq(0,775,50),  labels = seq(0,775,50)) +
		#Customize y axis label
		ylab("Annual Fatal Injuries")  +  
		#Add title
		ggtitle("Historic Traffic Fatals in Oregon") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
		
	#Graph select states total fatals, alcohol involved fatals, and the proportion
	#+++++++++++++++++++++++++
	#Define states of interest
	Select_States. <- c("Oregon", "Washington","Idaho")
	#Select states of interest
	Select_Data.. <- State_Fatal..[State_Fatal..$State%in%Select_States.,]
	#Select measures of interest
	Select_Data.. <- Select_Data..[Select_Data..$Measure%in%c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"),]
	#Order measures for graph
	Select_Data..$Measure <- factor(Select_Data..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Graph data---
	ggplot(Select_Data.., aes(x = Year, y = Count, group  = State)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = State)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = State)) +
		#Use facet command to create separate panels for each measure
		facet_grid(~Measure) +
		#Rotate label names for x-axis
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		scale_y_continuous(breaks = seq(0,1175,50),  labels = seq(0,1175,50)) +
		#Customize y axis label
		ylab("Annual Fatal Injuries")  +  
		#Add title
		ggtitle("Historic Traffic Fatals in Pacific Northwest") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
		
	
	#Normalize fatals to base year and compare to national average
	#+++++++++++++++++++++++++
	#Create national level summaries
	National_Data.. <- melt(tapply(State_Fatal..$Count, list(State_Fatal..$Year, State_Fatal..$Measure), sum), varnames = c("Year","Measure"), value.name = "Count")
	#Name State as US 
	National_Data..$State <- "US"
	#Reorder columns
	National_Data.. <- National_Data..[,colnames(State_Fatal..)]
	#Add to All State Data
	State_Fatal.. <- rbind(State_Fatal.., National_Data..)
	
	#Normalize each year and state's count by measure to base year (1975)
	Base_Year_Data.. <- State_Fatal..[ State_Fatal..$Year%in%"1975",]
	#Append to all data
	State_Fatal..$Base_Count <- Base_Year_Data..$Count[match(paste(State_Fatal..$State, State_Fatal..$Measure),
		paste(Base_Year_Data..$State, Base_Year_Data..$Measure))]
	#Calculate percent difference from base year
	State_Fatal..$Count_Change_Base <- (State_Fatal..$Count / State_Fatal..$Base_Count) - 1
	
	#Define states of interest
	Select_States. <- c("Oregon","California")
	#Select states of interest
	Select_Data.. <- State_Fatal..[State_Fatal..$State%in%Select_States.,]
	#Select measures of interest
	Select_Data.. <- Select_Data..[Select_Data..$Measure%in%c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"),]
	#Order measures for graph
	Select_Data..$Measure <- factor(Select_Data..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Graph data---
	ggplot(Select_Data.., aes(x = Year, y = Count_Change_Base, group  = State)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = State)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = State) +
		#Use facet command to create separate panels for each measure
		facet_grid(~Measure, scales = "free") +
		#Rotate label names for x-axis
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		scale_y_continuous(labels = percent) +
		#Add horizontal line
		geom_hline(yintercept=1, linetype="dashed", color = "red") +
		#Customize y axis label
		ylab("Change in Fatal Injuries Relative to 1975")  +  
		#Add title
		ggtitle("Traffic Deaths in the Pacific Northwest\n Change in Traffic Fatals Relative to 1975") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
		
	
		
	#Normalize fatals to base year and compare to national average
	#+++++++++++++++++++++++++
	#Create national level summaries
	National_Data.. <- melt(tapply(State_Fatal..$Count, list(State_Fatal..$Year, State_Fatal..$Measure), sum), varnames = c("Year","Measure"), value.name = "Count")
	#Normalize each year and state's count by measure to base year (1975)
	Base_Year_Data.. <- State_Fatal..[ State_Fatal..$Year%in%"1975",]
	#Append to all data
	State_Fatal..$Base_Count <- Base_Year_Data..$Count[match(paste(State_Fatal..$State, State_Fatal..$Measure),
		paste(Base_Year_Data..$State, Base_Year_Data..$Measure))]
	#Calculate percent difference from base year
	State_Fatal..$Count_Change_Base <- State_Fatal..$Count / State_Fatal..$Base_Count
	
	#Define states of interest
	Select_States. <- c("Oregon", "Washington","Idaho","California")
	#Select states of interest
	Select_Data.. <- State_Fatal..[State_Fatal..$State%in%Select_States.,]
	#Select measures of interest
	Select_Data.. <- Select_Data..[Select_Data..$Measure%in%c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"),]
	#Order measures for graph
	Select_Data..$Measure <- factor(Select_Data..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Graph data---
	ggplot(Select_Data.., aes(x = Year, y = Count_Change_Base, group  = State)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = State)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = State)) +
		#Use facet command to create separate panels for each measure
		facet_grid(State~Measure, scales = "free") +
		#Rotate label names for x-axis
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		scale_y_continuous(breaks = seq(0,2,.25),  labels = seq(0,2,.25)) +
		#Add horizontal line
		geom_hline(yintercept=1, linetype="dashed", color = "red") +
		#Customize y axis label
		ylab("Change in Fatal Injuries Relative to 1975")  +  
		#Add title
		ggtitle("Traffic Deaths in the Pacific Northwest\n Change in Traffic Fatals Relative to 1975") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
		
	#Graph Proportion of pedestrian fatals by function classification
	#+++++++++++++++++++++++++Select_States. <- c("Oregon")
	#Select states of interest
	Select_Data.. <- State_Fc_Desc_Fatal..[State_Fc_Desc_Fatal..$State%in%Select_States.,]
	#Select measures of interest
	Select_Data.. <- Select_Data..[Select_Data..$Measure%in%c("Proportion Pedestrian Fatals in Fc_Desc"),]
		
	#Order measures for graph
	#Select_Data..$Measure <- factor(Select_Data..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Graph data---
	ggplot(Select_Data.., aes(x = Year, y = Count, group  = Fc_Desc)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = Fc_Desc)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = Fc_Desc)) +
		#Use facet command to create separate panels for each measure
		#Add horizontal reference lines
		geom_hline(yintercept=seq(.05,.95,.05), alpha = 0.5, color = "grey") + 
		#Rotate label names for x-axis
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		scale_y_continuous(labels = percent) +
		#Customize y axis label
		ylab("Proportion of Annual Fatal Injuries")  +  
		#Add title
		ggtitle("Proportion of Pedestrian Fatal Injuries by \nFunctional Classification") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
	
		
	
	
	#Graph Proportion of pedestrian fatals by function classification
	#+++++++++++++++++++++++++
	Select_States. <- c("Oregon")
	#Select states of interest
	Select_Data.. <- State_Fc_Desc_Fatal..[State_Fc_Desc_Fatal..$State%in%Select_States.,]
	#Select measures of interest
	Select_Data.. <- Select_Data..[Select_Data..$Measure%in%c("Proportion Total Fatals in Fc_Desc"),]
		
	#Order measures for graph
	#Select_Data..$Measure <- factor(Select_Data..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals"))
	
	#Graph data---
	ggplot(Select_Data.., aes(x = Year, y = Count, group  = Fc_Desc)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = Fc_Desc)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = Fc_Desc)) +
		#Use facet command to create separate panels for each measure
		#facet_grid(~Measure) +
		#Rotate label names for x-axis
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		scale_y_continuous(labels = percent) +
		#Customize y axis label
		ylab("Proportion of Annual Fatal Injuries")  +  
		#Add title
		ggtitle("Proportion of Pedestrian Fatal Injuries by\n Functional Classification") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
	
		
	