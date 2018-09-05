
#Load libraries
#--------------------------
#	library(foreign)
	library(reshape2)
	library(ggplot2)
	library(scales)
	library(tidycensus)
	library(tictoc)
	library(dplyr)

#Set working directory
#--------------------------
	#Choose the directory manually through windows explorer
	#choose.dir()
	#Or set is within the code
	setwd("F:/Data/FARS")


#Create functions
#----------------------------	
	#Function that simplifies loading .RData objects
	assignLoad <- function(filename){
       load(filename)
       get(ls()[ls() != "filename"])
    }


#Define work space variables
#--------------------------
	#create years vector
	Years. <- as.character(c(1975:2016))
	#Create a vector of state names - add comment
	States. <- unique(fips_codes$state_name)[c(1:51,55)]
	
#Load Data
#----------------------
	#Load processed and formatted FARS data
	All_Fatal_Data.. <-  assignLoad(paste(getwd(),"/Data/Processed_Data/States_All_Ped_AlcInv.RData",sep=""))
	
#Exploratory Visualization
#-----------------------
	#Graph Oregon total fatals, alcohol involved fatals, and the proportion
	#+++++++++++++++++++++++++
	Select_States. <- c("Oregon")
	#Select states of interest
	Select_Fatal.. <- All_Fatal_Data..[All_Fatal_Data..$State%in%Select_States.,]
	#Select measures of interest
	Select_Fatal.. <- Select_Fatal..[Select_Fatal..$Measure%in%c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals","Drunk Pedestrian Fatals"),]
	#Order measures using factor() function for graph facets
	Select_Fatal..$Measure <- factor(Select_Fatal..$Measure, levels = c("All Fatals","Alcohol Involved Fatals","Pedestrian Fatals","Drunk Pedestrian Fatals"))
	
	#Graph data---
	ggplot(Select_Fatal.., aes(x = Year, y = Count, group  = Measure)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = Measure)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = Measure)) +
		#Use facet command to create separate panels for each measure
		facet_wrap(~Measure, nrow = 2, scales = "free") +
		#Rotate label names for x-axis
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		#scale_y_continuous(breaks = seq(0,775,50),  labels = seq(0,775,50)) +
		#Customize y axis label
		ylab("Annual Fatal Injuries")  +  
		#Add title
		ggtitle("Historic Traffic Fatals in Oregon") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
		
	#Graph select states total fatals, alcohol involved fatals, and the proportion
	#+++++++++++++++++++++++++
	#Define states of interest
	Select_States. <- c("Oregon","Washington","Idaho")
	#Select states of interest
	Select_Fatal.. <- All_Fatal_Data..[All_Fatal_Data..$State%in%Select_States.,]
	#Select measures of interest
	Select_Fatal.. <- Select_Fatal..[Select_Fatal..$Measure%in%c("All Fatals","Alcohol Involved Fatals"),]
	#Order measures using factor() function for graph facets
	Select_Fatal..$Measure <- factor(Select_Fatal..$Measure, levels = c("All Fatals","Alcohol Involved Fatals"))
	
	#Graph data---
	ggplot(Select_Fatal.., aes(x = Year, y = Count, group  = State)) +
		#Add line layer - define color based on measure
		geom_line(aes(color = State)) +
		#Add point layer - define point color based on measure
		geom_point(aes(color = State)) +
		#Use facet command to create separate panels for each measure
		facet_wrap(~Measure, nrow = 3, scales = "free") +
		#Add custom tick marks
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		#Customize y axis label
		ylab("Annual Fatal Injuries")  +  
		#Add title
		ggtitle("Historic Traffic Fatals in Pacific Northwest States") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5))
		
	
	#Normalize fatals to base year and compare to national average
	#+++++++++++++++++++++++++
	#Create national level summaries
	National_Data.. <- melt(tapply(All_Fatal_Data..$Count, list(All_Fatal_Data..$Year, All_Fatal_Data..$Measure), sum), varnames = c("Year","Measure"), value.name = "Count")
	#Name State as US 
	National_Data..$State <- "US"
	#Reorder columns
	National_Data.. <- National_Data..[,colnames(All_Fatal_Data..)]
	#Add to All State Data
	All_Fatal_Data.. <- rbind(All_Fatal_Data.., National_Data..)
	
	#Normalize each year and state's count by measure to base year (1975)
	Base_Year_Data.. <- All_Fatal_Data..[ All_Fatal_Data..$Year%in%c("1975","1976","1977"),]
	Base_Year_Data.. <- melt(tapply(Base_Year_Data..$Count, list(Base_Year_Data..$State, Base_Year_Data..$Measure), mean),varnames = c("State","Measure"), value.name = "Count")
		
	#Append to all data
	All_Fatal_Data..$Base_Count <- Base_Year_Data..$Count[match(paste(All_Fatal_Data..$State, All_Fatal_Data..$Measure),
		paste(Base_Year_Data..$State, Base_Year_Data..$Measure))]
	#Calculate percent difference from base year
	All_Fatal_Data..$Count_Change_Base <- (All_Fatal_Data..$Count / All_Fatal_Data..$Base_Count) - 1
	
	#Define states of interest
	Select_States. <- c("Oregon","Washington","Idaho")
	#Select states of interest
	Select_Data.. <- All_Fatal_Data..[All_Fatal_Data..$State%in%Select_States.,]
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
		facet_wrap(~Measure, nrow = 3, scales = "free") +
		#Create fewer ticks and labels
		scale_x_discrete(breaks = seq(1975,2016,5),  labels = seq(1975,2016,5)) +
		#Change labels to percentages
		scale_y_continuous(labels = percent) +
		#Customize y axis label
		ylab("Annual Fatal Injuries")  +  
		#Add title
		ggtitle("Change in Fatals Traffic Injuries in Pacific Northwest States Relative to 1975") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) +
		#Add horizontal line to better indicate 0% (no change)
		geom_hline(yintercept = 0, color = "red", linetype = "dashed")
		
	#Normalize fatals to base year and compare to national average
	#+++++++++++++++++++++++++
	#Create national level summaries
	National_Data.. <- melt(tapply(State_Data..$Count, list(State_Data..$Year, State_Data..$Measure), sum), varnames = c("Year","Measure"), value.name = "Count")
	#Normalize each year and state's count by measure to base year (1975)
	Base_Year_Data.. <- State_Data..[ State_Data..$Year%in%"1975",]
	#Append to all data
	State_Data..$Base_Count <- Base_Year_Data..$Count[match(paste(State_Data..$State, State_Data..$Measure),
		paste(Base_Year_Data..$State, Base_Year_Data..$Measure))]
	#Calculate percent difference from base year
	State_Data..$Count_Change_Base <- State_Data..$Count / State_Data..$Base_Count
	
	#Define states of interest
	Select_States. <- c("Oregon", "Washington","Idaho","California")
	#Select states of interest
	Select_Data.. <- State_Data..[State_Data..$State%in%Select_States.,]
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
		
	
		
	