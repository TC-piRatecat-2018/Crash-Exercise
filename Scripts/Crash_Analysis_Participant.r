#Author: Josh Roll 
#Date: 9/1/2018
#Agency: Oregon Department of Transportation
#Purpose:Transportation and Communities Workshop - R for Transportation Data Science: Application and Best Practices 



#Install & Load libraries
#--------------------------
	#For new R users the packages below likely need installation
	install.packages(c("reshape2",	"ggplot2",	"scales",	"tidycensus",	"acs",	"tictoc",	"dplyr",	"httr",	"ggmap",
	"broom",	"tigris",	"rgeos",	"leaflet",	"htmlwidgets",	"htmltools",	"rgdal",	"RColorBrewer",	"maptools"))
	
	#Load libraries
	library(reshape2)
	library(ggplot2)
	library(scales)
	library(tidycensus)
	library(acs)
	library(tictoc)
	library(dplyr)
	library(httr)
	library(ggmap)
	library(broom)
	library(tigris)
	library(rgeos)
	library(leaflet)
	library(htmlwidgets)
	library(htmltools)
	library(rgdal)
	library(RColorBrewer)
	library(maptools)
	
#Set script parameters
#--------------------------
	#Set working directory
	#choose.dir()
	#setwd("Z:/JRoll/Workshops/TC Data Science Workshop 2018/Crash Exercise/")
	#Set cache 
	options(tigris_use_cache = TRUE)
	#Configure firewall settings - important for security issues such that ODOT IS presents
	#set_config(use_proxy(url="proxynew.odot.state.or.us", port=8080)) 
	#	set_config( config( ssl_verifypeer = 0L ) )
		
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
	#Function to create bin labels
	label_Bins <- function(Breaks.){
		Labels_ <- list()
		for(i in 1:(length(Breaks.)-1)){
			Labels_[[i]] <- paste(Breaks.[[i]], Breaks.[[i + 1]], sep="-")
		}
		#Return result
		unlist(Labels_)
	}
			
#Create working folders (Be sure to have write access to any selected directories)
#------------------
	#Choose working directory
	#setwd(choose.dir())
	
#Define work space variables and conditions
#--------------------------
	#Set Census API Key (Get Census Key here ) https://api.census.gov/data/key_signup.html
	Developer_Key <- ""
	#Initialize Census Key
	#api.key.install(Developer_Key)
	census_api_key(Developer_Key)
	
	#Load ODOT Crash data file (2002-2016)
	#---------------------------
	Load_Crash.. <- assignLoad(paste(getwd(),"/Data/ODOT_Crash_02_16.RData",sep=""))
	
	#Explore data---
	#Data structure
	str(Load_Crash..)
	
	#number of rows
	nrow(Load_Crash..)
	#Number of columns
	ncol(Load_Crash..)
	
	#Look at years of data 
	unique(Load_Crash..$Crash_yr_no)
	
	#Look at top rows 
	head(Load_Crash..)
	
		
#------------------------------------------------------------------
#Unit 1 - Statewide Evaluation
#------------------------------------------------------------------
	###########################################################################################################
	#Exercise 1 - Chart the fatal injuries in Oregon for the years 2002-20016
	###########################################################################################################
	#Check data dictionary for crash data columns of interest
	#https://www.oregon.gov/ODOT/Data/documents/CDS_Code_Manual.pdf - pg. 208
		
	#Do simply summary of one injury severity type using just tapply()
	tapply(Load_Crash..$Tot_fatal_cnt, Load_Crash..$Crash_yr_no, sum)
	
	#Quick validation check 
	#https://www.oregon.gov/ODOT/Data/Documents/Crashes_County.pdf
	
	#Define metrics to summarize
	Metrics. <- c("Tot_fatal_cnt")
				
	#Summarize each injury severity for each year
	State_Summary.. <- melt(sapply(Metrics., function(x){tapply(Load_Crash..[,x],Load_Crash..$Crash_yr_no, sum)}),varnames = c("Year","Injury_Severity"), value.name = "Count")
	
	#Take a look at the summary data---
	#Look at new dataset
	head(State_Summary..)
	#Look at structure of data
	str(State_Summary..)
	#Number of rows
	nrow(State_Summary..)
		
	#Plot all Fatal injuries from ODOT data - 2002 - 2016
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
		ggtitle("Fatal Traffic Injuries in \nOregon") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,1),  labels = seq(2002,2016,1)) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 

			
	###########################################################################################################
	#Exercise 2 - Chart the fatal, severe, and total injuries in Oregon for the years 2002-20016
	###########################################################################################################
	#Define metrics to summarize
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
	#Summarize each injury severity for each year
	State_Summary.. <- melt(sapply(Metrics., function(x){tapply(Load_Crash..[,x],Load_Crash..$Crash_yr_no, sum)}),varnames = c("Year","Injury_Severity"), value.name = "Count")
	
	#Take a look at the summary data---
	#Look at new dataset
	head(State_Summary..)
	#Look at structure of data
	str(State_Summary..)
	#Number of rows
	nrow(State_Summary..)	
	
	#Plot all Fatal injuries from ODOT data - 2002 - 2016
	#++++++++++++++++++++++++
	#Make a copy
	dat <- State_Summary..
	#Convert injury severity codes to more meaningful terms
	#Convert to character 
	dat$Injury_Severity <- as.character(dat$Injury_Severity)
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_fatal_cnt"] <- "Fatal Injuries (K)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_lvl_a_cnt"] <- "Severe Injuries (A)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_cnt"] <- "All Injuries (ABC)"
	#As character class, 'Injury_Severity' wont order properly in Facet so convert to factor
	dat$Injury_Severity <- factor(dat$Injury_Severity,  levels = c("Fatal Injuries (K)", "Severe Injuries (A)", "All Injuries (ABC)"))
	#Initialize ggplot
	Plot <- ggplot(dat, aes(x = Year, y = Count)) + 
		#Add line layer
		geom_line(aes(color = Injury_Severity), size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 4) + 
		#Add facet command for separate panels for each injury severity
		facet_wrap(~Injury_Severity, scales = "free", nrow = 3) +
		#Add custom y-axis label
		ylab("Annual Traffic Injuries")  +  
		#Add title
		ggtitle("Traffic Injuries in \nOregon\n2002-2016") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,3),  labels = seq(2002,2016,3)) +
		#Add custom y axes ticks
		scale_y_continuous(labels = comma) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 
	#Call Plot since its store as object 'Plot'
	Plot
	#Store as .pdf
	#Open pdf file
	pdf(paste(getwd(),"/Reports/Oregon_Traffic_Injury_Summary_2002_2016.pdf",sep=""), width =11, height = 8.5)
	#Call chart 
	Plot
	#Close pdf 
	dev.off()
	
	###########################################################################################################
	#Exercise 3 - Select a specific county and complete the same graphic
	###########################################################################################################
	#Define metrics to summarize
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
	
	#Look at county column name
	colnames(Load_Crash..)
	#Explore county naming conventions
	names(table(Load_Crash..$Cnty_nm))
	
	#Create a temporary data frame of just selected county data
	Select_County <- "Lane"	
	Select_County.. <- Load_Crash..[Load_Crash..$Cnty_nm%in%Select_County,]
	
	#Take a look at the summarize data---
	#Look at new dataset
	head(Select_County..)
	#Number of rows
	nrow(Select_County..)
	
	#Summarize each injury severity for each year
	County_Summary.. <- melt(sapply(Metrics., function(x){tapply(Select_County..[,x],Select_County..$Crash_yr_no, sum)}),varnames = c("Year","Injury_Severity"), value.name = "Count")
	
	#Take a look at the summarize data---
	#Look at new dataset
	head(County_Summary..)
	#Look at structure of data
	str(County_Summary..)
	#Number of rows
	nrow(County_Summary..)
		
	#Plot all Fatal injuries from ODOT data - 2002 - 2016
	#++++++++++++++++++++++++
	dat <- County_Summary..[County_Summary..$Injury_Severity%in%"Tot_fatal_cnt",]
	#Initialize ggplot
	ggplot(dat, aes(x = Year, y = Count)) + 
		#Add line layer
		geom_line(color = "red", size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 4) + 
		#Add custom y-axis label
		ylab("Annual Traffic Fatal Injuries")  +  
		#Add title
		ggtitle(paste("Fatal Traffic Injuries in \n",Select_County," County",sep="")) +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,1),  labels = seq(2002,2016,1)) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) 
	
	
	###########################################################################################################
	#Exercise 4 - Add state summary, index data to base year, and compare with selected county
	###########################################################################################################
	#Combine state and select county data---
	#Add a variable for keeping measures separate in graphic
	State_Summary..$Measure <- "State of Oregon"
	County_Summary..$Measure <- Select_County
	Combined_Summary.. <- rbind(State_Summary.., County_Summary..)
	#Create data frame of values of first year of data set to index traffic injury counts
	Base_Year.. <- data.frame(Injury_Severity = Combined_Summary..$Injury_Severity[Combined_Summary..$Year%in%"2002"], Count = Combined_Summary..$Count[Combined_Summary..$Year%in%"2002"], 
		Measure = Combined_Summary..$Measure[Combined_Summary..$Year%in%"2002"])
	#Append the base year measure to combined data set 
	Combined_Summary..$Base_Year_Count <- Base_Year..$Count[match(paste(Combined_Summary..$Measure, Combined_Summary..$Injury_Severity), 
		paste(Base_Year..$Measure, Base_Year..$Injury_Severity))]
	#Calculate difference from base year
	Combined_Summary..$Base_Diff <- 1 - round(Combined_Summary..$Base_Year_Count / Combined_Summary..$Count, 3) 
	#Take a peek at data
	Combined_Summary..
	
	#Plot change from base year for select county and state as whole
	#++++++++++++++++++++++++
	#Make a copy
	dat <- Combined_Summary..
	dat$Injury_Severity <- as.character(dat$Injury_Severity)
	#Change ODOT injury codes to more presentable form
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_fatal_cnt"] <- "Fatal Injuries (K)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_lvl_a_cnt"] <- "Severe Injuries (A)"
	dat$Injury_Severity[dat$Injury_Severity%in%"Tot_inj_cnt"] <- "All Injuries (ABC)"
	#As character class, 'Injury_Severity' wont order properly in Facet so convert to factor
	dat$Injury_Severity <- factor(dat$Injury_Severity,  levels = c("Fatal Injuries (K)", "Severe Injuries (A)", "All Injuries (ABC)"))
	#Initialize ggplot
	ggplot(dat, aes(x = Year, y = Base_Diff)) + 
		#Add line layer
		geom_line(aes(color = Measure), size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 4) + 
		#Add facet 
		facet_wrap(~Injury_Severity, scales = "free", nrow = 3) +
		#Add custom y-axis label
		ylab("Change in Traffic Injuries\n (Base Year = 2002)")  +  
		#Add title
		ggtitle("Change in Traffic Injuries") +
		#Center title
		theme(plot.title = element_text(hjust = 0.5)) + 
		#Add custom x axes ticks
		scale_x_continuous(breaks = seq(2002,2016,2),  labels = seq(2002,2016,2)) +
		#Add commas 
		scale_y_continuous(labels = percent) +
		#Increase size of text
		theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=.5)) +
		  
		
#-------------------------------------------------------
#Unit 2 - In-depth County Evaluation
#-------------------------------------------------------
	###########################################################################################################
	#Exercise 5 - Chart all county data 
	###########################################################################################################
	#Define metrics to summarize
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
			
	#Make a copy
	Temp.. <- Load_Crash..
	#By year and county 
	TempList_ <- list()
	Metrics. <- c("Tot_fatal_cnt", "Tot_inj_lvl_a_cnt","Tot_inj_cnt")
	for(metric in Metrics.){
		TempList_[[metric]] <- tapply(Temp..[,metric],list(Temp..$Crash_yr_no, Temp..$Cnty_nm), sum,na.rm=T) 
	}
	TempList_FcSv <- melt(TempList_, varnames = c("Year","County"), value.name = "Count")
	#Name column 4 
	colnames(TempList_FcSv )[4] <- "Injury_Severity"
	#MAke a copy with more descriptive name
	County_Summary.. <- TempList_FcSv
	#Convert county to character from factor
	County_Summary..$County <- as.character(County_Summary..$County)
	#Rename injury severity to be more descriptive
	County_Summary..$Injury_Severity[County_Summary..$Injury_Severity%in%"Tot_fatal_cnt"] <- "Fatal Injuries (K)"
	County_Summary..$Injury_Severity[County_Summary..$Injury_Severity%in%"Tot_inj_lvl_a_cnt"] <- "Severe Injuries (A)"
	County_Summary..$Injury_Severity[County_Summary..$Injury_Severity%in%"Tot_inj_cnt"] <- "All Injuries (ABC)"
	
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
	#As character class, 'Injury_Severity' wont order properly in Facet so convert to factor
	dat$Injury_Severity <- factor(dat$Injury_Severity,  levels = c("Fatal Injuries (K)", "Severe Injuries (A)", "All Injuries (ABC)"))
	#Initialize ggplot
	ggplot(dat, aes(x = Year, y = Count)) + 
		#Add line layer
		geom_line(aes(color = County), size = 1.25) + 
		#Add point layer
		geom_point(color = "black", size = 2) + 
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
		
	#Instructor Notes
	#Point out there are other ways to wrangle - go into the stack overflow
	#https://stackoverflow.com/questions/51618742/maintain-tapply-indices-through-sapply
	#Google: r select certain rows from data frame example
	dat <- filter(dat, Injury_Severity == "All Injuries (ABC)")
	dat <- filter(dat, County == c("Multnomah","Washington","Clackamas"))
	#Maybe take a question from the class and try and find the answer
	
#-------------------------------------------------------------
#Unit 3 - Spatial analysis and population normalization
#-------------------------------------------------------------
	###########################################################################################################
	#Exercise 6 - Chart fatal and severe injury counts for all Oregon counties for 2010 in a map
	###########################################################################################################
	#Download spatial data for select counties
	County_Spatial <- counties(state = "OR", year = 2010)
	#Having trouble with Census API?  Load the data from file below by uncommenting the next line
	#County_Spatial <- assignLoad( file = "Data/County_Polygons_Data.RData")
	
	#Use the base graphics package to see the polygon shapefile we just 
	plot(County_Spatial, main = "Oregon Counties")
	#Lets take a look at what we downloaded	
	#Look at data structure
	str(County_Spatial)
	#Look at first few rows of data table or what would be attribute table in Arc GIS
	head(County_Spatial@data)
	
	
	#Graphing all the county data was messy so lets visualize the 2010 data in a map
	#############################
	#Convert shape file into data frame using fortify for plotting in ggplot
	County_Spatial_Fort <- fortify(County_Spatial, region = "NAME10")
	#Evaluate class of fortified object
	class(County_Spatial_Fort)
	class(County_Spatial)
		
	#Select 2010 from injury data
	#Use base package approach
	County_Summary_2010.. <- County_Summary..[County_Summary..$Year%in%"2010",]
	#Or use dplyr package approach
	County_Summary_2010.. <- filter(County_Summary.., Year == "2010")
	#Add the fatal and severe injuries into single measure---
	#Create temporary data frame of just fatal and sever injuries
	Temp.. <- County_Summary_2010..[County_Summary_2010..$Injury_Severity%in%c("Fatal Injuries (K)","Severe Injuries (A)"),]
	#Add them by county
	Fatal_Severe_Summary_2010. <- tapply(Temp..$Count, Temp..$County, sum)
	#Add to county summary injury data frame
	County_Summary_2010..$Count <- Fatal_Severe_Summary_2010.[match(County_Summary_2010..$County,names(Fatal_Severe_Summary_2010.))]
	#Select only one injury category
	County_Summary_2010.. <-  County_Summary_2010..[County_Summary_2010..$Injury_Severity%in%"Fatal Injuries (K)",]
	#Change injury Severity name to properly reflect new metric
	County_Summary_2010..$Injury_Severity <- "Fatal & Severe Injury (KA)" 
	
	#Append to spatial data
	County_Spatial_Fort$Injury_Count <- Fatal_Severe_Summary_2010.[match(County_Spatial_Fort$id,names(Fatal_Severe_Summary_2010.))]
	#Create data frame of centroids to use in labeling the map
	Labels.. <- as.data.frame(gCentroid(County_Spatial, byid = T))
	#Add county names
	Labels..$County = unique(County_Spatial$NAME10)
	#Add count of injuries
	Labels..$Count <- County_Summary_2010..$Count[match(Labels..$County, County_Summary_2010..$County)]
	#Create a label (use "\n" to return the value under the county name
	Labels..$Label <- paste(Labels..$County, "\n",Labels..$Count, sep="")
	
	#Map Counts---
	ggplot() +
		#Add county boundary layer
		geom_polygon(data = County_Spatial_Fort, aes(x = long, y = lat, group = id, fill = Injury_Count), color = "black") +
		#Add text labels
		geom_text(data = Labels.., aes(label = Label, x = x, y = y), fontface = "bold") +
		#Remove labels from x and y axes since they are just lat/long
		labs(x="",y="") +
		#Also remove tick marks 
		theme(axis.text=element_blank(),axis.ticks=element_blank()) +
		#Add title 
		ggtitle("Fatal and Severe \nTraffic Injury Count in Oregon\n2010") +
		#Center plot and increase size of title text
		theme(plot.title = element_text(hjust = 0.5, size = 20)) +
		#Add a gradient color scheme
		scale_fill_gradient( low = "green", high = "red")
    
	
	###########################################################################################################
	#Exercise 7 - Chart fatal and severe injury rates for all Oregon counties for 2010 in a map
	###########################################################################################################
	#Lets use 2010 population to categorize the counties by size and visualize
	#Retrieve population data---
	#Use get_decennial() from tidycensus package to retrieve population data
	#The function requires a key which is free and available from US Census. 
	County_Population.. <-  as.data.frame(get_decennial(geography = "county", variables = "P0030001", year = 2010, state = "Oregon",key = Developer_Key))
	#Having trouble with Census API?  Load the data from file below by uncommenting the next line
	#County_Population.. <- assignLoad(file = "Data/County_Population_Data.RData")
	
	#Take a look at data---
	#Look at structure of data
	str(County_Population..)
	#Number of rows
	nrow(County_Population..)
	#look at first few rows
	head(County_Population..)
		
	#Format data---
	#Clean up county name to make joining with the injury data possible we need to reformat the county GEOID
	County_Population..$County <- as.character(gsub(" County","",County_Population..$NAME))
	#Change 'value' to Population
	County_Population..$Population <- County_Population..$value
	#Now join population data to Traffic Injury data
	County_Summary_2010..$Population <- County_Population..$Population[match(County_Summary_2010..$County, County_Population..$County)]
	
	#Now calculate the population injury rate(Injury Rate per 100,000 People)
	County_Summary_2010..$Pop_Injury_Rate <- as.numeric(round(County_Summary_2010..$Count / (County_Summary_2010..$Population / 100000), 1) )
	#Append to spatial data
	County_Spatial_Fort$Pop_Injury_Rate <- County_Summary_2010..$Pop_Injury_Rate[match(County_Spatial_Fort$id,County_Summary_2010..$County)]
	County_Spatial@data$Pop_Injury_Rate <- County_Summary_2010..$Pop_Injury_Rate[match(County_Spatial@data$NAME10,County_Summary_2010..$County)]
		
	#Map Rates---
	#Create labels 
	Labels..$Pop_Injury_Rate <- County_Summary_2010..$Pop_Injury_Rate[match(Labels..$County, County_Summary_2010..$County)]
	#Create a label (use "/n" to return the value under the county name
	Labels..$Label <- paste(Labels..$County, "\n",Labels..$Pop_Injury_Rate, sep="")
	#Determine breaks
	Breaks. <- quantile(County_Summary_2010..$Pop_Injury_Rate, probs = seq(0, 1, .10))
	#Apply custom function to create bin labels based on Breaks.
	Bin_Labels. <- label_Bins(Breaks.)
	#Add creaks to spatial data frame
	County_Spatial_Fort$Pop_Injury_Rate_Bin <- cut(County_Spatial_Fort$Pop_Injury_Rate, breaks = Breaks., labels = Bin_Labels., right = T, include.lowest = T)
	#Create custom colors for gradient
	Colors. <- seq_gradient_pal("green","red")(seq(0,1,length.out=length(Breaks.)))
	#Initialize plot
	ggplot() +
		#Add county boundary layer
		geom_polygon(data = County_Spatial_Fort, aes(x = long, y = lat, group = id, fill = Pop_Injury_Rate_Bin), color = "black") +
		#Add text labels
		geom_text(data = Labels.., aes(label = Label, x = x, y = y), fontface = "bold") +
		#Remove labels from x and y axes since they are just lat/long
		labs(x="",y="") +
		#Also remove tick marks 
		theme(axis.text=element_blank(),axis.ticks=element_blank()) +
		#Add title 
		ggtitle("Traffic Fatal and Severe \nInjury Rate in Oregon\n2010") +
		#Center plot and increase size of title text
		theme(plot.title = element_text(hjust = 0.5, size = 22)) +
		#Add a gradient color scheme
		scale_fill_manual( values = Colors.) +
		#Lets move legend to bottom of map
		theme(legend.position = "bottom", panel.background = element_rect(fill = NA, colour = "#cccccc")) +
		#Add legend title
		guides(fill=guide_legend(title="Injury Rate Category")) 
    
	
	###########################################################################################################
	#Exercise 8 - Map fatal and severe injury rates for all Oregon counties for 2010 in a dynamic leaflet based map
	###########################################################################################################	
	#Define a pallet of colors for the polygon
	Breaks. <- c(quantile(County_Summary_2010..$Pop_Injury_Rate, probs = seq(0, 1, .10)), 625)
	#Apply custom function to create bin labels based on Breaks.
	Bin_Labels. <- label_Bins(Breaks.)
	#Add population to spatial geography for tool tip (popup)
	County_Spatial$Population <- County_Population..$value[match(County_Spatial$NAME10, County_Population..$County)]
	#Add KA injury count to spatial geography for tool tip (popup)
	County_Spatial@data$Injury_Count <- County_Summary_2010..$Count[match(County_Spatial@data$NAME10,County_Summary_2010..$County)]
	#Create custom colors
	Colors. <- seq_gradient_pal("green","red")(seq(0,1,length.out=length(Breaks.)))
	names(Colors.) <- Bin_Labels.
	#Create a color palette for visualizing rates
	Poly_Pal <-  colorBin(colorRamp(c("green","yellow", "red")), domain = County_Spatial$Pop_Injury_Rate, bins = Breaks.)
	
	#Set up html title 
	tag.map.title <- tags$style(HTML("
		.leaflet-control.map-title { 
		transform: translate(-50%,20%);
		position: fixed !important;
		left: 50%;
		text-align: center;
		padding-left: 10px; 
		padding-right: 10px; 
		background: rgba(255,255,255,0.75);
		font-weight: bold;
		font-size: 28px;
	}"))
	title <- tags$div(
		tag.map.title, HTML("Traffic Fatal & Severe Injury Rates in Oregon")
	)  
	#Initialize leaflet map
	Map <- leaflet(County_Spatial) %>%
		#Add polygons 
		addPolygons(data = County_Spatial, color = "black", fillColor = ~Poly_Pal(Pop_Injury_Rate),fillOpacity = 0.7,
            stroke = TRUE, smoothFactor = .1,
			#Define pop-ups of polygon
			popup = paste(County_Spatial$NAME10, " County","<br>",
            "Fatal & Severe Injury Count: ",County_Spatial@data$Injury_Count, "<br>",
			"County Population: ",County_Spatial$Population, "<br>",
			"Injury Rate per 100,000: ",County_Spatial@data$Pop_Injury_Rate, "<br>"))  %>%
		#Add Open Streets Map layer
		addTiles %>%
		#Add legend
		addLegend(position = "bottomleft", pal = Poly_Pal, values = County_Spatial$Pop_Injury_Rate,
			labels = Bin_Labels., title = "Rate per 100,000 People",opacity = 1) %>%
		 #Add title
		 addControl(title, position = "topleft", className="map-title")
	#Call map in browser
	Map  
	 
	#Save this leafelt app
	saveWidget(Map, file=paste(getwd(),"/Reports/Population_Injury_Rates_Oregon_2010.html",sep=""))
		
	##########################################################################################################
	#Exercise 9 - Show crashes on a dynamic map
	###########################################################################################################	
	#Look at specific county and crash type
	Select_Crash.. <- Load_Crash..[Load_Crash..$Cnty_nm%in%"Lane" & (Load_Crash..$Tot_ped_inj_cnt > 0 | Load_Crash..$Tot_pedcycl_inj_cnt > 0),]
	#Spatial data only available for 2007 - onward
	Select_Crash..  <- Select_Crash..[Select_Crash..$Crash_yr_no%in%as.character(2007:2016),]
	#Add flags for ped crash
	Select_Crash..$Injury_Type[Select_Crash..$Tot_pedcycl_inj_cnt >0] <- "Bicycle Injury"
	Select_Crash..$Injury_Type[Select_Crash..$Tot_ped_inj_cnt >0] <- "Pedestrian Injury"
	Select_Crash..$Injury_Type[Select_Crash..$Tot_ped_inj_cnt >0 & Select_Crash..$Tot_pedcycl_inj_cnt > 0] <- "Bicycle & Pedestrian Injury"
	#Look at number of crash records
	nrow(Select_Crash..)
	#Look at number of each crash types
	table(Select_Crash..$Crash_Type)
	
	#Convert the crash file into a spatial data frame----
	#Define projection based on county spatial data
	Projection <-  proj4string(County_Spatial)
	#Create a data frame of just the latitude and longitude information
	Coords.. <- Select_Crash..[,c("Lat_dd","Longtd_dd")]
	#Use coordinates data.frame to convert to x and y for mapping in leaflet
	Crash_Spatial <- SpatialPointsDataFrame(coords = Coords.., data = Coords..,
                               proj4string = CRS(Projection))
	#Add attribution form crash file---
	#Crash ID - in case additional inspection is desired
	Crash_Spatial$Crash_Id <- Select_Crash..$Crash_id
	#Ped Injury Count
	Crash_Spatial$Pedestrian_Injury_Count  <- Select_Crash..$Tot_ped_inj_cnt
	#Bike Injury Count
	Crash_Spatial$Bike_Injury_Count  <- Select_Crash..$Tot_pedcycl_inj_cnt
	#Add Crash Type
	Crash_Spatial$Injury_Type  <- Select_Crash..$Injury_Type
	#Year
	Crash_Spatial$Year <- Select_Crash..$Crash_yr_no
		
	#Define palettes
	Circle_Pal <- colorFactor(palette = c("#009E73", "#e79f00", "#0072B2"), domain = Crash_Spatial$Injury_Type)
	#Define popup
	Circle_Popup <- paste("Crash Id: ",Crash_Spatial$Crash_Id,"<br>",
			"Injury Type: ", Crash_Spatial$Injury_Type,"<br>",
			"Pedestrian Injury Count: ", Crash_Spatial$Pedestrian_Injury_Count,"<br>",
			"Bicycle Injury Count: ", Crash_Spatial$Bike_Injury_Count,"<br>",
			"Year ", Crash_Spatial$Year,"<br>"
            )
	
	#Initialize leaflet map
	leaflet() %>%
		#Add polygons 
		addCircles(data = Crash_Spatial, lng = Crash_Spatial$Longtd_dd, lat = Crash_Spatial$Lat_dd,
			color = ~ Circle_Pal(Injury_Type),radius = 12,fillOpacity = 1.0,
		#Define popup
		popup = Circle_Popup) %>%
		#Add legend
		addLegend(position = "bottomleft", pal = Circle_Pal, values = Crash_Spatial$Injury_Type,
			labels = unique(Crash_Spatial$Injury_Type), title = "Injury Type",opacity = 1) %>%
		#Add Open Streets Map layer
		#Add background map
		addProviderTiles(providers$Esri.WorldStreetMap)		
		
