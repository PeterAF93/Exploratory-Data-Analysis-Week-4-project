#Exploratory Data Analysis Project

#download files
if(!(file.exists("summarySCC_PM25.rds") && 
     file.exists("Source_Classification_Code.rds"))) { 
  archiveFile <- "exdata_data_NEI_data.zip"
  if(!file.exists(archiveFile)) {
    archiveURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    download.file(url=archiveURL,destfile=archiveFile)
  }  
  unzip(archiveFile) 
}

#reading files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#view the data
head(NEI)
head(SCC)

#load libraries
library(ggplot2)
library(plyr)


#total emissions
totalEmission <- aggregate(Emissions ~ year, NEI, sum)
totalEmission

#base plotting system
barplot(
  (totalEmission$Emissions)/10^6,
  names.arg=totalEmission$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All US Sources"
)

#answer for question 1
#the total emissions from PM2.5 decreased in the United States from 1999 to 2008

#question 2
#total emissions from PM2.5 in the Baltimore City
NEIdataBaltimore<-subset(NEI, fips == "24510")
totalEmissionBaltimore <- aggregate(Emissions ~ year, NEIdataBaltimore, sum)
totalEmissionBaltimore

#base plotting system
barplot(
  (totalEmissionBaltimore$Emissions)/10^6,
  names.arg=totalEmissionBaltimore$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All Baltimore"
)

#answer for question 2
#from 1999 to 2002 decreased then from 2002 to 2005 increased then from 2005 to 2008 decreased again.

#question 3
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City?
#using ggplot2
g<-ggplot(aes(x = year, y = Emissions, fill=type), data=NEIdataBaltimore)
g+geom_bar(stat="identity")+
  facet_grid(.~type)+
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))+
  guides(fill=FALSE)

#answer for the question 3
#"NON-ROAD", "NONPOINT" and "ON-ROAD" decreased but "POINT" show increasing till 2005.0 then it decreasing again in 2007.5

#question 4
#Accross the us,how have emissions from coal combustion-related sources changed from 1999-2008?
SCCcombustion<-grepl(pattern = "comb", SCC$SCCLevelOne, ignore.case = TRUE)
SCCCoal<-grepl(pattern = "coal", SCC$SCCLevelFour, ignore.case = TRUE)
#extract data
SCCCoalCombustionSCC<-SCC[SCCcombustion & SCCCoal,]$SCC
NIECoalCombustionValues<-NEI[NEI$SCC %in% SCCCoalCombustionSCC,]


#Plotting the subsets
g+geom_bar(stat="identity",fill="grey",width=0.75) +
  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))

#answer for question 4
#graph show decreasing with increasing from 2002 to 2005 then back to decreasing


#Question 5
#emissions from motor vehicle sources change in Baltimore from 99-08

SCCvehicle<-grepl(pattern = "vehicle", SCC$EISector, ignore.case = TRUE)
SCCvehicleSCC <- SCC[SCCvehicle,]$SCC

#get rows from the baltimore data
NEIvehicleSSC <- NEI[NEI$SCC %in% SCCvehicleSCC, ]
NEIvehicleBaltimore <- subset(NEIvehicleSSC, fips == "24510")


#plots
g+geom_bar(stat="identity",fill="grey",width=0.75) +
  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))

#answer of question 5
#it decreased from 99-02 then increased from 02-05 then decreased again till 08.


#Question 6
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
#which city has greater changes over time in motor vehicle emissions?

NEIvehicleBalti<-subset(NEIvehicleSSC, fips == "24510")

NEIvehiclela<-subset(NEIvehicleSSC, fips == "06037")

NEIBothCity <- rbind(NEIvehicleBalti, NEIvehiclela)

#Plots
ggplot(NEIBothCity, aes(x=year, y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(.~type) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

#there's an error in data and there's no existance :)
