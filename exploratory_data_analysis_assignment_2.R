# Download file at https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip,
# unzip and assign data to variables.
zipfile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(zipfile, basename(zipfile))
unzip(basename(zipfile))
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.
yearly_data<- setNames(aggregate(NEI$Emissions, list(NEI$year), sum), c("year", "emissions"))
png("plot1.png")
barplot(yearly_data$emissions, names.arg = yearly_data$year, main = "US Yearly PM2.5 Emissions", xlab = "year", ylab = "PM2.5 Emissions", beside = TRUE)
dev.off()
# The total emissions from PM2.5 did decrease in the United States from 1999 to 2008.

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.
maryland_data <- subset(NEI, fips == "24510")
yearly_maryland_data<- setNames(aggregate(maryland_data$Emissions, list(maryland_data$year), sum), c("year", "emissions"))
png("plot2.png")
barplot(yearly_maryland_data$emissions, names.arg = yearly_maryland_data$year, main = "Baltimore City, MD Yearly PM2.5 Emissions", xlab = "year", ylab = "PM2.5 Emissions", beside = TRUE)
dev.off()
# In Maryland, the total emissions from PM2.5 did decrease from 1999 to 2008 but no consistently.

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
baltimore_data <- subset(NEI, fips == "24510")
yearly_baltimore <- setNames(aggregate(baltimore_data$Emissions, list(baltimore_data$year, baltimore_data$type), sum), c("year", "type", "emissions"))
png("plot3.png", width = 1000, height = 400)
qplot(year, emissions, data = yearly_baltimore, facets = .~type, color = type, main = "Baltimore City, MD PM2.5 emissions by source types")
dev.off()
# In Baltimore City, the non-road, non-point and on-road type emissions have decreased but
# the point type emissions have increased.

# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
unique_EI_Sector <- unique(SCC$EI.Sector)
comb_sector <- grep('[cC][oO][aA][lL]', unique_EI_Sector, value = TRUE)
comb_coal_sector <- grep('[cC][oO][mM][bB]', comb_sector, value = TRUE)
comb_coal_sector_data <- SCC[SCC$EI.Sector %in% comb_coal_sector,]
comb_coal_emissions <- merge(comb_coal_sector_data, NEI, by.x = "SCC")
comb_coal_emissions <- comb_coal_emissions[, c("SCC", "Short.Name", "EI.Sector", "fips", "Emissions", "year")]
yearly_comb_coal_emissions <- setNames(aggregate(comb_coal_emissions$Emissions, list(comb_coal_emissions$year), sum), c("year", "emissions"))
png("plot4.png")
barplot(yearly_comb_coal_emissions$emissions, names.arg = yearly_comb_coal_emissions$year, main = "US Coal combustion-related emissions", xlab = "year", ylab = "Coal combustion-related emissions", beside=TRUE)
dev.off()
# US Coal combustion-related sources have changed from 1999 to 2008.

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
unique_EI_Sector <- unique(SCC$EI.Sector)
mobile_sector <- grep('[mM][oO][bB][iI][lL][eE]', unique_EI_Sector, value = TRUE)
mobile_vehicle_sector <- grep('[vV][eE][hH][iI][cC][lL][eE]', mobile_sector, value = TRUE)
mobile_vehicle_data <- SCC[SCC$EI.Sector %in% mobile_vehicle_sector,]
baltimore_emissions <- subset(NEI, fips == "24510")
baltimore_vehicle_emissions <- merge(mobile_vehicle_data, baltimore_emissions, by.x = "SCC")
yearly_baltimore_vehicle_emissions <- setNames(aggregate(baltimore_vehicle_emissions$Emissions, list(baltimore_vehicle_emissions$year), sum), c("year", "emissions"))
png("plot5.png")
barplot(yearly_baltimore_vehicle_emissions$emissions, names.arg = yearly_baltimore_vehicle_emissions$year, main = "Baltimore City, MD motor vehicles emissions", xlab = "year", ylab = "Motor vehicles emissions", beside=TRUE)
dev.off()
# In Baltimore City, MD, emissions from motor vehicle sources have changed from 1999 to 2008.

# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has 
# seen greater changes over time in motor vehicle emissions?
unique_EI_Sector <- unique(SCC$EI.Sector)
mobile_sector <- grep('[mM][oO][bB][iI][lL][eE]', unique_EI_Sector, value = TRUE)
mobile_vehicle_sector <- grep('[vV][eE][hH][iI][cC][lL][eE]', mobile_sector, value = TRUE)
mobile_vehicle_data <- SCC[SCC$EI.Sector %in% mobile_vehicle_sector,]
bc_la_emissions <- subset(NEI, fips == "24510" | fips == "06037")
bc_la_vehicle_emissions <- merge(mobile_vehicle_data, bc_la_emissions, by.x = "SCC")
yearly_bc_la_vehicle_emissions <- setNames(aggregate(bc_la_vehicle_emissions$Emissions, list(bc_la_vehicle_emissions$year, bc_la_vehicle_emissions$fips), sum), c("year", "fips", "emissions"))
yearly_bc_la_vehicle_emissions$city <- lapply(yearly_bc_la_vehicle_emissions$fips, function(x) if (x == "24510") { "Baltimore City, MD"} else if (x == "06037") { "Los Angeles, CA" })
yearly_bc_la_vehicle_emissions$city <- as.character(yearly_bc_la_vehicle_emissions$city)
png("plot6.png", width = 600, height = 500)
qplot(year, emissions, data = yearly_bc_la_vehicle_emissions, facets = city~., color = city, main = "Baltimore City, MD | Los Angeles, CA - Motor vehicles emissions", ylab = "Motor vehicle emissions")
dev.off()
# From 1999 to 2008, the motor vehicles emissions decreased in Baltimore City, MD and increased in Los Angeles, CA.



