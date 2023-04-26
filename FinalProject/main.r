library(dplyr)
library(ggplot2)
library(ISLR)
library(corrplot)
library(lubridate)
library(tidyr)
library(corrplot)


#import csv files
corn_data <- read.csv("corn_quality.csv")
surface_air_temp <- read.csv("surface_air_new.csv")
canopy_surface_water <- read.csv("canopy_water_new.csv")
evapotranspiration <- read.csv("evapotranspiration_new.csv")
soil_moisture <- read.csv("soil_moisture_new.csv")
precipitation <- read.csv("precipitation_new.csv")
water_vapor <- read.csv("water_vapor_new.csv")

#Exploratory Data Analysis
summary(corn_data)
summary(surface_air_temp)
summary(canopy_surface_water)
summary(evapotranspiration)
summary(soil_moisture)
summary(precipitation)
summary(water_vapor)




#surface_air_temp and water_vapor have character values, so we need to convert them to numeric
head(surface_air_temp)
head(water_vapor)

surface_air_temp$mean_AIRS3STD_006_SurfAirTemp_A <- as.numeric(surface_air_temp$mean_AIRS3STD_006_SurfAirTemp_A)
summary(surface_air_temp)
#NA's introduced so print rows with NA values for mean_AIRS3STD_006_SurfAirTemp_A
surface_air_temp[is.na(surface_air_temp$mean_AIRS3STD_006_SurfAirTemp_A),]

summary(water_vapor)
water_vapor$mean_MYD08_D3_6_1_Atmospheric_Water_Vapor_Low_QA_Mean <- as.numeric(water_vapor$mean_MYD08_D3_6_1_Atmospheric_Water_Vapor_Low_QA_Mean)
#NA's introduced so print rows with NA values for mean_MYD08_D3_6_1_Atmospheric_Water_Vapor_Low_QA_Mean
water_vapor[is.na(water_vapor$mean_MYD08_D3_6_1_Atmospheric_Water_Vapor_Low_QA_Mean),]


#make corn_data ascending by date
corn_data <- corn_data[order(corn_data$Week.Ending),]
head(corn_data)

#Make the date ranges match -> delete all rows in corn_data before 4/1/2003
corn_data <- corn_data[corn_data$Week.Ending >= "2003-04-01",]


#For each row in corn_data, average the values in the other data sets for the past 7 days not including the current day
#For example, for the row with date 2003-06-08, average the values from 2003-06-01 to 2003-06-07



# Convert time columns in other dataframes to date format
surface_air_temp$time <- as.Date(surface_air_temp$time, format = "%m/%d/%Y")
canopy_surface_water$time <- as.Date(canopy_surface_water$time, format = "%m/%d/%Y")
evapotranspiration$time <- as.Date(evapotranspiration$time, format = "%m/%d/%Y")
soil_moisture$time <- as.Date(soil_moisture$time, format = "%m/%d/%Y")
precipitation$time <- as.Date(precipitation$time, format = "%m/%d/%Y")
water_vapor$time <- as.Date(water_vapor$time, format = "%m/%d/%Y")

head(corn_data)


#Convert Week Ending to date format and create a new column called Week Starting that is 7 days before Week Ending
corn_data$Week.Ending <- as.Date(corn_data$Week.Ending, format = "%Y-%m-%d")
corn_data$Week.Starting <- corn_data$Week.Ending - 7

#For each row in corn_data, find days in other dataframes that are between Week.Starting and Week.Ending and average the values in those rows
#For example, for the row with date 2003-06-08, average the values from 2003-06-02 to 2003-06-08 in the other dataframes

get_avg_past_7_days <- function(df, var_name, date) {
  past_week_start <- date - 7
  past_week_end <- date - 1
  past_week_rows <- df$time >= past_week_start & df$time <= past_week_end
  mean(df[past_week_rows,][[var_name]], na.rm = TRUE)
}

# For each row in corn_data, find days in other dataframes that are between Week.Starting and Week.Ending and average the values in those rows
for (i in 1:nrow(corn_data)) {
  date <- corn_data$Week.Ending[i]
  corn_data$Surface_Air_Temp_Avg[i] <- get_avg_past_7_days(surface_air_temp, "mean_AIRS3STD_006_SurfAirTemp_A", date)
  corn_data$Canopy_Surface_Water_Avg[i] <- get_avg_past_7_days(canopy_surface_water, "mean_GLDAS_CLSM025_DA1_D_2_2_CanopInt_tavg", date)
  corn_data$Evapotranspiration_Avg[i] <- get_avg_past_7_days(evapotranspiration, "mean_GLDAS_CLSM025_DA1_D_2_2_Evap_tavg", date)
  corn_data$Soil_Moisture_Avg[i] <- get_avg_past_7_days(soil_moisture, "mean_GLDAS_CLSM025_DA1_D_2_2_SoilMoist_RZ_tavg", date)
  corn_data$Precipitation_Avg[i] <- get_avg_past_7_days(precipitation, "mean_GPM_3IMERGDF_06_precipitationCal", date)
  corn_data$Water_Vapor_Avg[i] <- get_avg_past_7_days(water_vapor, "mean_MYD08_D3_6_1_Atmospheric_Water_Vapor_Low_QA_Mean", date)
}

head(corn_data)
#remove rows with NA values and negative values in corn_data$Surface_Air_Temp_Avg
corn_data <- corn_data[!is.na(corn_data$Surface_Air_Temp_Avg),]
corn_data <- corn_data[corn_data$Surface_Air_Temp_Avg >= 0,]

#histogram of 6 variables
hist(corn_data$Surface_Air_Temp_Avg)
hist(corn_data$Canopy_Surface_Water_Avg)
hist(corn_data$Evapotranspiration_Avg)
hist(corn_data$Soil_Moisture_Avg)
hist(corn_data$Precipitation_Avg)
hist(corn_data$Water_Vapor_Avg)

#Plot each variable in relation to the week ending date
plot(corn_data$Week.Ending, corn_data$Surface_Air_Temp_Avg)
plot(corn_data$Week.Ending, corn_data$Canopy_Surface_Water_Avg)
plot(corn_data$Week.Ending, corn_data$Evapotranspiration_Avg)
plot(corn_data$Week.Ending, corn_data$Soil_Moisture_Avg)
plot(corn_data$Week.Ending, corn_data$Precipitation_Avg)
plot(corn_data$Week.Ending, corn_data$Water_Vapor_Avg)

#Plot surface air temp in 2003 with a line
plot(corn_data$Week.Ending[corn_data$Week.Ending >= "2003-01-01" & corn_data$Week.Ending <= "2003-12-31"], corn_data$Surface_Air_Temp_Avg[corn_data$Week.Ending >= "2003-01-01" & corn_data$Week.Ending <= "2003-12-31"], type = "l")

#Now plot the surface air temp from 2003 to 2022 where each year is a seperate line and the date range is the months

corn_data$year <- format(as.Date(corn_data$Week.Ending), "%Y")
corn_data$month <- format(as.Date(corn_data$Week.Ending), "%m")

# Group the data by year and month and calculate the mean surface air temperature
temp_data <- corn_data %>%
  group_by(year, month) %>%
  summarize(mean_temp = mean(Surface_Air_Temp_Avg))

# Plot the data using ggplot2
ggplot(data = corn_data, aes(x = month, y = Surface_Air_Temp_Avg, group = year, color = year)) +
  geom_line() +
  labs(title = "Surface Air Temperature by Month and Year",
       x = "Month",
       y = "Surface Air Temperature (°K)",
       color = "Year")


#Had to create a quality score otherwise multiple entries for each week made it difficult to analyze
#Create a quality score for each week based on the condition of the corn

quality_scores <- c("CORN - CONDITION, MEASURED IN PCT EXCELLENT",
                     "CORN - CONDITION, MEASURED IN PCT FAIR", 
                     "CORN - CONDITION, MEASURED IN PCT GOOD", 
                     "CORN - CONDITION, MEASURED IN PCT POOR", 
                     "CORN - CONDITION, MEASURED IN PCT VERY POOR")

# Group by date and calculate quality score
corn_data_new <- corn_data %>% 
  group_by(Week.Ending) %>% 
  summarise(Week.Ending = first(Week.Ending),
            State = first(State),
            Surface_Air_Temp_Avg = first(Surface_Air_Temp_Avg),
            Canopy_Surface_Water_Avg = first(Canopy_Surface_Water_Avg),
            Evapotranspiration_Avg = first(Evapotranspiration_Avg),
            Soil_Moisture_Avg = first(Soil_Moisture_Avg),
            Precipitation_Avg = first(Precipitation_Avg),
            Water_Vapor_Avg = first(Water_Vapor_Avg),
            crop_quality = sum(ifelse(Data.Item == quality_scores[1], 5, ifelse(Data.Item == quality_scores[3], 4, ifelse(Data.Item == quality_scores[2], 3, ifelse(Data.Item == quality_scores[4], 2, 1)))) * Value / 100)) %>% 
  ungroup()

# View the new dataframe
head(corn_data_new)

#Crop quality plot overall
ggplot(data = corn_data_new, aes(x = Week.Ending, y = crop_quality)) +
  geom_line() +
  labs(title = "Crop Quality by Week",
       x = "Week Ending",
       y = "Crop Quality")


#Do the same for crop quality
# Group the data by year and week
quality_data <- corn_data_new %>%
  group_by(year, week) %>%
  summarize(mean_quality = mean(crop_quality))

# Plot the data using ggplot2
ggplot(data = corn_data_new, aes(x = week, y = crop_quality, group = year, color = year)) +
  geom_line() +
  labs(title = "Crop Quality by Week and Year",
       x = "Week",
       y = "Crop Quality",
       color = "Year")

corn_data_new$year <- format(as.Date(corn_data_new$Week.Ending), "%Y")
corn_data_new$week <- format(as.Date(corn_data_new$Week.Ending), "%U")

# Group the data by year and week and calculate the mean surface air temperature
temp_data <- corn_data_new %>%
  group_by(year, week) %>%
  summarize(mean_temp = mean(Surface_Air_Temp_Avg))

# Plot the data using ggplot2
ggplot(data = corn_data_new, aes(x = week, y = Surface_Air_Temp_Avg, group = year, color = year)) +
  geom_line() +
  labs(title = "Surface Air Temperature by Week and Year",
       x = "Week",
       y = "Surface Air Temperature (°K)",
       color = "Year")

#Repeat for other variables
# Group the data by year and week and calculate the mean canopy surface water
water_data <- corn_data_new %>%
  group_by(year, week) %>%
  summarize(mean_water = mean(Canopy_Surface_Water_Avg))

# Plot the data using ggplot2
ggplot(data = corn_data_new, aes(x = week, y = Canopy_Surface_Water_Avg, group = year, color = year)) +
  geom_line() +
  labs(title = "Canopy Surface Water by Week and Year",
       x = "Week",
       y = "Canopy Surface Water (mm)",
       color = "Year")

# Group the data by year and week and calculate the mean evapotranspiration
evap_data <- corn_data_new %>%
  group_by(year, week) %>%
  summarize(mean_evap = mean(Evapotranspiration_Avg))

# Plot the data using ggplot2
ggplot(data = corn_data_new, aes(x = week, y = Evapotranspiration_Avg, group = year, color = year)) +
  geom_line() +
  labs(title = "Evapotranspiration by Week and Year",
       x = "Week",
       y = "Evapotranspiration (mm)",
       color = "Year")

# Group the data by year and week and calculate the mean soil moisture
soil_data <- corn_data_new %>%
  group_by(year, week) %>%
  summarize(mean_soil = mean(Soil_Moisture_Avg))

# Plot the data using ggplot2
ggplot(data = corn_data_new, aes(x = week, y = Soil_Moisture_Avg, group = year, color = year)) +
  geom_line() +
  labs(title = "Soil Moisture by Week and Year",
       x = "Week",
       y = "Soil Moisture (mm)",
       color = "Year")

# Group the data by year and week and calculate the mean precipitation
precip_data <- corn_data_new %>%
  group_by(year, week) %>%
  summarize(mean_precip = mean(Precipitation_Avg))

# Plot the data using ggplot2
ggplot(data = corn_data_new, aes(x = week, y = Precipitation_Avg, group = year, color = year)) +
  geom_line() +
  labs(title = "Precipitation by Week and Year",
       x = "Week",
       y = "Precipitation (mm)",
       color = "Year")

# Group the data by year and week and calculate the mean water vapor
vapor_data <- corn_data_new %>%
  group_by(year, week) %>%
  summarize(mean_vapor = mean(Water_Vapor_Avg))

# Plot the data using ggplot2
ggplot(data = corn_data_new, aes(x = week, y = Water_Vapor_Avg, group = year, color = year)) +
  geom_line() +
  labs(title = "Water Vapor by Week and Year",
       x = "Week",
       y = "Water Vapor (mm)",
       color = "Year")

#Group the data by year and calculate the mean crop quality, then plot year to year mean crop quality
year_data <- corn_data_new %>%
  group_by(year) %>%
  summarize(mean_quality = mean(crop_quality))

plot(year_data$year, year_data$mean_quality, type = "l", xlab = "Year", ylab = "Mean Crop Quality", main = "Mean Crop Quality by Year")

#run chi square test on crop quality and soil moisture
chisq.test(corn_data_new$Soil_Moisture_Avg, corn_data_new$crop_quality)

#run chi square test on crop quality and precipitation
chisq.test(corn_data_new$Precipitation_Avg, corn_data_new$crop_quality)

#run chi square test on crop quality and water vapor
chisq.test(corn_data_new$Water_Vapor_Avg, corn_data_new$crop_quality)

#run chi square test on crop quality and evapotranspiration
chisq.test(corn_data_new$Evapotranspiration_Avg, corn_data_new$crop_quality)

#run chi square test on crop quality and canopy surface water
chisq.test(corn_data_new$Canopy_Surface_Water_Avg, corn_data_new$crop_quality)

#run chi square test on crop quality and surface air temperature
chisq.test(corn_data_new$Surface_Air_Temp_Avg, corn_data_new$crop_quality)

summary(corn_data_new)

#Corrplot of columns 3-9 in corn_data_new
corrplot(cor(corn_data_new[,3:9]), method = "circle")

#Run linear regression on crop quality
quality_model <- lm(crop_quality ~ Soil_Moisture_Avg + Precipitation_Avg + Water_Vapor_Avg + Evapotranspiration_Avg + Canopy_Surface_Water_Avg + Surface_Air_Temp_Avg, data = corn_data_new)
summary(quality_model)

#plot residuals
plot(quality_model, which = 1)


#add year and week number to corn_data_new
corn_data_new$year <- format(as.Date(corn_data_new$Week.Ending), "%Y")
corn_data_new$week <- format(as.Date(corn_data_new$Week.Ending), "%U")

colnames(corn_data_new)
corn_data_new$year <- as.numeric(corn_data_new$year)
corn_data_new$week <- as.numeric(corn_data_new$week)

#chi square test on crop quality and year
chisq.test(corn_data_new$year, corn_data_new$crop_quality)

#chi square test on crop quality and week
chisq.test(corn_data_new$week, corn_data_new$crop_quality)

#run linear regression on crop quality with year and week
quality_model2 <- lm(crop_quality ~ Soil_Moisture_Avg + Precipitation_Avg + Water_Vapor_Avg + Evapotranspiration_Avg + Canopy_Surface_Water_Avg + Surface_Air_Temp_Avg + year + week, data = corn_data_new)
summary(quality_model2)

#plot residuals
plot(quality_model2, which = 1)

summary(corn_data_new)

#export corn_data_new to csv to be used for machine learning 
write.csv(corn_data_new, "corn_data_new.csv", row.names = FALSE)

