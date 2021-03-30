# code to generate dataset of air pollution and socioeconomics of China in year 2018

require(dplyr)

setwd("F:/Box Sync/air polution/pilot analysis/air quality data_2018/air quality data_20180101-20181231")
filenames = list.files(pattern="*.csv")

####################################### condense air polution data to mean of each date #######################################
data <- list() # empty list to read data of each date
data_date <- list() # empty list to write data of each date
date_sum <- c() # empty vector to write date

for(file in  filenames) {
  date <- substr(file, 13, 20) # substr filename to date
  date_sum <- c(date_sum, date) # bind date to date_sum
  data[[date]] <- read.csv(file) # import air polution data of each date
  # 20181222 - 20181226 were dropped due to problematic document
  
  type <- as.vector(filter(data[[date]], hour == "0")$type) # type of pollutant
  id <- colnames(data[[date]])[4:length(colnames(data[[date]]))] # id of air quality station
  
  data_pollutant <- data.frame(matrix(nrow = length(type), ncol = length(id)), row.names = type) # create empty df for mean of this date
  colnames(data_pollutant) <- id
  
  for(pollutant in type) { # for each type of pollutant in each day, calculate the mean value of each hour
    data_type <- filter(data[[date]], type == pollutant) # filter data to specific pollutant type
    
    summary <- vector() # empty vector for mean of this pollutant type
    for(i in 4:ncol(data_type)) {
      summary <- c(summary, mean(data_type[1:nrow(data_type), i], na.rm = TRUE)) # bind the mean value of each air quality station
    }
    
    data_pollutant[c(pollutant), ] <- summary # bind the mean value of each type of pollutant to the df, for this date
  }
  
  data_date[[date]] <- data_pollutant # bind the df of each date
  data_pollutant <- data.frame() # recover data_pollutant to empty dataframe for next date
}

####################################### merge air polution data with air quality station data #######################################
setwd("F:/Box Sync/air polution/pilot analysis/air quality data_2018/station")
station_20171209 <- read.csv("station from-2017.12.09.csv", encoding = "UTF-8") # import air quality station data
station_20180119 <- read.csv("station from-2018.01.19.csv", encoding = "UTF-8")
station_20180222 <- read.csv("station from-2018.02.22.csv", encoding = "UTF-8")
station_20180728 <- read.csv("station from-2018.07.28.csv", encoding = "UTF-8")
station_20181108 <- read.csv("station from-2018.11.08.csv", encoding = "UTF-8")
colnames(station_20171209) <- c("station_id", "station_name", "city", "long", "lat")
colnames(station_20180119) <- c("station_id", "station_name", "city", "long", "lat")
colnames(station_20180222) <- c("station_id", "station_name", "city", "long", "lat")
colnames(station_20180728) <- c("station_id", "station_name", "city", "long", "lat")
colnames(station_20181108) <- c("station_id", "station_name", "city", "long", "lat")

data_date_summary <- data.frame(matrix(nrow = 0, ncol = 0)) # empty dataframe to bind data of each date, merge with air quality station data

# merge with air quality station 20180101 - 20180118
for(date in date_sum[1:18]) {
  if(nrow(data_date[[date]]) > 0) {
    data_date_t <- t(data_date[[date]])
    data_date_t <- as.data.frame(data_date_t)
    
    data_date_t$station_id <- rownames(data_date_t)
    data_date_t$station_id <- substr(data_date_t$station_id, 2, 6)
    data_date_t$date <- date
    
    data_date_t <- left_join(data_date_t, station_20171209, by = "station_id")
  }
  
  data_date_summary <- bind_rows(data_date_summary, data_date_t)
}

# merge with air quality station 20180119 - 20180221
for(date in date_sum[19:52]) {
  if(nrow(data_date[[date]]) > 0) {
    data_date_t <- t(data_date[[date]])
    data_date_t <- as.data.frame(data_date_t)
    
    data_date_t$station_id <- rownames(data_date_t)
    data_date_t$station_id <- substr(data_date_t$station_id, 2, 6)
    data_date_t$date <- date
    
    data_date_t <- left_join(data_date_t, station_20180119, by = "station_id")
  }
  
  data_date_summary <- bind_rows(data_date_summary, data_date_t)
}

# merge with air quality station 20180222 - 20180727
for(date in date_sum[53:208]) {
  if(nrow(data_date[[date]]) > 0) {
    data_date_t <- t(data_date[[date]])
    data_date_t <- as.data.frame(data_date_t)
    
    data_date_t$station_id <- rownames(data_date_t)
    data_date_t$station_id <- substr(data_date_t$station_id, 2, 6)
    data_date_t$date <- date
    
    data_date_t <- left_join(data_date_t, station_20180222, by = "station_id")
  }
  
  data_date_summary <- bind_rows(data_date_summary, data_date_t)
}

# merge with air quality station 20180728 - 20181107
for(date in date_sum[209:311]) {
  if(nrow(data_date[[date]]) > 0) {
    data_date_t <- t(data_date[[date]])
    data_date_t <- as.data.frame(data_date_t)
    
    data_date_t$station_id <- rownames(data_date_t)
    data_date_t$station_id <- substr(data_date_t$station_id, 2, 6)
    data_date_t$date <- date
    
    data_date_t <- left_join(data_date_t, station_20180728, by = "station_id")
  }
  
  data_date_summary <- bind_rows(data_date_summary, data_date_t)
}

# merge with air quality station 20181108 - 20181231
for(date in date_sum[312:360]) {
  if(nrow(data_date[[date]]) > 0) {
    data_date_t <- t(data_date[[date]])
    data_date_t <- as.data.frame(data_date_t)
    
    data_date_t$station_id <- rownames(data_date_t)
    data_date_t$station_id <- substr(data_date_t$station_id, 2, 6)
    data_date_t$date <- date
    
    data_date_t <- left_join(data_date_t, station_20181108, by = "station_id")
  }
  
  data_date_summary <- bind_rows(data_date_summary, data_date_t)
}

####################################### condense air pollution data to annual mean of each station #######################################
data_annual <- data.frame() # empty dataframe to bind annual mean of each station
station_id <- station_20181108$station_id # total station id of this year

for(id in station_id) {
  data_station <- filter(data_date_summary, station_id == id)
  
  summary <- data_station %>% # calculate annual mean
              summarise(AQI = mean(AQI),
                        PM2.5 = mean(PM2.5),
                        PM2.5_24h = mean(PM2.5_24h),
                        PM10 = mean(PM10),
                        PM10_24h = mean(PM10_24h),
                        SO2 = mean(SO2),
                        SO2_24h = mean(SO2_24h),
                        NO2 = mean(NO2),
                        NO2_24h = mean(NO2_24h),
                        O3 = mean(O3),
                        O3_24h = mean(O3_24h),
                        O3_8h = mean(O3_8h),
                        O3_8h_24h = mean(O3_8h_24h),
                        CO = mean(CO),
                        CO_24h = mean(CO_24h))
  
  summary <- bind_cols(summary, data_station[1, c(16, 18:21)]) # bind with station data
  
  data_annual <- bind_rows(data_annual, summary) # bind annual mean of each station to data_annual
}

# filter NA value (maybe better to use inner_join previously?)
data_annual <- filter(data_annual, is.na(station_name) == FALSE)

for(i in 1:15) {
  data_annual[, i] <- as.numeric(data_annual[, i])
}
data_annual <- filter(data_annual, is.nan(AQI) == FALSE)

saveRDS(data_annual, file = "F:/Box Sync/air polution/pilot analysis/code/China_air_socioeco/data/air pollution data_2018.rds")

####################################### condense air pollution data of each station to each city #######################################
data_annual_city <- data_annual %>% # calculate annual mean of each city
                      group_by(city) %>%
                        summarise(AQI = mean(AQI),
                        PM2.5 = mean(PM2.5),
                        PM2.5_24h = mean(PM2.5_24h),
                        PM10 = mean(PM10),
                        PM10_24h = mean(PM10_24h),
                        SO2 = mean(SO2),
                        SO2_24h = mean(SO2_24h),
                        NO2 = mean(NO2),
                        NO2_24h = mean(NO2_24h),
                        O3 = mean(O3),
                        O3_24h = mean(O3_24h),
                        O3_8h = mean(O3_8h),
                        O3_8h_24h = mean(O3_8h_24h),
                        CO = mean(CO),
                        CO_24h = mean(CO_24h))
data_annual_city <- left_join(data_annual_city, data_annual[, 16:20], by = "city") # bind with station data

saveRDS(data_annual_city, file = "F:/Box Sync/air polution/pilot analysis/code/China_air_socioeco/data/air pollution data_2018_city level.rds")

data_annual_city$city <- paste(data_annual_city$city, "市", sep = "")

####################################### merge air pollution data with socioeconomics data #######################################
data_eco <- read.csv("F:/Box Sync/air polution/pilot analysis/socioeco data 2018_Yearbook 2019/city socioeconomics data_2018.csv",
                     header = FALSE, encoding = "GB2312")
colnames(data_eco) <- data_eco[1,]

data <- inner_join(data_annual_city, data_eco, by = "city")
saveRDS(data, file = "F:/Box Sync/air polution/pilot analysis/code/China_air_socioeco/data/air pollution & socioeco data_2018_city level.rds")