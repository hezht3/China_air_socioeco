#################################### China air pollution data cleaning (2014 - 2020) ####################################

require(tidyverse)
require(data.table)
require(nortest)

setwd("F:/air polution/China_airpollution")

######################################### merge air pollution data of each year #########################################

dir <- c("dataset/air quality/station_20140513-20141231/", "dataset/air quality/station_20150101-20151231/", "dataset/air quality/station_20160101-20161231/",
         "dataset/air quality/station_20170101-20171231/", "dataset/air quality/station_20180101-20181231/", "dataset/air quality/station_20190101-20191231/",
         "dataset/air quality/station_20200101-20201231/")

data <- list()
merge.data <- list()
for(path in dir) {
  filenames <- list.files(path)
  file <- paste(path, filenames, sep = "")
  year <- substr(path, 29, 32)
  
  for(i in 1:length(file)) {
    date <- substr(file[i], 59, 66)
    data[[year]][[date]] <- fread(file[i], header = TRUE, sep =",")
  }
  
  merge.data[[year]] <- rbindlist(data, use.names = TRUE, fill = TRUE, idcol = FALSE)
}

######################################### merge air pollution data of all years #########################################

filenames <- list.files("outputted dataset/annual data")
file <- paste("outputted dataset/annual data", filenames, sep = "/")

data.all <- readRDS(file[1])
for(i in 2:length(file)) {
  data.all <- bind_rows(data.all, readRDS(file[i]))
}
saveRDS(data.all, file = "F:/Box Sync/air polution/China_airpollution/outputted dataset/2014-2020.rds")

data.all <- data.all %>%
              mutate(date = paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, 8), sep = "/")) %>%
                mutate(date = as.Date(date))

###################################################### station info #####################################################

# verify whether there is a station id that represent different station in different date
filenames <- list.files("dataset/station id", pattern = "*.csv")
file <- paste("dataset/station id", filenames, sep = "/")

station_pre <- read.csv(file[1], header = TRUE, sep = ",", encoding = "UTF-8")
colnames(station_pre) <- c("station_id", "station_name", "city", "long", "lat")

for(i in 2:length(file) - 1) {
  station_new <- read.csv(file[i], header = TRUE, sep = ",", encoding = "UTF-8")
  colnames(station_new) <- c("station_id", "station_name", "city", "long", "lat")
  
  station_diff <- list()
  for(var in c("station_id", "station_name", "city", "long", "lat")) {
    station_diff[[var]] <- setdiff(station_pre$var, station_new$var)
    if(length(station_diff[[var]]) > 0) {
      print(station_new, station_pre)
    }
  }
  
  station_pre <- station_new
  
} # no differences were found

station_ini <- read.csv(file[length(file)], header = TRUE, sep = ",", encoding = "UTF-8")
station_ini <- as.data.frame(t(station_ini))
station_ini$station_id <- rownames(station_ini)
colnames(station_ini) <- c("station_name", "city", "station_id")

for(var in c("station_id", "station_name", "city")) {
  station_diff[[var]] <- setdiff(station_ini$var, station_new$var)
  if(length(station_diff[[var]]) > 0) {
    print(station_ini, station_new)
  }
} # no differences were found

station <- station_new

######################################### gather dataset and merge with station #########################################

filenames <- list.files("outputted dataset/annual data")
file <- paste("outputted dataset/annual data", filenames, sep = "/")
for(i in 1:length(file)) {
  data <- readRDS(file[i])
  data <- as.data.table(data)
  
  for(typ in c("AQI", "CO", "CO_24h", "NO2", "NO2_24h", "O3", "O3_24h", "O3_8h", "O3_8h_24h", "PM10", "PM10_24h", "PM2.5", "PM2.5_24h", "SO2", "SO2_24h")) {
    data.typ <- data %>%
                    filter(type == typ) %>%
                        gather(station_id, value, starts_with("X"), na.rm = TRUE) %>%
                            left_join(station, by = "station_id", suffix = c(".data", ".station"))
      # merge(station, by = "station_id", all.x = TRUE)
    saveRDS(data.typ, file = paste("F:/air polution/China_airpollution/outputted dataset/annual data with station info/", substr(file[i], 31, 34), "_", typ,"_station info.rds", sep = ""))
  }
}

######################################### normal distribution test of each year #########################################

filenames <- list.files("outputted dataset/annual data")
file <- paste("outputted dataset/annual data", filenames, sep = "/")

type <- c('AQI', 'CO', 'CO_24h', 'NO2', 'NO2_24h', 'O3', 'O3_24h', 'O3_8h', 'O3_8h_24h', 'PM10', 'PM10_24h', 'PM2.5', 'PM2.5_24h', 'SO2', 'SO2_24h')
normtest <- data.frame(year = NA, type = NA, station_id = NA, test_method = NA)

for(i in 1:length(file)) {
  data <- readRDS(file[i])
  
  for(typ in type) {
    data.typ <- filter(data, type == typ)
    data.typ <- as.matrix(data.typ[, 4:ncol(data.typ)])
    
    # Anderson-Darling test for normality
    for(j in 1:ncol(data.typ)) {
       data.typ.temp <- na.omit(data.typ[,j])
       if(length(data.typ.temp) > 7) { # ad.test requires sample size > 7
         adtest <- ad.test(data.typ.temp)
       }
    
       if(isTRUE(adtest$p.value > 0.05)) {
         ad.test.norm <- c(substr(file[i], 31, 34), typ, colnames(data.typ)[j], "A-D test")
         normtest <- rbind(normtest, ad.test.norm)
       }
    }

    # Kolmogorov-Smirnov test test for normality
    for(j in 1:ncol(data.typ)) {
       data.typ.temp <- na.omit(data.typ[,j])
       if(length(data.typ.temp) > 7) {
         kstest <- ks.test(x = data.typ.temp, y = "pnorm", alternative = "two.sided")
       }
    
       if(isTRUE(kstest$p.value > 0.05)) {
         ks.test.norm <- c(substr(file[i], 31, 34), typ, colnames(data.typ)[j], "K-S test")
         normtest <- rbind(normtest, ks.test.norm)
       }
    }
    
    # graphic test
    for(j in 1:ncol(data.typ)) {
      data.typ.temp <- na.omit(data.typ[,j])
      if(length(data.typ.temp) > 7) {
          tiff(file = paste("F:/Box Sync/air polution/China_airpollution/distribution test/graphic test/", paste(substr(file[i], 31, 34), typ, colnames(data.typ)[j], sep = ", "), ".tiff", sep = ""), res = 200, width = 8000, height = 4290)
          qqnorm(data.typ.temp)
          qqline(data.typ.temp)
          dev.off()
      }
    }
  }
}

saveRDS(normtest[2:nrow(normtest), ], file = "F:/Box Sync/air polution/China_airpollution/distribution test/statistical test/normal distribution test.rds")