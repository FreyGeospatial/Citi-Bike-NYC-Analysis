#Jordan Frey
#intermediate quant
#November 4, 2018

#Load Packages
#########################################
library(ggplot2)
library(plyr) #for count function
library(pastecs)
#library(geosphere)
library(ppcor)
library(mctest)
library(plotrix)
#############################################


#############################################

#load citi-bike data
#############################################
january <- read.csv("201701-citibike-tripdata.csv", stringsAsFactors = F)
february <- read.csv("201702-citibike-tripdata.csv", stringsAsFactors = F)
march <- read.csv("201703-citibike-tripdata.csv", stringsAsFactors = F)
april <- read.csv("201704-citibike-tripdata.csv", stringsAsFactors = F)
may <- read.csv("201705-citibike-tripdata.csv", stringsAsFactors = F)
june <- read.csv("201706-citibike-tripdata.csv", stringsAsFactors = F)
july <- read.csv("201707-citibike-tripdata.csv", stringsAsFactors = F)
august <- read.csv("201708-citibike-tripdata.csv", stringsAsFactors = F)
september <- read.csv("201709-citibike-tripdata.csv", stringsAsFactors = F)
october <- read.csv("201710-citibike-tripdata.csv", stringsAsFactors = F)
november <- read.csv("201711-citibike-tripdata.csv", stringsAsFactors = F)
december <- read.csv("201712-citibike-tripdata.csv", stringsAsFactors = F)
#############################################

#set column names equal in data frame
#############################################
names(january) <- tolower(names(january))
names(february) <- names(january)
names(march) <- names(january)
names(april) <- names(january)
names(may) <- names(january)
names(june) <- names(january)
names(july) <- names(january)
names(august) <- names(january)
names(september) <- names(january)
names(october) <- names(january)
names(november) <- names(january)
names(december) <- names(january)
#############################################

#combine data frames
#############################################
bikeData <- rbind(january, february, march, april, may, june, july, august, september, october, november, december)
#remove unnecessary data and clear RAM
rm(january, february, march, april, may, june, july, august, september, october, november, december)
gc()

#CLEAN DATA
############################################################

#remove records where birth.year contain "NULL" character string
#bikeData <- bikeData[bikeData$birth.year != "NULL",]
#remove NAs from dataset
bikeData <- na.omit(bikeData)
#change column to integer
#bikeData$birth.year <- as.integer(bikeData$birth.year)

#change trip duration from seconds to hours
bikeData$trip.duration_HOURS <- ((bikeData$trip.duration/60)/60)


#PIE CHART: user type
bikeData_UserCount <- bikeData$user.type
bikeData_UserCount <- count(bikeData_UserCount)
bikeData_UserCount <- bikeData_UserCount[2:3,]
#create a percentage column
bikeData_UserCount$prop <- paste(round(bikeData_UserCount$freq/sum(bikeData_UserCount$freq),4) * 100, "%", sep="")

slices <- bikeData_UserCount$freq
lbls <- bikeData_UserCount$x

plotrix::pie3D(slices, 
               labels = bikeData_UserCount$prop, 
               explode = 0.1, 
               col = c("red", "steelblue"),
               start = pi/2, main = "Proportion of Subscribers to Customers")
legend("topright", legend=c("Customers", "Subscribers"), 
       fill=c("red", "steelblue"),
       bty = "n")


#PIE CHART: under 120 seconds
bikeData_under120 <- bikeData$trip.duration[bikeData$trip.duration < 120]
bikeData_under120 <- rep("> 120 Seconds", length(bikeData_under120))
bikeData_over1hr <- bikeData$trip.duration_HOURS[bikeData$trip.duration_HOURS > 1]
bikeData_over1hr <- rep("> 1 Hour", length(bikeData_over1hr))
bikeData_goodTime <- bikeData[bikeData$trip.duration > 120 & bikeData$trip.duration_HOURS < 1, "trip.duration"]
bikeData_goodTime <- rep("Good Time", length(bikeData_goodTime))

timeCounts <- c(bikeData_goodTime, bikeData_over1hr, bikeData_under120)
timeCounts2 <- count(timeCounts)
rm(bikeData_goodTime)
rm(bikeData_over1hr)
rm(bikeData_under120)
gc()

#PIE Chart: duration
slices <- timeCounts2$freq
lbls <- timeCounts2$x

#create a percentage column
timeCounts2$prop <- paste(round(timeCounts2$freq/sum(timeCounts2$freq),4) * 100, "%", sep="")


plotrix::pie3D(slices, 
               labels = timeCounts2$prop, 
               explode = 0.1, 
               col = c("red", "steelblue", "grey"),
               start = pi/2, main = "Proportion Rental Durations Under 120 Seconds and Over 1 Hour")

legend("topright", legend=c(">1 Hour", "<120 Seconds", "Normal Rental Duration"), 
       fill=c("red", "steelblue", "grey"),
       bty = "n")

#remove records where trip.duration is over 1 hours
bikeData <- bikeData[bikeData$trip.duration_HOURS < 1,]


#filter by subscribers
bikeData <- bikeData[bikeData$user.type == "Subscriber",]

#bikeData <- bikeData[bikeData$gender != 0,]

bikeData <- bikeData[bikeData$trip.duration > 120,]



#filter by start/end location
# locationPairs <- c("start.station.name",
#                    "end.station.name")
# 
# combinationCount <- ddply(.data = bikeData,
#                           .variables = locationPairs,
#                           .fun = nrow)

#reset rownames
rownames(bikeData) <- NULL


# take sample of 50,000
bikeSample <- sample(rownames(bikeData), size = 10000, replace = F)
bikeData <- bikeData[bikeSample,]


#format dates and times
bikeData$startDate <- format(as.POSIXlt(bikeData$start.time, tz="EST"), "%Y-%m-%d")
bikeData$startHour <- format(as.POSIXlt(bikeData$start.time, tz="EST"), "%H")
bikeData$endDate <- format(as.POSIXlt(bikeData$stop.time, tz="EST"), "%Y-%m-%d")
bikeData$endHour <- format(as.POSIXlt(bikeData$stop.time, tz="EST"), "%H")
bikeData$startMonth <- format(as.POSIXlt(bikeData$startDate, tz="EST"), "%m")

#create "weekdays" variable, and arrange weekdays in order from Monday - Sunday
bikeData$startDay <- factor(weekdays(as.POSIXct(bikeData$startDate, tz="EST")), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
levels(bikeData$startDay)

#create categorical variable, "weekdays" for regression analysis
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
bikeData$isWeekday <- factor(bikeData$startDay %in% weekdays, levels = c(FALSE, TRUE), labels=c("Weekend", "Weekday"))

#load weather data
weather <- read.csv("2017_Weather_centralPark.csv", stringsAsFactors = F)

#merge bikeData with weather
bikeData <- merge(bikeData, weather[,c("DATE", "AWND", "PRCP", "TMAX", "TMIN")], by.x="startDate", by.y="DATE")



#create average temperature column
bikeData$TAVG <- NA

#reset rownames
rownames(bikeData) <- NULL

#add average temp column
for (i in 1:nrow(bikeData)){
  bikeData[i,"TAVG"] <- mean(as.vector(as.matrix(bikeData[i,c("TMAX", "TMIN")])))
}

weather$TAVG <- NA

for (i in 1:nrow(weather)){
  weather[i,"TAVG"] <- mean(as.vector(as.matrix(weather[i,c("TMAX", "TMIN")])))
}

#aggregate based on row counts
usersPerDay <- count(bikeData$startDate)
colnames(usersPerDay) <- c("date", "freq")
usersPerDay <- merge(usersPerDay, weather[,c("DATE", "AWND", "PRCP", "TAVG")], 
                     by.x="date", by.y="DATE")

#create categorical variable, "weekdays" for regression analysis
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

usersPerDay$startDay <- factor(weekdays(as.POSIXct(usersPerDay$date, tz="EST")), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

usersPerDay$isWeekday <- factor(usersPerDay$startDay %in% weekdays, levels = c(FALSE, TRUE), labels=c("Weekend", "Weekday"))
usersPerDay$isWeekday <- as.integer(usersPerDay$isWeekday)

for(i in 1:nrow(usersPerDay)){
  if (usersPerDay[i,"isWeekday"] == 2){
    usersPerDay[i, "isWeekday"] <- 0
  }
}



usersPerDay.copy <- usersPerDay

usersPerDay$date <- as.character(usersPerDay$date)

#these are "bank" holidays: https://gist.github.com/shivaas/4758439
holidays <- read.csv("holidays/US Bank holidays.csv", stringsAsFactors = F, header = F)
holidays$V1 <- NULL
colnames(holidays) <- c("date", "name")


rm(weather)
gc()


usersPerDay <- merge(x = usersPerDay, y = holidays, by = "date", all.x = T, all.y = F)
usersPerDay$name <- as.integer(!is.na(usersPerDay$name))

#####################################

#READ IN SAVED SAMPLE

#####################################

usersPerDay <- read.csv("usersPerDay.csv", stringsAsFactors = F)
usersPerDay$X <- NULL

colnames(usersPerDay) <- c("date", "freq", "AWND", "PRCP", "TAVG", "isWeekday", "holiday")


#initial Visuals
############################################

#loess: rentals - avg temp
ggplot(data = usersPerDay, aes(x = TAVG, y = freq))+
  geom_point(position = "jitter")+
  stat_smooth(method = "loess")+
  ggtitle("Rentals to Average Temperature")+
  ylab("Rentals")+
  xlab("Average Temperature")

#root:
ggplot(data = usersPerDay, aes(x = TAVG, y = sqrt(freq)))+
  geom_point(position = "jitter")+
  stat_smooth(method = "loess")+
  ggtitle("Rentals to Average Temperature: Square Root Transformed")+
  ylab("Rentals")+
  xlab("Average Temperature")



ggplot(data = usersPerDay, aes(x = AWND, y = freq))+
  geom_point(position = "jitter")+
  stat_smooth(method = "loess")+
  ggtitle("Rentals to Average Windspeed")+
  ylab("Rentals")+
  xlab("Average Wind Speed")

#root:
ggplot(data = usersPerDay, aes(x = AWND, y = sqrt(freq)))+
  geom_point(position = "jitter")+
  stat_smooth(method = "loess")+
  ggtitle("Rentals to Average Windspeed: Square Root Transformed")+
  ylab("Rentals")+
  xlab("Average Wind Speed")



ggplot(data = usersPerDay, aes(x = PRCP, y = freq))+
  geom_point(position = "jitter")+
  stat_smooth(method = "loess")+
  ggtitle("Rentals to Precipitation")+
  ylab("Rentals")+
  xlab("Precipitation (inches)")


#root
ggplot(data = usersPerDay, aes(x = PRCP, y = sqrt(freq)))+
  geom_point(position = "jitter")+
  stat_smooth(method = "loess")+
  ggtitle("Rentals to Precipitation: Square Root Transformed")+
  ylab("Rentals")+
  xlab("Precipitation (inches)")


#root has no effect on categorical variables

ggplot(data = usersPerDay, aes(x = isWeekday, y = freq))+
  geom_point()+
  stat_smooth(method = "lm", se=F)+
  ylab("Rentals")+
  xlab("Type of Day")+
  ggtitle("Weekday vs Weekend Rentals")


ggplot(data = usersPerDay, aes(x = holiday, y = freq))+
  geom_point()+
  stat_smooth(method = "lm", se=F)+
  ylab("Rentals")+
  xlab("Type of Day")+
  ggtitle("Holiday vs Regular Day Rentals")



# #log transformed: loess: rentals - avg temp
# ggplot(data = usersPerDay, aes(x = log10(TAVG), y = log10(freq)))+
#   geom_point()+
#   stat_smooth(method = "loess")

#descriptive stats
descriptiveStats <- pastecs::stat.desc(usersPerDay)
write.csv(descriptiveStats, file = "descriptiveStats.csv")

#remove unusued column
#usersPerDay$startDay <- NULL

#analysis
########################################

#linear model
myRegression <- lm(data = usersPerDay, formula = freq~TAVG+ AWND+ PRCP+ isWeekday+ holiday)
summary(myRegression)

#root transformed linear model with holidays and w/ wind
myRegression_root <- lm(data = usersPerDay, formula = sqrt(freq)~TAVG+ PRCP+ isWeekday+ AWND+ holiday)
summary(myRegression_root)

#model without AWND
myRegression_noWind <- lm(data = usersPerDay, formula = freq~TAVG+ PRCP+ isWeekday+ holiday)
summary(myRegression_noWind)

#root model without AWND
myRegression_noWind_root <- lm(data = usersPerDay, formula = sqrt(freq)~TAVG+ PRCP+ isWeekday+ holiday)
summary(myRegression_noWind_root)


#bivariate
bivariate <- lm(data = usersPerDay, formula = freq~TAVG)
summary(bivariate)

#3-variate
threeVars <- lm(data = usersPerDay, formula = freq~TAVG+ PRCP)
summary(threeVars)

#4-variate
fourVars <- lm(data = usersPerDay, formula = freq~TAVG+ PRCP+ isWeekday)
summary(fourVars)

#5-variate
fiveVars <- lm(data = usersPerDay, formula = freq~TAVG+ AWND+ PRCP+ isWeekday)
summary(fiveVars)

#Tolerance and VIF 
omcdiag(usersPerDay[,3:7], y = usersPerDay$freq)
imcdiag(usersPerDay[,3:7], y = usersPerDay$freq)

imcdiag(usersPerDay[,3:7], y = usersPerDay$freq)

#write cleaned data to .csv
#write.csv(x = usersPerDay, file = "E://OneDrive//Documents//School Work//Clark//Intermediate Quant//finalProj//usersPerDay.csv")


#residual plots
plot(myRegression)
plot(myRegression_root)
plot(myRegression_noWind)
plot(myRegression_noWind_root)





#correlation matrix
varMatrix <- as.matrix(cbind(usersPerDay$freq, usersPerDay$PRCP, 
                             usersPerDay$TAVG, usersPerDay$AWND, 
                             usersPerDay$isWeekday, usersPerDay$holiday))

colnames(varMatrix) <- c("freq", "PRCP", "TAVG", "AWND", "isWeekday", "holiday")
corMatrix <- cor(varMatrix, method = "pearson")
write.csv(corMatrix, file = "corMatrix.csv")


#time series plot:
ggplot(data = usersPerDay, aes(x = as.Date(date), y = freq))+
  geom_line()+
  stat_smooth(method = "loess")+
  ggtitle("Time Series of Bicycle Rentals")+
  xlab("Date")+
  ylab("Rentals")
  


