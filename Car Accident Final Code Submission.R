
################## PROJECT SETUP ################## 

## load libraries
suppressMessages(library(tidyverse))
suppressMessages(library(randomForest))

## load data
accidents_original <- read_csv('AccTrain.csv')
accidents <- accidents_original

################## NEW VARIABLES ################## 

## date and time
accidents[, c("Date", "Time")] <- str_split_fixed(accidents$Start_Time, " ", 2)
accidents[, c("Year", "Month", "Day")] <- str_split_fixed(accidents$Date, "-", 3)
accidents$Year <- as.factor(accidents$Year)
accidents$Month <- as.factor(accidents$Month)
accidents <- accidents %>% select(-Date, -Day)

accidents[, c("Hour", "Minute", "Second")] <- str_split_fixed(accidents$Time, ":", 3)
accidents$Minute <- as.numeric(accidents$Minute) / 60
accidents$Hour <- as.numeric(accidents$Hour) + accidents$Minute
accidents <- accidents %>% select(-Time, -Minute, -Second)

## numeric metrics
accidents$Change_Time <- accidents$End_Time - accidents$Start_Time
accidents$Change_Lat <- accidents$End_Lat - accidents$Start_Lat
accidents$Change_Lng <- accidents$End_Lng - accidents$Start_Lng

## description analytics
accidents$Closed1 <- str_detect(accidents$Description, "Closed") 
accidents$Closed2 <- str_detect(accidents$Description, "closed") 
accidents$Accident1 <- str_detect(accidents$Description, "Accident") 
accidents$Accident2 <- str_detect(accidents$Description, "accident") 
accidents$Traffic <- str_detect(accidents$Description, "Traffic|traffic")
accidents$Blocked <- str_detect(accidents$Description, "Blocked|blocked")
accidents$Caution <- str_detect(accidents$Description, "Caution")
accidents$Incident <- str_detect(accidents$Description, "Incident|incident")
accidents$roadclosed <- str_detect(accidents$Description, "Road closed due to accident")

## weather condition
accidents[which(is.na(accidents$Weather_Condition)), ]$Weather_Condition <- "None"
accidents$Rain <- str_detect(accidents$Weather_Condition, "Showers|Rain|Precipitation")
accidents$Cloudy <- str_detect(accidents$Weather_Condition, "Cloud")
accidents$Windy <- str_detect(accidents$Weather_Condition, "Wind")
accidents$Thunderstorm <- str_detect(accidents$Weather_Condition, "Thunder|T-Storm")
accidents$Haze <- str_detect(accidents$Weather_Condition, "Haze|Fog|Dust")
accidents$Snow <- str_detect(accidents$Weather_Condition, "Snow|Sleet")
accidents$Clear <- str_detect(accidents$Weather_Condition, "Fair|Clear")

################## TIMEZONE NA VALUES ################## 

## Timezone
for(i in 1:35000){
  if(is.na(accidents$Timezone[i])){
    state <- accidents$State[i]
    timezone <- mode(accidents[which(accidents$State == state), ])
    accidents$Timezone[i] <- timezone
  }
}

################## COERCE CLASS TO FACTOR ################## 

accidents$Timezone <- as.factor(accidents$Timezone)
accidents$Side <- as.factor(accidents$Side)
accidents$State <- as.factor(accidents$State)
accidents$Change_Time <- as.numeric(accidents$Change_Time)
accidents$Severity <- as.factor(accidents$Severity)

################## FINAL MODEL ################## 

## Final Predictors:
# year, month, hour
# side, state, timezone, change_time, change_lat, change_lng
# closed, accident, traffic, blocked, caution, incident
# rain, cloudy, windy, thunderstorm, haze, snow, clear
my_data <- accidents %>% select(Severity, 
                                Year, Month, Hour, 
                                Side, State, Timezone, Change_Time, Change_Lat, Change_Lng,
                                Closed1, Closed2, Accident1, Accident2, Traffic, Blocked, Caution, Incident,
                                Rain, Cloudy, Windy, Thunderstorm, Haze, Snow, Clear, roadclosed)

## Random Forest Model
set.seed(1103)
RF_model <- randomForest(Severity ~ ., data = my_data, mtry = 6)
RF_pred <- predict(RF_model, type = 'class')

################## LOAD TESTING DATA ################## 

testing <- read_csv('AcctestNoY.csv')

################## NEW VARIABLES ################## 

## date and time
testing[, c("Date", "Time")] <- str_split_fixed(testing$Start_Time, " ", 2)
testing[, c("Year", "Month", "Day")] <- str_split_fixed(testing$Date, "-", 3)
testing$Year <- as.factor(testing$Year)
testing$Month <- as.factor(testing$Month)
testing <- testing %>% select(-Date, -Day)

testing[, c("Hour", "Minute", "Second")] <- str_split_fixed(testing$Time, ":", 3)
testing$Minute <- as.numeric(testing$Minute) / 60
testing$Hour <- as.numeric(testing$Hour) + testing$Minute
testing <- testing %>% select(-Time, -Minute, -Second)

## numeric metrics
testing$Change_Time <- testing$End_Time - testing$Start_Time
testing$Change_Lat <- testing$End_Lat - testing$Start_Lat
testing$Change_Lng <- testing$End_Lng - testing$Start_Lng

## description analytics
testing$Closed1 <- str_detect(testing$Description, "Closed")
testing$Closed2 <- str_detect(testing$Description, "closed") 
testing$Accident1 <- str_detect(testing$Description, "Accident") 
testing$Accident2 <- str_detect(testing$Description, "accident") 
testing$Traffic <- str_detect(testing$Description, "Traffic|traffic")
testing$Blocked <- str_detect(testing$Description, "Blocked|blocked")
testing$Caution <- str_detect(testing$Description, "Caution")
testing$Incident <- str_detect(testing$Description, "Incident|incident")
testing$roadclosed <- str_detect(testing$Description, "Road closed due to accident")

## weather condition
testing[which(is.na(testing$Weather_Condition)), ]$Weather_Condition <- "None"
testing$Rain <- str_detect(testing$Weather_Condition, "Showers|Rain|Precipitation")
testing$Cloudy <- str_detect(testing$Weather_Condition, "Cloud")
testing$Windy <- str_detect(testing$Weather_Condition, "Wind")
testing$Thunderstorm <- str_detect(testing$Weather_Condition, "Thunder|T-Storm")
testing$Haze <- str_detect(testing$Weather_Condition, "Haze|Fog|Dust")
testing$Snow <- str_detect(testing$Weather_Condition, "Snow")
testing$Clear <- str_detect(testing$Weather_Condition, "Fair|Clear")

################## TIMEZONE NA VALUES ################## 

## Timezone
for(i in 1:15000){
  if(is.na(testing$Timezone[i])){
    state <- testing$State[i]
    timezone <- mode(testing[which(testing$State == state), ])
    testing$Timezone[i] <- timezone
  }
}

################## CLASS TO FACTOR ################## 

testing$Timezone <- as.factor(testing$Timezone)
testing$Side <- as.factor(testing$Side)
testing$State <- as.factor(testing$State)
testing$Change_Time <- as.numeric(testing$Change_Time)

################## TESTING PREDICTION ################## 

my_data <- testing %>% select(Year, Month, Hour, 
                              Side, State, Timezone, Change_Time, Change_Lat, Change_Lng,
                              Closed1, Closed2, Accident1, Accident2, Traffic, Blocked, Caution, Incident,
                              Rain, Cloudy, Windy, Thunderstorm, Haze, Snow, Clear, roadclosed)

## prediction
SEVERITY <- predict(RF_model, my_data, type = 'class')
Ob <- 1:15000
kaggle <- data.frame(Ob, SEVERITY)
write.csv(kaggle, 'kaggle.csv')

