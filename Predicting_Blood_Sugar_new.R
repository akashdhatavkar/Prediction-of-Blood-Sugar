##----- Installing Required Packages -----##
if(!require(tidyverse)){install.packages("tidyverse")};require(tidyverse)
if(!require(dplyr)){install.packages("dplyr")};require(dplyr)
if(!require(data.table)){install.packages("data.table")};require(data.table)
if(!require(MASS)){install.packages("MASS")};require(MASS)
if(!require(randomForest)){install.packages("randomForest")};require(randomForest)
if(!require(e1071)){install.packages("e1071")};require(e1071)
if(!require(zoo)){install.packages("zoo")};require(zoo)
if(!require(ega)){install.packages("ega")};require(ega)
if(!require(neuralnet)){install.packages("neuralnet")};require(neuralnet)
if(!require(gbm)){install.packages("gbm")};require(gbm)
if(!require(dummies)){install.packages("dummies")};require(dummies)

##----- Reading files -----##
bg <- read.csv("D:\\Personal\\Bioconscious\\data\\blood-glucose-data.csv", na.strings = c("", "N/A"), stringsAsFactors = F)
da <- read.csv("D:\\Personal\\Bioconscious\\data\\distance-activity-data.csv", na.strings = c("", "N/A"), stringsAsFactors = F)
hr <- read.csv("D:\\Personal\\Bioconscious\\data\\heart-rate-data.csv", na.strings = c("", "N/A"), stringsAsFactors = F)

##----- Converting date column to date-time -----##
bg$point_timestamp <- as.POSIXct(bg$point_timestamp)
hr$point_timestamp <- as.POSIXct(hr$point_timestamp)
da$point_timestamp <- as.POSIXct(da$point_timestamp)

##----- Interpolating Blood-Sugar data -----##
bg2 <- bg
bg2$point_timestamp <- as.POSIXct(paste0(substring(bg2$point_timestamp,1,17),"00"))
bg2 <- bg2 %>%
  full_join(data.frame(point_timestamp = seq(
    from = min(.$point_timestamp),
    to = max(.$point_timestamp),
    by = 'min'))) %>%
  arrange(point_timestamp) %>%
  mutate(point_value.mg.dL. = approx(point_value.mg.dL., n = n())$y)
bg2$point_value.mg.dL. <- round(bg2$point_value.mg.dL.)
bg2 <- bg2 %>% group_by(point_timestamp) %>% summarise(point_value.mg.dL. = mean(point_value.mg.dL.))

##----- Interpolating Heart-Rate data -----##
hr1 <- hr %>%
  full_join(data.frame(point_timestamp = seq(
    from = min(.$point_timestamp),
    to = max(.$point_timestamp),
    by = 'min'))) %>%
  arrange(point_timestamp) %>%
  mutate(point_value = approx(point_value, n = n())$y)

hr1 <- hr1 %>% group_by(point_timestamp) %>% summarise(point_value = mean(point_value))
hr1$point_value <- round(hr1$point_value)

##----- Joining Heart Rate data on Blood Sugar -----##
bg2 <- bg2 %>% left_join(hr1, by = c("point_timestamp"))
bg2 <- na.omit(bg2)
bg2$point_value <- round(bg2$point_value)   

##----- Interpolating Distance data -----##
da_iphone <- da[da$device == "iPhone",]
da_fitbit <- da[da$device == "FitbitWatch",]
da_fitbit <- da_fitbit[da_fitbit$point_value.kilometers.>0,]
da_iphone <- da_iphone[da_iphone$point_value.kilometers.>0,]
da_iphone$point_timestamp <- as.POSIXct(da_iphone$point_timestamp)
da_fitbit$point_timestamp <- as.POSIXct(da_fitbit$point_timestamp)

da_iphone$point_timestamp <- paste0(substring(da_iphone$point_timestamp,1,17),"00")
da_fitbit$point_timestamp <- paste0(substring(da_fitbit$point_timestamp,1,17),"00")

da_full <- rbind(da_iphone, da_fitbit)
da_full <- da_full %>% group_by(point_timestamp) %>% summarise(point_value.kilometers. = mean(point_value.kilometers.))
da_full$point_timestamp <- as.POSIXct(paste0(substring(da_full$point_timestamp,1,17),"00"))

da_full <- da_full  %>%
  full_join(data.frame(point_timestamp = seq(
    from = min(.$point_timestamp),
    to = max(.$point_timestamp),
    by = 'min'))) %>%
  arrange(point_timestamp) %>%
  mutate(point_value.kilometers. = approx(point_value.kilometers., n = n())$y)

##----- Joinging Distance to Blood sugar and Heart rate -----##
bg2 <- bg2 %>% left_join(da_full, by = c("point_timestamp" = "point_timestamp"))  

##----- Aggregating data to 5 min intervals -----##
bg2 <- bg2[1:(nrow(bg2) - nrow(bg2) %% 5),]
bg2$grp <- rep(1:(nrow(bg2)/5), each = 5)
bg2 <- bg2 %>% group_by(grp) %>% 
  summarise(point_value.mg.dL. = mean(point_value.mg.dL., na.rm = T),
            point_value = mean(point_value, na.rm = T),
            point_value.kilometers. = mean(point_value.kilometers., na.rm = T), point_timestamp = min(point_timestamp))

bg2$point_value.mg.dL. <- round(bg2$point_value.mg.dL.)
bg2$point_value <- round(bg2$point_value)

##----- Creating 5 min Future Value as Y -----##
bg2$future <- as.POSIXct(bg2$point_timestamp + 300)
bg2 <- bg2 %>% inner_join(bg2[, colnames(bg2) %in% 
                                c("point_timestamp", "point_value.mg.dL.")], by = c("future" = "point_timestamp"))

colnames(bg2)[colnames(bg2) %in% 
                c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- c("point_value.mg.dL.", "Y")

##----- Getting Moving average of past Blood-Sugar level as a Predictor -----##
bg2 <- na.omit(bg2)
bg2 <- bg2 %>% mutate(maverage = rollapplyr(bg2$point_value.mg.dL., width = 12, FUN = mean, partial = T)) 
mavg <- bg2$maverage
mavg <- c(NA,mavg[1:length(mavg)-1])
bg2$maverage <- mavg
bg2$maverage[1] <- bg2$maverage[2]
bg2$maverage <- round(bg2$maverage)

##----- Splitting data into Train and Test -----##
bg2_train <- bg2[1:max(grep("2017-05-29 ",bg2$point_timestamp)),]
bg2_Test <- bg2[(max(grep("2017-05-29 ",bg2$point_timestamp))+1):max(grep("2017-07-03 ",bg2$point_timestamp)),]
bg2_actTest <- bg[(max(grep("2017-05-29 ",bg$point_timestamp))+1):max(grep("2017-07-03 ",bg$point_timestamp)),]
bg2_actTest$point_timestamp <- as.POSIXct(paste0(substring(bg2_actTest$point_timestamp,1,17),"00"))

bg2_actTest <- bg2_actTest %>% group_by(point_timestamp) %>% 
  summarise(point_value.mg.dL. = mean(point_value.mg.dL.))

##----- Assuming that the test data has a new starting point. Recalculating moving average -----##
bg2_Test$maverage <- NULL
bg2_Test <- bg2_Test %>% mutate(maverage = rollapplyr(bg2_Test$point_value.mg.dL., 
                                                      width = 12, FUN = mean, partial = T)) 

mavg2 <- bg2_Test$maverage
mavg2 <- c(NA,mavg2[1:length(mavg2)-1])
bg2_Test$maverage <- mavg2
bg2_Test$maverage[1] <- bg2_Test$maverage[2]
bg2_Test$maverage <- round(bg2_Test$maverage)

bg2_train$speed <- bg2_train$point_value.kilometers./(5/60)
bg2_Test$speed <- bg2_Test$point_value.kilometers./(5/60)
bg2_train$day_night <- ifelse(as.numeric(substring(bg2_train$point_timestamp,12,13)) >= 7 & 
                                as.numeric(substring(bg2_train$point_timestamp,12,13)) < 19,1,0) 

bg2_Test$day_night <- ifelse(as.numeric(substring(bg2_Test$point_timestamp,12,13)) >= 7 & 
                               as.numeric(substring(bg2_Test$point_timestamp,12,13)) < 19,1,0) 

bg2_train$day_night <- as.factor(bg2_train$day_night)
bg2_Test$day_night <- as.factor(bg2_Test$day_night)
bg2_train <- as.data.frame(bg2_train)
bg2_train <- cbind(bg2_train, dummy("day_night", data = bg2_train))
bg2_train$day_night <- NULL
bg2_train$day_night0 <- NULL

bg2_Test <- as.data.frame(bg2_Test)
bg2_Test <- cbind(bg2_Test, dummy("day_night", data = bg2_Test))
bg2_Test$day_night <- NULL
bg2_Test$day_night0 <- NULL

## Ensuring the 1 min difference in the actual test data matches the interpolated one to calculate actual RMSE
bg2_actTest$point_timestamp <- as.POSIXct(ifelse(substring(bg2_actTest$point_timestamp,16,16) == 2 |  
                                                   substring(bg2_actTest$point_timestamp,16,16) == 7,
                                                 as.POSIXct(bg2_actTest$point_timestamp - 60), 
                                                 ifelse(substring(bg2_actTest$point_timestamp,16,16) == 0 |  
                                                          substring(bg2_actTest$point_timestamp,16,16) == 5,
                                                        as.POSIXct(bg2_actTest$point_timestamp + 60),
                                                        as.POSIXct(bg2_actTest$point_timestamp))), origin='1970-01-01')


##----- Scaling Data for Neural Nets -----##
bg2_train$grp <- NULL
bg2_Test <- bg2_Test[, colnames(bg2_train)]
a <- rbind(bg2_train, bg2_Test)
maxs <- apply(a[, !colnames(a) %in% 
                  c("point_timestamp", "future")], 2, max)
mins <- apply(a[, !colnames(a) %in% 
                  c("point_timestamp", "future")], 2, min)

scaled <- as.data.frame(scale(a[, !colnames(a) %in% 
                                  c("grp", "point_timestamp", "future")], center = mins, scale = maxs - mins))

scaled_train <- scaled[1:nrow(bg2_train),]
scaled_test <- scaled[(nrow(bg2_train)+1):nrow(scaled),]

##----- Fitting neural nets -----#
scaled_test1 <- scaled_test
list_pred <- list()
set.seed(451)
bg_nn <- neuralnet(Y~., scaled_train[, !colnames(scaled_train) %in% 
                                       c("grp", "point_timestamp", "future")], hidden=c(3,2), linear.output = T, 
                   learningrate = 0.1, stepmax = 250000)

plot(bg_nn)

##----- Looping for every 5 mins till 60 mins ------##
for(i in 1:12){
  pr.nn <- compute(bg_nn, scaled_test1[, colnames(scaled_test1) %in% 
                                         c("point_value.mg.dL.", "point_value","point_value.kilometers.", 
                                           "maverage", "speed", "day_night1")])
  
  pr.nn_ <- pr.nn$net.result*(max(a$Y)-min(a$Y))+min(a$Y)
  pr.nn_ <- round(pr.nn_)
  list_pred[[i]] <- pr.nn_
  scaled_test1$maverage <- (scaled_test1$maverage+scaled_test1$point_value.mg.dL.)/2
  scaled_test1$point_value.mg.dL. <- pr.nn$net.result
}

nn_pred <- as.data.frame(do.call(cbind, list_pred))
colnames(nn_pred) <- c(paste(seq(5,60,5), "Mins", sep = "_"))

##----- MSE & Parkes Zone Accuracy -----##
row.names(nn_pred) <- NULL
bg2_actTest$cc <- 1 
bg2_Test <- bg2_Test %>% left_join(bg2_actTest[,colnames(bg2_actTest) %in% c("point_timestamp", "cc")])

bg2_Test <- bg2_Test[,colnames(bg2_Test) %in% c("point_value.mg.dL.", "point_value", 
                                                "point_value.kilometers.", "point_timestamp", "future", "maverage", "speed",                 
                                                "day_night1","cc")]

bg2_Test$point_value.mg.dL. <- ifelse(bg2_Test$cc == 1, bg2_Test$point_value.mg.dL.,
                                      "NA")

## Creating 5 mins actual future Value 
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_5_actual")

## Creating 10 mins actual future Value 
bg2_Test$future10 <- as.POSIXct(bg2_Test$point_timestamp + 600)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future10" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_10_actual")

## Creating 15 mins actual future value
bg2_Test$future15 <- as.POSIXct(bg2_Test$point_timestamp + 900)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future15" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_15_actual")

## Creating 20 mins actual future value
bg2_Test$future20 <- as.POSIXct(bg2_Test$point_timestamp + 1200)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future20" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_20_actual")


## Creating 25 mins actual future value
bg2_Test$future25 <- as.POSIXct(bg2_Test$point_timestamp + 1500)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future25" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_25_actual")

## Creating 30 mins actual future value
bg2_Test$future30 <- as.POSIXct(bg2_Test$point_timestamp + 1800)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future30" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_30_actual")

## Creating 35 mins actual future value
bg2_Test$future35 <- as.POSIXct(bg2_Test$point_timestamp + 2100)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future35" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_35_actual")

## Creating 40 mins actual future value
bg2_Test$future40 <- as.POSIXct(bg2_Test$point_timestamp + 2400)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future40" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_40_actual")

## Creating 45 mins actual future value
bg2_Test$future45 <- as.POSIXct(bg2_Test$point_timestamp + 2700)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future45" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_45_actual")

## Creating 50 mins actual future value
bg2_Test$future50 <- as.POSIXct(bg2_Test$point_timestamp + 3000)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future50" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_50_actual")

## Creating 55 mins actual future value
bg2_Test$future55 <- as.POSIXct(bg2_Test$point_timestamp + 3300)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future55" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_55_actual")

## Creating 60 mins actual future value
bg2_Test$future60 <- as.POSIXct(bg2_Test$point_timestamp + 3600)
bg2_Test <- bg2_Test %>% left_join(bg2_Test[, colnames(bg2_Test) %in% 
                                              c("point_timestamp", "point_value.mg.dL.")], by = c("future60" = "point_timestamp"))

colnames(bg2_Test)[colnames(bg2_Test) %in% c("point_value.mg.dL..x", "point_value.mg.dL..y")] <- 
  c("point_value.mg.dL.","Mins_60_actual")


dd <- colnames(bg2_Test[grep("Mins_", colnames(bg2_Test))])
de <- colnames(bg2_Test)[!colnames(bg2_Test) %in% colnames(bg2_Test[grep("Mins_", colnames(bg2_Test))])]
bg2_Test <- bg2_Test[, c(de,dd)] 

act <- bg2_Test[,c(dd)]
mse_nn <- sum((act - nn_pred)^2, na.rm =T) / ((nrow(act) * ncol(act)) - sum(is.na(act)))

zones_list <- list()
for(i in 1:(nrow(act) - 12) ){
  if(sum(!is.na(as.vector(t(act[i,1:12])))) == 12){
    zones_nn <- getParkesZones(as.vector(t(act[i,1:12])) , as.vector(t(nn_pred[i,1:12])), unit = "gram")  
    zones_list[[i]] <- zones_nn
  }
}
zones_nn <- do.call(rbind, zones_list)
table(zones_nn)/length(zones_nn)*100

##----- Plotting the Clarke Zone for 60 mins into the future -----##
table(zones_nn[,12])/length(zones_nn[,12])*100 ## Parkes Zones for 60 mins into the future predictions
plotParkesGrid(act[!(is.na(act$Mins_60_actual)),12], nn_pred[!is.na(act$Mins_60_actual),12])

