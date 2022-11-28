################################################
# IST687 - Final Project
#
# Group Members: Warren Fernandes, Mark Gopani, Jeffrey Stephens
# 
# Date due: 05/13/2021
#
################################################

# Importing the required libraries
library(tidyverse)
library(imputeTS)

setwd("C:/Users/warre/Downloads") # Set working directory of files

# Loading the data
H1 <- readxl::read_xlsx("H1-Resort.xlsx")
H2 <- readxl::read_xlsx("H2-City.xlsx")

# Viewing the data
View(H1)
View(H2)
sum(is.na(H2$Children))
# Structure of data
str(H1)  #28 columns and 40060 rows
str(H2) #28 columns and 79330 rows

######################################################################
## Cleaning and Transforming

# Replacing the NA values using interpolation function from ImputeTS
table(is.na(H1)) # No NA values
table(is.na(H2)) # We can see that there are 39270 NA values in the all column of H2 dataset.

H2 <- na_interpolation(H2) #Filling the NA values based on the values around them using na_interpolation() function.
sum(is.na(H2)) #We can see that there are no NA values in City dataset anymore
#H2 <- na.omit(H2)

# Casting the following char type variables into factors using as.factor() function and fixing data type of Children in H2.
H1$IsCanceled <- as.factor(H1$IsCanceled)
H1$ReservedRoomType <- as.factor(H1$ReservedRoomType)
H1$AssignedRoomType <- as.factor(H1$AssignedRoomType)
H1$DepositType <- as.factor(H1$DepositType)
H1$CustomerType <- as.factor(H1$CustomerType)
H1$Meal <- as.factor(H1$Meal)
H1$IsRepeatedGuest <- as.factor(H1$IsRepeatedGuest)

H2$IsCanceled <- as.factor(H2$IsCanceled)
H2$ReservedRoomType <- as.factor(H2$ReservedRoomType)
H2$AssignedRoomType <- as.factor(H2$AssignedRoomType)
H2$DepositType <- as.factor(H2$DepositType)
H2$CustomerType <- as.factor(H2$CustomerType)
H2$Meal <- as.factor(H2$Meal)
H2$Children <- as.numeric(H2$Children)
H2$IsRepeatedGuest <- as.factor(H2$IsRepeatedGuest)

# Summary of numeric values in the data sets
summary(H1)
summary(H2)
View(H2)
# Changing the unknown value into check-out.
table(H1$ReservationStatus) #We can see that there is an unknown/error value(`) in the ReservationStatus column of the H1 dataset.
H1$ReservationStatus[13] <- 'Check-Out' #We are assuming the data to be 'check-out' based on the values around which have a similar ReservationStatusDate.

# Factorizing arrival dates into seasons
H1$month_name <- format(H1$`Arrival Date`, "%b")
H1$month <- as.numeric(format(H1$`Arrival Date`, "%m"))
H1$season <- 'Winter'
H1$season[H1$month >= 3 & H1$month <=5] <- 'Spring'
H1$season[H1$month >= 6 & H1$month <=8] <- 'Summer'
H1$season[H1$month >= 9 & H1$month <=11] <- 'Fall'
H1$season <- factor(H1$season, levels = c("Summer", "Fall", "Winter", "Spring"))
summary(H1$season)

H2$month_name <- format(H2$`Arrival Date`, "%b")
H2$month <- as.numeric(format(H2$`Arrival Date`, "%m"))
H2$season <- 'Winter'
H2$season[H2$month >= 3 & H2$month <=5] <- 'Spring'
H2$season[H2$month >= 6 & H2$month <=8] <- 'Summer'
H2$season[H2$month >= 9 & H2$month <=11] <- 'Fall'
H2$season <- factor(H2$season, levels = c("Summer", "Fall", "Winter", "Spring"))
summary(H2$season)
sum(is.na(Hotels$month_name))

# Creating the Visitor Type variable for H1 and H2
H1$VisitorType <- 'Single'
H1$totalVisitors <- H1$Adults + H1$Children + H1$Babies
H1$VisitorType[H1$totalVisitors == 1] <- 'Single'
H2$VisitorType[H2$totalVisitors == 2] <- 'Couple'
H2$VisitorType[H2$totalVisitors > 2] <- 'Family'
H1$VisitorType <- as.factor(H1$VisitorType)
H1$totalVisitors <- NULL
summary(H1$VisitorType)

H2$VisitorType <- 'Single'
H2$totalVisitors <- H2$Adults + H2$Children + H2$Babies
H2$VisitorType[H2$totalVisitors == 1] <- 'Single'
H2$VisitorType[H2$totalVisitors == 2] <- 'Couple'
H2$VisitorType[H2$totalVisitors > 2] <- 'Family'
H2$VisitorType <- as.factor(H2$VisitorType)
H2$totalVisitors <- NULL
summary(H2$VisitorType)

# Calculating the Average Revenue of Stay into a new column
H1$ARS <- (H1$StaysInWeekendNights + H1$StaysInWeekNights) * H1$ADR
H2$ARS <- (H2$StaysInWeekendNights + H2$StaysInWeekNights) * H2$ADR

# Combining the two datasets for comparitive visualizations
H1$HotelType = ("Resort")
H2$HotelType = ("City")
#Hotels = na.omit(Hotels)

Hotels = rbind(H2, H1)
# Average Revenue per stay
#Hotels$ARS <- (Hotels$StaysInWeekendNights + Hotels$StaysInWeekNights) * Hotels$ADR
View(Hotels)
#Hotels$ARS

# Creating Revenue Category for analysis of revenue classes
cuts = quantile(Hotels$ARS, c(0, 0.3, 0.6, 0.8, 1))
table(Hotels$RevenueCategory)
Hotels$RevenueCategory <- "Very High"
Hotels$RevenueCategory[Hotels$ARS >= cuts[1] & Hotels$ARS <  cuts[2]]  = "Low"
Hotels$RevenueCategory[Hotels$ARS >= cuts[2] & Hotels$ARS <  cuts[3]]  = "Moderate"
Hotels$RevenueCategory[Hotels$ARS >= cuts[3] & Hotels$ARS <  cuts[4]] = "High"
#Hotels$RevenueCategory[Hotels$ARS >= cuts[4] & Hotels$ARS <=  cuts[5]] = ""
View(Hotels$RevenueCategory)

## EDA and Visualizations
# Mean Lead Time comparision
H1$TotDaysResort <-H1$StaysInWeekendNights+H1$StaysInWeekNights
LongStayResort<-H1[which(H1$TotDaysResort>=10),]
means<-c(mean(LongStayCity$LeadTime),mean(LongStayResort$LeadTime))
barplot(means, main="Mean Lead Time comparision",xlab="Lead Time",
        names.arg = c("City", "Resort"),
        col = "darkBlue",
        horiz = TRUE)
# Guests staying for 10 days or more 
Totalobs<-c(nrow(LongStayCity),nrow(LongStayResort))
barplot(Totalobs, main="Customers Staying 10 or more Days",xlab="No. of Customers",
        names.arg = c("City", "Resort"),
        col = "darkred",
        horiz = TRUE)
# Histograms and bar graphs of numeric variables and factors
ggplot(data = Hotels,aes(IsCanceled))+ geom_histogram(stat= "count", binwidth = 0.5, col='black', fill='blue', alpha = 0.4) + facet_wrap(~HotelType)
#table(H2$IsCanceled)
ggplot(data = Hotels,aes(x= ARS, y= CustomerType))+ geom_bar(stat= 'identity', fill='blue', alpha = 0.4) + facet_wrap(~HotelType)
ggplot(data = Hotels,aes(IsRepeatedGuest))+ geom_histogram(stat= 'count', binwidth = 0.5, col='black', fill='blue', alpha = 0.4) + facet_wrap(~HotelType)

# Number of booking by month and by season
Hotels%>%group_by(month_name)%>%summarise(Count = n())%>%arrange(-Count)%>%ggplot(aes(x = month_name, y = Count)) +
  geom_bar(stat = 'identity',fill = "dodgerblue") + coord_flip()
Hotels%>%group_by(season)%>%summarise(Count = n())%>%arrange(-Count)%>%ggplot(aes(x = season, y = Count)) +
  geom_bar(stat = 'identity',fill = "dodgerblue") + coord_flip()

# Country wise map of booking data
H1$Country[H1$Country == 'CN'] <- 'CHN' # Fixing the Country code of rows where the country code is not in ISO3 format.
library(rworldmap)
library(RColorBrewer)
#par(mai=c(0,0,1,0),xaxs="i",yaxs="i")# The first line starting par ... below and in subsequent plots simply ensures the plot fills the available space on the page.
#mapDevice(mai=c(0,0,0.5,0)) # mapDevive does the same but also creates a new window to plot the map
mapDevice("x11")
colourPalette <- brewer.pal(6,'YlGnBu')

nPDF <- joinCountryData2Map(Hotels, joinCode = "ISO3", nameJoinColumn = "Country")

mapCountryData( nPDF, nameColumnToPlot="Meal", colourPalette =c('pink1', 'burlywood3', 'midnightblue') , 
                oceanCol='lightblue', missingCountryCol= 'white', mapTitle ='Meal preference of guest by Country', borderCol = 'gray20', lwd = 0.2)
mapCountryData( nPDF, nameColumnToPlot="Meal", colourPalette =c('pink1', 'burlywood3', 'midnightblue') , 
                oceanCol='lightblue', missingCountryCol= 'white', mapTitle ='Meal preference of guest by Country', borderCol = 'gray20', lwd = 0.2 , mapRegion = "europe")

mapCountryData( nPDF, nameColumnToPlot="RevenueCategory", colourPalette = colourPalette, mapRegion = "eurasia", 
                oceanCol='lightblue', missingCountryCol= 'white', mapTitle ='Average Revenue by Country (Categorized)', borderCol = 'gray20', lwd = 0.2  )
mapCountryData( nPDF, nameColumnToPlot="RevenueCategory", colourPalette = colourPalette, mapRegion = "eurasia", 
                oceanCol='lightblue', missingCountryCol= 'white', mapTitle ='Average Revenue by Country (Categorized)', borderCol = 'gray20', lwd = 0.2  )
#identifyCountries(nPDF)
#mapBubbles(nPDF, nameZSize= RevenueCategory, colourPalette='rainbow', oceanCol='lightblue', landCol = "wheat")

guest_country_pct <- Hotels%>%group_by(Country)%>%summarise(Count = n())
guest_country_pct$percent <- guest_country_pct$Count/nrow(Hotels) * 100


sPDF <- joinCountryData2Map(guest_country_pct, joinCode = "ISO3", nameJoinColumn = "Country")

mapParams <- mapCountryData(sPDF, nameColumnToPlot="percent", colourPalette = colourPalette, numCats = 5)
do.call(addMapLegend, c(mapParams
                        ,legendLabels="all"
                        ,legendWidth=0.5
                        ,legendIntervals="page"))

############################################################################
# Association rules
library(imputeTS)
library(Hmisc)
library("readxl")
library(arules)
library(arulesViz)
library(ggplot2)
library(rworldmap)

H2 <- read_excel("/Users/apple/Desktop/H2-City (1).xlsx")
H1 <- read_excel("/Users/apple/Desktop/H1-Resort (2).xlsx")
H2<-na_interpolation(H2)
H1<-na_interpolation(H1)

Hmisc::describe(H2)
Hmisc::describe(H1)

sum(is.na(H1))
sum(is.na(H2))


depor<- as.factor(H1$DepositType)
canr<-as.factor(ifelse(H1$IsCanceled==0,
                       "Not Canceled","Canceled"))
repr<-as.factor(ifelse(H1$IsRepeatedGuest==0,
                       "New Guest","Repeated Guest"))
cuscategr<-'Single'
H1$cuscategtot<-as.numeric(H1$Adults) + as.numeric(H1$Children) + as.numeric(H1$Babies)
cuscategr<-as.factor(ifelse(H1$cuscategtot>2,
                            "Family","Couple"))
prevcatr<-as.factor(ifelse(H1$PreviousCancellations>0,
                           "has cancelled before","Never cancelled before"))
newdfResort<-data.frame(depor,cuscategr,canr,repr,prevcatr)

depo <-as.factor(H2$DepositType)
can<-as.factor(ifelse(H2$IsCanceled==0,
                      "Not Canceled","Canceled"))
rep<-as.factor(ifelse(H2$IsRepeatedGuest==0,
                      "New Guest","Repeated Guest"))
cuscateg<-'Single'
H2$Children <- na_interpolation(H2$Children) 
H2$cuscategtot<-as.numeric(H2$Adults) + as.numeric(H2$Children) + as.numeric(H2$Babies)
cuscateg<-as.factor(ifelse(H2$cuscategtot>2,
                           "Family","Couple"))
prevcat<-as.factor(ifelse(H2$PreviousCancellations>0,
                          "has cancelled before","Never cancelled before"))
newdfCity<-data.frame(depo,cuscateg,can,rep,prevcat)
#summary(H2$cuscategtot)

Hotels = rbind(H2, H1)

Hotels$ARS <- (Hotels$StaysInWeekendNights + Hotels$StaysInWeekNights) * Hotels$ADR
cuts = quantile(Hotels$ARS, c(0, 0.25, 0.5, 0.75, 1))
length(cuts)
Hotels$RevenueCategory <-("Very High")
Hotels$RevenueCategory[Hotels$ARS >= cuts[1] & Hotels$ARS <  cuts[2]]  = "Low"
Hotels$RevenueCategory[Hotels$ARS >= cuts[2] & Hotels$ARS <  cuts[3]]  = "Moderate"
Hotels$RevenueCategory[Hotels$ARS >= cuts[3] & Hotels$ARS <  cuts[4]] = "High"
#table(Hotels$RevenueCategory)
Hotels$recat<-as.factor(Hotels$RevenueCategory)

cuscateg<-'Single'
cuscateg<-as.factor(ifelse(Hotels$cuscategtot>2,
                           "Family","Couple"))
newdfHotels<-data.frame(Hotels$recat,cuscateg,as.factor(Hotels$MarketSegment),as.factor(Hotels$ReservedRoomType),as.factor(Hotels$Meal))
newdfHotelstrans <-as(newdfHotels,"transactions")

Citytrans <- as(newdfCity,"transactions") 
Resorttrans <-as(newdfResort,"transactions")

rulesCity <- apriori(Citytrans, 
                     parameter=list(supp=0.05, conf=0.55), 
                     control=list(verbose=F), 
                     appearance=list(default="lhs",rhs = "can=Canceled"))
#summary(rulesCity)

rulesResort <- apriori(Resorttrans,
                       parameter=list(supp=0.005, conf=0.55),
                       control=list(verbose=F),
                       appearance=list(default="lhs",rhs = "canr=Canceled"))
#summary(rulesResort)

rulesHotel <- apriori(newdfHotelstrans,
                      parameter=list(supp=0.005, conf=0.55),
                      control=list(verbose=F),
                      appearance=list(default="lhs",rhs = "Hotels.recat=Very High"))
#summary(rulesHotel)



goodrulesCity<-rulesCity[quality(rulesCity)$lift>1]
inspect(goodrulesCity[1:5])
summary(goodrulesCity)

goodrulesResort<-rulesResort[quality(rulesResort)$lift>3]
inspect(goodrulesResort[1:5])
summary(goodrulesResort)

goodrulesHotel<-rulesHotel[quality(rulesHotel)$lift>2.5]
inspect(goodrulesHotel[1:5])
summary(goodrulesHotel)


plot(goodrulesCity, method = "two-key plot")   
plot(goodrulesResort, method = "two-key plot") 
plot(goodrulesHotel, method = "two-key plot") 

#########################################################
# LM models with all variables for exploring the variables that explain ADR, IsCanceled, IsRepeatedGuest 
lmADRH1 = lm(formula = ADR~ ., data = Hotels)
summary(lmADRH1)

lmH1 = lm(formula = IsCanceled~ ., data = Hotels)
summary(lmH1)

lmGuestH1 = lm(formula = IsRepeatedGuest~ ., data = Hotels)
summary(lmGuestH1)

# LM models for ADR
lmAdrH1 = lm(formula = ADR~ ReservationStatus + StaysInWeekNights + StaysInWeekendNights + IsRepeatedGuest + 
               PreviousBookingsNotCanceled + Adults + Children + ReservedRoomType + Country + DepositType + CustomerType +TotalOfSpecialRequests, data = Hotels)
summary(lmAdrH1)
## probabiblity of ADR
lmAdrH1$predict <-predict(lmAdrH1)
p = mean(lmAdrH1$predict)
p 

#Linear Model for IsRepeatedguest
lmRguestH1 = lm(formula = IsRepeatedGuest ~ LeadTime+ `Arrival Date`+ ReservationStatus + StaysInWeekendNights 
                +StaysInWeekNights + Adults + Children + Babies + Country +DistributionChannel +PreviousBookingsNotCanceled+ PreviousCancellations, data = Hotels)
summary(lmRguestH1)

# probabiblity of repeated Guest
lmRguestH1$predict <-predict(lmRguestH1) 
p = mean(lmRguestH1$predict)
p

#Linear Model for IsCanceled

lmCanceled = lm(formula = IsCanceled~ LeadTime + CustomerType + Hotel +
                  DepositType + ADR + TotalOfSpecialRequests, data = Hotels)
summary(lmCanceled)

## probabiblity of canceled the booking
lmCanceled$predict <-predict(lmCanceled) 
p = mean(lmCanceled$predict)
p 


#######################################################
#SVM
# Loading the required packages for SVM into the environment
library(kernlab) 
library(e1071) 
library(caret) 

## SVM models for H1 - Resort Hotel dataset
H1svm <- H1[,c('Agent','CustomerType','DepositType','PreviousCancellations','IsRepeatedGuest', 'ReservedRoomType', 'AssignedRoomType','season', 'MarketSegment', 'LeadTime', 'Meal', 'IsCanceled')]
dim(H1svm) 
trainList1 <- createDataPartition(y=H1svm$IsCanceled,p=.40,list=FALSE) 
# p=0.4 because we want 40% of the data in our trainSet for faster computation. Usually a larger partition is given to the trainSet. 
trainSet1 <- H1svm[trainList1,] # 16024 observations of 13 variables 
testSet1 <- H1svm[-trainList1,] #24036 observations of 13 variables 
table(testSet1$IsCanceled) #table functions give the number of classifications for each classification. Canceled = 6592, Not Canceled = 17444
str(trainSet1)
table(H1svm$VisitorType)
# ALL VARIABLES
svm_Resort <- ksvm(IsCanceled ~., data=trainSet1, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_Resort
pred_resort <- predict(svmResort, testSet1)
confusionMatrix(svmResort, testSet1$IsCanceled)

# CUSTOMER TYPE AND PREVIOUS CANCELLATIONS
svm_Res_CustCan <- ksvm(IsCanceled ~ CustomerType + PreviousCancellations, data=trainSet1,C=5,cross=3,prob.model=TRUE)
svm_Res_CustCan
pred4 <- predict(svm_City_ms, newdata=testSet1, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(pred4, testSet1$IsCanceled)
# Number of support vectors - 8258, training error - 0.257036, ce - 0.2516,  accuracy - 75.84%

# MARKET SEGMENT
svm_Res_ms <- ksvm(IsCanceled ~ MarketSegment, data=trainSet1, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_Res_ms
pred4 <- predict(svm_Res_ms, newdata=testSet1, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(pred4, testSet1$IsCanceled)
confusionMatrix(pred4, testSet1$IsCanceled)
# Number of support vectors - 8898, training error - 0.2776, ce - 0.27762,  accuracy - 72.24%

#SEASON
svm_season <- ksvm(IsCanceled ~ season, data=trainSet1, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_season
predOut_season <- predict(svm_season, newdata=testSet1, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(predOut_season, testSet1$IsCanceled)
confusionMatrix(predOut_season, testSet1$IsCanceled)
# Number of Support Vectors - 8898, training error - 0.27762, ce - 0.27762,  accuracy - 72.24%

# COMBINED MODEL (WITH BEST PREDICTORS FROM PREVIOUS MODELS) - PREVIOUS CANCELLATIONS + CUSTOMER TYPE + MARKET SEGMENT
svm_combined <- ksvm(IsCanceled ~ PreviousCancellations + CustomerType + MarketSegment, data=trainSet1, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_combined
pred_combined <- predict(svm_combined, newdata=testSet1, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(pred_combined, testSet1$IsCanceled)
confusionMatrix(pred_combined, testSet1$IsCanceled)

# The Best predictor is Previous Cancellations and Customer Type. All other variables have an accuracy around 72%. Hence, we combined many variables which have an accuracy around 72% and each variable increased about 1% accuracy in the combined variable.


## SVM models for H2 - City Hotel dataset

H2svm <- H2[,c('CustomerType','DepositType','PreviousCancellations','MarketSegment', 'VisitorType', 'ReservedRoomType', 'AssignedRoomType', 'season', 'IsCanceled')]
dim(H2svm) 
trainList2 <- createDataPartition(y=H2svm$IsCanceled,p=.60,list=FALSE) 
trainSet2 <- H2svm[trainList2,] # 47599 observations of 13 variables 
testSet2 <- H2svm[-trainList2,] # 31731 observations of 13 variables 

# ALL VARIABLES
svm_City <- ksvm(IsCanceled ~., data=trainSet2, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_City
# The cross validation error is 0.227 which means that about 22.7% of the instances that the model was learning on was mistaken.
# Larger cross validation error indicates that the model is not good.
pred_City <- predict(svm_City, newdata=testSet2, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
pred_City #Printing the preOut variable to the console.
confusionMatrix(pred_City, testSet1$IsCanceled)
# plot(svm_City, testSet1)

# VISITOR TYPE
svm_City_VisitorType <- ksvm(IsCanceled ~ VisitorType, data=trainSet2, C=5,cross=3,prob.model=TRUE)
svm_City_VisitorType
pred2 <- predict(svm_City_VisitorType, newdata=testSet2, type="response")
table(pred2, testSet2$IsCanceled)
confusionMatrix(pred2, testSet2$IsCanceled)
# Number of Support Vectors - 39724, training error - 0.417, ce - 0.417,  accuracy - 58.27%


# RESERVED ROOM TYPE
svm_City_rrt <- ksvm(IsCanceled ~ ReservedRoomType, data=trainSet2, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_City_rrt
pred3 <- predict(svm_rrt, newdata=testSet2, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(pred3, testSet2$IsCanceled)
confusionMatrix(pred3, testSet2$IsCanceled)
# Number of Support Vectors - 26482, training error - 0.4171, ce - 0.4171,  accuracy - 58.29%

# MARKET SEGMENT
svm_City_ms <- ksvm(IsCanceled ~ MarketSegment, data=trainSet2, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_City_ms
pred4 <- predict(svm_City_ms, newdata=testSet2, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(pred4, testSet2$IsCanceled)
confusionMatrix(pred4, testSet2$IsCanceled)
# Number of Support Vectors - 22134, training error - 0.3486, ce - 0.3486,  accuracy - 64.78%

# SEASON
svm_season2 <- ksvm(IsCanceled ~ season, data=trainSet2, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_season2
predOut_season <- predict(svm_season2, newdata=testSet2, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(predOut_season, testSet2$IsCanceled)
confusionMatrix(predOut_season, testSet2$IsCanceled)
# Number of Support Vectors - 10786, training error - 0.169, ce - 0.169,  accuracy - 83.09%

# COMBINED MODEL (WITH BEST PREDICTORS FROM PREVIOUS MODELS) - PREVIOUS CANCELLATIONS + SEASON + MARKET SEGMENT
svm_combined2 <- ksvm(IsCanceled ~ PreviousCancellations + season + MarketSegment, data=trainSet2, C=5, cross=3, prob.model=TRUE) #Creating a svm model using ksvm() function.
svm_combined2
pred_combined2 <- predict(svm_combined2, newdata=testSet2, type="response") #Predicting the class of data in testSet to validate the model generated using predict() function and passing the model and data as parameters. Storing these prediction in predOut variable.
table(pred_combined2, testSet2$IsCanceled)
confusionMatrix(pred_combined2, testSet2$IsCanceled)
# Number of Support Vectors- 9164, training error - 0.1432, ce - 0.14369,  accuracy - 85.7%
str(testSet2)
# Business Analysis

# Average week and weekend ARS
avg_ARS_week <- sum(H1$ARS[H1$StaysInWeekNights!=0])/NROW(H1$StaysInWeekNights[H1$StaysInWeekNights!=0])
avg_ARS_weekend <- sum(H1$ARS[H1$StaysInWeekendNights!=0])/NROW(H1$StaysInWeekendNights[H1$StaysInWeekendNights !=0])

avg_ARS_weekend / avg_ARS_week #1.24958
# Average ARS generated on weekends is greater than that of week days by 1.25 times.

