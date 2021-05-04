#Final Project Code
#Sebastian Castillo-Sanchez

library(timeDate)
library(lubridate)
library(ggplot2)
library(dplyr)
library(cluster)
library(rpart)
library(rpart.plot)
library(class)
library(corrplot)
library(caret)

#Load in datasets
cb_data <- read.csv("data/bcodmo_dataset_773466_712b_5843_9069.csv", header = T, na.strings = "NaN")[-1,]
aq_data <- read.csv("data/aquaculture-farmed-fish-production.csv", header = T)
un_kba_data <- read.csv("data/UN_kbas_data.csv", header = T)

#-------------------------------------
#Data cleaning for Coral Bleaching data
#-------------------------------------

#view data
head(cb_data)
str(cb_data)

#change specific columns to numeric data
direction_columns <- c("latitude", "longitude")
cb_data[, direction_columns] <- lapply(direction_columns, function(x) as.numeric(cb_data[[x]]))
cb_data[, 14:ncol(cb_data)] <- lapply(14:ncol(cb_data), function(x) as.numeric(cb_data[[x]]))
#change date column to date type 
cb_data$Date <- as.Date(as.character(cb_data$Date), format = "%m/%d/%Y")
cb_data$Year <- year(cb_data$Date) #get specific year
str(cb_data)

cb_data_cond <- subset(cb_data, select = -c(ID, latitude, longitude, Date, Date2, Realm,
                                            Ecoregion, State_Island_Province, City_Town,
                                            City_Town_2, City_Town_3)) #condense data
cb_data_cond <- cb_data_cond[order(cb_data_cond$Country_Name, cb_data_cond$Year),] #sort data by country and year
cb_data_cond <- cb_data_cond[cb_data_cond$Year >= 2000,] #get data 2000 or later
rownames(cb_data_cond) <- NULL

#----------------------------------
#Data Cleaning for Aquaculture data
#---------------------------------

#view data
head(aq_data)
str(aq_data)

#removing rows with no country code
aq_data[aq_data==""] <- NA
aq_data <- aq_data[complete.cases(aq_data),] #removing null values
aq_data_sorted <- aq_data[order(aq_data$Entity),] #sorting data by country ("Entity")
aq_data_sorted <- subset(aq_data_sorted, select = -c(Code)) #remove code column
aq_data_sorted <- aq_data_sorted[aq_data_sorted$Year >= 2000,] #year 2000 or over
names(aq_data_sorted)[names(aq_data_sorted) == "Entity"] <- "Country_Name"

#----------------------------------
#Data Cleaning for UN KBA data
#---------------------------------
#view data
head(un_kba_data)
str(un_kba_data)
un_kba_data <- un_kba_data[, 15:length(colnames(un_kba_data))] #select only needed columns
un_kba_data <- un_kba_data[un_kba_data$type == "Country",] #select only country types
un_kba_data <- un_kba_data[order(un_kba_data$geoAreaName),] #order countries
rownames(un_kba_data) <- NULL
names(un_kba_data)[names(un_kba_data) == "geoAreaName"] <- "Country_Name" #rename column

#reformat dataset
years <- seq(2000, 2019, by=1)
un_kba_reformat <- data.frame(Year = rep(years, times=nrow(un_kba_data))) 
un_kba_reformat$Country_Name <- rep(un_kba_data$Country_Name, each=length(years)) #years for country
un_kba_reformat$KBA_value <- 0 #initialize value

kba_values <- un_kba_data %>% 
  select(-starts_with("lowerbound")) %>%
  select(-starts_with("upperbound")) %>%
  select(starts_with("value_")) #subset data
kba_values$Country_Name <- un_kba_data$Country_Name #add country names

year <- 2000
for(i in 1:nrow(un_kba_reformat)) { #add kba value
  un_kba_reformat$KBA_value[i] <- kba_values[(kba_values$Country_Name == un_kba_reformat$Country_Name[i]), colnames(kba_values)[grepl(as.character(year), colnames(kba_values))]]
  if(year == 2019) year = 2000 else year = year + 1
}

#----------------------------------
#Data Synthesis/Merging
#---------------------------------
cb_data_merge <- cb_data_cond %>%
  group_by(Country_Name, Year) %>%
  summarise(Ocean = max(Ocean), across(!contains("Ocean"), mean)) #group cb data with dplyr

first_merge <- cb_data_merge %>% inner_join(aq_data_sorted, by=c("Country_Name", "Year")) #merge cb and aq data by country and year

full_data <- first_merge %>% inner_join(un_kba_reformat, by=c("Country_Name", "Year")) %>%
  rename(aquaculture_production=Aquaculture.production..metric.tons.)#merge all three datasets

full_data <- full_data %>% select(-ends_with("Deviation")) %>%
  select(-ends_with("Maximum")) %>% select(-ends_with("Minimum")) #subset data

head(full_data)



#--------------------------------
#EDA
#--------------------------------
full_data_numeric = subset(full_data, select = -c(Country_Name, Ocean, SSTA_Mean)) #get numeric data

#mean aq by year
mean_data_aq <- group_by(full_data_numeric, Year) %>% summarise(Average_Bleaching=mean(Average_Bleaching, na.rm=TRUE))
ggplot(data = mean_data_aq, aes(x=Year, y=Average_Bleaching)) + geom_line(color="Blue") + geom_point(color="Blue") + labs(title="Average Bleaching per Year", y="Average Bleaching (%)")

#mean kba value by year
mean_data_kba <- group_by(full_data_numeric, Year) %>% summarise(KBA_value=mean(KBA_value, na.rm=TRUE))
ggplot(data = mean_data_kba, aes(x=Year, y=KBA_value)) + geom_line(color="Green") + geom_point(color="Green") + labs(title="Average KBA per Year", y="Average KBA (%)")


par(mfrow=c(2,2)) #histograms
hist(full_data_numeric$Temperature_Mean, col = c("Red"))
hist(full_data_numeric$Windspeed, col = c("Green"))
hist(full_data_numeric$SSTA, col = c("Blue"))
hist(full_data_numeric$depth, col = c("Black"))

#boxplots
boxplot(full_data$KBA_value) 
ggplot(data=full_data, aes(KBA_value, depth, Windspeed, Average_Bleaching)) + geom_boxplot()

#correlation matrix
cormatrix <- cor(full_data_numeric, use = "pairwise.complete.obs")
corrplot(cormatrix, title="Correlation Matrix", method = "circle")


#-------------------------------
#Modeling
#-------------------------------
set.seed(123) #set random seed
#create cross-validation
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
full_data_numeric <- full_data_numeric[complete.cases(full_data_numeric),] #remove null values

#create train and test sets
split <- sample(1:nrow(full_data_numeric), size = round(0.7 * nrow(full_data_numeric)), replace=F)
train <- full_data_numeric[split,]
test <- full_data_numeric[-split,]
#-------------------------------
#Linear regression analysis
#-------------------------------

reg1 <- lm(Average_Bleaching~., data=train) #bleaching on all variables
summary(reg1)

reg2 <- lm(KBA_value~., data=train) #bleaching on kba value
summary(reg1)

reg3 <- lm(aquaculture_production~., data=train) #bleaching on aquaculutre
summary(reg3)


#two other regression values for only one independent variable
summary(lm(aquaculture_production~Average_Bleaching, data=train))
summary(lm(KBA_value~Average_Bleaching, data=train))

#-------------------------------
#Decision tree regression
#-------------------------------
#kba
dtree_kba <- train(KBA_value~., data = full_data_numeric, method="rpart", trControl=trControl)
rpart.plot(dtree_kba$finalModel)
#aq production
dtree_aq <- train(aquaculture_production~., data = full_data_numeric, method="rpart", trControl=trControl)
rpart.plot(dtree_aq$finalModel)

#-------------------------------
#Support Vector Regression
#-------------------------------
library(e1071)
#kba
svmmodel_kba <- train(KBA_value~., data = full_data_numeric, method="svmLinear2", trControl=trControl)
svmmodel_kba
#aq production
svmmodel_aq <- train(aquaculture_production~., data = full_data_numeric, method="svmLinear2", trControl=trControl)
svmmodel_aq



#-------------------------------
#Predictions
#-------------------------------
#par(mfrow=c(3,1))
library(cowplot)

#predictions for linear regression
pred_reg1 <- predict(reg1, test)
varImp(reg1) #view variable importance

pred_reg2 <- predict(reg2, test)
varImp(reg2)

pred_reg3 <- predict(reg3, test)
varImp(reg3)
#plotting predictions
lr1 <- qplot(Average_Bleaching, pred_reg1, colour=Year, data=test)
lr2 <- qplot(KBA_value, pred_reg2, colour=Average_Bleaching, data=test)
lr3 <- qplot(aquaculture_production, pred_reg3, colour=Average_Bleaching, data=test)
plot_grid(lr1, lr2, lr3, labels=c("Fit 1" ,"Fit 2", "Fit 3"))

#predictions for decision trees and ploting
pred_dt1 <- predict(dtree_kba, test)
pred_dt2 <- predict(dtree_aq, test)
dt1 <- qplot(KBA_value, pred_dt1, colour=Average_Bleaching, data=test)
dt2 <- qplot(aquaculture_production, pred_dt2, colour=Average_Bleaching, data=test)
plot_grid(dt1, dt2, labels=c("Fit 1" ,"Fit 2"))

#predictions for svm regression and plotting
pred_svm1 <- predict(svmmodel_kba, test)
pred_svm2 <- predict(svmmodel_aq, test)
svm1 <- qplot(KBA_value, pred_svm1, colour=Average_Bleaching, data=test)
svm2 <- qplot(aquaculture_production, pred_svm2, colour=Average_Bleaching, data=test)
plot_grid(svm1, svm2, labels=c("Fit 1" ,"Fit 2"))


#-------------------------------
#Kmeans clustering analysis
#-------------------------------
#plot bleaching class
full_data$Average_Bleaching_Class <- as.factor(cut(full_data$Average_Bleaching, br=c(-5, 5, 20, 80), labels = c("low", "med", "high")))
ggplot(full_data, aes(KBA_value, aquaculture_production, color=Average_Bleaching_Class)) + geom_point(size=3)

#Perform kmeans
norm_data <- scale(na.omit(full_data_numeric))
within <- (nrow(norm_data)-1)*sum(apply(norm_data,2,var)) #find number of wss
for (i in 2:5) within[i] <- sum(kmeans(norm_data, centers=i)$withinss)
plot(1:5, within, type="b", xlab="Number of Clusters", ylab="WSS") 

set.seed(100)
kmeans_cluster <- kmeans(norm_data, 3) #using kmeans function
aggregate(norm_data,by=list(kmeans_cluster$cluster),FUN=mean)
norm_data <- data.frame(norm_data, kmeans_cluster$cluster)

#plot clusters
clusplot(norm_data, kmeans_cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)







