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
cb_data <- read.csv("bcodmo_dataset_773466_712b_5843_9069.csv", header = T, na.strings = "NaN")[-1,]
aq_data <- read.csv("aquaculture-farmed-fish-production.csv", header = T)
un_kba_data <- read.csv("UN_kbas_data.csv", header = T)

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
un_kba_data <- un_kba_data[, 15:length(colnames(un_kba_data))]
un_kba_data <- un_kba_data[un_kba_data$type == "Country",]
un_kba_data <- un_kba_data[order(un_kba_data$geoAreaName),]
rownames(un_kba_data) <- NULL
names(un_kba_data)[names(un_kba_data) == "geoAreaName"] <- "Country_Name"

#reformat dataset
years <- seq(2000, 2019, by=1)
un_kba_reformat <- data.frame(Year = rep(years, times=nrow(un_kba_data)))
un_kba_reformat$Country_Name <- rep(un_kba_data$Country_Name, each=length(years))
un_kba_reformat$KBA_value <- 0

kba_values <- un_kba_data %>% 
  select(-starts_with("lowerbound")) %>%
  select(-starts_with("upperbound")) %>%
  select(starts_with("value_"))
kba_values$Country_Name <- un_kba_data$Country_Name

year <- 2000
for(i in 1:nrow(un_kba_reformat)) {
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
head(full_data)

#--------------------------------
#EDA
#--------------------------------
full_data_numeric = subset(full_data, select = -c(Country_Name, Year, Ocean)) #get numeric data

ggplot(data = full_data, aes(x="Average_Bleaching")) + geom_histogram(na.rm = TRUE)
hist(full_data$Average_Bleaching)
hist(full_data$Temperature_Mean)


ggplot(data = full_data, aes(x=Year, y=aquaculture_production)) + geom_line()

boxplot(full_data$KBA_value)

cormatrix <- cor(full_data_numeric)
corrplot(cormatrix)


#-------------------------------
#Modeling
#-------------------------------
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#-------------------------------
#Linear regression analysis
#-------------------------------

reg1 <- lm(Average_Bleaching~., data=full_data_numeric) #bleaching on all variables
summary(reg1)

reg2 <- lm(aquaculture_production~Average_Bleaching, data=full_data_numeric) #bleaching on aquaculutre
summary(reg2)

reg3 <- lm(KBA_value~Average_Bleaching, data=full_data_numeric) #bleaching on kba value
summary(reg3)


#-------------------------------
#Decision tree regression
#-------------------------------
dtree_kba <- rpart(KBA_value~Average_Bleaching, data=full_data_numeric, method = "anova") #bleaching on kba value
rpart.plot(dtree_kba)

dtree_aq <- rpart(aquaculture_production~Average_Bleaching, data=full_data_numeric, method = "anova") #bleaching on kba value
rpart.plot(dtree_aq)

#-------------------------------
#Support Vector Regression
#-------------------------------
library(e1071)
full_data_numeric <- full_data_numeric[complete.cases(full_data_numeric),]
svmmodel_kba <- svm(KBA_value~Average_Bleaching, data = full_data_numeric)
pred_kba <- predict(svmmodel_kba, full_data_numeric)
plot(full_data_numeric$Average_Bleaching, pred_kba)

svmmodel_aq <- svm(aquaculture_production~Average_Bleaching, data = full_data_numeric)
pred_aq <- predict(svmmodel_aq, full_data_numeric)
plot(full_data_numeric$Average_Bleaching, pred_aq)



#-------------------------------
#KNN analysis
#-------------------------------
full_data$Average_Bleaching_Class <- as.factor(cut(full_data$Average_Bleaching, br=c(-5, 5, 20, 80), labels = c("low", "med", "high")))
knn_data <- full_data[4:41]
knn_data$Average_Bleaching <- NULL
knn_data <- knn_data[complete.cases(knn_data),]
#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}
#knn_data[4:39] <- as.data.frame(lapply(knn_data[4:39], normalize))
#summary(knn_data$depth)
ind <- sample(2, nrow(knn_data), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- knn_data[ind==1,]
KNNtest <- knn_data[ind==2,]

KNNpred <- knn(train = KNNtrain[1:36], test = KNNtest[1:36], cl = KNNtrain$Average_Bleaching_Class, k = 14)
KNNpred
table(KNNpred)

#-------------------------------
#Kmeans clustering analysis
#-------------------------------
ggplot(full_data, aes(KBA_value, aquaculture_production, color=Average_Bleaching_Class)) + geom_point(size=3)
set.seed(100)
kmeans_data <- subset(knn_data, select = -c(SSTA_Mean))
dataClusters <- kmeans(kmeans_data[,1:35], 3, nstart=20)
print(dataClusters)
table(dataClusters$cluster, kmeans_data$Average_Bleaching_Class)
clusplot(kmeans_data,dataClusters$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)








