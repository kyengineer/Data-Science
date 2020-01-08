#Install package to read csv file

library(tidyverse)
library(caret)
library(dplyr)
library(randomForest)
#Read data into dataframe
data1 <- read_csv("water-treatment.data")
str(data1)
#Read column name into a data frame
data1
#Column names
colnames(data1) <- c("Date", "Q_E","ZN_E","PH_E","DBO_E","DQO_E","SS_E" ,"SSV_E" ,"SED_E" ,"COND_E","PH_P" ,"DBO_P","SS_P","SSV_P","SED_P","COND_P","PH_D","DBO_D","DQO_D","SS_D","SSV_D","SED_D","COND_D","PH_S","DBO_S","DQO_S","SS_S","SSV_S","SED_S","COND_S","RD_DBO_P","RD_SS_P","RD_SED_P","RD_DBO_S","RD_DQO_S","RD_DBO_G","RD_DQO_G","RD_SS_G","RD_SSE_G")
#Extract Date column to add back later
date_column <- select(data1, "Date")
date_column

#Convert character columns into data 
data2 <- data1 %>% map_if(is.character, as.numeric)
view(data2)
#Add Date column in date format
data3 <- bind_cols(date_column, data2)
head(data3)
#Remove Date1 column tha is all NAa
data3$Date1 <- NULL #delete date1 column that was convered to NA
head(data3)
dim(data3)
#Rename the dataframe we are working with to water treatment
water_treatment <- data3 

View(water_treatment)
#As you can see from the view there are many NAs to be removed
#Save the water treatment data so it does not have to be regernerated
save(water_treatment, file = "water_treatment.RDA")
#Check file after saving
load("water_treatment.RDA")
str(water_treatment)
class(water_treatment)
#Remove observations with NA
wt <- water_treatment %>% drop_na()
dim(wt)
str(wt)
#Running str shows all the data is numeric accept the Date column is charactor

set.seed(3456)
#We are predicting BOD  
wt_BOD <- select(wt, Q_E, DBO_E, DBO_S, RD_DBO_P, RD_SS_P, RD_SED_P, RD_DBO_S, RD_DQO_S)
View(wt_BOD)
str(wt_BOD)


mean(wt_BOD$DBO_E)
mean(wt_BOD$DBO_S)

sd(wt_BOD$DBO_E)
sd(wt_BOD$DBO_S)

wt_BOD$DBO_E - wt_BOD$DBO_S

ggplot(data = wt_BOD, aes(y=DBO_S)) +
  geom_boxplot()
  

ggplot(data = wt_BOD) +
  geom_point(mapping = aes(y = DBO_S, x = DBO_E)) +
  xlab("INPUT BOD") +
  ylab("OUTPUT BOD")

ggplot(data = wt_BOD) +
  geom_point(mapping = aes(y = DBO_S, x = Q_E)) +
  xlab("FLOW") +
  ylab("OUTPUT BOD")

ggplot(data = wt_BOD) +
  geom_point(mapping = aes(y = DBO_E, x = RD_DBO_P)) +
  xlab("BOD PRIMARY SETTLER") +
  ylab("INPUT BOD")

ggplot(data = wt_BOD) +
  geom_point(mapping = aes(y = DBO_S, x = RD_DBO_P)) +
  xlab("BOD PRIMARY SETTLER") +
  ylab("OUTPUT BOD")

ggplot(data = wt_BOD) +
  geom_point(mapping = aes(y = DBO_S, x = RD_SS_P)) +
  xlab("SS PRIMARY SETTLER") +
  ylab("OUTPUT BOD")

#split database into test and train
test_index <- createDataPartition(wt_BOD$Q_E,times = 1, p = 0.8, list = FALSE)
train_water <- wt_BOD[test_index,]
test_water <- wt_BOD[-test_index,]
#Use head function to review dataframe
head(test_water)
head(train_water)


#create two models for testing glm and knn 
fit_lm <- train(DBO_S ~ ., method = "lm", data = train_water )
fit_glm <- train(DBO_S ~ ., method = "glm", data = train_water )
fit_knn <- train(DBO_S ~ ., method = "knn", data = train_water, tuneGrid = data.frame(k = seq(2, 201, 5)) )
fit_kknn <- train(DBO_S ~ ., method = "kknn", data = train_water)
fit_brnn <- train(DBO_S ~ ., method = "brnn", data = train_water)
fit_rf <- train(DBO_S ~ ., method = "rf", data = train_water)

fit_lm
fit_glm 
fit_knn
fit_kknn
fit_brnn
fit_rf


summary(fit_lm)
summary(fit_glm)
summary(fit_knn)
summary(fit_kknn)
summary(fit_brnn)
summary(fit_rf)

y_hat_wt_BOD_lm <- predict(fit_lm, data = test_water)
y_hat_wt_BOD_glm <- predict(fit_glm, data = test_water)
y_hat_wt_BOD_knn <- predict(fit_knn, data = test_water)
y_hat_wt_BOD_kknn <- predict(fit_kknn, data = test_water)
y_hat_wt_BOD_brnn <- predict(fit_brnn, data = test_water)
y_hat_wt_BOD_rf <- predict(fit_rf, data = test_water)


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

RMSE(test_water$DBO_S, y_hat_wt_BOD_lm)
RMSE(test_water$DBO_S, y_hat_wt_BOD_glm)
RMSE(test_water$DBO_S, y_hat_wt_BOD_knn)
RMSE(test_water$DBO_S, y_hat_wt_BOD_kknn)
RMSE(test_water$DBO_S, y_hat_wt_BOD_brnn)
RMSE(test_water$DBO_S, y_hat_wt_BOD_rf)


rlm <- RMSE(test_water$DBO_S, y_hat_wt_BOD_lm)
rglm <- RMSE(test_water$DBO_S, y_hat_wt_BOD_glm)
rknn <- RMSE(test_water$DBO_S, y_hat_wt_BOD_knn)
rkknn <- RMSE(test_water$DBO_S, y_hat_wt_BOD_kknn)
rbrnn <- RMSE(test_water$DBO_S, y_hat_wt_BOD_brnn)
rrf <- RMSE(test_water$DBO_S, y_hat_wt_BOD_rf)

temp <- data.frame(rlm,rglm, rknn, rkknn, rbrnn, rrf)
temp





