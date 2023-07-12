# codes are written in R studio
```r
# reading the data
data <- read.csv(file.choose())
str(data) 
summary(data)
hist(data$Taxable.Income)
hist(data$City.Population)
```
```r
#since decision tree requires all data to be discretized, therefore, converting taxable income,
#city population,work experience into discrete categories
#using cut function, providing range and labels
data$Taxable.Income <- cut(data$Taxable.Income, breaks =c(10002,30000,99620), labels = c("risky", "good"), right = FALSE)
summary(data$Taxable.Income) #check to see if NA is there or not. should not be
data$City.Population <- cut(data$City.Population, breaks = c(25778,70000,150000,200000), labels = c("low",
"medium", "high"), right = FALSE)
summary(data$City.Population)
data$Work.Experience <- cut(data$Work.Experience, breaks = c(0,15,24,31), labels = c("low","medium", "high"), right = FALSE)
summary(data$Work.Experience)
```
```r
#install c50 and tree for this
#split the data set in 2 categories, i.e. risky and good
risky <- data[data$Taxable.Income=="risky",]
good <- data[data$Taxable.Income=="good",]
#build train and test dataset
train <- rbind(risky[1:96,],good[1:380,])
prop.table(table(train$Taxable.Income)*100)
test <- rbind(risky[97:124,], good[381:476,])
prop.table(table(test$Taxable.Income)*100)
```
```r
#building the model
# installing C50
install.packages("C50")
library("C50")
model1<- C5.0(train[,-3],train$Taxable.Income)
# plotting the model
plot(model1)
```
```r
# Training accuracy
mean(train$Taxable.Income==predict(model1,train)) # 79.83% Accuracy
predtest <- predict(model1, newdata = test) #predicting on test data
mean(predtest==test$Taxable.Income) # 77.41% accuracy 
```

```r

# Checking the accuracy
library(gmodels)
CrossTable(test$Taxable.Income,predtest)
table(test$Taxable.Income, predtest)
library("caret")
confusionMatrix(table(test$Taxable.Income, predtest))
```
