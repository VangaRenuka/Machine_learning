# Reading data
housing <- read.csv("USA_Housing.csv",header = TRUE, sep = ",")
#to remove un necessary column from the data frame
housing$Address<-NULL
# Print top 6 observations
head(housing)

library(ggplot2)
# Building histogram
ggplot(data=housing, aes(Price)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

summary(housing)
colnames(housing)

# loading psych package for stats
#intall.packages("psych")
library(psych)
psych::describe(housing)

#Checking Outliers Using Boxplots
#install.packages('reshape')
library(reshape)
meltData <- melt(housing)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")

#Apart from Area of Number of Bedrooms all other variables seem to have outliers


#Correlation Matrix Visualization -
require(corrgram)
corrgram(housing, order=TRUE)


# devidingdata setin training and test sets
library(caret)
# Split data into train and test
index <- createDataPartition(housing$Price, p = .70, list = FALSE)
train <- housing[index, ]
test <- housing[-index, ]


# Checking the dim of train
library(caret)
# Split data into train and test
index <- createDataPartition(housing$Price, p = .70, list = FALSE)
train <- housing[index, ]
test <- housing[-index, ]


# Split data into train and test
library(caret)

index <- createDataPartition(housing$Price, p = .70, list = FALSE)
train <- housing[index, ]
test <- housing[-index, ]

# Checking the dim of train
dim(train)

#building linear model
# Taining model
lmModel <- lm(Price ~ . , data = train)
# Printing the model object
print(lmModel)

# Checking model statistics
summary(lmModel)


# Using AIC function
AIC(lmModel)
# Using BIC function
BIC(lmModel)  


# Checking model object for actual and predicted values
names(lmModel)


library(Metrics)
rmse(actual = train$Price, predicted = lmModel$fitted.values)

mse(actual = train$Price, predicted = lmModel$fitted.values)

# Histogram to check the distribution of errors
hist(lmModel$residuals, color = "grey")
plot(lmModel)


# Predicting Price in test dataset
test$PreditedPrice <- predict(lmModel, test)
# Priting top 6 rows of actual and predited price
head(test[ , c("Price", "PreditedPrice")])


actual <- test$Price
preds <- test$PreditedPrice
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq
