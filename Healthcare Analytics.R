## Loading the data
quality <- read.csv("quality.csv", header = T)

## Looking at the structure of the data
str(quality)

## Looking at the table of outcome variable
table(quality$PoorCare)

## Accuracy of the baseline model
98/131

## Installing the required package
install.packages("caTools")

## Loading this package
library(caTools)

## Setting the seed
set.seed(88)

## Splitting the data into training and testing sets
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)

## Taking a look at split
split

## Training set
qualityTrain <- subset(quality, split == TRUE)

## Testing set
qualityTest <- subset(quality, split == FALSE)

## Number of rows in the training set
nrow(qualityTrain)

## Number of rows in the testing set
nrow(qualityTest)

## Building a Logistic Regression model
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)

## Looking at the summary of the model
summary(QualityLog)
                  
## Accuracy on the training set
predictTrain <- predict(QualityLog, type = "response")

## Taking a look at the statistical summary of our predictions
summary(predictTrain)

## Checking if we are getting higher probabilities for Poor Care
tapply(predictTrain, qualityTrain$PoorCare, mean)

## Taking threshold value of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)

## Sensitivity
10/25

## Specificity
70/74

## Taking threshold value of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)

## Sensitivity
8/25

## Specificity
73/74

## Taking threshold value of 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)

## Sensitivity
16/25

## Specificity
54/74

## Installing the package ROCR
install.packages("ROCR")

## Loading this library
library(ROCR)

## ROCR curve
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
                
## Making predictions on the test set
predictTest <- predict(QualityLog, type = "response", newdata = qualityTest)

## Using 0.3 as threshold for test set
table(qualityTest$PoorCare, predictTest > 0.3)

## Accuracy
(19 + 6)/(19 + 5 + 2 + 6)

