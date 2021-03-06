#Load the two files into R:
dataset <- read.csv("spamdata.csv",header=FALSE,sep=";")
names <- read.csv("spamnames.csv",header=FALSE,sep=";")

#Set the names of the dataset dataframe:
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))

#make column y a factor variable for binary classification (spam or non-spam)
dataset$y <- as.factor(dataset$y)

#get a sample of 1000 rows
sample <- dataset[sample(nrow(dataset), 1000),]

require(caret)
require(kernlab)

#Split the data in dataTrain and dataTest
trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)
dataTrain <- sample[ trainIndex,]
dataTest  <- sample[-trainIndex,]
levels(dataTrain$y) <- list(ham="0", spam="1") 
levels(dataTest$y) <- list(ham="0", spam="1") 
#Create the SVM model:

### finding optimal value of a tuning parameter
sigDist <- sigest(y ~ ., data = dataTrain, frac = 1)
### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))

x <- train(y ~ .,
           data = dataTrain,
           method = "svmRadial",
           preProc = c("center", "scale"),
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  TRUE))

#Evaluate the model
pred <- predict(x,dataTest[,1:57])

acc <- confusionMatrix(pred,dataTest$y)