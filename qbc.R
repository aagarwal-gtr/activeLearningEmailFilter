

require(itertools2)
require(caret)
require(entropy)
require(kernlab)

#Computes the disagreement measure for each of the unlabeled observations based on the either the predicted class label
vote_entropy <- function(x, type='class', entropy_method='ML') {
  it <- do.call(itertools2::izip, x)
  disagreement <- sapply(it, function(obs) {
    entropy(table(unlist(obs)), method=entropy_method)
  })
  disagreement
}

# Returns a vector of indices of unlabeled observations.
which_unlabeled <- function(y) {
  which(is.na(y))
}

# Returns a vector of indices of labeled observations.
which_labeled <- function(y, return_logical = FALSE) {
  which(!is.na(y))
}

# Splits a matrix and its class labels into labeled and unlabeled pairs.
split_labeled <- function(x, y) {
  x <- as.matrix(x)
  y <- factor(y)
  
  unlabeled_i <- which_unlabeled(y)
  list(x_labeled=x[-unlabeled_i, ],
       y_labeled=y[-unlabeled_i],
       x_unlabeled=x[unlabeled_i, ],
       y_unlabeled=y[unlabeled_i])
}

# Automatic query of an oracle.
query_oracle <- function(i, y_truth) {
  as.vector(y_truth[i])
}

# The 'query by bagging' approach to active learning applies bootstrap aggregating (bagging) by randomly sampling with replacement
query_bagging <- function(x, y, fit_f, predict_f,
                          disagreement="vote_entropy", "post_entropy",
                          num_query=1, C=50, ...) {
  
  disagreement <- match.arg(disagreement)
  
  x <- as.matrix(x)
  y <- factor(y)
  p <- ncol(x)
  split_out <- split_labeled(x, y)
  
  bag_control <- bagControl(
    fit=fit_f,
    predict=predict_f,
    aggregate=disagree_f,
    oob=FALSE,
    allowParallel=TRUE
  )
  
  bag_out <- try(
    bag(x=split_out$x_labeled,
        y=split_out$y_labeled,
        B=C, vars=p, bagControl=bag_control, ...),
    silent=TRUE
  )
  
  if (inherits(bag_out, "try-error")) {
    stop("The following error occurred while training the bagged classifiers:\n",
         bag_out)
  }
  
  disagreement <- predict(bag_out, split_out$x_unlabeled)
  
  # Determines the order of the unlabeled observations by disagreement measure.
  query <- head(order(disagreement, decreasing=TRUE), n=num_query)
  
  list(query=query, disagreement=disagreement)
}

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

fit_f <- function(x, y, ...) {
  caret::train(x, y, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                        classProbs =  TRUE))
}

predict_f <- function(object, x) {
  getElement(predict(object, x), "y")
}

toquery <- query_bagging(x=x, y=y, fit_f=fit_f, predict_f=predict_f, C=10,
                         disagreement="vote_entropy", num_query=5)
queried <- query_oracle(toquery, y)

auxind <- match(queried$y, y$y)
yy <- (rbind(y[,], queried)[-auxind,])
yy <- yy[order(yy$y),]
y[,] <- yy

x <- train(x, y, method = "svmRadial",
           preProc = c("center", "scale"),
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  TRUE))

#Evaluate the model
pred <- predict(x,dataTest[,1:57])

acc <- confusionMatrix(pred,dataTest$y)
