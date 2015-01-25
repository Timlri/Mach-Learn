
pml.training <- read.csv("~/Spectrum-R/Coursera/MachineLearning/pml-training.csv", 
              header = TRUE, stringsAsFactors = TRUE)

pml.testing  <- read.csv("~/Spectrum-R/Coursera/MachineLearning/pml-testing.csv", 
                     header = TRUE, stringsAsFactors = TRUE)


# Here is a nice tool to "see" the missing data in the training set

image(is.na(training), main = "Missing Values", xlab = "Observation", 
      ylab = "Variable", xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(training)), 1:nrow(training), col = "white")
axis(2, seq(0, 1, length.out = ncol(training)), 1:ncol(training), tick = FALSE)


# train1 - simply omitting any rows with NA values shrinks the data set a lot
train1 <- na.omit(training)
# This greatly reduces the data set to 406 observations of 160 variables
good <- sapply(train1, function(x) all(!is.na(x)))
train3  <- train1[, good]



# train2 - A better choice might be to omit any columns that contain NA values
good <- sapply(training, function(x) all(!is.na(x)))
train2  <- training[, good]
# extract the data predictors, omitting the summary records
train4 <- train2[ , c(8:11,21:42,49:51,61:73,83:93)] 



# Now look at the r2 data frame
image(is.na(train2), main = "Missing Values", xlab = "Observation", 
      ylab = "Variable", xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(train2)), 1:nrow(r2), col = "white")
axis(2, seq(0, 1, length.out = ncol(train2)), 1:ncol(r2), tick = FALSE)

# This leaves us with 19,622 observations of 93 variables




set.seed(13234)

ModO <- glm(classe ~ ., data = train4, family = "binomial")

missClass <- function(values, prediction) {
  sum(((prediction > 0.5)*1) != values)/length(values)
}

# missclassification rate for test set
missClass(testing$classe, predict(ModO, testing, type = "response"))

# missclassification rate for training set
missClass(training$classe, predict(ModO,training, type = "response"))


names <- colnames(training)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$classe, plot = "pairs")

###################

pred.corr <- cor(proc.pml.training[, names(proc.pml.training) != "classe"])
pal <- colorRampPalette(c("blue", "white", "red"))(n = 199)
heatmap(pred.corr, col = pal)