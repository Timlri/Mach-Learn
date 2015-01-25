rm(list = ls())

library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(MASS)
library(corrplot)

# Read in the data
pml.training <- read.csv("~/Spectrum-R/Coursera/MachineLearning/pml-training.csv", 
                         header = TRUE, stringsAsFactors = TRUE)

pml.testing  <- read.csv("~/Spectrum-R/Coursera/MachineLearning/pml-testing.csv", 
                         header = TRUE, stringsAsFactors = TRUE)

# Display the image showing empty values
image(is.na(pml.training), main = "Missing Values", xlab = "Observation", 
      ylab = "Variable", xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(pml.training)), 1:nrow(pml.training), col = "white")
axis(2, seq(0, 1, length.out = ncol(pml.training)), 1:ncol(pml.training), tick = FALSE)

# Remove useless predictors
pml.training <- pml.training[ , c(8:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:160)]
pml.testing  <- pml.testing[ , c(8:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:160)]

# Divide up the data into training and testing
tr.index <- createDataPartition(y = pml.training$classe, p = 0.80, list = FALSE)
training <- pml.training[ tr.index, ]
testing  <- pml.training[ -tr.index, ]

# Look for near zero-variance predictors
nzv <- nearZeroVar(training, saveMetrics= FALSE)

# Remover highly correlated predictors
M <- cor(training[1:52])
corrplot(M, order = "original", tl.pos = "n")
hCor <- findCorrelation(M, 0.80)
training <- training[, -hCor]
testing  <- testing[, -hCor]

# Preprocess with PCA
xPre <- preProcess(training[, 1:40], method = "pca")
training.pca  <- predict(xPre, training[, 1:40])
testing.pca   <- predict(xPre, testing[ , 1:40])

# Run an LDA model
set.seed(8181)
rp2    <- train(training$classe ~ ., data = training.pca, method="lda")

# Display the accuracy
rp2$results[1,2]