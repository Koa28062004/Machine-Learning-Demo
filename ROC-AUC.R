# Install Packages
# install.packages("pROC")
# install.packages("randomForest")

# Load library
library(pROC)
library(randomForest)

# Set variables
set.seed(420)
num.samples <- 100
weight <- sort(rnorm(n=num.samples, mean = 172, sd=29))
obese <- ifelse(test=(runif(n=num.samples,) < (rank(weight)/100)), yes=1, no=0)
obese

plot(x = weight, y = obese)

# Logistic Regression
glm.fit = glm(obese~weight, family = binomial)
lines(weight, glm.fit$fitted.values)

# ROC curve
roc(obese, glm.fit$fitted.values, plot=TRUE)

# Get rid of the padding
par(pty = "s") # plot type = square

# Random Forest
rf.model <- randomForest(factor(obese) ~ weight)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4, cex = 0.6)

