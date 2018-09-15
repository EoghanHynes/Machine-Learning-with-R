# ========================================================================
# Wilt
# kNN Script.
# ========================================================================

#install.packages('class')
#install.packages('gmodels')

library(class)
library(gmodels)

# Data already provided as seperate Training and Testing files from UCI site.
wilt <- read.csv("data/WiltTraining.csv")
wilttest <- read.csv("data/WiltTesting.csv")

# Summary of the Wilt data set.
summary(wilt)

# Structure of Wilt data set.
str(wilt)

# Randomise trainging data.
set.seed(1)
wilt_rand <- wilt[order(runif(4339)), ]

# Create Scaled Data Frame and drop class column
wilt_z <- as.data.frame(scale(wilt_rand[-1]))

# Create Scaled Data Frame for testing from test data minus class column
# Test Data already Randomised.
wilt_zt <- as.data.frame(scale(wilttest[-1]))

# Renaming data frames for comprehension
wilt_train <- wilt_z[1:4339, ]
wilt_test <- wilt_zt[1:500, ]

# Get Ordinal classifications
# w = Wilt (Diseased), n = Not Diseased.
wilt_train_class <- wilt_rand[1:4339, 1]
wilt_test_class <- wilttest[1:500, 1]


# Make Predictions for one nearest neighbour gives the best results
# which in this case is safe.
wilt_predictions <- knn(train = wilt_train, test = wilt_test, cl = wilt_train_class, k=1)

# summary of wilt predictions.
summary(wilt_predictions)

CrossTable(x = wilt_test_class, y = wilt_predictions, prop.chisq=FALSE)




# --------------------------------------------------------------------------------------------
# Make Predictions for 2 nearest neighbours gives the best results
wilt_predictions <- knn(train = wilt_train, test = wilt_test, cl = wilt_train_class, k=2)

CrossTable(x = wilt_test_class, y = wilt_predictions, prop.chisq=FALSE)
#---------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------
# Make Predictions for 3 nearest neighbours gives more errors & one false negative
# which if given a weight could be a worse overall result.
wilt_predictions <- knn(train = wilt_train, test = wilt_test, cl = wilt_train_class, k=3)

CrossTable(x = wilt_test_class, y = wilt_predictions, prop.chisq=FALSE)
#---------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------
#5 nearest neighbours
wilt_predictions <- knn(train = wilt_train, test = wilt_test, cl = wilt_train_class, k=5)

CrossTable(x = wilt_test_class, y = wilt_predictions, prop.chisq=FALSE)
#---------------------------------------------------------------------------------------------

