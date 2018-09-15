# ========================================================================
# Wilt
# Decision Tree Script.
# ========================================================================

#install.packages('C50')
#install.packages('gmodels')
library(C50)
library(gmodels)

# --------------------------------------------
# Data Analysis
# --------------------------------------------

# Data already provided as seperate Training and Testing files from UCI site.
wilt <- read.csv("data/WiltTraining.csv") 

# to access attributes directly
attach(wilt)

# Summary of the Wilt data set classifications.
summary(class)

# table of Mean_Green values.
table(Mean_Green)

# table of SD_pan values.
table(SD_pan)

# summary of SD_pan values
summary(SD_pan)

# Create a histogram of mean red values
# to show mean red as an indication
# of disease
hist(Mean_Red)

# Create a histogram of GLCM-pan values
# to see impact on classification
hist(GLCM_pan)

# Histogram of Mean_NIR impact on classification.
hist(Mean_NIR)

# check the proportion of class variable
prop.table(table(wilt$class))

# UCI provides test file seperately.
# Already random
wilttest <- read.csv("data/WiltTesting.csv")

# Attach to reference attributes directly.
attach(wilttest)

prop.table(table(class))


# -----------------------------------------------
# Use C5 algorithm to create a Tree Map Model
# -----------------------------------------------

# Re-Attach wilt to reference its class attribute directly.
attach(wilt)

# Make a Tree Model for Wilt predictions.
WiltTreeModel <- C5.0(class ~ ., data = wilt)

# Plot the Tree Model.
plot(WiltTreeModel)

# Summaries the Wilt Tree Model.
summary(WiltTreeModel)




# ---------------------------------------------
# Use Model to make predictions using test data
# ---------------------------------------------


# Test Model Predictions using the test data
Wilt_pred <- predict(WiltTreeModel, wilttest)




# ---------------------------------------------
# Evaluate the Model using a Confusion Matrix.
# ---------------------------------------------

# Re Attach wilttest to reference class directly.
attach(wilttest)

# Create Confusion Matrix for evaluation.
CrossTable(Wilt_pred, wilttest$class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted wilt', 'actual wilt'))




# --------------------------------------------
# Create error matrix to reduce false negatives.
# --------------------------------------------

# Presume false negatives are more unwanted.
# error_cost <- matrix(c(0, 1, 45, 0), nrow = 2)
error_cost <- matrix(c(0, 1, 20, 0), nrow = 2)

# Display Cost Matrix.
error_cost 


# apply cost matrix to the tree
wilt_cost_model <- C5.0(class ~ ., data = wilt, costs = error_cost)

# Plot the Tree Model.
plot(wilt_cost_model)

# Summaries the Wilt Tree Model.
summary(wilt_cost_model)


wilt_cost_predictions <- predict(wilt_cost_model, wilttest)

CrossTable(wilt_cost_predictions, wilttest$class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted wilt', 'actual wilt'))

