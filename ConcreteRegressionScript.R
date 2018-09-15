# ========================================================================
# Concrete Compressive Strength.
# Linear Regression Script.
# ========================================================================

#install.packages('GGally')

# Used for mutli-attribute scatter plot matrix.
library(GGally)

# Read in data file.
ConcData <- read.csv("data/Concrete_Data.csv")

# Structure of the data.
str(ConcData)

# View Data
ConcData


# ========================================================================
# Analysis of the data.
# ========================================================================

# ConcData Attributes
names(ConcData)

# The beginning of the Data Set
head(ConcData)

# Table of Cement attribute.
table(ConcData$Cement)

# Table of Water attribute.
table(ConcData$Water)

# -----------------------------------------------------
# Indiviatual Scatter Plots & Correlations of Concrete
# Compressive Strength with each significant attribute.
# -----------------------------------------------------

# To access attributes directly.
attach(ConcData)

# Strong Correlation between the amount
# of cement in the mix and Compressive Strength.
plot(CompressiveStrength ~ Cement,  data = ConcData)
abline(lm(CompressiveStrength ~ Cement,  data = ConcData))
cor(CompressiveStrength, Cement)

# Weak correltion with furnace slag component.
plot(CompressiveStrength ~ FurnaceSlag,  data = ConcData)
abline(lm(CompressiveStrength ~ FurnaceSlag,  data = ConcData))
cor(CompressiveStrength, FurnaceSlag)

plot(CompressiveStrength ~ FlyAsh,  data = ConcData)
abline(lm(CompressiveStrength ~ FlyAsh,  data = ConcData))
# Negative Correlation between fly ash component
# and Concrete Compressive strength.
# Fly Ash negatively affects Compressive Strength.
cor(CompressiveStrength, FlyAsh)

plot(CompressiveStrength ~ Water,  data = ConcData)
abline(lm(CompressiveStrength ~ Water,  data = ConcData))
# Too much water in the mix affect Compressive
# Strength negatively.
cor(CompressiveStrength, Water)

# Positive Correlation for plasticizer.
plot(CompressiveStrength ~ Superplasticizer,  data = ConcData)
abline(lm(CompressiveStrength ~ Superplasticizer,  data = ConcData))
cor(CompressiveStrength, Superplasticizer)

plot(CompressiveStrength ~ CoarseAggregate,  data = ConcData)
abline(lm(CompressiveStrength ~ CoarseAggregate,  data = ConcData))
cor(CompressiveStrength, CoarseAggregate)

plot(CompressiveStrength ~ FineAggregate,  data = ConcData)
abline(lm(CompressiveStrength ~ FineAggregate,  data = ConcData))
cor(CompressiveStrength, FineAggregate)

# Compressive Strength improves as Concrete Cures.
plot(CompressiveStrength ~ Age,  data = ConcData)
abline(lm(CompressiveStrength ~ Age,  data = ConcData))
cor(CompressiveStrength, Age)


#-----------------------------------------------------
# Correlations of significant components with
# Compresive Strength in descending order.
# ----------------------------------------------------

cor(CompressiveStrength, Age + Cement)

cor(CompressiveStrength, Superplasticizer + Cement)

cor(CompressiveStrength, Water + Cement)

cor(CompressiveStrength, Superplasticizer + Age)

cor(CompressiveStrength, Water + Age)

# The three most significant concrete componenets.
cor(CompressiveStrength, Superplasticizer + Cement + Age)


# ----------------------------------------------------
# Attribute Correlation Matrix using GGally.Pairs
# ----------------------------------------------------

# Scatterplot Matrix of all attributes.
pairs(~CompressiveStrength+FurnaceSlag+FlyAsh+Water+Cement
      +Superplasticizer+Age+CoarseAggregate+FineAggregate, data=ConcData)

ggpairs(with(ConcData, data.frame(CompressiveStrength, Cement, FurnaceSlag, 
                                  FlyAsh, Water, Superplasticizer, 
                                  CoarseAggregate, FineAggregate, Age)))



# ==================================================================================
# Create a Linear Model based on all Data Set Attributes.
# ==================================================================================

# A concrete mixture ratio of 1 part cement, 3 parts sand
# and 3 parts aggregate will produce a concrete mix of approximately 
# 3000 psi*
# *http://www.everything-about-concrete.com/concrete-mixing-ratios.html
# 
# 3000 psi = 20.68427 MPa.

ConcDatalm = lm(CompressiveStrength ~  Cement + FurnaceSlag + FlyAsh + Water
                + Superplasticizer + CoarseAggregate + FineAggregate + Age, data=ConcData) 

# Sumarise the Linear Model
summary(ConcDatalm)

# Remove insignificant Attributes to make the linear model more 
# efficient without changing the result too much.
ConcDatalm = lm(CompressiveStrength ~  Cement + FurnaceSlag + FlyAsh + Water
                + Superplasticizer + Age, data=ConcData) 

# Sumarise new Linear Model.
summary(ConcDatalm)

attributes(ConcDatalm)

ConcDatalm$coefficients

ConcDatalm$rank



# ===================================================================================
# Use linear model to make predictions for new data values.
# ===================================================================================

# Values based on exisitng values.
summary(ConcData)

# New Data as data frame based on existing Mean values 
# accross all significant attributes taken from data summary:
newdata = data.frame(Cement=281.2, FurnaceSlag=22, FlyAsh=54.19,
                     Water=181.6, Superplasticizer=6.205, Age=35.82) 

# In Meagapascals
predict(ConcDatalm, newdata)  

# what is the coefficient of determinations
summary(ConcDatalm)$r.squared 

# Summary of Confidence
predict(ConcDatalm, newdata, interval="confidence")
predict(ConcDatalm, newdata, interval="predict")



# New Data as data frame based on existing Min values 
# accross all significant attributes taken from data summary:
newdata = data.frame(Cement=102.0, FurnaceSlag=0, FlyAsh=0,
                     Water=121.8, Superplasticizer=0, Age=1) 

# In Megapscals
predict(ConcDatalm, newdata)

# Summary of Confidence.
predict(ConcDatalm, newdata, interval="confidence")
predict(ConcDatalm, newdata, interval="predict")



# New Data as data frame based on existing Max values 
# accross all significant attributes taken from data summary:
newdata = data.frame(Cement=540.0, FurnaceSlag=359.4, FlyAsh=200.10,
                     Water=247.0, Superplasticizer=32.2, Age=365) 

# In Megapascals
predict(ConcDatalm, newdata)

# Summary of Confidence.
predict(ConcDatalm, newdata, interval="confidence")
predict(ConcDatalm, newdata, interval="predict")




# Tweak New Data to find optimal values in KG/m3 & days accross all 
# significant components relating to Compressive Strength:
newdata = data.frame(Cement=550, FurnaceSlag=350, FlyAsh=100,
                     Water=200, Superplasticizer=35, Age=365) 

# Megapascals.
predict(ConcDatalm, newdata)

# Summary of Confidence.
predict(ConcDatalm, newdata, interval="confidence")
predict(ConcDatalm, newdata, interval="predict")




# Tweak New Data to find optimal values in KG/m3 & days accross all 
# significant components relating to Compressive Strength:
newdata = data.frame(Cement=600, FurnaceSlag=350, FlyAsh=50,
                     Water=150, Superplasticizer=40, Age=365) 

# Megapascals
predict(ConcDatalm, newdata)

# Summary of Confidence.
predict(ConcDatalm, newdata, interval="confidence")
predict(ConcDatalm, newdata, interval="predict")




# Tweak New Data to find optimal values in KG/m3 & days accross all 
# significant components relating to Compressive Strength:
newdata = data.frame(Cement=1000, FurnaceSlag=500, FlyAsh=50,
                     Water=150, Superplasticizer=50, Age=365) 

# Megapascals
predict(ConcDatalm, newdata)

# Summary of Confidence.
predict(ConcDatalm, newdata, interval="confidence")
predict(ConcDatalm, newdata, interval="predict")





# New Data with Min values from data summary accross all significant 
# attributes for Concrete cured over 10 years:
newdata = data.frame(Cement=102.0, FurnaceSlag=0, FlyAsh=0,
                     Water=121.8, Superplasticizer=0, Age=3650) 

# In Megapscals
predict(ConcDatalm, newdata)

# Summary of Confidence.
predict(ConcDatalm, newdata, interval="confidence")
predict(ConcDatalm, newdata, interval="predict")




# ==============================================================
# Conclusion
# ==============================================================

# min values from data summary with equivalent age.
newdata = data.frame(Cement=102.0, FurnaceSlag=0, FlyAsh=0,
                     Water=121.8, Superplasticizer=0, Age=631) 

predict(ConcDatalm, newdata)

# max values from data summary with equivalent age.
newdata = data.frame(Cement=540.0, FurnaceSlag=359.4, FlyAsh=200.10,
                     Water=247.0, Superplasticizer=32.2, Age=1)

predict(ConcDatalm, newdata)




# Compressive Strength Doubles every year.
newdata = data.frame(Cement=255, FurnaceSlag=0, FlyAsh=0,
                     Water=10, Superplasticizer=0, Age=1) 

predict(ConcDatalm, newdata)

newdata = data.frame(Cement=255, FurnaceSlag=0, FlyAsh=0,
                     Water=10, Superplasticizer=0, Age=365) 

predict(ConcDatalm, newdata)




# ===================================================================
# Multiple linear regression models for most significant componenets.
# ===================================================================

model2 = lm(CompressiveStrength ~ Cement + Age, data=ConcData) 
summary(model2)
model2$coefficients
model2$residuals
residuals(model2)
plot(model2)


model2 = lm(CompressiveStrength ~ Cement + Superplasticizer, data=ConcData) 
summary(model2)
model2$coefficients
plot(model2)


model2 = lm(CompressiveStrength ~ Cement + Superplasticizer + Age, data=ConcData) 
summary(model2)
model2$coefficients
plot(model2)


