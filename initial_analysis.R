# clear environment
rm(list = ls())

# load packages
library(ggplot2)
library(tidyverse)
library(arm)
library(e1071)
require(gridExtra)
library(caret)
library(pROC)

# Load in the data, select columns
smoking <- read.csv("data/smoking.csv")


# Create indicator variable for gestation
smoking$premature <- factor(ifelse(smoking$gestation < 270, 'Premature', 'Not Premature'))
smoking$premature_n <- as.numeric(smoking$premature)
smoking$premature_n <- ifelse(smoking$premature_n == 2, 1, 0)
smoking <- dplyr::select(smoking, -gestation, -date, -bwt.oz)

# Create categorical variables for others
smoking$mrace <- as.factor(ifelse(smoking$mrace < 6, 'White', 
                                  ifelse(smoking$mrace == 6, 'Mexican', 
                                         ifelse(smoking$mrace == 7, 'Black', 
                                                ifelse(smoking$mrace == 8, 'Asian', 
                                                       ifelse(smoking$mrace == 9, 'Mixed', 
                                                              ifelse('Unknown')))))))
smoking$med <- as.factor(ifelse(smoking$med == 0, 'Less than 8th Grade', 
                                ifelse(smoking$med == 1, '8th-12th Grade', 
                                       ifelse(smoking$med == 2, 'High School', 
                                              ifelse(smoking$med == 3, 'High School plus Trade School', 
                                                     ifelse(smoking$med == 4, 'Some College', 
                                                            ifelse(smoking$med == 5, 'College',
                                                                   ifelse(smoking$med < 8, 'Trade School',
                                                                          ifelse('Unknown')))))))))
smoking$inc <- as.factor(smoking$inc)
smoking$smoke <- factor(smoking$smoke, levels = c(0,1), labels = c('Not Smoking', 'Smoking'))

# Look at the data
head(smoking)
summary(smoking)
str(smoking)
table(smoking$premature)

# Response variable not well balanced (705 vs. 165)
# For education, less than 8th grade and trade school have very few variables, and mixed race also has few.

# Now we can do boxplots for numeric response variables vs. our response variables.
# Previous # of pregnancies, no visible change.
ggplot(smoking, aes(x=premature, y = parity, fill=premature)) + geom_boxplot()
plot(0:11, tapply(smoking$premature_n, smoking$parity, mean), col = 'blue4', pch = 10)
# Age of mother, the average age of mothers with premature babies appear to be lower than those of not premature babies
ggplot(smoking, aes(x=premature, y = mage, fill=premature)) + geom_boxplot()
# Mother's height, slightly different spread, but on average, looks the same
ggplot(smoking, aes(x=premature, y = mht, fill=premature)) + geom_boxplot()
# Mother's pre-pregnancy weight, again, spread looks different but average looks the same
ggplot(smoking, aes(x=premature, y = mpregwt, fill=premature)) + geom_boxplot()

# Then interactions between continuous variables. First we'll do everything with race. Previous # of pregnancies by race. Looks like there could be something w/ Mexican
ggplot(smoking, aes(x=premature, y = parity, fill=premature)) + geom_boxplot() + facet_wrap(~mrace)
# Age of mother by race. No Immediate trend?
ggplot(smoking, aes(x=premature, y = mage, fill=premature)) + geom_boxplot() + facet_wrap(~mrace)
# Mother's height by race.
ggplot(smoking, aes(x=premature, y = mht, fill=premature)) + geom_boxplot() + facet_wrap(~mrace)
# Mother's weight by race. Not really.
ggplot(smoking, aes(x=premature, y = mpregwt, fill=premature)) + geom_boxplot() + facet_wrap(~mrace)

# Now let's do everything with mother's education.
# Previous # of pregnancies by education. Maybe something w/ Mexican?
ggplot(smoking, aes(x=premature, y = parity, fill=premature)) + geom_boxplot() + facet_wrap(~med)
# Age of mother by education. Maybe a trend?
ggplot(smoking, aes(x=premature, y = mage, fill=premature)) + geom_boxplot() + facet_wrap(~med)
# Mother's height by education. No trend
ggplot(smoking, aes(x=premature, y = mht, fill=premature)) + geom_boxplot() + facet_wrap(~med)
# Mother's weight by education. No trend.
ggplot(smoking, aes(x=premature, y = mpregwt, fill=premature)) + geom_boxplot() + facet_wrap(~med)

# Now let's do everything with income.
# Previous # of pregnancies by income. 
ggplot(smoking, aes(x=premature, y = parity, fill=premature)) + geom_boxplot() + facet_wrap(~inc)
# Age of mother by income. Could be something here.
ggplot(smoking, aes(x=premature, y = mage, fill=premature)) + geom_boxplot() + facet_wrap(~inc)
# Mother's height by income. 
ggplot(smoking, aes(x=premature, y = mht, fill=premature)) + geom_boxplot() + facet_wrap(~inc)
# Mother's weight by income. Maybe??
ggplot(smoking, aes(x=premature, y = mpregwt, fill=premature)) + geom_boxplot() + facet_wrap(~inc)

# Finally, everything by smoke
# Previous # of pregnancies by smoking status. Maybe??
ggplot(smoking, aes(x=premature, y = parity, fill=premature)) + geom_boxplot() + facet_wrap(~smoke)
# Age of mother by smoking status. Could be something with premature non-smokers being younger.
ggplot(smoking, aes(x=premature, y = mage, fill=premature)) + geom_boxplot() + facet_wrap(~smoke)
# Mother's height by smoking status. Also looks like an effect here! could be too little data?
ggplot(smoking, aes(x=premature, y = mht, fill=premature)) + geom_boxplot() + facet_wrap(~smoke)
# Mother's weight by smoking status. Maybe an effect here?
ggplot(smoking, aes(x=premature, y = mpregwt, fill=premature)) + geom_boxplot() + facet_wrap(~smoke)


# Now tables for non-numeric values
# First, mother's race. It does seem like there is a difference here!
tapply(smoking$premature, smoking$mrace, function(x) table(x)/sum(table(x)))

# Income! Doesn't look super significant
tapply(smoking$premature, smoking$inc, function(x) table(x)/sum(table(x)))

# Education. There may be an education effect!
tapply(smoking$premature, smoking$med, function(x) table(x)/sum(table(x)))

# Smoking status. There is an effect here.
tapply(smoking$premature, smoking$smoke, function(x) table(x)/sum(table(x)))
# Chi-squared test. However p-value only 0.0694!
chisq.test(table(smoking[,c('premature', 'smoke')]))

## Now investigate interactions between categorical variables
# Race and smoke. The white % premature goes down with smoking, but the black does not.
smoking_yes <- smoking[smoking$smoke == 'Smoking',]
smoking_no <- smoking[smoking$smoke == 'Not Smoking',]
apply(table(smoking_yes[,c("premature","mrace")])/sum(table(smoking_yes[,c("premature","mrace")])),
      2,function(x) x/sum(x)) 
apply(table(smoking_no[,c("premature","mrace")])/sum(table(smoking_no[,c("premature","mrace")])),
      2,function(x) x/sum(x)) 

# Smoke and Education. Hard to interpret, but there does seem to be an effect
apply(table(smoking_yes[,c("premature","med")])/sum(table(smoking_yes[,c("premature","med")])),
      2,function(x) x/sum(x))
apply(table(smoking_no[,c("premature","med")])/sum(table(smoking_no[,c("premature","med")])),
      2,function(x) x/sum(x))

# Lets do binned plots.
# Previous pregnancies. Slight increase, but not crazy.
binnedplot(y = smoking$premature_n, smoking$parity, xlab = "Previous # of Pregnancies", 
           ylab = "Premature Birth?", ylim= c(0,1), col.pts = 'navy', 
           main = 'Binned Previous Pregnancies and Premature Birth', col.int = 'white')
# Mother's age. Maybe there is some effect for young and old mothers? Parabolic? Quadratic?
binnedplot(y = smoking$premature_n, smoking$mage, xlab = "Mother's Age",
           ylab = "Premature Birth?", ylim= c(0,1), col.pts = 'navy',
           main = "Binned Mother's Age and Premature Birth", col.int = 'white')
# Mother's height. Does look like there's a trend here!
binnedplot(y = smoking$premature_n, smoking$mht, xlab = "Mother's Height",
           ylab = "Premature Birth?", ylim= c(0,1), col.pts = 'navy',
           main = "Binned Mother's Height and Premature Birth", col.int = 'white')
# Mother's weight.
binnedplot(y = smoking$premature_n, smoking$mpregwt, xlab = "Mother's Weight", 
           ylab = "Premature Birth?", ylim= c(0,1), col.pts = 'navy', 
           main = "Binned Mother's Weight and Premature Birth", col.int = "white")

# Model Fitting
# First, center the continuous predictors
smoking$parityc <- smoking$parity - mean(smoking$parity)
smoking$magec <- smoking$mage - mean(smoking$mage)
smoking$mhtc <- smoking$mht - mean(smoking$mht)
smoking$mpregwtc <- smoking$mpregwt - mean(smoking$mpregwt)

# Relevel categorical variables
smoking$mrace <- relevel(smoking$mrace, ref = "White")
smoking$med <- relevel(smoking$med, ref = "High School")

# Noticed interactions: mother's height x race, parity x med, weight and smoking, smoking and height, smoking and race
# General variables to include: smoke, parity, mother's age, mother's height, mother's weight, education, income, race

model1 <- glm(premature_n ~ mpregwtc + parityc
              + magec + mhtc + smoke + mrace + med + 
                inc + mhtc*mrace + parityc*med + mpregwtc*smoke + 
                smoke*mhtc, data = smoking, family = binomial)

# save the residuals
resid1 <- residuals(model1, "resp")

# binned residuals plot
binnedplot(x=fitted(model1), y=resid1, xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# now for continuous variables. First, weight
binnedplot(x=smoking$mpregwtc, y = resid1, xlab="Weight centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# Then parity
binnedplot(x=smoking$parityc, y = resid1, xlab="Previous Pregnancies centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# Then age
binnedplot(x=smoking$magec, y = resid1, xlab="Mother's Age centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# Then height
binnedplot(x=smoking$mhtc, y = resid1, xlab="Mother's Height centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


# Model validation
Conf_mat1 <- confusionMatrix(as.factor(ifelse(fitted(model1) >= 0.5, "1","0")),
                         as.factor(smoking$premature_n),positive = "1")
Conf_mat2 <- confusionMatrix(as.factor(ifelse(fitted(model1) >= mean(smoking$premature_n), "1","0")),
                            as.factor(smoking$premature_n),positive = "1")

# ROC Curve
roc(smoking$premature_n,fitted(model1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

model1 <- glm(premature_n ~ mpregwtc + parityc
              + magec + mhtc + smoke + mrace + med + 
                inc + mhtc*mrace + parityc*med + mpregwtc*smoke + smoke*mrace + 
                smoke*mhtc, data = smoking, family = binomial)
# Model Selection
n <- nrow(smoking)
null_model <- glm(premature_n ~ smoke*mrace,data=smoking,family=binomial)

bic <- step(null_model,scope=list(upper = model1, lower = null_model),direction="both",
     trace=0,k = log(n))
aic_forward <- step(null_model,scope=list(upper = model1, lower = null_model),direction="forward",
            trace=0)
aic_backward <- step(null_model,scope=list(upper = model1, lower = null_model),direction="backward",
                 trace=0)
aic_step <- step(null_model,scope=list(upper = model1, lower = null_model),direction="both",
                     trace=0)
# Create confusion matrices for both models at 0.5 then mean value
Conf_aic <- confusionMatrix(as.factor(ifelse(fitted(aic) >= 0.5, "1","0")),
                             as.factor(smoking$premature_n),positive = "1")
Conf_bic <- confusionMatrix(as.factor(ifelse(fitted(bic) >= 0.5, "1","0")),
                           as.factor(smoking$premature_n),positive = "1")



# aic residuals
resid_aic <- residuals(aic_step, "resp")

# checking assumptions for aic
binnedplot(x=fitted(aic_step), y=resid_aic, xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# Continuous variables assumption. Pre-pregnancy weight
binnedplot(x=smoking$mpregwtc, y = resid_aic, xlab="Previous Weights centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# ROC Curve
roc(smoking$premature_n,fitted(aic),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

# bic residuals
resid_bic<- residuals(bic, "resp")

# checking assumptions for bic
binnedplot(x=fitted(bic), y=resid_bic, xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# No continuous variables to check, ROC curve.
roc(smoking$premature_n,fitted(aic_step),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

# Model minus race:smoke interaction
minus <- glm(premature_n ~ smoke + mrace + med + mpregwtc, family = binomial, data = smoking)
anova(aic_step, minus, test="Chisq")
