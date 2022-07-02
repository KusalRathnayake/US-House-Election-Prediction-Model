
# Import Libraries

library(e1071)
library(rpart)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)

# Attach datasets from 2000 to 2020 into dataframes and row-bind the datasets into one dataframe

df00 <- as.data.frame(df_2000)
df02 <- as.data.frame(df_2002)
df04 <- as.data.frame(df_2004)
df06 <- as.data.frame(df_2006)
df08 <- as.data.frame(df_2008)
df10 <- as.data.frame(df_2010)
df12 <- as.data.frame(df_2012)
df14 <- as.data.frame(df_2014)
df16 <- as.data.frame(df_2016)
df18 <- as.data.frame(df_2018)
df20 <- as.data.frame(df_2020)

tr_df <- rbind(df00,df02,df04,df06,df08,df10,df12,df14,df16)

# Create dependent variables

tr_df$yvar <- NA

for (i in 1:length(tr_df$win_party)) 
{
  if(tr_df$win_party[i] == 1 |tr_df$win_party[i] == 3 )
    tr_df$yvar[i] = 1
  else
    tr_df$yvar[i] = 0
}

df18$yvar <- NA

for (i in 1:length(df18$win_party)) 
{
  if(df18$win_party[i] == 1 |df18$win_party[i] == 3 )
    df18$yvar[i] = 1
  else
    df18$yvar[i] = 0
}

# Create training and test sets

training_set <- data.frame('year' = tr_df$year, 'state' = tr_df$state, 'district' = tr_df$district, 'win_party' = tr_df$win_party,  
                           'white' = (tr_df$white*100),'black'=(tr_df$black*100),'asian'=(tr_df$asian*100),'hispanic'=(tr_df$hispanic*100),
                           'male' = (tr_df$male*100), 'female'=(tr_df$female*100),'age18_55'=(tr_df$age_18_55*100),
                           'age_55p'=(tr_df$age_55_plus),'interM'=scale(tr_df$interM),'domesM'=scale(tr_df$domesM), 'farm'=(tr_df$farm_E_I*100),
                           'nonfarm'=(tr_df$nonfarm_E*100), 'personal'=log10(tr_df$personal),'yvar'=factor(tr_df$yvar))


test_set <- data.frame('year' = df18$year, 'state' = df18$state, 'district' = df18$district, 'win_party' = df18$win_party, 
                       'white' =(df18$white*100),'black'=(df18$black*100),'asian'=(df18$asian*100), 'hispanic'=(df18$hispanic*100),
                       'male' = (df18$male*100), 'female'=(df18$female*100), 'age18_55'=(df18$age_18_55*100), 'age_55p'=(df18$age_55_plus),
                       'interM'=scale(df18$interM), 'domesM'=scale(df18$domesM), 'farm'=(df18$farm_E_I*100),'nonfarm'=(df18$nonfarm_E*100), 
                       'personal'=log10(df18$personal),'yvar'=factor(df18$yvar))



# Point Biserial Correlations

cor.test(tr_df$white,tr_df$yvar)
cor.test(tr_df$black,tr_df$yvar)
cor.test(tr_df$asian,tr_df$yvar)
cor.test(tr_df$hispanic,tr_df$yvar)
cor.test(tr_df$female,tr_df$yvar)
cor.test(tr_df$age_18_55,tr_df$yvar)
cor.test(tr_df$age_55_plus,tr_df$yvar)
cor.test(tr_df$interM,tr_df$yvar)
cor.test(tr_df$domesM,tr_df$yvar)
cor.test(tr_df$farm_E_I,tr_df$yvar)
cor.test(tr_df$nonfarm_E,tr_df$yvar)
cor.test(tr_df$personal,tr_df$yvar)

# Support Vector Machines (SVM) Clasification Model

# SVM Classification
clasifier <- svm(factor(yvar)~white+black+asian+hispanic+male+female+age18_55+age_55p+interM+domesM+farm+nonfarm+personal,data = training_set, type = 'C-classification')

summary(classifier)

ypred <- predict(classifier,newdata = test_set[-18])

yact <- test_set$yvar
cm <- table(yact, ypred)
cm

# Decision trees Clasification Model

# Decision Tree Classification
training_set <- na.omit(training_set)

classifier <- rpart(factor(yvar)~white+black+asian+hispanic+female+age18_55+age_55p+interM+domesM+farm+personal, data = training_set)
ypred <- predict(classifier,newdata = test_set[-18],type = 'class')

yact <- test_set$yvar
cm <- table(yact, ypred)
cm

# Random Forests (RF) Clasification Model

# Random Forest Classification
classifier <- randomForest(x = training_set[-c(4,18)],y = training_set$yvar,ntree = 300)
ypred <- predict(classifier,newdata = test_set[-c(4,18)])

yact <- test_set$yvar
cm <- table(yact, ypred)
cm






