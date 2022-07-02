# Artificial Neural Network (ANN) Clasification Model

# ANN Analysis

library(h2o)

tr_ANN <- data.frame('year' = tr_df$year, 'state' = tr_df$state, 'district' = tr_df$district, 'win_party' = tr_df$win_party, 'white' = (tr_df$white*100),'black'=(tr_df$black*100),'asian'=(tr_df$asian*100),'hispanic'=(tr_df$hispanic*100),'male' = (tr_df$male*100), 'female'=(tr_df$female*100),'age18_55'=(tr_df$age_18_55*100), 'age_55p'=(tr_df$age_55_plus),'interM'=tr_df$interM, 'domesM'=tr_df$domesM,'farm'=(tr_df$farm_E_I*100),'nonfarm'=(tr_df$nonfarm_E*100), 'personal'=tr_df$personal, 'yvar' = factor(tr_df$yvar))

training_ANN <- tr_ANN[,5:18]
training_ANN <- na.omit(training_ANN)

ts_ANN <- data.frame('year' = df18$year, 'state' = df18$state, 'district' = df18$district, 'win_party' = df18$win_party, 'white' = (df18$white*100),'black'=(df18$black*100),'asian'=(df18$asian*100),'hispanic'=(df18$hispanic*100),'male' = (df18$male*100), 'female'=(df18$female*100),'age18_55'=(df18$age_18_55*100), 'age_55p'=(df18$age_55_plus),'interM'=df18$interM, 'domesM'=df18$domesM, 'farm'=(df18$farm_E_I*100),'nonfarm'=(df18$nonfarm_E*100), 'personal'=df18$personal, 'yvar' = factor(df18$yvar))

test_ANN <- ts_ANN[,5:18]

h2o.init(nthreads = -1)
classifier_ANN <- h2o.deeplearning(y = 'yvar',
                                   training_frame = as.h2o(training_ANN),
                                   activation = 'Rectifier',
                                   hidden = c(15,15,15),
                                   epochs = 200,
                                   train_samples_per_iteration = -2)

prob_pred_ANN <- h2o.predict(classifier_ANN,newdata = as.h2o(test_ANN[-14]))

prob_pred_ANN <- as.vector(prob_pred_ANN)
ypred_ANN <- as.factor(prob_pred_ANN[1:435])

yact <- test_ANN$yvar
cm <- table(yact, ypred_ANN)
cm

h2o.shutdown()