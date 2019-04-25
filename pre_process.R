require(caret)
require(dplyr)

############################
# filtering macro database #
############################

# creating a dataframe to manipulate
db = db_to_predict %>% filter(segmento == segment) %>% select(-cod_contrato)

# ploting percentage of "bad payer", which is a level of target variable
perc = table(db$target)[2]/(table(db$target)[1]+table(db$target)[2])
paste0("The percentage of bad payers is: ", round(perc, 4), ". ") %>% cat()

# library to "unregister" a foreach backend, registering the sequential backend
registerDoSEQ()
# removing features with variance = 0 (no discrimination values)
zerovar = nearZeroVar(db, uniqueCut = 0, foreach = TRUE, allowParallel = TRUE)
db = db[,-zerovar]

###########################
# missing data imputation #
###########################

# missing data imputation with 5 nearest neighbourhoods
preproc = preProcess(db, method = "knnImpute", k = 5) # preProcess(db, method = "bagImpute"): Alternatively, bagged trees can also be used to impute. 
# For each predictor in the data, a bagged tree is created using all of the other predictors in the 
# training set. When a new sample has a missing predictor value, the bagged model is used to predict 
# the value. While, in theory, this is a more powerful method of imputing, the computational costs 
# are much higher than the nearest neighbor technique. 
preproc$method$center = NULL # automatically, preProcess function scale and center variables.
preproc$method$scale = NULL # automatically, preProcess function scale and center variables.
db <- predict(preproc, db) # applying preProcess results in dataframe.

db$target = as.factor(db$target) 

# another option of missing data imputation (pmm method)
require(mice)

db_to_predict = mice(db_to_predict, m=5, seed = 123) # mice: multivariate imputation by chained equations.
db = complete(db_to_predict, 1) # predicting new values with first model