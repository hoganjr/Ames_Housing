#--------------------------------------------------------------------------------
## Title: Ames_Housing.R
## Author: Jake Hogan
## Date created: 11.18.2020
## -----------------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)


##########################################################
# Create data sets, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

#download the zip file from my github repo and store in "AmesData" local dir
dl <- tempfile()
download.file(
  "https://github.com/hoganjr/Ames_Housing/raw/main/house-prices-advanced-regression-techniques.zip",dl)
unzip(dl, exdir = "AmesData")

#The data is delivered in 2 csv files: a train file and a test file. Reading those in.
train_raw <- read.csv("AmesData/train.csv")
test_raw <- read.csv("AmesData/test.csv")


##########################################################
# Data Exploration and Cleaning
##########################################################

#what's the make up of the data
str(train_raw) #1460 home sales with 81 parameters for each sale
str(test_raw) #1459 observations with 80 parameters (no sale price)
#so train/test is a 50/50 split here.

#look at max/min/med sale prices
max(train_raw$SalePrice) #755,000
min(train_raw$SalePrice) #34,900
median(train_raw$SalePrice) #163,000
mean(train_raw$SalePrice) #180,921


#visualizing sale price to gain insight
qplot(SalePrice, data = train_raw, bins = 30,
      color = I("black"),  
      main = "Sale Price Distribution", 
      xlab = "Sale Price (USD)", 
      ylab = "Home Count"
      ) #right-skewed

#normalizing the saleprice distribution using log transformation
qplot(log(SalePrice), data = train_raw, bins = 30,
      color = I("black"),  
      main = "Sale Price Distribution", 
      xlab = "Log Sale Price (USD)", 
      ylab = "Home Count"
) #normalized

#from intuition we know that location (neighborhood), square footage, beds/baths and age can all 
#impact sale price

#mean sale price by location (neighborhood)

train_raw %>% group_by(Neighborhood) %>% 
  dplyr::summarize(n = n(), avg = mean(SalePrice), 
            se = sd(SalePrice)/sqrt(n()), 
            med = median(SalePrice)) %>%
  mutate(Neighborhood = reorder(Neighborhood, avg)) %>%
  ggplot(aes(x = Neighborhood, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Average Sale Price by Neighborhood") +
  xlab("Neighborhood") +
  ylab("Average Sale Price (USD)") 

#mean sale price by square footage. According to the documentation the total 
#square footage is GrLivArea (sq ft above ground) + TotalBsmtSF (total basement 
#square feet)
train_raw <- train_raw %>% mutate(TotalSF = GrLivArea + TotalBsmtSF) 
train_raw %>%
  ggplot(aes(TotalSF, SalePrice)) + geom_point() + 
  ggtitle("Sale Price vs. Total Square Footage") + xlab("Total Square Footage") +
  ylab("Sale Price") #seems to be 2 outliers beyond 7500 sq. ft.
#find these two outliers and mark them for removal
outliers_sf <- which(train_raw$TotalSF > 7500)

#looking at bedrooms/bathrooms.  The data splits baths by location & size (BsmtFull,
#BsmtHalfBath, FullBath, HalfBath).  Plotting these vs sale price. 
plot1 <- train_raw %>% ggplot(aes(BsmtFullBath, SalePrice)) + geom_point() +
  xlab("Number of Basement Full Bathrooms") +
  ylab("Sale Price")
plot2 <- train_raw %>% ggplot(aes(BsmtHalfBath, SalePrice)) + geom_point() +
  xlab("Number of Basement Half Bathrooms") +
  ylab("Sale Price")
plot3 <- train_raw %>% ggplot(aes(FullBath, SalePrice)) + geom_point() +
  xlab("Number of Above Ground Full Bathrooms") +
  ylab("Sale Price")
plot4 <- train_raw %>% ggplot(aes(HalfBath, SalePrice)) + geom_point() +
  xlab("Number of Above Ground Half Bathrooms") +
  ylab("Sale Price")

gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

#Upon inspection there seems to be little to no dependence on bathrooms. 
#In my experience the number of bathrooms matters more than location.  
#Creating total baths variable.
train_raw <- train_raw %>% mutate(TotalBaths = BsmtFullBath + 
                                    0.5*BsmtHalfBath + FullBath + 0.5*HalfBath) 
train_raw %>% ggplot(aes(TotalBaths, SalePrice)) + 
  geom_point() +
  ggtitle("Total Bathrooms vs. Sale Price") + xlab("Number of Bathrooms") +
  ylab("Sale Price") 
#definitely a trend associated with number of bathrooms with a couple outliers above 
#4.5 baths
outliers_baths <- which(train_raw$TotalBaths > 4.5)

#looking at bedrooms (only data is bedrooms above grade)
train_raw %>% ggplot(aes(BedroomAbvGr, SalePrice)) +
  geom_point() +
  ggtitle("Bedrooms Above Grade vs. Sale Price") + xlab("Number of Bedrooms Above Grade") +
  ylab("Sale Price") 
#doesn't appear to be an obvious relationship here.

#the data gives two age categories, YearBuilt (year home was contructed), and
# YearRemodAdd (year a remodel was done, equals YearBuilt if no remods).  Plotting 
#both vs average sale price
train_raw %>%
  ggplot(aes(x = value, SalePrice, color = Parameter)) + 
  geom_point(aes(x = YearBuilt, col = "Year Built")) +
  geom_point(aes(x = YearRemodAdd, col = "Year Remodeled")) +
  ggtitle("Sale Price vs. Year Built or Remodeled") + xlab("Year Built/Remodeled") +
  ylab("Sale Price") + theme(legend.position = "right")
#no obvious distinction between the two params but there appears to be a strange group
#in the year remodeled data. Looks like a lot of homes were remodeled in the early 50s.
#could this be like modern amenities such as plumbing/electric?
qplot(YearRemodAdd, data = train_raw, bins = 10, color = I("black"),  
      main = "Year Remodeled Distribution", 
      xlab = "Year Remodeled", 
      ylab = "Remodel Year Count",
      ) 
#since the remod year seems to be confusing let's convert this data to relative age 
#and a binary of whether or not the house was remodeled.
train_raw <- train_raw %>% mutate(Remodeled = ifelse(YearBuilt == YearRemodAdd,
                                   0, 1))#0 = not remodeled, 1 = remodeled 
train_raw <- train_raw %>% mutate(RelAge = YrSold - YearRemodAdd) #how long since remodel
train_raw  %>%
  ggplot(aes(RelAge, SalePrice)) + geom_point() + geom_smooth() +
  ggtitle("Sale Price vs. Relative Age") + xlab("Relative Age (years)") +
  ylab("Sale Price")
#homes lose value the longer they go without a remodel

#The rest of the parameters are more obscure in whether or not they'll be
#valuable for predicting sale price.  Let's try to find the params that do not
#seem to have a relatinoship to sale price.

#look for NAs and fill back in
colNAs <-  names(which(colSums(is.na(train_raw)) >0))

#inspecting these columns shows that most of the categorical data has NA = none 
#and numeric data NA = 0 with the exception of GarageYrBlt. Converting NAs to either
#"none" or "0" where appropriate.
train_raw <- train_raw %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  mutate_if(is.character, ~replace(., is.na(.), "None"))


#performing same operation on test set
test_raw <- test_raw %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  mutate_if(is.character, ~replace(., is.na(.), "None"))



numeric_vars <- names(which(sapply(train_raw, is.numeric))) #number of numeric variables
factor_vars <- names(which(sapply(train_raw, is.character))) #number of categorical variables
#after reviewing these lists it appears there are similar features in each list. For
#example there are "Quality" type features in each list. Convert all "Quality" type vars 
#to numeric. Leaving out fence quality because it doesn't appear ordinal.

#all quality params use Po = Poor, Fa = Fair, TA = Typicla/Average, Gd = Good, Ex = Excellent
#I have added "None" above to replace NAs.
qual_vals <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

#Exterior Quality. Fixing train and test sets
train_raw$ExterQual <- as.integer(plyr::revalue(train_raw$ExterQual, qual_vals))
test_raw$ExterQual <- as.integer(plyr::revalue(test_raw$ExterQual, qual_vals))

#Exterior Condition has sames levels as quality. Fixing train and test sets
train_raw$ExterCond <- as.integer(plyr::revalue(train_raw$ExterCond, qual_vals))
test_raw$ExterCond <- as.integer(plyr::revalue(test_raw$ExterCond, qual_vals))

#Kitchen Quality. Fixing train and test sets
train_raw$KitchenQual <- as.integer(plyr::revalue(train_raw$KitchenQual, qual_vals))
test_raw$KitchenQual <- as.integer(plyr::revalue(test_raw$KitchenQual, qual_vals))

#Fireplace Quality. Fixing train and test sets
train_raw$FireplaceQu <- as.integer(plyr::revalue(train_raw$FireplaceQu, qual_vals))
test_raw$FireplaceQu <- as.integer(plyr::revalue(test_raw$FireplaceQu, qual_vals))

#Garage Quality. Fixing train and test sets
train_raw$GarageQual <- as.integer(plyr::revalue(train_raw$GarageQual, qual_vals))
test_raw$GarageQual <- as.integer(plyr::revalue(test_raw$GarageQual, qual_vals))

#Garage Condition appears to have the same levels as quality
train_raw$GarageCond <- as.integer(plyr::revalue(train_raw$GarageCond, qual_vals))
test_raw$GarageCond <- as.integer(plyr::revalue(test_raw$GarageCond, qual_vals))

#Pool Quality. Fixing train and test sets
train_raw$PoolQC <- as.integer(plyr::revalue(train_raw$PoolQC, qual_vals))
test_raw$PoolQC <- as.integer(plyr::revalue(test_raw$PoolQC, qual_vals))

#Heating Quality. Fixing train and test sets
train_raw$HeatingQC <- as.integer(plyr::revalue(train_raw$HeatingQC, qual_vals))
test_raw$HeatingQC  <- as.integer(plyr::revalue(test_raw$HeatingQC, qual_vals))

#Basement Quality actually evaluates height but uses the same rankings. Fixing train and test sets
train_raw$BsmtQual <- as.integer(plyr::revalue(train_raw$BsmtQual, qual_vals))
test_raw$BsmtQual  <- as.integer(plyr::revalue(test_raw$BsmtQual, qual_vals))

#Basement Condition actually is similar to garage condition. Fixing train and test sets
train_raw$BsmtCond <- as.integer(plyr::revalue(train_raw$BsmtCond, qual_vals))
test_raw$BsmtCond  <- as.integer(plyr::revalue(test_raw$BsmtCond, qual_vals))

#MSSubClass appears as numeric but after review it's clear this is categorical with
#numbers representing each category
train_raw$MSSubClass <- as.factor(train_raw$MSSubClass)

#If we separate out the numeric features we can create a correlation matrix to see
#which features might be worth keeping/dropping.
train_numeric <- which(sapply(train_raw, is.numeric)) #find numeric vars
cor_mat_numeric <- cor(train_raw[,train_numeric]) #build correlation matrix 

#select only the stronger SalePrice correlation cases (e.g. abs(cor) > 0.5)
index_corr <- (which(abs(cor_mat_numeric[,'SalePrice']) > 0.5 , arr.ind = T))
#plot the reduced correlation matrix
corrplot::corrplot(cor_mat_numeric[index_corr,index_corr], 
                   method = "color",
                   addCoef.col = "black",
                   tl.col = "black", 
                   addgrid.col = "gray",
                   number.cex= 0.6,
                   number.digits = 2)

#make list of params to ignore based on low correlation
corr_remove <- names(which(abs(cor_mat_numeric[,'SalePrice']) <= 0.4 , arr.ind = T))
corr_remove <- corr_remove[ corr_remove != "Id"] #need to keep house Id 
#add "FullBath" due to high cor w/ "TotalBaths"
#add "GrLivArea", "TotalBsmtSF", "1stFlrSF" due to high cor w/ "TotalSF"
#add "YearBuilt" and "YearRemodAdd" due to feature engineering
corr_remove <- append(corr_remove, c("GrLivArea",
                                     "TotalBsmtSF",
                                     "X1stFlrSF",
                                     "FullBath", 
                                     "YearBuilt",
                                     "YearRemodAdd"))

#use regression to look at categorical variables
#lets look at the variables that remain
char_names <- names(which(sapply(train_raw, is.character)))
str(train_raw[,char_names]) #doesn't appear to be any kind of ranking with the remaining variables.
#convert remaining variables to factors.
train_raw[char_names] <- lapply(train_raw[char_names], factor)
#repeat for test data
char_names_test <- names(which(sapply(test_raw, is.character)))
test_raw[char_names_test] <- lapply(test_raw[char_names_test], factor)

#use ANOVA (aov()) to look at relationships between sale price and categorical variables
char_cor_mat <- train_raw[append(char_names,c("SalePrice","MSSubClass"))]#create categorical matrix
aov_cat_var <- broom::tidy(aov(SalePrice ~., char_cor_mat )) #run aov on the categorical matrix

aov_sum <- aov_cat_var %>% filter(!is.na(statistic)) %>%
  arrange(p.value) #summarize the aov test with ascending p-value

#plot log transformed inverse p-value vs variable
aov_sum  %>% mutate(term = reorder(term, p.value)) %>%
  ggplot(aes(term,log(1/p.value))) + geom_col() +
  ggtitle("Variable vs Log of inverse p-value") + xlab("Variable") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Log of inverse p-value")
#looks like maybe zoning, neighborhood, lot shape, bldg type, and basement exposure 
#might be valuable 
min(aov_sum$p.value)
#pick variables to remove
remove_aov <- aov_sum %>% filter(log(1/p.value) <= 0.01*max(log(1/p.value))) %>% pull(term)

#use nearzerovariance for pulling params out of model
nzv <- nearZeroVar(train_raw)
names(train_raw[,nzv]) #makes sense b/c these params all either had low F stat in anova or low cor
nzv_remove <- names(train_raw[,nzv]) #create list of cols to remove
remove_combined <-unique(c(corr_remove,remove_aov,nzv_remove)) #combine all variables to be removed


#removing all previously discussed parameters
train_simplified <- train_raw[, !names(train_raw) %in% remove_combined] #remove all unused params

#removing square footage and bathroom outliers
train_simplified <- train_simplified[-append(outliers_sf,outliers_baths),]

#clean as you go
rm(dl, char_names, char_names_test, colNAs, corr_remove, factor_vars, index_corr,
   numeric_vars, nzv, nzv_remove, qual_vals, remove_aov, train_numeric, outliers)

##########################################################
# Model Development
##########################################################

#WARNING: the code below could take 1+ hrs to run.

#Define log RMSE function on which the competition is judged. 
Log_RMSE <- function(actual_prices, pred_prices){
  sqrt(mean((log(actual_prices) - log(pred_prices))^2))
}

#split train set for train/test of algos
#first we need to divide the train set in to train/test sets.  Giving 10% to test
set.seed(1, sample.kind = "Rounding")
train_split_index <- createDataPartition(y = train_simplified$SalePrice, times = 1, p = 0.1, 
                                      list = FALSE)
split_train <- train_simplified[-train_split_index,]
split_test <- train_simplified[train_split_index,]


#starting with Random Forests (aka Rborist for tuning)
library(Rborist)
control <- trainControl(method="cv", number = 10, p = 0.8) #cross validation
grid <- expand.grid(minNode = c(1,5) , predFixed = seq(1,15,2))
train_rf <-  train(split_train[, !names(split_train) %in% c("Id","SalePrice")], # don't train on id or price
                   log(split_train$SalePrice), #normalized sale price
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf) + ggtitle("Random Forests Tuning Results") #plut tuning results
train_rf$bestTune #best  min node size and variables sampled at each split 
#now fit the rf model with the best tune params
fit_rf <- train(split_train[, !names(split_train) %in% c("Id","SalePrice")],
                log(split_train$SalePrice), 
                  method = "Rborist",
                  nTree = 1000,
                  tuneGrid = expand.grid(minNode = train_rf$bestTune$minNode,
                                         predFixed =train_rf$bestTune$predFixed))

pred_rf <- predict(fit_rf, 
                   split_test)#predict using best fit

rmse_rf <- Log_RMSE(split_test$SalePrice,exp(pred_rf)) #reverse the log transformation for real

rmse_results <- tibble(Method = "Random Forests", LogRMSE = rmse_rf) #make performance table
rmse_results


#looks like one other popular classification/regression model is with gradient boosting
#eXtreme Gradient Boosting

#eXtreme Gradient Boosting does not accept categorical variables.  Categorical variables
#must be converted to a numeric representation.  This is accomplished by one-hot encoding
#the categorical variables. Basically each category gets a column with 1 or 0


#use dummyVars function to generate one-hot df of categorical variables.
split_train_num <- split_train[,names(which(sapply(split_train, is.numeric)))] #create numeric df
split_train_cat <- split_train[,names(which(sapply(split_train, is.factor)))] #create categorical df
dummy <- dummyVars("~.", data = split_train_cat) #create dummy data frame
dummy_df <- data.frame(predict(dummy, newdata = split_train_cat)) #fill dummy df with categorical data

oh_split_train <- cbind(dummy_df,split_train_num) #combine numeric and one-hot cat df into single df
#look for NAs and fill back in
colNAs_oh_train <-  names(which(colSums(is.na(oh_split_train)) >0)) #empty


xgb_control <- trainControl(method="cv", number = 5, p = 0.8) #cross validation
train_xgb <- train(x = oh_split_train[, !names(oh_split_train) %in% c("Id","SalePrice")],
                   y = log(oh_split_train$SalePrice),
                   method = "xgbTree", #initially tried xgbLinear but was getting response to tuning w/ high RMSEs
                   trControl = xgb_control,
                   tuneGrid = expand.grid(nrounds = 1000 ,
                                          eta = c(0.01, 0.05, 0.1),
                                          max_depth = c(2,3,4,5,6),
                                          gamma = 0,
                                          colsample_bytree = 1,
                                          min_child_weight = c(1,2,3,4,5),
                                          subsample = 1)
                   )
ggplot(train_xgb) + ggtitle("Gradient Boost Tuning Results") #plot tuning results
train_xgb$bestTune #best  min node size and variables sampled at each split 
#now fit to best tune params
fit_xgb <- train(x = oh_split_train[, !names(oh_split_train) %in% c("Id","SalePrice")],
                 y = log(oh_split_train$SalePrice),
                 method = "xgbTree",
                 tuneGrid = expand.grid(nrounds = 1000 , #tuning params
                                        eta = train_xgb$bestTune$eta,
                                        max_depth = train_xgb$bestTune$max_depth,
                                        gamma = 0,
                                        colsample_bytree = 1,
                                        min_child_weight = train_xgb$bestTune$min_child_weight,
                                        subsample = 1))

#create one-hot encoded split_test for prediction
split_test_num <- split_test[,names(which(sapply(split_test, is.numeric)))] #create numeric df
split_test_cat <- split_test[,names(which(sapply(split_test, is.factor)))] #create categorical df
dummy <- dummyVars("~.", data = split_test_cat) #create dummy data frame
dummy_df <- data.frame(predict(dummy, newdata = split_test_cat)) #fill dummy df with categorical data

oh_split_test <- cbind(dummy_df,split_test_num) #combine numeric and one-hot cat df into single df
pred_xgb <- predict(fit_xgb, 
                   oh_split_test) #make prediction on one-hot split test set

rmse_xgb <- Log_RMSE(oh_split_test$SalePrice,exp(pred_xgb)) #calc log RMSE
rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Gradient Boosting",
                                 LogRMSE = rmse_xgb)) #add to the table
rmse_results

#glmnet implements a penalized glm model. Using one-hot encoded params
glm_control <- trainControl(method="cv", number = 10) #cross validation
train_glm <- train(x = oh_split_train[, !names(oh_split_train) %in% c("Id","SalePrice")],
                   y = log(oh_split_train$SalePrice),
                   method = "glmnet", 
                   preProcess = c("zv","center","scale"), #preprocessing zv = zero variance
                   trControl = glm_control,
                   tuneLength = 6
)

ggplot(train_glm) + ggtitle("GLM Tuning Results") #plot tuning results
train_glm$bestTune #print best tune data
#now fit the best tune params 
fit_glm <- train(x = oh_split_train[, !names(oh_split_train) %in% c("Id","SalePrice")],
                   y = log(oh_split_train$SalePrice),
                   method = "glmnet", 
                   tuneGrid = expand.grid(alpha = train_glm$bestTune$alpha, #mixing between Lasso and Ridge regression
                                          lambda = train_glm$bestTune$lambda)
)
pred_glm <- predict(fit_glm, 
                    oh_split_test) #make prediction on the one-hot split test set
rmse_glm <- Log_RMSE(oh_split_test$SalePrice,exp(pred_glm)) #calculate the log RMSE

rmse_results <- bind_rows(rmse_results,
                          tibble(Method="GLM",
                                 LogRMSE = rmse_glm)) #add to the table
rmse_results

#creating an ensemble of all methods (just simple average)
pred_ens <- data.frame(Id = split_test$Id, SalePrice = (exp(pred_rf) + exp(pred_xgb) + exp(pred_glm))/3)

rmse_ens <- Log_RMSE(split_test$SalePrice,pred_ens$SalePrice) #calc new log rmse

rmse_results <- bind_rows(rmse_results,
                          tibble(Method="All Combined",
                                 LogRMSE = rmse_ens)) #add to the table
rmse_results


##########################################################
# PREDICTION ON HOLD OUT TEST SET FOR KAGGLE SUBMISSION
##########################################################
#using the original test set
#predict the sale price based on the test set params.  We'll need to perform some of cleaning
#that was performed on the training set. NAs and categorical data have already been addressed above.
#Need to add Total Sq Ft, Total Baths, Remodeled, and Relative Age Features
test_raw <- test_raw %>% mutate(TotalSF = GrLivArea + TotalBsmtSF) #add total sq ft
test_raw <- test_raw %>% mutate(TotalBaths = BsmtFullBath + 
                                    0.5*BsmtHalfBath + FullBath + 0.5*HalfBath) #add total baths
test_raw <- test_raw %>% mutate( Remodeled = ifelse(YearBuilt == YearRemodAdd,
                                                      0, 1))#0 = not remodeled, 1 = remodeled 
test_raw <- test_raw %>% mutate(RelAge = YrSold - YearRemodAdd) #how long since remodel
#now remove all the unused variables defined in the data cleaning of the train set
test_simplified <- test_raw[, !names(test_raw) %in% remove_combined] #remove all unused params

#one-hot encoding the test set 
test_simplified_num <- test_simplified[,names(which(sapply(test_simplified, is.numeric)))] #create numeric df
test_simplified_cat <- test_simplified[,names(which(sapply(test_simplified, is.factor)))] #create categorical df
dummy <- dummyVars("~.", data = test_simplified_cat) #create dummy data frame
dummy_df <- data.frame(predict(dummy, newdata = test_simplified_cat)) #fill dummy df with categorical data

oh_test_simplified <- cbind(dummy_df,test_simplified_num) #combine numeric and one-hot cat df into single df

#train each model on the combined unsplit train set before predicting on the test set
train_simplified_num <- train_simplified[,names(which(sapply(train_simplified, is.numeric)))] #create numeric df of full train set
train_simplified_cat <- train_simplified[,names(which(sapply(train_simplified, is.factor)))] #create categorical df full train set
dummy <- dummyVars("~.", data = train_simplified_cat) #create dummy data frame
dummy_df <- data.frame(predict(dummy, newdata = train_simplified_cat)) #fill dummy df with categorical data
oh_train_simplified <- cbind(dummy_df,train_simplified_num) #combine numeric and one-hot cat df into single df

#now look to see if there are params in test set not seen in training, then remove those params
unique_test_params <- names(oh_test_simplified)[!(names(oh_test_simplified) %in% names(oh_split_train))] #params uniqe to test
unique_train_params <- names(oh_train_simplified)[!(names(oh_train_simplified) %in% names(oh_test_simplified))] #params unique to train
unique_params <- c(unique_test_params,unique_train_params[-which(unique_train_params == "SalePrice")]) #combine but remove saleprice
oh_test_simplified <- oh_test_simplified[, !names(oh_test_simplified) %in% unique_params] #remove all unique params
oh_train_simplified <- oh_train_simplified[, !names(oh_train_simplified) %in% unique_params] #remove all unique params

#run training on the entire test set using the optimized parameters.
final_fit_rf <- train(train_simplified[, !names(train_simplified) %in% c("Id","SalePrice")],
                log(train_simplified$SalePrice), 
                method = "Rborist",
                nTree = 1000,
                tuneGrid = expand.grid(minNode = train_rf$bestTune$minNode,
                                       predFixed =train_rf$bestTune$predFixed))

final_fit_xgb <- train(x = oh_train_simplified[, !names(oh_train_simplified) %in% c("Id","SalePrice")],
                 y = log(oh_train_simplified$SalePrice),
                 method = "xgbTree",
                 tuneGrid = expand.grid(nrounds = 1000 ,
                                        eta = train_xgb$bestTune$eta,
                                        max_depth = train_xgb$bestTune$max_depth,
                                        gamma = 0,
                                        colsample_bytree = 1,
                                        min_child_weight = train_xgb$bestTune$min_child_weight,
                                        subsample = 1))
final_fit_glm <- train(x = oh_train_simplified[, !names(oh_train_simplified) %in% c("Id","SalePrice")],
                 y = log(oh_train_simplified$SalePrice),
                 method = "glmnet", 
                 tuneGrid = expand.grid(alpha = train_glm$bestTune$alpha, #mixing between Lasso and Ridge regression
                                        lambda = train_glm$bestTune$lambda)
)
#make final predictions
pred_test_glm <- predict(final_fit_glm, 
                           oh_test_simplified)
pred_test_xgb <- predict(final_fit_xgb, 
                         oh_test_simplified)
pred_test_rf <- predict(final_fit_rf, 
                         test_simplified)
#create final ensemble
pred_test_ens <- data.frame(Id = test_simplified$Id, SalePrice = (exp(pred_test_glm) + exp(pred_test_xgb) + 
                                                                    exp(pred_test_rf))/3)
#make csv for submission to kaggle
write.csv(pred_test_ens, file = "Ames_Price_predictions.csv", row.names = FALSE)

