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
train_raw %>%filter(TotalSF < 6000) %>%
  ggplot(aes(TotalSF, SalePrice)) + geom_point() + geom_smooth() +
  ggtitle("Sale Price vs. Total Square Footage") + xlab("Total Square Footage") +
  ylab("Sale Price")

#looking at bedrooms/bathrooms.  The data splits baths by location & size (BsmtFull,
#BsmtHalfBath, FullBath, HalfBath).  Plotting these vs sale price. 
train_raw %>% 
  ggplot(aes(x = value, SalePrice, color = Bathroom)) + 
  geom_point(aes(x = BsmtFullBath, col = "Basement Full Baths")) +
  geom_point(aes(x = BsmtHalfBath, col = "Basement Half Baths")) +
  geom_point(aes(x = FullBath, col = "Above Grade Full Baths")) +
  geom_point(aes(x = HalfBath, col = "Above Grade Half Baths")) +
  ggtitle("Bathrooms Quantity vs. Sale Price") + xlab("Number of Bathrooms") +
  ylab("Sale Price") + theme(legend.position = "right")

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
  ggtitle("Sale Price vs. Year Rebuilt or Remodeled") + xlab("Year Built/Remodeled") +
  ylab("Sale Price") + theme(legend.position = "right")
#no obvious distinction between the two params but there appears to be a strange group
#in the year remodeled data. Looks like a lot of homes were remodeled in the early 50s.
#could this be like modern amenities such as plumbing/electric?
qplot(YearRemodAdd, data = train_raw, bins = 10, color = I("black"),  
      main = "Year Remodeled Distribution", 
      xlab = "Year Remodeled", 
      ylab = "Remodel Year Count",
      ) 


#The rest of the parameters are more obscure in in whether or not they'll be
#valuable for predicting sale price.  Let's try to find the params that do not
#seem to have a relatinoship to sale price.

#look for NAs and fill back in
colNAs <-  names(which(colSums(is.na(train_raw)) >0))
colNAs
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
train_numeric <- which(sapply(train_raw, is.numeric)) 
cor_mat_numeric <- cor(train_raw[,train_numeric])

#select only the stronger SalePrice correlation cases (e.g. abs(cor) > 0.5)
index_corr <- (which(abs(cor_mat_numeric[,'SalePrice']) > 0.5 , arr.ind = T))
#plot the reduced correlation matrix
corrplot::corrplot(cor_mat_numeric[index_corr,index_corr], 
                   method = "color",
                   addCoef.col = "black",
                   tl.col = "black", 
                   addgrid.col = "gray",
                   number.cex= 0.75,
                   number.digits = 2)

#make list of params to ignore based on low correlation
corr_remove <- names(which(abs(cor_mat_numeric[,'SalePrice']) <= 0.5 , arr.ind = T))
corr_remove <- corr_remove[ corr_remove != "Id"] #need to keep house Id 
#add "FullBath" due to high cor w/ "TotalBaths"
#add "GrLivArea", "TotalBsmtSF", "1stFlrSF" due to high cor w/ "TotalSF"
corr_remove <- append(corr_remove, c("GrLivArea",
                                     "TotalBsmtSF",
                                     "X1stFlrSF",
                                     "FullBath"))

#use regression to look at categorical variables
#lets look at the variables that remain
char_names <- names(which(sapply(train_raw, is.character)))
str(train_raw[,char_names]) #doesn't appear to be any kind of ranking with the remaining variables.
#convert remaining variables to factors.
train_raw[char_names] <- lapply(train_raw[char_names], factor)
#repeat for test data
char_names_test <- names(which(sapply(train_raw, is.character)))
test_raw[char_names_test] <- lapply(test_raw[char_names_test], factor)

#use ANOVA (aov()) to look at relationships between sale price and categorical variables
char_cor_mat <- train_raw[append(char_names,c("SalePrice","MSSubClass"))]
aov_cat_var <- broom::tidy(aov(SalePrice ~., char_cor_mat ))

aov_sum <- aov_cat_var %>% filter(!is.na(statistic)) %>%
  arrange(desc(statistic))
#looks like maybe zoning, neighborhood, lot shape, bldg type, and basement exposure 
#might be valuable 
max(aov_sum$statistic)
remove_aov <- aov_sum %>% filter(statistic <= 0.1*max(statistic)) %>% pull(term)

#use nearzerovariance for pulling params out of model
nzv <- nearZeroVar(train_raw)
names(train_raw[,nzv]) #makes sense b/c these params all either had low F stat in anova or low cor
nzv_remove <- names(train_raw[,nzv]) #create list of cols to remove
remove_combined <-unique(c(corr_remove,remove_aov,nzv_remove))


#removing all previously discussed parameters
train_simplified <- train_raw[, !names(train_raw) %in% remove_combined] #remove all unused params


#split train set for train/test of algos
#first we need to divide the train set in to train/test sets.  Giving 10% to test
set.seed(1, sample.kind = "Rounding")
train_split_index <- createDataPartition(y = train_simplified$SalePrice, times = 1, p = 0.1, 
                                      list = FALSE)
split_train <- train_simplified[-train_split_index,]
split_test <- train_simplified[train_split_index,]


#train 2 algos (maybe Random Forest? and ?)


#add TotalSF and TotalBaths to test set then filter out all unused params

