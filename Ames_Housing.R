#--------------------------------------------------------------------------------
## Title: Ames_Housing.R
## Author: Jake Hogan
## Date created: 11.18.2020
## -----------------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

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

#from intuition we know that location (neighborhood), square footage, age can all 
#impact sale price

#mean sale price by location (neighborhood)
train_raw %>% group_by(Neighborhood) %>% 
  summarize(n = n(), avg = mean(SalePrice), se = sd(SalePrice)/sqrt(n())) %>%
  mutate(Neighborhood = reorder(Neighborhood, avg)) %>%
  ggplot(aes(x = Neighborhood, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Average Sale Price by Neighborhood") +
  xlab("Neighborhood") +
  ylab("Average Sale Price (USD)") 




