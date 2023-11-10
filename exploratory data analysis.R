##################################################################
######### EDA using xda####################################

library(devtools)
install_github("ujjwalkarn/xda")

library(xda)

data("ToothGrowth")
data(mtcars)
str(ToothGrowth)

#summarize the quantitative data
numSummary(mtcars)

## n = total number of rows for that variable
## miss = number of rows with missing value
## miss% = percentage of total rows with missing values ((miss/n)*100)
## 5% = 5th percentile value of that variable (value below which 5 percent of the observations may be found)
## the percentile values are helpful in detecting outliers

#summarize the qualitative data
charSummary(ToothGrowth)
charSummary(iris)

## n = total number of rows for that variable
## miss = number of rows with missing value
## miss% = percentage of total rows with missing values ((n/miss)*100)
## unique = number of unique levels of that variable
## note that there is only one character column (Species) in the iris dataset

bivariate(ToothGrowth,'supp','len')
## here, data is divided in bins, and observations grouped by first variable
bivariate(mtcars, 'cyl','disp')

Plot(iris,'Petal.Length') #plot petal.lnegth with other numerical 
#variables and variation in PL across 3 species 
Plot(ToothGrowth, 'len')