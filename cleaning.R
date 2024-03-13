
##########MAIN###################

###CUSTOMER DEMOGRAPHICS####
library(readxl)

dirty_data_CD <- read_excel("Raw_data.xlsx", sheet = "CustomerDemographic")

# Remove rows with any form of NA values in any column
dirty_data_CD <- dirty_data_CD[complete.cases(dirty_data_CD), ]

# Check for the occurrence of the substring 'n/a' in the entire sheet
na_occurrences <- apply(dirty_data_CD, 1, function(row) any(grepl("n/a", as.character(row), ignore.case = TRUE)))

# Subset the data frame to keep only rows without 'n/a' occurrences
dirty_data_CD <- dirty_data_CD[!na_occurrences, ]

#Dropping the columns with Random values
dirty_data_CD <- dirty_data_CD[, -which(names(dirty_data_CD) == "deceased_indicator")]
dirty_data_CD <- dirty_data_CD[, -which(names(dirty_data_CD) == "default")]

# Now you can continue with your analysis or view the updated data frame if needed
View(dirty_data_CD)

library(dplyr)
dirty_data_CD<-dirty_data_CD %>%
  rename(CustomerID="customer_id")
dirty_data_CD<-dirty_data_CD %>%
  rename(FNAME='first_name',LNAME='last_name',Gender='gender');

dirty_data_CD<-dirty_data_CD %>%
  rename(JobTitle="job_title")


View(dirty_data_CD)
##################################################


dirty_data_CA <- read_excel("Raw_data.xlsx", sheet = "CustomerAddress")

# Remove rows with any form of NA values in any column
dirty_data_CA <- dirty_data_CA[complete.cases(dirty_data_CA), ]

# Check for the occurrence of the substring 'n/a' in the entire sheet
na_occurrences <- apply(dirty_data_CA, 1, function(row) any(grepl("n/a", as.character(row), ignore.case = TRUE)))

# Subset the data frame to keep only rows without 'n/a' occurrences
dirty_data_CA <- dirty_data_CA[!na_occurrences, ]

View(dirty_data_CA)
library(dplyr)
dirty_data_CA<-dirty_data_CA %>%
  rename(CustomerID="customer_id",Address="address",Postcode="postcode",State="state",Country="country")

#########################

#########Transactions########
dirty_data_TR <- read_excel("Raw_data.xlsx", sheet = "Transactions")

# Remove rows with any form of NA values in any column
dirty_data_TR <- dirty_data_TR[complete.cases(dirty_data_TR), ]

# Check for the occurrence of the substring 'n/a' in the entire sheet
na_occurrences <- apply(dirty_data_TR, 1, function(row) any(grepl("n/a", as.character(row), ignore.case = TRUE)))

# Subset the data frame to keep only rows without 'n/a' occurrences
dirty_data_TR <- dirty_data_TR[!na_occurrences, ]

dirty_data_TR<-dirty_data_TR %>%
  rename(TransactionID="transaction_id",ProductID="product_id",CustomerID="customer_id",Brand="brand",)

View(dirty_data_TR)

#####################################

dirty_data_NC <- read_excel("Raw_data.xlsx", sheet = "NewCustomerList")

View(dirty_data_NC)



