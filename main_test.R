library(dplyr)    # For data manipulation
library(ggplot2)  # For data visualization
library(lubridate) # For working with dates and times

# Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)

# Set ggplot2 theme
theme_set(theme_bw())

# Loading the Transactions and Customer Demographics Datasets
trans <- read.csv('Transactions_Cleaned.csv')
cust <- read.csv('CustomerDemographic_Cleaned.csv')



head(trans) # Will display the first 6 rows by default
tail(trans) # Will display the last 6 rows by default


# Total records (rows) in the Transaction Dataset
cat("Total records (rows) in the Transaction Dataset:", nrow(trans), "\n")

# Total features (columns) in the Transaction Dataset
cat("Total features (columns) in the Transaction Dataset:", ncol(trans), "\n")

head(cust)  # Displays the first 6 rows
tail(cust)  # Displays the last 6 rows

# Total records (rows) in the Customer Demographics Dataset
cat("Total records (rows) in the Customer Demographics Dataset:", nrow(cust), "\n")

# Total features (columns) in the Customer Demographics Dataset
cat("Total features (columns) in the Customer Demographics Dataset:", ncol(cust), "\n")

library(dplyr)

merged_trans_cust <- inner_join(trans, cust, by = "customer_id")

head(merged_trans_cust)  # Displays the first 6 rows
tail(merged_trans_cust)  # Displays the last 6 rows



# Total records (rows) in the Merged Dataset
cat("Total records (rows) in the Merged Dataset:", nrow(merged_trans_cust), "\n")

# Total features (columns) in the Merged Dataset
cat("Total features (columns) in the Merged Dataset:", ncol(merged_trans_cust), "\n")


summary(merged_trans_cust)


library(lubridate)  # Load the lubridate package

# Convert transaction_date column to Date format
merged_trans_cust$transaction_date <- ymd(merged_trans_cust$transaction_date)

########################################RFM ANALYSIS#########################################################


library(lubridate)
# Maximum Transaction Date
max_trans_date <- max(merged_trans_cust$transaction_date)
max_trans_date


library(lubridate)

# Maximum Transaction Date
max_trans_date <- max(merged_trans_cust$transaction_date)
# Convert max_trans_date to a Date object
comparison_date <- as.Date(max_trans_date)



library(dplyr)

# Assuming 'comparison_date' is a variable containing the desired date for comparison
rfm_table <- merged_trans_cust %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(comparison_date - max(transaction_date), units = "days"),
    frequency = n_distinct(product_id),
    monetary = sum(Profit, na.rm = TRUE)
    
  )

column_names <- names(rfm_table)
print(column_names)

print(names(rfm_table))

library(dplyr)

rfm_table <- rfm_table %>%
  rename(
    recency = transaction_date,
    frequency = product_id,
    monetary = Profit
  )


# Calculate quartiles for recency
rfm_table$r_quartile <- cut(rfm_table$recency, breaks = quantile(rfm_table$recency, probs = seq(0, 1, 0.25)), labels = c('4', '3', '2', '1'))

# Calculate quartiles for frequency
rfm_table$f_quartile <- cut(rfm_table$frequency, breaks = quantile(rfm_table$frequency, probs = seq(0, 1, 0.25)), labels = c('1', '2', '3', '4'))

# Calculate quartiles for monetary
rfm_table$m_quartile <- cut(rfm_table$monetary, breaks = quantile(rfm_table$monetary, probs = seq(0, 1, 0.25)), labels = c('1', '2', '3', '4'))



# RFM_table dataset

rfm_table


# Calculate RFM score
rfm_table$rfm_score <- 100 * as.integer(rfm_table$r_quartile) + 
  10 * as.integer(rfm_table$f_quartile) + 
  as.integer(rfm_table$m_quartile)



# Assigning customer titles based on RFM score ranges
rfm_table$customer_title <- cut(rfm_table$rfm_score, 
                                breaks = quantile(rfm_table$rfm_score, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                                labels = c('Bronze', 'Silver', 'Gold', 'Platinum'),
                                include.lowest = TRUE)
# RFM table dataset

rfm_table









