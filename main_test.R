library(dplyr)    # For data manipulation
library(ggplot2)  # For data visualization
library(lubridate) # For working with dates and times



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

colnames(rfm_table)
# Renaming columns in the rfm_table data frame
rfm_table <- rfm_table %>%
  rename(

    recency = recency,
    frequency = frequency,
    monetary = monetary
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



### Merging both RFM Table with Transaction and Customer Tables

# Inner join on 'customer_id'
cust_trans_rfm <- merge(merged_trans_cust, rfm_table, by = "customer_id", all = FALSE)

# Print structure of the data frame
str(cust_trans_rfm)

# Summary statistics of the data frame
summary(cust_trans_rfm)


### Creating an Age Group Feature

# Define breaks for age groups
breaks <- seq(0, ceiling(max(cust_trans_rfm$Age)/10)*10, by = 10)

# Create Age_Group column
cust_trans_rfm$Age_Group <- cut(cust_trans_rfm$Age, breaks = breaks, labels = FALSE)

# Display the first few rows to verify the result
head(cust_trans_rfm)

### Creating a Detail Customer title / tag based on RFM Score

library(dplyr)

# Define function to lookup appropriate customer titles based on RFM score
cust_score_title_lkup <- function(rfm_score) {
  case_when(
    rfm_score >= 444 ~ "Platinum Customer",
    rfm_score >= 433 & rfm_score < 444 ~ "Very Loyal",
    rfm_score >= 421 & rfm_score < 433 ~ "Becoming Loyal",
    rfm_score >= 344 & rfm_score < 421 ~ "Recent Customer",
    rfm_score >= 323 & rfm_score < 344 ~ "Potential Customer",
    rfm_score >= 311 & rfm_score < 323 ~ "Late Bloomer",
    rfm_score >= 224 & rfm_score < 311 ~ "Losing Customer",
    rfm_score >= 212 & rfm_score < 224 ~ "High Risk Customer",
    rfm_score >= 124 & rfm_score < 212 ~ "Almost Lost Customer",
    rfm_score >= 112 & rfm_score < 124 ~ "Evasive Customer",
    TRUE ~ "Lost Customer"
  )
}


# Display the resulting data frame
head(cust_trans_rfm)


# Apply the cust_score_title_lkup function to create the 'detail_cust_title' column
cust_trans_rfm$detail_cust_title <- apply(cust_trans_rfm["rfm_score"], 1, cust_score_title_lkup)

# Display the resulting data frame
head(cust_trans_rfm)


###########################################
###########################################
# Define function to provide ranks to customers based on their titles
get_rank <- function(title) {
  switch(title,
         "Platinum Customer" = 1,
         "Very Loyal" = 2,
         "Becoming Loyal" = 3,
         "Recent Customer" = 4,
         "Potential Customer" = 5,
         "Late Bloomer" = 6,
         "Loosing Customer" = 7,
         "High Risk Customer" = 8,
         "Almost Lost Customer" = 9,
         "Evasive Customer" = 10,
         11)  # Default rank for other titles
}



# Apply the get_rank function to create the 'rank' column
cust_trans_rfm$rank <- apply(cust_trans_rfm["detail_cust_title"], 1, get_rank)

# Display the resulting data frame
head(cust_trans_rfm)




## Data Analysis and Exploration
## ###  New Customer vs Old Customer Age Distributions
# Read the CSV file into a data frame
new_cust <- read.csv('NewCustomerList_Cleaned.csv')

# Display the structure of the data frame
str(new_cust)

# Set the size of the plot
options(repr.plot.width=10, repr.plot.height=8)

# Create the histogram
hist(new_cust$Age.Group, breaks = 15, main = "New Customers - Age Distribution", xlab = "Age Group", ylab = "Number of Customers")
library(ggplot2)
library(plotly)

# Your original ggplot code
# Load required packages
library(ggplot2)
library(plotly)

# Your original ggplot code
gg <- ggplot(new_cust, aes(x = Age.Group)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  labs(title = "New Customers - Age Distribution",
       x = "Age Group",
       y = "Number of Customers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Convert ggplot object to plotly object
ggplotly(gg)


# Create the histogram
library(ggplot2)
library(plotly)

gg22 <- ggplot(cust_trans_rfm, aes(x = Age_Group)) +
  geom_histogram(bins = 15, fill = "lightgreen", color = "darkgreen") +
  labs(title = "Old Customers - Age Distribution",
       x = "Age Group",
       y = "Number of Customers") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggplotly(gg22)


##########################################



#########################################Bike related purchases over last 3 years by gender#################


library(dplyr)

# Group by 'gender' and aggregate the sum of 'past_3_years_bike_related_purchases'
cust_bike_purchase_by_gender <- cust_trans_rfm %>%
  group_by(gender) %>%
  summarise(past_3_years_bike_related_purchases = sum(past_3_years_bike_related_purchases))

# Print the resulting data frame
print(cust_bike_purchase_by_gender)


# Calculate the total sum of 'past_3_years_bike_related_purchases'
total_records <- sum(cust_trans_rfm$past_3_years_bike_related_purchases)

# Print the total sum
print(total_records)


# Calculate the percentage of total bike-related purchases for each gender
cust_bike_purchase_by_gender$Percent_of_total <- (cust_bike_purchase_by_gender$past_3_years_bike_related_purchases / total_records) * 100

# Print the resulting data frame
print(cust_bike_purchase_by_gender)
cust_bike_purchase_by_gender




library(ggplot2)
library(plotly)

# Set the size of the plot
options(repr.plot.width=8, repr.plot.height=5)

# Create the bar plot
gg33 <- ggplot(cust_bike_purchase_by_gender, aes(x = gender, y = Percent_of_total, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Percent of Total Purchases", title = "Female vs Male past 3 years Bike purchases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

ggplotly(gg33)



################################Job Industry Customer Distribution##############

library(ggplot2)

# Filter out rows where 'job_industry_category' is 'Missing'
filtered_new_cust <- new_cust[!(new_cust$job_industry_category == 'Missing'), ]

# Set the size of the plot
options(repr.plot.width=15, repr.plot.height=8)

# Create the count plot
ggplot(filtered_new_cust, aes(x = job_industry_category)) +
  geom_bar(fill = "skyblue") +
  labs(x = "Job Industry", y = "Number of Customers", title = "New Customers - Job Industry Customer Distribution") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed



library(ggplot2)
library(plotly)

# Filter out rows where 'job_industry_category' is 'Missing'
filtered_new_cust <- new_cust[!(new_cust$job_industry_category == 'Missing'), ]

# Set the size of the plot
options(repr.plot.width=15, repr.plot.height=8)

# Create the count plot
gg44 <- ggplot(filtered_new_cust, aes(x = job_industry_category)) +
  geom_bar(fill = "skyblue") +
  labs(x = "Job Industry", y = "Number of Customers", title = "New Customers - Job Industry Customer Distribution") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

ggplotly(gg44)

##############################Wealth Segmentation by Age Group####################
##############################

library(dplyr)

# Group by 'wealth_segment' and 'Age_Group' and calculate the count of each group
wealth_age_seg_new <- new_cust %>%
  group_by(wealth_segment, Age.Group) %>%
  summarise(count = n())

# Print the resulting data frame
print(wealth_age_seg_new)


# Rename the column '0' to 'Number of Customers'
wealth_age_seg_new <- wealth_age_seg_new %>%
  rename("Number of Customers" = count)

# Print the resulting data frame
print(wealth_age_seg_new)



library(ggplot2)
library(plotly)

# Set the size of the plot
options(repr.plot.width=15, repr.plot.height=8)

# Create the bar plot
gg55 <- ggplot(wealth_age_seg_new, aes(x = Age.Group, y = `Number of Customers`, fill = wealth_segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Number of Customers", title = "New Customers - Wealth Segmentation by Age Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

ggplotly(gg55)


#####################Old Customers###########################
###################
library(dplyr)

# Group by 'wealth_segment' and 'Age_Group' and calculate the count of each group
wealth_age_seg_old <- cust_trans_rfm %>%
  group_by(wealth_segment, Age_Group) %>%
  summarise(count = n())

# Print the resulting data frame
print(wealth_age_seg_old)


# Rename the column '0' to 'Number of Customers'
wealth_age_seg_old <- wealth_age_seg_old %>%
  rename("Number of Customers" = count)

# Print the resulting data frame
print(wealth_age_seg_old)

str(wealth_age_seg_old)



library(ggplot2)
library(plotly)

# Set the size of the plot
options(repr.plot.width=15, repr.plot.height=8)

# Create the bar plot
gg66 <- ggplot(wealth_age_seg_old, aes(x = Age_Group, y = `Number of Customers`, fill = wealth_segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Number of Customers", title = "Old Customers - Wealth Segmentation by Age Group") +
  theme_minimal()

ggplotly(gg66)


##################Car owner across each State##########################
################## 
# Read the CSV file into a data frame
cust_addr_info <- read.csv('CustomerAddress_Cleaned.csv')

# Display the structure of the data frame
str(cust_addr_info)



# Merge the data frames based on the 'customer_id' column
cust_trans_addr <- merge(cust_trans_rfm, cust_addr_info, by = "customer_id")

# Print the merged data frame
print(cust_trans_addr)


# Print the number of records in cust_trans_rfm and cust_addr_info data frames
cat("RFM table Records count: ", nrow(cust_trans_rfm), "\n")
cat("Address Table Records count: ", nrow(cust_addr_info), "\n")




library(dplyr)

# Select columns 'state', 'owns_car', and 'customer_id', drop duplicates, and group by 'state' and 'owns_car' while calculating the count of each group
state_car_owners <- cust_trans_addr %>%
  select(state, owns_car, customer_id) %>%
  distinct() %>%
  group_by(state, owns_car) %>%
  summarise(count = n())

# Print the resulting data frame
print(state_car_owners)



# Rename the column '0' to 'Number of Customers'
state_car_owners <- state_car_owners %>%
  rename("Number of Customers" = count)

# Print the resulting data frame
print(state_car_owners)



library(ggplot2)
library(plotly)

# Set the size of the plot
options(repr.plot.width=8, repr.plot.height=7)

# Create the bar plot
gg77 <- ggplot(state_car_owners, aes(x = state, y = `Number of Customers`, fill = owns_car)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "States", y = "Number of Customers", title = "Number of Customers who own a car") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

ggplotly(gg77)

################RFM Analysis Scatter Plots#############

##########Recency vs Monetary
library(ggplot2)
library(plotly)

# Set the size of the plot
options(repr.plot.width=8, repr.plot.height=7)

# Create the scatter plot
gg88 <- ggplot(cust_trans_rfm, aes(x = recency, y = monetary, color = frequency)) +
  geom_point(size = 3, shape = 17, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Recency", y = "Monetary ($)", title = "Recency vs Monetary",
       color = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

ggplotly(gg88)




##############Frequency vs Monetary###############
##############
library(ggplot2)
library(plotly)

# Set the size of the plot
options(repr.plot.width=8, repr.plot.height=7)

# Create the scatter plot
gg99 <- ggplot(cust_trans_rfm, aes(x = frequency, y = monetary, color = recency)) +
  geom_point(size = 3, shape = 16, alpha = 0.7) +
  scale_color_gradientn(colors = c("green4", "yellow", "red")) +
  labs(x = "Frequency", y = "Monetary ($)", title = "Frequency vs Monetary",
       color = "Recency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

ggplotly(gg99)



#################Customer Segment Distribution###################
library(dplyr)
library(ggplot2)
library(plotly)

# Set the size of the plot
options(repr.plot.width=15, repr.plot.height=8)

# Create the bar plot
gg10 <- ggplot(cust_per_title, aes(x = `Number of Customers`, y = detail_cust_title)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Number of Customers", y = "Customer Segment", title = "Number of Customers by Customer Segment") +
  theme_minimal()

# Convert to plotly object
ggplotly(gg10)






# Rename columns
names(cust_per_title)[3] <- "Number of Customers"

# Print the resulting data frame
print(cust_per_title)



###################################################################################################################
###################################################################################################################
###################################################################################################################





