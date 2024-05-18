# Customer Segmentation Project using R

This project involves segmenting customers based on their transaction and demographic data. We will perform RFM (Recency, Frequency, Monetary) analysis to categorize customers and provide insights into their behavior.

## Table of Contents

1. [Installation](#installation)
2. [Data Preparation](#data-preparation)
3. [Data Exploration](#data-exploration)
4. [RFM Analysis](#rfm-analysis)
5. [Visualization](#visualization)
6. [Insights and Conclusion](#insights-and-conclusion)

## Installation

Ensure you have the following R packages installed:

```R
install.packages(c("dplyr", "ggplot2", "lubridate", "plotly"))
```

## Data Preparation

### Loading Data

Load the transaction and customer demographic data:

```R
library(dplyr)
library(ggplot2)
library(lubridate)

# Load datasets
trans <- read.csv('Transactions_Cleaned.csv')
cust <- read.csv('CustomerDemographic_Cleaned.csv')

# Display data overview
head(trans)
tail(trans)
cat("Total records in Transaction Dataset:", nrow(trans), "\n")
cat("Total features in Transaction Dataset:", ncol(trans), "\n")

head(cust)
tail(cust)
cat("Total records in Customer Demographics Dataset:", nrow(cust), "\n")
cat("Total features in Customer Demographics Dataset:", ncol(cust), "\n")
```

### Merging Data

Merge the transaction and customer data on `customer_id`:

```R
merged_trans_cust <- inner_join(trans, cust, by = "customer_id")
cat("Total records in Merged Dataset:", nrow(merged_trans_cust), "\n")
cat("Total features in Merged Dataset:", ncol(merged_trans_cust), "\n")
summary(merged_trans_cust)
```

### Data Preprocessing

Convert the `transaction_date` column to Date format:

```R
merged_trans_cust$transaction_date <- ymd(merged_trans_cust$transaction_date)
```

## Data Exploration

### Basic Statistics

Examine the structure and summary statistics of the merged dataset:

```R
head(merged_trans_cust)
tail(merged_trans_cust)
summary(merged_trans_cust)
```

## RFM Analysis

### Calculating RFM Metrics

Compute Recency, Frequency, and Monetary values:

```R
max_trans_date <- max(merged_trans_cust$transaction_date)
comparison_date <- as.Date(max_trans_date)

rfm_table <- merged_trans_cust %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(comparison_date - max(transaction_date), units = "days"),
    frequency = n_distinct(product_id),
    monetary = sum(Profit, na.rm = TRUE)
  )

rfm_table <- rfm_table %>%
  rename(recency = recency, frequency = frequency, monetary = monetary)
```

### Assigning RFM Scores

Calculate quartiles and assign RFM scores:

```R
rfm_table$r_quartile <- cut(rfm_table$recency, breaks = quantile(rfm_table$recency, probs = seq(0, 1, 0.25)), labels = c('4', '3', '2', '1'))
rfm_table$f_quartile <- cut(rfm_table$frequency, breaks = quantile(rfm_table$frequency, probs = seq(0, 1, 0.25)), labels = c('1', '2', '3', '4'))
rfm_table$m_quartile <- cut(rfm_table$monetary, breaks = quantile(rfm_table$monetary, probs = seq(0, 1, 0.25)), labels = c('1', '2', '3', '4'))

rfm_table$rfm_score <- 100 * as.integer(rfm_table$r_quartile) + 10 * as.integer(rfm_table$f_quartile) + as.integer(rfm_table$m_quartile)
```

### Customer Segmentation

Assign customer titles based on RFM scores:

```R
rfm_table$customer_title <- cut(rfm_table$rfm_score, 
                                breaks = quantile(rfm_table$rfm_score, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                                labels = c('Bronze', 'Silver', 'Gold', 'Platinum'),
                                include.lowest = TRUE)
```

## Visualization

### Age Distribution

Visualize the age distribution of new and old customers:

```R
# New Customers Age Distribution
gg <- ggplot(new_cust, aes(x = Age.Group)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  labs(title = "New Customers - Age Distribution", x = "Age Group", y = "Number of Customers") +
  theme_minimal()
ggplotly(gg)

# Old Customers Age Distribution
gg2 <- ggplot(cust_trans_rfm, aes(x = Age_Group)) +
  geom_histogram(bins = 15, fill = "lightgreen", color = "darkgreen") +
  labs(title = "Old Customers - Age Distribution", x = "Age Group", y = "Number of Customers") +
  theme_minimal()
ggplotly(gg2)

# Display side by side
subplot(ggplotly(gg), ggplotly(gg2), nrows = 1)
```

### Bike Purchases by Gender

Visualize bike purchases over the last 3 years by gender:

```R
cust_bike_purchase_by_gender <- cust_trans_rfm %>%
  group_by(gender) %>%
  summarise(past_3_years_bike_related_purchases = sum(past_3_years_bike_related_purchases))

cust_bike_purchase_by_gender$Percent_of_total <- (cust_bike_purchase_by_gender$past_3_years_bike_related_purchases / sum(cust_trans_rfm$past_3_years_bike_related_purchases)) * 100

gg33 <- ggplot(cust_bike_purchase_by_gender, aes(x = gender, y = Percent_of_total, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Percent of Total Purchases", title = "Female vs Male past 3 years Bike purchases") +
  theme_minimal()
ggplotly(gg33)
```

### Wealth Segmentation by Age Group

Visualize wealth segmentation by age group for new and old customers:

```R
# New Customers
wealth_age_seg_new <- new_cust %>%
  group_by(wealth_segment, Age.Group) %>%
  summarise(count = n()) %>%
  rename("Number of Customers" = count)

gg55 <- ggplot(wealth_age_seg_new, aes(x = Age.Group, y = `Number of Customers`, fill = wealth_segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Number of Customers", title = "New Customers - Wealth Segmentation by Age Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(gg55)

# Old Customers
wealth_age_seg_old <- cust_trans_rfm %>%
  group_by(wealth_segment, Age_Group) %>%
  summarise(count = n()) %>%
  rename("Number of Customers" = count)

gg66 <- ggplot(wealth_age_seg_old, aes(x = Age_Group, y = `Number of Customers`, fill = wealth_segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Number of Customers", title = "Old Customers - Wealth Segmentation by Age Group") +
  theme_minimal()
ggplotly(gg66)
```

### Customer Segments Distribution

Visualize the distribution of customer segments:

```R
cust_per_title <- cust_trans_rfm %>%
  group_by(detail_cust_title) %>%
  summarise(`Number of Customers` = n())

gg10 <- ggplot(cust_per_title, aes(x = `Number of Customers`, y = detail_cust_title)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Number of Customers", y = "Customer Segment", title = "Number of Customers by Customer Segment") +
  theme_minimal()
ggplotly(gg10)
```

## Insights and Conclusion

This analysis provided insights into customer behavior and segmentation using RFM analysis. By categorizing customers into different segments, we can better understand their purchasing patterns and tailor marketing strategies accordingly.

---

**Note:** Replace placeholders with actual paths and ensure all necessary data files are available in your working directory.
