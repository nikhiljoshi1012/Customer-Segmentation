
#data exploration.
#import the essential packages required for this role and then read our data.
customer_data=read.csv("C:\\Users\\nikhi\\Downloads\\customer-segmentation-dataset\\customer-segmentation-dataset\\Mall_Customers.csv")
str(customer_data)
names(customer_data)


head(customer_data)

summary(customer_data$Age)

sd(customer_data$Age)

summary(customer_data$Annual.Income..k..)

sd(customer_data$Annual.Income..k..)

summary(customer_data$Age)