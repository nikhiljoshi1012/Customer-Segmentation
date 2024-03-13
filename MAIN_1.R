
#data exploration.
#import the essential packages required for this role and then read our data.
customer_data <- read.csv("C:\\Users\\nikhi\\OneDrive\\Desktop\\PNS_PROJECT\\Raw_data.xlsx")
str(customer_data)
names(customer_data)


head(customer_data)

summary(customer_data$Age)

sd(customer_data$Age)

summary(customer_data$Annual.Income..k..)

sd(customer_data$Annual.Income..k..)

summary(customer_data$Age)

sd(customer_data$Spending.Score..1.100.)


#Customer Gender Visualization
a=table(customer_data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),

              legend=rownames(a))




pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")



summary(customer_data$Age)



hist(customer_data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)


boxplot(customer_data$Age,
        col="purple",
        main="Boxplot for Descriptive Analysis of Age")



summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)



plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="#ccff66")


#Analyzing Spending Score of the Customers

boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")



hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)














