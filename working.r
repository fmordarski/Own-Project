rm(list=ls())
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# Introduction

## provide below path to working directory
setwd("C:/Users/Uzytkownik/Documents/R courses edX/Harvard/capstone/own project")

##loading data
data <- read.csv("online_shoppers_intention.csv",header=TRUE)


# Analysis

## Data cleaning

# removing any rows with NA value
data <- data[complete.cases(data), ]

# change type of 'Revenue' from boolean to factor



y <- data$Revenue

# Create trainset and test set 
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
test_set <- data[test_index, ]
train_set <- data[-test_index, ]

## Data exploration and visualization

# table with frequency percentage if shopper will generate revenue

table_freq <- data %>%
  summarize(FreqPercTrue=mean(Revenue),FreqPercFalse=1-mean(Revenue)) 

# pie chart

bp <- ggplot(melt(table_freq), aes(x="",y=value,fill=variable)) +
  geom_bar(width = 1, stat = "identity") +
  ggtitle("Did the visitor generate revenue?")

pie <- bp + coord_polar("y", start=0)

# create blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
#create nice chart

pie + scale_fill_brewer("Legend", label = c(FreqPercTrue = "Yes", FreqPercFalse = "No")) + 
  blank_theme +
  theme(axis.text.x=element_blank()) 

# Sort factor month in reverse order ("Feb","Mar","May","June","Jul","Aug","Sep","Oct","Nov","Dec") 

data$Month = factor(data$Month,levels(data$Month)[c(2,8,9,10,1,4,5,7,6,3)])

# Create plot and dataframe with counts of Revenue grouped by month

data %>% ggplot(aes(Month))  + 
  geom_bar(aes(fill = Revenue), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_brewer("Revenue") +
  theme(axis.text.x=element_blank()) +
  ggtitle("Revenue counts by month") +
  blank_theme 

group_month <- data %>%
  group_by(Month) %>%
  summarise(True=sum(Revenue=='TRUE'),False=sum(Revenue=='FALSE'),Total=n()) 

# Create plot and dataframe with counts of Revenue grouped by Visitor Type

data %>% ggplot(aes(VisitorType))  + 
  geom_bar(aes(fill = Revenue), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_brewer("Revenue") +
  theme(axis.text.x=element_blank()) +
  ggtitle("Revenue counts by Visitortype") +
  blank_theme 

group_visitor <- data %>%
  group_by(VisitorType) %>%
  summarise(True=sum(Revenue=='TRUE'),False=sum(Revenue=='FALSE'),FreqPercTrue=round(sum(Revenue=='TRUE')/n(),2),Total=n()) 

# Create dataframe with freq perc of revenue group by special day value

special_day <- data %>%
  group_by(SpecialDay) %>%
  summarise(Counts=n(),FreqPerc=round(sum(Revenue=='TRUE')/n(),2))

special_day %>% ggplot(aes(SpecialDay,FreqPerc)) +
  geom_line(colour = "lightblue",size=2) +
  ggtitle("Frequency Percentage True Revenue vs Special Day indicator") +
  theme_minimal()
  
# Create dataframe with true revenue counts group by region

group_region <- data %>%
  group_by(Region) %>%
  summarise(Counts=sum(Revenue=='TRUE'))

data %>% ggplot(aes(Region))  + 
  geom_bar(aes(fill = Revenue)) +
  scale_fill_brewer("Revenue") +
  theme(axis.text.x=element_blank()) +
  ggtitle("Revenue counts by Region") +
  blank_theme 
  
# train dataset with method Random Forest using caret package
train_set$Revenue <- as.factor(train_set$Revenue)
test_set$Revenue <- as.factor(test_set$Revenue)
train_rf <- train(Revenue~., method = 'rf',data = train_set)

#cross validation

ggplot(train_rf) +
  geom_line(colour = "lightblue",size=2) +
  ggtitle("Cross validation") +
  theme_minimal()

train_rf$finalModel$mtry

y_hat_knn <- predict(train_rf, test_set,type = "raw")

confusionMatrix(data=y_hat_knn, reference=test_set$Revenue)
confusionMatrix(data=y_hat_knn, reference=test_set$Revenue)$overall["Accuracy"]
