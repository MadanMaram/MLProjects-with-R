library(ggplot2)
library(corrplot)
library(caTools)
getwd()
setwd("F:/R/Insirance project in R")

insurance <-read.csv("insurance.csv",stringsAsFactors = F)
str(insurance)
summary(insurance)

insurance$children = as.factor(insurance$children)



#scatter plot of BMI vs expenses with sex as color variant

ggplot(insurance, aes( bmi,expenses , col= sex)) + 
  geom_point()+
  scale_colour_manual(values = c("green", "red", "blue"))
  ggtitle("Expenses vs BMI ")+
  
  xlab('BMI')+
  ylab('Expenses')


# scatter plot of Age vs expenses with sex as color variant

ggplot(insurance, aes( age,expenses , col= sex)) + 
  geom_point()+
  scale_colour_manual(values = c("green", "red", "blue"))
  ggtitle("Expenses vs Age ")+
  
  xlab('Age')+
  ylab('Expenses')

#corrplot(insurance)

# box plot of expenses vs sex

ggplot(data=insurance)+
  geom_boxplot(aes(x=sex ,y=expenses))

# Bar chart of average expenses vs region

meanExpensesRegion = as.data.frame(aggregate(expenses~region, insurance, FUN=mean))
ggplot(meanExpensesRegion, aes(region, expenses ))+
  geom_bar(stat='identity' ,fill = c("red","blue","green","yellow"))+
  geom_text(aes(label=round(expenses)),vjust=-0.5)



# Bar chart of average expenses vs smoker

meanExpensesSmoker = as.data.frame(aggregate(expenses~smoker, insurance, FUN=mean))
ggplot(meanExpensesSmoker, aes(smoker, expenses ))+
  geom_bar(stat='identity' ,fill = c("green","red"))+
  geom_text(aes(label=round(expenses)),vjust=-0.5)

# Bar chart of average expenses vs smoker

meanExpensesChildren = as.data.frame(aggregate(expenses~children, insurance, FUN=mean))
ggplot(meanExpensesChildren, aes(children, expenses ))+
  geom_bar(stat='identity' ,fill = c("green","red","blue","yellow","pink","grey"))+
  geom_text(aes(label=round(expenses)),vjust=-0.5)


#scatter plot of BMI vs expenses with Smoker as color variant


ggplot(insurance, aes( bmi,expenses , col= smoker)) + 
  geom_point()+
  ggtitle("Expenses vs BMI ")+
  scale_colour_manual(values = c("green", "red", "blue"))+
  xlab('BMI')+
  ylab('Expenses')

# linear regression for expenses/loss

# Splitting the dataset into the Training set and Test set''

set.seed(123)
split = sample.split(insurance$expenses, SplitRatio = 0.8)
training_set = subset(insurance, split == TRUE)
test_set = subset(insurance, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = expenses ~ .,
               data = training_set)
summary(regressor)
# summary
# when i analysed the summary it shows excatly what i have analysed in the charts above
# now we will take out unwanted columns or features from regression. Then redo regression
# in the above we got adjusted R2 as 0.7564 

regressor = lm(formula = expenses ~ age + bmi + smoker + children ,
               data = training_set)
summary(regressor)

# we are getting adjusted R2 as 0.7555 which is close to 0.7564 so we can go with 
# features selected as final.

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

test_set$predicted = y_pred









