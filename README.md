#import
salarydata=read.csv(file.choose())

#exploring
str(salarydata)
summary(salarydata)

#spliting
set.seed(123)
split=sample.split(salarydata$Salary,SplitRatio = 2/3)
training_set=subset(salarydata,split==TRUE)
testset_set=subset(salarydata,split==FALSE)

#building a linear model using training data

regressor=lm(formula=Salary~YearsExperience,data=training_set)
summary(regressor)

#predict using test data

y_pred=predict(regressor,newdata = testset_set)

#compare the prediction with actual valur

y_pred
testset_set$Salary


#Visualising the Training set results

library(ggplot2)
ggplot()+
geom_point(aes(x=training_set$YearsExperience,y=
                 training_set$Salary),
           colout='red') +
  
geom_line(aes(x=training_set$YearsExperience,y=
                predict(regressor,newdata = training_set)),colour='blue')+

ggtitle('Salary vs Experience(Training set)' +

xlab('Years of experience') +

ylab('Salary')


#visualising the test set results

library(ggplot2)

ggplot()+  
  
geom_point(aes(x=testset_set$YearsExperience,y=
                 testset_set$Salary),
           colour='red')+

geom_line(aes(x=training_set$YearsExperience,y=
                predict(regressor,newdata = training_set)),
          colour='blue')+

ggtitle('Salary vs Experience (Test set)')+

xlab('Years Experience')+

ylab('Salary')
