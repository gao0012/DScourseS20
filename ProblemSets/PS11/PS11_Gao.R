---
title: "PS 1"
author: "Binyue Ling"
date: "January 28, 2019"
output:
  pdf_document: default
  html_document: default
---
##Question 1. [Joint Distribution and Visualization]##

#  Using R, report the joint distribution (probability mass function) for survival status and passenger class. Interpret the results. Do these results make sense? 
# For Titanic passengers are not survived, the probality of  the 1st class is 8.978676%, the 2nd class is 10.886644%, and the 3rd class is 41.750842%. The probability of survived passengers has 15.263749% in 1st class, 9.764310% in 2nd class, and 13.355780% in 3rd class.
```{r}
titanic <- read.csv("/Users/gaoningjing/Desktop/titanic.csv", header = TRUE)
x <- titanic$Survived
y <- titanic$Pclass
mytable<-table(x,y)

 
data<-as.data.frame(cbind(x,y)) 
mytable<-xtabs(~ x+y, data=data)


prop.table(mytable)
```
# Using R, calculate marginal distributions for both survival status and passenger class as well. # Plot a Mosaic plot to visualize the relationship between survival status and passenger class.
```{r}
library(vcd)
addmargins(prop.table(mytable))
mosaic(mytable, shade=TRUE, legend=TRUE)
```

#  What is the probability of survival for passengers in the ???rst and second classes? 
```{r}
(mytable[2,1]+mytable[2,2])/sum(mytable[2,])
```

#  Conduct the test of independence between survival status and passenger class in R. 
```{r}
#  Write down the Expected Table
chisq.test(mytable)
Xsq <- chisq.test(mytable)
Xsq$observed
Xsq$expected
round(Xsq$expected,2)
#  Manually calculate the test statistic using your Expected Table
133.09/549
82.91/216
113.37/549
70.62/216
(Xsq$observed-Xsq$expected)^2/Xsq$expected
sum((Xsq$observed-Xsq$expected)^2/Xsq$expected)
1-pchisq(102.889, df=2)

# Report and intepret your results. the null hypothesis is independent, and we get conclusion is zero. Hence, we reject it. It is dependent.
```

#  Propose a way to visualize independence, without using statistical tests, such as those suggested by Heather and Richard in class. Denponging on the bar chart, we can learn the probability of 3rd class for people not survived is larger than 1st and  2nd class, and the 1st class for survived is higher than others. Hence, the relationship between survived and Pclass is dependent.
```{r}
library(ggplot2)
ggplot(titanic,aes(x=Pclass))+geom_histogram()+facet_grid(~Survived)+theme_bw()

```
Titanic passengers:
1. Survived Survival (0 = No; 1 = Yes) 
2. pclass Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd) 
3. name Name 
4. sex Sex 
5. age Age 
6. sibsp Number of Siblings/Spouses Aboard 
7. parch Number of Parents/Children Aboard 
8. ticket Ticket Number 
9. fare Passenger Fare 
10. cabin Cabin 
11. embarked Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)
# 1. The prediction using these predictors, Pr[Survival|Pclass, Sex], is simply a function of Pclass,Sex. Use R to calculate survival predictions for each combination of Pclass,Sex values. 
# (a) Write out this function using your answer in (1) Pr[Survival=1|Pclass, Sex]] and Pr[Survival = 0|Pclass, Sex]for every possible combination of predictors. 
```{r}
library(titanic)
mytable <- xtabs(~Pclass + Sex + Survived, data = titanic_train)
mytable
mytable1 <- xtabs(~Pclass + Sex, data = titanic_train)
mytable1
```
```{r}
joint.dist <- prop.table(mytable) 
margins <- addmargins(joint.dist)
margins
joint.dist1 <- prop.table(mytable1) 
margins1 <- addmargins(joint.dist1)
margins1
```  

Pr[Survival=1|Pclass, Sex]]
```{r}
margins["1","female","1"]/margins1["1","female"]
margins["1","male","1"]/margins1["1","male"]
margins["2","female","1"]/margins1["2","female"]
margins["2","male","1"]/margins1["1","male"]
margins["3","female","1"]/margins1["3","female"]
margins["3","male","1"]/margins1["1","male"]
margins["Sum","Sum","1"]/margins1["Sum","Sum"]
```


Pr[Survival=0|Pclass, Sex]]
```{r}
margins["1","female","0"]/margins1["1","female"]
margins["1","male","0"]/margins1["1","male"]
margins["2","female","0"]/margins1["2","female"]
margins["2","male","0"]/margins1["1","male"]
margins["3","female","0"]/margins1["3","female"]
margins["3","male","0"]/margins1["1","male"]
margins["Sum","Sum","0"]/margins1["Sum","Sum"]
```



# (b) Write out this function in ONE equation using your answer in 1.a with the help of an indicator function, as we did in class.
Conditional Independent Distribution
```{r}
library(naivebayes)
model <- naive_bayes(factor(Survived) ~ Pclass + Sex, data = titanic_train)
model
```
```{r}
titanic <- titanic_train
y <- titanic$Survived
x <- titanic$Pclass
z <- titanic$Sex
data <- as.data.frame(cbind(y,x,z)) 
 
data$dummies <- model.matrix(~factor(y)+0, data=data) 

aggregate(data$dummies,by=list(x,z),FUN=mean) 
```

## Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
##     Title + FamilySize
```{r}
logprediction<-glm(formula = Survived ~ Pclass + Sex + Age + Title + FamilySize
    
```
##     family = binomial(link = "logit"), data = train)
