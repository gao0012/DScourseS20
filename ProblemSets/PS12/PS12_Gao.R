```{r}
library(stargazer)
library(tidyverse)
library(sampleSelection)

wage <- read.csv("/Users/gaoningjing/Downloads/wages12.csv")
view(wage)
college <- as.factor(wage$college)
married <- as.factor(wage$married)
union <- as.factor(wage$union)

stargazer(wage)


#7
reg <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data=wage, na.action=na.omit)
print(summary(reg))





wage$logwage[is.na(wage$logwage)] <- mean(wage$logwage,na.rm=T)

wage_mean <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data=wage)
print(summary(wage_mean))



wages1<- read.csv("/Users/gaoningjing/Downloads/wages12.csv")
wages1<- as.data.frame(wages1)
wages1$valid<- 1
wages1$valid[is.na(wages1$logwage)] <- 0
head(wages1)

wages1$logwage[is.na(wages1$logwage)]<- 0

heckman.results<- selection(selection = valid ~ hgc + union + college + exper + married + kids,
          outcome = logwage ~ hgc + union + college + exper + I(exper^2),
          data = wages1, method = "2step")
print(summary(heckman.results))



valid = is.na(wage$logwage) <- 0

heckman_selection <- selection (selection = valid ~ hgc + union + college + exper + married + kids , outcome = logwage ~ hgc + union + college + exper + I (exper ^2) , data = wages1, method = "2step")
summary(wage_selection)


stargazer(listwise.results, results.means, heckman.results, title = "Results", align = TRUE)

stargazer(reg, wage_mean,heckman.results)


#8
wage_probit = glm(union ~ hgc + college + exper + married + kids, family = binomial(link="probit"), data =wage)
print(summary(wage_probit))


wage$predProbit <- predict(wage_probit, newdata = wage, type = "response")
print(summary(wage$predProbit))


#9
wage_probit$coefficients["kids"] <- 0
wage_probit$coefficients["married"] <- 0

wage$counterprobit <- predict(wage_probit, newdata = wage, type = "response")
wage_diff <- wage$predProbit-wage$counterprobit
summary(wage_diff)

```
