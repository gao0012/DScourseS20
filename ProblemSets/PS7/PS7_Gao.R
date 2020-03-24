---
title: "ps7"
output: ningjing gao
---


```{r}
library(mice)
library(tidyverse)
library(stargazer)
library(broom)
library(mice)
library(tidyr)
library(magrittr)
library(HotDeckImputation)
library(stargazer)
library(tidyverse)
wages <-  read.csv("https://raw.githubusercontent.com/gao0012/DScourseS20/master/ModelingOptimization/wages.csv")
stargazer(wages)
https://raw.githubusercontent.com/gao0012/DScourseS20/master/ModelingOptimization/wages.csv
wages1 <- wages %>% drop_na(hgc, tenure)
#1
wages2 <- wages1 %>% drop_na(logwage)
est1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages2)
stargazer(est1)


#2
mean_wages <- wages1
mean_wages$logwage[is.na(mean_wages$logwage)] <- mean(mean_wages$logwage, na.rm = TRUE) 
est2 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=mean_wages)
stargazer(est2)


#3
wages1$logwage_pred <- wages1$logwage
est3 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages1, na.action=na.exclude)
wages1$preds <- NA
wages1$preds [!is.na(wages1$hgc) & !is.na(wages1$tenure)] <- predict(est3, wages1, na.action=na.exclude)


stargazer(est1, est2, est3)


wages1.imp = mice(wages1, seed = 12345, m = 20)
summary(wages1.imp)
fit <- with(wages1.imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) +age +married))
summary(pool(fit))
```