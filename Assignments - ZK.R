#####################Assignment 1########################
install.packages("tidyverse")
library("tidyverse")
install.packages("psych")
library("psych")
install.packages("gridExtra")
library("gridExtra")

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
view(data_sample_1)

data_mutate <- data_sample_1 %>%
  filter(age < 100, STAI_trait > 10) %>% 
    mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))
view (data_mutate)
mode_test <- lm(pain ~ age + sex, data = data_sample_1 )

summary(mode_test)
model1 <- lm(pain ~ age + sex_recode, data = data_mutate )
model2 <- lm(pain ~ age + sex_recode + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data=data_mutate)

cooks.distance(model1) > 1
par(mar = c(1, 1, 1, 1))
plot(x = model1, which = 4)

cooks.distance(model2) > 1
par(mar = c(1, 1, 1, 1))
plot(x = model2, which = 4)

plot(x = model1, which = 5)
plot(x = model2, which = 5)

hist( x = residuals(model1), 
       xlab = "Value of residual", 
       main = "",
       breaks = 156
      ) 
plot( x = model1, which = 2 )

hist( x = residuals(model2), 
      xlab = "Value of residual", 
      main = "",
      breaks = 156
      )
plot( x = model2, which = 2 )

yhat.2 <- fitted.values( object = model1 )
plot( x = yhat.2,
      y = data_mutate$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
      )

yhat.2 <- fitted.values( object = model2 )
plot( x = yhat.2,
      y = data_mutate$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
      )

install.packages("car")
library("car")
install.packages("lmtest")
library("lmtest")

plot(x = model1, which = 1) 
plot(x = model1, which = 3) 
coeftest( model1, vcov= hccm )
vif( mod = model1) 

plot(x = model2, which = 1) 
plot(x = model2, which = 3) 
coeftest( model2, vcov= hccm )
vif( mod = model2)

###Model comparison#
summary(model1)
summary(model2)

confint(model1)
confint(model2)

data_scale <- lm(scale(pain) ~ scale(age) + scale(sex_recode) , data = data_mutate)
data_scale2 <- lm(scale(pain) ~ scale(age) + scale(sex_recode) + scale(STAI_trait) + scale(pain_cat) + scale(mindfulness) + scale(cortisol_serum) + scale(cortisol_saliva), data = data_mutate)
summary(data_scale)
summary(data_scale2)

summary(model1)$adj.r.squared
summary(model2)$adj.r.squared

AIC(model1)
AIC(model2)

anova(model1, model2) 

#####################Assignment 2#########################
install.packages("tidyverse")
library("tidyverse")
install.packages("psych")
library("psych")
install.packages("gridExtra")
library("gridExtra")

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
data_mutate2 <- data_sample_1 %>%
  filter(age < 100, STAI_trait > 10 ,IQ > 50, household_income > 0 ) %>% 
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))
view(data_sample_1)

model3 <- lm(pain ~ age + sex_recode + STAI_trait + pain_cat + mindfulness +cortisol_serum + weight + IQ + household_income, data = data_mutate2)

a<- cooks.distance(model3) > 4/length(data_mutate2)
length(a [a == TRUE])

par(mar = c(1, 1, 1, 1))
plot(x = model3, which = 4)

plot(x = model3, which = 5)
plot(x = model3, which = 2)

plot(x = model3, which = 1)
plot(x = model3, which = 3)

yhat.2 <- fitted.values( object = model3 )
plot( x = yhat.2,
      y = data_mutate2$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
      )

backward_model <- step(model3, direction = "backward")
theory_based_model <- model2

AIC(backward_model)
AIC(theory_based_model)

summary(backward_model)
summary(theory_based_model)

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")
data2_recode<- data_sample_2 %>% 
  filter(mindfulness <=6) %>% 
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))
view(data2_recode)

pred_backward <- predict(backward_model, data2_recode)
pred_theory <- predict(theory_based_model, data2_recode)
RSS_backward = sum((data2_recode[, "pain"] - pred_backward)^2)
RSS_theory = sum((data2_recode[, "pain"] - pred_theory)^2)

RSS_backward
RSS_theory

summary(backward_model)

data_scale3 <- lm(scale(pain) ~ scale(age) + scale(sex_recode) + scale(pain_cat) + scale(mindfulness) + scale(cortisol_serum) + scale(household_income), data = data_mutate2)
summary(data_scale3)

confint(backward_model)

AIC(backward_model)
AIC(model3)

##################Assignment 3###################
library(psych) 
library(tidyverse) 
install.packages("cAIC4")
library(cAIC4) 
install.packages("r2glmm")
library(r2glmm) 
library(lme4) 
library(lmerTest) 
install.packages("MuMIn")
library(MuMIn) 
install.packages("optimx")
library(optimx)
library("gridExtra")
install.packages("influence.ME")
library("influence.ME")

library(lattice) # for qqmath
library(lme4) # for mixed models
library(lmerTest) #
data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")
data_sample_3 <- data_sample_3 %>% 
  mutate(hospital = factor(hospital))
data_mutate_as3 <- data_sample_3 %>% 
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1,
                             "femlae" = 1))
test_reg <-lmer(pain ~ age + sex_recode + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva + (1|hospital), data = data_mutate_as3)
a<- cooks.distance(test_reg) > 4/length(data_mutate_as3)
length(a [a == TRUE])

test_reg
model2

confint(test_reg)
confint(model2)

summary(test_reg)

r.squaredGLMM(test_reg)

data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")
data_sample_4 <- data_sample_4 %>% 
  mutate(hospital = factor(hospital))
data_sample_4_recode <- data_sample_4 %>% 
  mutate(sex_recode = recode(sex,
                             "male" = 0,
                             "female" = 1))
view(data_sample_4)

RSS_as3 <- sum((data_sample_4_recode$pain - predict(test_reg, data_sample_4_recode, allow.new.levels = TRUE))^2)
mod_mean<- lm(pain~1, data_mutate_as3)
TSS_as3 <-sum((data_sample_4_recode$pain - predict(mod_mean, data_sample_4_recode))^2)
RSS_as3
TSS_as3
1-RSS_as3/TSS_as3

test_reg_scale = lmer(scale(pain) ~ scale(age) + scale(sex_recode) + scale(STAI_trait) + scale(pain_cat) + scale(mindfulness) + scale(cortisol_serum) + scale(cortisol_saliva) + (1|hospital), data = data_mutate_as3)
test_reg_scale

reg_as3_inf_int <- lmer(pain ~ cortisol_saliva +(1|hospital), data = data_mutate_as3)
reg_as3_inf_slope <- lmer(pain ~ cortisol_saliva +(cortisol_saliva|hospital),data = data_mutate_as3)

data_mutate_as3_slope <- data_mutate_as3 %>% 
  mutate(pred_int = predict(reg_as3_inf_int),
         pred_slope = predict(reg_as3_inf_slope))

cAIC(reg_as3_inf_int)$caic
cAIC(reg_as3_inf_slope)$caic

anova(reg_as3_inf_slope, reg_as3_inf_int)

data_mutate_as3_slope %>%
  ggplot() +
  aes(y = pain, x = cortisol_saliva, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_int, x=cortisol_saliva))+
  facet_wrap( ~ hospital, ncol = 2)

data_mutate_as3_slope %>%
  ggplot() +
  aes(y = pain, x = cortisol_saliva, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_slope, x=cortisol_saliva))+
  facet_wrap( ~ hospital, ncol = 2)


slope_plot = data_mutate_as3_slope%>%
  ggplot() +
  aes(y = pain, x = cortisol_saliva, color = hospital) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F, fullrange=TRUE) +
  xlim(0, 10)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

slope_plot
