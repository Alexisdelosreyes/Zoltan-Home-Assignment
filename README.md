# Zoltan-Home-Assignment
Psyp13
###Alexis De Los Reyes
## Zoltan Home Assignment 

###### Assignment 1

#load packages
library(psych)
library(lm.beta)
library(tidyverse)
library(gridExtra)
source("GraphPlot.R")

#load data
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")
paindata <- data_sample_1

#delete ID column
paindata$ID <- NULL

#summary 
paindata %>% summary()


#model .1
model.1 <- lm(pain ~ sex + age, data = paindata)

summary(model.1)
#Call:lm(formula = pain ~ sex + age, data = paindata)

#Residuals:
 # Min      1Q  Median      3Q     Max 
#-3.5837 -0.9468  0.0546  0.8401  5.0285 

#Coefficients:
 #3 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.98795    0.93994   8.498  1.4e-14 ***
 # sexmale      0.64818    0.22905   2.830 0.005265 ** 
#  age         -0.08924    0.02279  -3.915 0.000134 ***
  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 1.443 on 157 degrees of freedom
#Multiple R-squared:  0.1398,	Adjusted R-squared:  0.1289 
#F-statistic: 12.76 on 2 and 157 DF,  p-value: 7.317e-06

#plot of age and sex
openGraph()
paindata %>% ggplot() + aes(x = sex, y = age) + geom_point() + geom_smooth(method = "lm")

#Residuals vs Leverage graph 
openGraph()
model.1 %>% plot(which = 5)

#Cook's distance graph
openGraph()
model.1 %>% plot(which = 4)
# problematic cases are greater than one and in this subset no cases are greater than 1 

# QQ plot
openGraph()
model.1 %>% plot(which = 2)
# case 88 and 123 seem to be outliers

#histogram
openGraph()
residuals_model.1 = enframe(residuals(model.1))
residuals_model.1 %>% ggplot() + aes(x= value) + geom_histogram()
#at least one case where there is an outlier with a value of 5

#skew and kurtosis 
describe(residuals(model.1)) 
#vars      n mean   sd median   trimmed  mad   min  max range skew kurtosis se
# X1    1 160    0 1.43   0.05   -0.02 1.36 -3.58 5.03  8.61 0.19     0.29 0.11

#skew = 0.19 kurtosis = 0.29

#cut outliers 
outliers_model.1 = paindata %>% slice(-c(88,123))
model.11 = lm(pain ~ sex + age, data = outliers_model.1)
describe(residuals(model.11))
#   vars   n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
#X1    1 158    0 1.36   0.14    0.01 1.37 -3.6 3.38  6.98 -0.09    -0.31 0.11
#skew = -0.09 kurtosis = -0.31
# Less of skew and kurtosis after the removal of outliers

#new histogram 
residuals_model.11 = enframe(residuals(model.11))
openGraph()
residuals_model.11 %>% ggplot() + aes(x = value) + geom_histogram()
#no longer any outlier with a value of 5 or higher


#linearity 
library(car)
openGraph()
model.11 %>% residualPlots()
#           Test stat Pr(>|Test stat|)
#sex                                  
#age           0.0851           0.9323
#Tukey test   -0.6460           0.5183


#homoscedasticity
library(lmtest)
model.11 %>% plot(which = 3) #outliers might be 55,74, and 28
model.11 %>% ncvTest() #Chisquare = 0.09386776, Df = 1, p = 0.75932
model.11 %>% bptest() #BP = 0.58911, df = 2, p-value = 0.7449
# p value is not lower than 0.05, need to transform?



#summary of new data
summary(model.11)

#Call: lm(formula = pain ~ sex + age, data = outliers_model.1)
#Residuals:
 # Min      1Q  Median      3Q     Max 
#-3.6004 -0.9223  0.1407  0.7686  3.3838 
#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.97981    0.88920   8.974 8.92e-16 ***
 # sexmale      0.75516    0.21789   3.466 0.000684 ***
 # age         -0.09173    0.02157  -4.253 3.63e-05 ***

#Residual standard error: 1.364 on 155 degrees of freedom
#Multiple R-squared:  0.1751,	Adjusted R-squared:  0.1645 
#F-statistic: 16.45 on 2 and 155 DF,  p-value: 3.309e-07

AIC(model.11) #551.48

#confidence intervals
confint(model.11)
#                 2.5 %      97.5 %
#(Intercept)  6.2232962  9.73632809
#sexmale      0.3247446  1.18558128
#age         -0.1343297 -0.04912261

#standardized coefficients 
lm.beta(model.11)
#Standardized Coefficients::
#(Intercept)     sexmale         age 
#   0.0000000   0.2537890  -0.3114371 







#model 2.2 
model.2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = paindata)
model.2

#summary of model.2
summary(model.2)
str(model.2)
head(model.2)

# no graph, maybe too many variables?
openGraph()
paindata %>% ggplot() + aes( y = pain, x = sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva) + geom_point()

#Cook's distance graph
openGraph()
model.2 %>% plot(which = 4)
# no cases more than 1, outliers might be 74,88, and 123

#normality 
openGraph()
model.2 %>% plot(which = 2)
# 74 and 88 are a little off the line but not significantly

residuals_model.2 = enframe(residuals(model.2))
openGraph()
residuals_model.2 %>% ggplot() + aes(x = value) + geom_histogram()
#about 1 or 2 outliers from the histogram witha value of 3 

describe(residuals(model.2))
#vars   n mean   sd median trimmed  mad  min  max range  skew kurtosis   se
#X1    1 160    0 1.13  -0.07    0.01 1.09 -2.8 2.99  5.78 -0.04    -0.33 0.09
#skew = -0.04 kurtosis = -0.33

#linearity 
openGraph()
model.2 %>% residualPlots()

#Test stat
#sex                      
#age               -0.8391
#STAI_trait         0.8916
#pain_cat           1.4687
#mindfulness        0.5141
#cortisol_serum    -0.1464
#cortisol_saliva   -1.0928
#Tukey test         0.7126

#Pr(>|Test stat|)
#sex                             
#age                       0.4027
#STAI_trait                0.3740
#pain_cat                  0.1440
#mindfulness               0.6079
#cortisol_serum            0.8838
#cortisol_saliva           0.2762
#Tukey test                0.4761

#all tests are non significant thus the linearity assumption holds true

#Homoscedasticity
openGraph()
model.2 %>% plot(which = 3)
#88, 74 and 119 look like outliers

model.2 %>% ncvTest()
#Chisquare = 0.4217238, Df = 1, p = 0.51608

model.2 %>% bptest()
#BP = 8.9661, df = 7, p-value = 0.2551
#both tests are non significant

model.2 %>% vif()
#   sex             age 
#1.111984        1.460652 
#STAI_trait        pain_cat 
#1.598513        1.915498 
#mindfulness  cortisol_serum 
#1.428859        4.859013 
#cortisol_saliva 
#5.256584

#cortisol_serum/saliva and pain cat are the best predictors of pain, VIF larger than 1.79

#correlation matrix
openGraph()
paindata %>% select(pain, sex, age, STAI_trait, pain_cat,mindfulness, cortisol_serum, cortisol_saliva) %>% pairs.panels(col = "red", lm = T)


#coefficent table measures
summary(model.2)
AIC(model.2) #509.58
confint(model.2)
#                      2.5 %       97.5 %
#(Intercept)     -1.13560219  4.999488064
#sex1            -0.41594480 -0.035927754
#age             -0.06403452  0.022650643
#STAI_trait      -0.08146457 -0.001010183
#pain_cat         0.05785620  0.151672870
#mindfulness     -0.49896046 -0.069184177
#cortisol_serum  -0.45503057  0.322885914
#cortisol_saliva  0.26072968  1.117464051

lm.beta(model.2)



#summary of models
summary(model.1)$adj.r.squared #0.13
summary(model.2)$adj.r.squared #0.44
# variance is more explained by adding the additional predictors

#AIC
AIC(model.1) #576.34
AIC(model.2) #509.58
#models are significantly different in their model fit because the difference is greater than 2

#ANOVA
anova(model.1, model.2) 
# Res.Df    RSS   Df Sum of Sq   F      Pr(>F)    
#1    157 326.83                                  
#2    152 202.29  5    124.54 18.716  1.815e-14 ***
#F = 18.716
#df = 5
#P value = 1.82 e-14


#regression equation of model 2





#########Assignment 2

#load packages
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

#load data
data_sample_2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
paindata2 <- data_sample_2

#remove ID column
paindata2$ID <- NULL

#run backward regression with data file 1 first 
#exclude any participants you excluded in the first analysis
# did not exclude any participants from model.2 

#inital backmodel proposed by person
backmodel.1 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = paindata)
summary(backmodel.1)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       5.743e+00  1.839e+00   3.123 0.002146 ** 
#  sex1             -2.098e-01  9.886e-02  -2.122 0.035500 *  
#  age              -4.886e-02  2.151e-02  -2.271 0.024540 *  
#  STAI_trait       -1.484e-02  1.944e-02  -0.763 0.446532    
#pain_cat          8.455e-02  2.316e-02   3.650 0.000361 ***
#  mindfulness      -3.242e-01  1.113e-01  -2.914 0.004113 ** 
#  cortisol_serum    4.527e-01  1.101e-01   4.113 6.41e-05 ***
#  weight           -1.589e-02  9.377e-03  -1.694 0.092267 .  
#IQ               -7.163e-03  6.145e-03  -1.166 0.245537    
#household_income -5.031e-06  3.800e-06  -1.324 0.187475    
---

#Residual standard error: 1.177 on 150 degrees of freedom
#Multiple R-squared:  0.4533,	Adjusted R-squared:  0.4205 
#F-statistic: 13.82 on 9 and 150 DF,  p-value: 4.963e-16



#backward regression
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
backmodel <- train(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = paindata, method = "leapBackward", tuneGrid = data.frame(nvmax = 1:9), trControl = train.control)


backmodel$results
summary(backmodel)
backmodel$bestTune #nine variables?
summary(backmodel$finalModel) #pain_cat and cortisol_serum actually mean something

coef(backmodel$finalModel, 9)
#(Intercept)             sex1              age       STAI_trait         pain_cat      mindfulness   cortisol_serum 
#5.742752e+00    -2.097651e-01    -4.885730e-02    -1.483579e-02     8.455104e-02    -3.242498e-01     4.526997e-01 
#weight               IQ household_income 
#-1.588804e-02    -7.163500e-03    -5.031341e-06 

#only using pain_cat and cortisol serum because they are the only ones that are significant predictors


#use the predictors found in the first data for backward model
backward_model <- lm(pain ~ pain_cat + cortisol_serum, data = paindata)
theory_based_model <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = paindata)

summary(backward_model)

confint(backward_model)
                      #2.5 %     97.5 %
 # (Intercept)    -2.46739564 0.01474024
#pain_cat        0.08642786 0.16441340
#cortisol_serum  0.24511914 0.65112645

lm.beta(backward_model)
#(Intercept)       pain_cat cortisol_serum 
#0.0000000      0.4326855      0.2969481

#AIC
AIC(backward_model)#526.63
AIC(theory_based_model) #509.58

#anova
anova(backward_model,theory_based_model)
#Analysis of Variance Table
#Model 1: pain ~ pain_cat + cortisol_serum
#Model 2: pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    157 239.55                                  
#2    152 202.29  5    37.261 5.5995 9.138e-05 ***


#compare inital backward model to created backward model
AIC(backmodel.1) #517.83
AIC(backward_model) #526.63
anova(backmodel.1, backward_model)
#Res.Df    RSS Df Sum of Sq     F   Pr(>F)   
#1    150 207.73                               
#2    157 239.55 -7   -31.817 3.282 0.002831 **


#load new data
home_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
paindata3 <- home_sample_2

paindata3$ID <- NULL
str(paindata3)
describe(paindata3)

#predicting performance with the backward and theory-based model
pred_test <- predict(backward_model, paindata3)
pred_test_theory <-predict(theory_based_model, paindata3)

#calc sum of squared residuals
RSS_test_backward = sum((paindata3[,"pain"] - pred_test)^2)
RSS_test_theory <- sum((paindata3[,"pain"] - pred_test_theory)^2)

RSS_test_backward #259.84
RSS_test_theory #229.46
#backward model has more error than the theory based model

####### Assignment 3

#load data
home_sample_3 <-read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")
paindata4 <- home_sample_3

home_sample_4 <-read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")
paindata5 = home_sample_4
str(paindata5)

#linear mixed model of data 3
#load packages
library(psych)
library(tidyverse)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
source("GraphPlot.R")

#view data and clean data
str(paindata4)
paindata4$ID <- NULL

#addition variable of hospital with 10 levels 
#sex is messed up 
paindata4_cleaned <- paindata4 %>% mutate(sex = droplevels(replace(sex, 
                                                                      sex == "Female", "female")))
str(paindata4_cleaned) #looks good 
summary(paindata4_cleaned)

#mutate data
paindata4 %>% mutate(class = factor(hospital))
# max reached and omitted 124 rows but will see if this messes up the data 

#ggplot
windows()
paindata4_cleaned %>% ggplot() + aes(y = pain, x = cortisol_serum) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F )

int_plot = paindata4_cleaned %>% ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) + geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)

#ggplot with regression lines
windows()
int_plot
summary(int_plot)

#random intercept model
rnd_int_model = lmer(pain ~ cortisol_serum + (1 | hospital), data= paindata4_cleaned)

sum(residuals(rnd_int_model)^2) #323.84
cAIC(rnd_int_model)$caic #684.41
summary(rnd_int_model)

#ggplot with the hospitals seperated 
windows()
rnd_int_model %>% ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) + geom_point(aes(color = hospital), size = 4) + geom_line(color = "red", aes(y= pain, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

summary(rnd_int_model)
confint(rnd_int_model)

r2beta(rnd_int_model, method = "nsj", data = paindata4_cleaned)

#marginal and conditional R squared values
r.squaredGLMM(rnd_int_model)

#standard beta
stdCoef.merMod(rnd_int_model)

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}
library("lme4")
fixef(rnd_int_model)
stdCoef.merMod(rnd_int_model) #0.44


#predict using regression equation from data file 3
# y= 1.6 + 0.7X



#attempt at predicting 
repeated_variables = c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9","hospital_10")

cortisol_serum = describe(paindata5[, repeated_variables])$mean
repeated_painCIS = describe(paindata5[, repeated_variables])$se + 1.96
hospitals = as.numeric(as.factor(repeated_variables))
hospitals = as.data.frame(hospitals)
data_for_plot = cbind(hospitals,cortisol_serum, repeated_painCIS)

data_for_plot %>% ggplot() + aes( x = pain, y = cortisol_serum)
str(paindata5)

str(paindata5_int)

paindata5_int = lmer(pain ~ cortisol_serum + (1 | hospital), data = paindata5)


paindata5_int_pred = predict(rnd_int_model)


openGraph()
ggplot(paindata5_int, aes(y = pain, x = cortisol_serum, group = hospital)) + geom_point(size=3) + geom_line(color = "red", aes(y= paindata5_int_pred, x = cortisol_serum)) + facet_wrap(~ID, ncol=5)


#variance from 1-(RSS/TSS)
