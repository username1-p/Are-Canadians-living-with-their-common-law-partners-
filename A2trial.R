#This is some rough work I did during the data analysis. There may be some errors 
#in this r file. My RMD file is the file with final models, final plots, etc..


rm(list = ls())
library(tidyverse)
library(nnet)
library(rms)
library(dplyr)
library(glmnet)
#install.packages("corrplot")
library(corrplot)
                                                       
require(gridExtra)
library(VGAM)

library(ggplot2)
library(rms)
#install.packages("imputeMissings")
library(imputeMissings)
#install.packages("RSQLite")
library(RSQLite)


mydata <- read.csv("gss.csv")
is.na(mydata)
colSums(is.na(mydata))
((colSums(is.na(mydata)))/20602)*100




#remove variable caseid,and major missing variables 80-90% data missing 
mydata <- mydata[-c(4,6,7,9,10,16,18,25,26,36,50,51,52,55,65,66,
                    67,68,69,70,71,72,73,74,75,76,79)]

#newdata <as.data.frame(mydata)
newdata <- impute(mydata,method = "median/mode")
colSums(is.na(newdata))

lapply(mydata[], unique)

colSums(is.na(mydata))

test <- newdata[sample(seq_len(nrow(newdata)), size = 6180),]
train <- newdata[!newdata$caseid %in% test$caseid,]

countno <-  which(newdata$lives_with_partner=="No")
length(countno)
length(countno)/20602

countyes <-  which(newdata$lives_with_partner=="Yes")
length(countyes)
length(countyes)/20602


#mydata[] <- lapply(mydata, factor, exclude = NULL)
#above line makes NA a factor as well? try it to see 

#omit rows with NA values, no all rows are gone then
#data2 <- na.omit(mydata)

#eda plots
newdata %>% 
  ggplot(aes(y=lives_with_partner, x = age)) +
  geom_boxplot() +ggtitle("Age vs. Living with Partner") +labs(y= "Lives with partner", x = "Age")



plot1<-newdata %>% 
  ggplot(aes(y=feelings_life, x = age)) +
  geom_point() +ggtitle("Age and Feelings in life")
plot1


plot2<-newdata %>% 
  ggplot(aes(y=marital_status, x = age)) +
  geom_boxplot() +ggtitle("Age vs Marital Status")
plot2

plot3<-newdata %>% 
  ggplot(aes(y=living_arrangement, x = age)) +
  geom_boxplot() +ggtitle("Age vs Living Arrangements")
plot3

require(gridExtra)
grid.arrange(plot2, plot3, ncol=2)





plot6<-newdata %>% 
  ggplot(aes(y=hh_type, x = age)) +
  geom_boxplot() +ggtitle("Age vs hh_type")

plot6

plot7<-newdata %>% 
  ggplot(aes(x=age, y= age_first_child)) +
  geom_point() +ggtitle("Age vs Age of first child")
plot7

plot8<-newdata %>% 
  ggplot(aes(x=age, y= total_children)) +
  geom_point() +ggtitle("Age vs Total Children")
plot8

plot9<-newdata %>% 
  ggplot(aes(x=age, y= age_at_first_birth)) +
  geom_point() +ggtitle("Age vs Age at first birth")
plot9

#above graph very important!!

newdata %>% 
  ggplot(aes(y=marital_status, x= age)) +
  geom_boxplot() +ggtitle("1")


newdata %>% 
  ggplot(aes(y=ever_given_birth, x= age)) +
  geom_boxplot() +ggtitle("1")

newdata %>% 
  ggplot(aes(y=future_children_intention, x= age)) +
  geom_boxplot() +ggtitle("1")

newdata %>% 
  ggplot(aes(y=ever_given_birth, x= age)) +
  geom_boxplot() +ggtitle("1")

hist(newdata$age)







modelnew <- glm(as.factor(lives_with_partner) ~ ., newdata, family="binomial")
#doesnt converge!

#model3 is model that worked which also converged

model3<- glm(as.factor(lives_with_partner) ~ 
                  
                  age + age_first_child  +age_at_first_birth + feelings_life+
                  sex+
                  
                  place_birth_canada           +
                  place_birth_province      +  province     + marital_status+
               aboriginal+ vis_minority + citizenship_status +education+own_rent
              +partner_vis_minority  
             
             + as.factor(self_rated_mental_health) +
               ever_given_birth + ever_fathered_child
            #+as.factor(children_in_household)
            +number_total_children_intention + has_grandchildren
            +grandparents_still_living
            #+ever_married
            +number_marriages + future_children_intention 
                  
                  , train, family="binomial")


summary(model3)

#aic selection
aic <- step(model3, trace = 1, k=2)

#BIC SELECTION
n <- nrow(train)
bic <- step(model3, trace = 1, k = log(n), direction = "both") 
bic<-attr(terms(bic), "term.labels")   
bic

modelselect <- glm(as.factor(lives_with_partner) ~ age_first_child + feelings_life + 
                     marital_status + aboriginal + partner_vis_minority, 
                   train[-c(1),], family="binomial"
                     
)

#trying to get good predictors, i will add what i believe to be important now
anova(modelselect, test="Chisq")

modelcompare1 <- glm(as.factor(lives_with_partner) ~ age+age_first_child + feelings_life + 
                     marital_status + aboriginal + partner_vis_minority, 
                   train[-c(1),], family="binomial"
                   
)

anova(modelselect, modelcompare1, test="LRT")

#age is added

modelcompare1 <- glm(as.factor(lives_with_partner) ~ age+age_first_child + feelings_life + 
                       marital_status + aboriginal + partner_vis_minority, 
                     train[-c(1),], family="binomial"
                     
)

modelcompare2 <- glm(as.factor(lives_with_partner) ~ age+age_first_child + feelings_life + 
                       marital_status + aboriginal + partner_vis_minority + total_children +sex
                     +place_birth_canada +province , 
                     train[-c(1),], family="binomial"
                     
)
modelcompare3 <- glm(as.factor(lives_with_partner) ~ age+age_first_child + feelings_life + 
                       marital_status + aboriginal + partner_vis_minority + total_children +sex
                     +place_birth_canada +province +living_arrangement +pop_center+vis_minority
                     +average_hours_worked +self_rated_health+self_rated_mental_health 
                     +income_family+children_in_household, 
                     train[-c(1),], family="binomial"
                     
)

summary(modelcompare3)

anova(modelcompare3, test="Chisq")

anova(modelcompare2, modelcompare3, test="LRT")


#health and mental health!!income_family!!children in household!!
#total chldren add, sex add, place_birth_canada, province, LIVING ARRANGMENT MUST ADD, avg hours worked!!
#pop_center, vis_minority unsure...



modellogit <- glm(as.factor(lives_with_partner) ~ age+age_first_child + feelings_life + 
                       marital_status + aboriginal  +sex
                     +place_birth_canada  +living_arrangement 
                     +average_hours_worked +self_rated_health+self_rated_mental_health 
                     +income_family+children_in_household, 
                     train[-c(1),], family="binomial"
                     
)

summary(modellogit)



#aic selection
aic <- step(modellogit, trace = 1, k=2)

#BIC SELECTION
n <- nrow(train)
bic <- step(modellogit, trace = 1, k = log(n), direction = "both") 
bic<-attr(terms(bic), "term.labels")   
bic


modelfinal <- glm(as.factor(lives_with_partner) ~ age+ marital_status 
                    
                    + feelings_life  +children_in_household +place_birth_canada
                  +income_family ,
                    
                  
                  train[-c(1),], family="binomial")
                  

plot(modelfinal)


summary(modelfinal)
modelfinal$coefficients

#install.packages("gtsummary")
#library(gtsummary)

tbl_regression(modelfinal, exponentiate = TRUE)



#lviing arranement goes to 0 or 1 and messes model 
#mental health has big SE
#martial status one factor has be SE but keepingit because it is significant

#age, +place_birth_canada  income_family, living arrangment, self rated mental health

res.dev <- residuals(modelfinal, type = "deviance")
plot(res.dev)+ title("Residuals Plot")

## Survey Estimation for Logistic Regression- simple random sampling
n=length(train$lives_with_partner)
N1=37000
#N 37000 is the target population


library(survey)
## Using the Survey Library
fpc.srs = rep(N1, n)

design <- svydesign(id=~1, data=train, fpc=fpc.srs)

mysvyglm <- svyglm(as.factor(lives_with_partner) ~ age+ marital_status 
                   
                   + feelings_life  +children_in_household +place_birth_canada
                   +income_family
             
                   , 
                   design, family="binomial")
summary(mysvyglm)
library(survey)
## Using the Survey Library stratified sampling
#population canada into 10 provinces each with its populaton 
newdata$N <- NA
newdata$N[newdata$province == "Ontario"] <- 14193384
newdata$N[newdata$province == "Newfoundland and Labrador"] <- 528817	
newdata$N[newdata$province == "Prince Edward Island"] <- 152021
newdata$N[newdata$province == "Nova Scotia"] <- 953869

newdata$N[newdata$province == "New Brunswick"] <- 759655
newdata$N[newdata$province == "Quebec"] <- 8394034
newdata$N[newdata$province == "Manitoba"] <- 1338109
newdata$N[newdata$province == "Saskatchewan"] <- 1163925
newdata$N[newdata$province == "Alberta"] <- 4286134
newdata$N[newdata$province == "British Columbia"] <- 4817160	
total<-  36627108
#ONLINE note says residents yukon,  northwest territories, and Nunavut excluded


newdata$NH <- NA
newdata$NH[newdata$province == "Ontario"] <- 14193384/total
newdata$NH[newdata$province == "Newfoundland and Labrador"] <- 528817	/total
newdata$NH[newdata$province == "Prince Edward Island"] <- 152021/total
newdata$NH[newdata$province == "Nova Scotia"] <- 953869/total

newdata$NH[newdata$province == "New Brunswick"] <- 759655/total
newdata$NH[newdata$province == "Quebec"] <- 8394034/total
newdata$NH[newdata$province == "Manitoba"] <- 1338109/total
newdata$NH[newdata$province == "Saskatchewan"] <- 1163925/total
newdata$NH[newdata$province == "Alberta"] <- 4286134/total
newdata$NH[newdata$province == "British Columbia"] <- 4817160	/total
newdata<-as.data.frame(newdata)
db1<- data.frame()

design <- svydesign(id=~1, strata ~province, data=as.matrix(newdata), fpc=~N, weights=~NH)
design <- svydesign(id=~1, strata ~province, data=as.matrix(newdata), fpc=~N, weights=~NH)

#doesnt work^
mysvyglm <- svyglm((lives_with_partner) ~ age+ marital_status 
                   
                   + feelings_life  +children_in_household +place_birth_canada
                   +income_family
                   
                   , 
                   design, family="binomial")
summary(mysvyglm)


#elastic net
fit = glmnet(x=model.matrix(~ ., data=train[, -c(1)]), y=train$lives_with_partner, family = "binomial")
plot(fit, xvar = "dev", label = TRUE)

#predict(fit, newx = x[1:5,], type = "class", s = c(0.05, 0.01))

cvfit = cv.glmnet(x=model.matrix(~ ., data=train[,  -c(1)]), 
                  y=train$lives_with_partner, family = "binomial", type.measure = "class")


plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se

coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")






#lrm.model<- lrm(formula = as.factor(lives_with_partner) ~ age_first_child + feelings_life + 
#                 marital_status + aboriginal + partner_vis_minority, train[-c(1),]
#               
 #               ,maxit=100,
  #                      x =TRUE, y = TRUE, model= T)
#
#cross.calib <- rms::calibrate(lrm.model, method="crossvalidation", B=10) # model calibration
#
#par(family = 'serif')
#plot(cross.calib, las=1, xlab = "Predicted Probability")





## Fit the model with lrm from rms package ##
#lrm.final <- rms::lrm(formula=as.factor(lives_with_partner) ~ age_first_child + feelings_life + 
 #                  marital_status + aboriginal + partner_vis_minority, data=train[-c(1),], 
  #               
   #              x =TRUE, y = TRUE, model= T, maxit=100)
#cross.calib <- rms::calibrate(lrm.final, method="crossvalidation", B=10) # model calibration

#par(family = 'serif')
#plot(cross.calib, las=1, xlab = "Predicted Probability")







#more rough work 

model1 <- glm(lives_with_partner ~  age + age_first_child +total_children +age_at_first_birth + feelings_life+
              sex+
              
              place_birth_canada     +         place_birth_father      +        place_birth_mother +
              place_birth_province      +                  province          +                region 
              , mydata, family ="binomial")



model1 <- glm(lives_with_partner ~ .
              , mydata, family="binomial")




#which variables are not working??
aic <- step(model1, trace = 0, k=2)
#model1 must work first!!



modellinear <- lm(future_children_intention ~ ., mydata)
mydata <- as.data.frame(mydata)
#omit rows with NA values
mydata2 <- mydata[!is.na(mydata$has_grandchildren), ]

lapply(mydata2[], unique)

mylogitnew<-glm(ever_married ~ age + age_first_child+age_youngest_child_under_6, data=mydata2, family="binomial")
summary(mylogitnew)

mydata2


