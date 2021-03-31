library(readr)
library(haven)
library(ggplot2)
library(tidyverse)
library(car)
library(olsrr)
library(sandwich)
library(lmtest)
library(knitr)
library(kableExtra)
library(estimatr)#robust_lm
library(stargazer) #for creating tables
gradeData = read_dta("/Users/collinkennedy/Google Drive/Programming Folder (4Transfer to Collin's Files V2)/R/econometrics/portugal_math.dta")

#========================================================================================================



#convert all categorical string variables manually to factors
gradeData$school = factor(gradeData$school)
gradeData$sex = factor(gradeData$sex)
gradeData$address = factor(gradeData$address)
gradeData$famsize = factor(gradeData$famsize)
gradeData$pstatus = factor(gradeData$pstatus)
gradeData$medu = factor(gradeData$medu)
gradeData$fedu = factor(gradeData$fedu)
gradeData$mjob = factor(gradeData$mjob)
gradeData$fjob = factor(gradeData$fjob)
gradeData$reason = factor(gradeData$reason)
gradeData$guardian = factor(gradeData$guardian)
gradeData$traveltime = factor(gradeData$traveltime)
gradeData$studytime = factor(gradeData$studytime)
gradeData$schoolsup = factor(gradeData$schoolsup)
gradeData$famsup = factor(gradeData$famsup)
gradeData$paid = factor(gradeData$paid)
gradeData$activities = factor(gradeData$activities)
gradeData$nursery = factor(gradeData$nursery)
gradeData$higher = factor(gradeData$higher)
gradeData$internet = factor(gradeData$internet)
gradeData$romantic = factor(gradeData$romantic)
gradeData$famrel = factor(gradeData$famrel)
gradeData$freetime = factor(gradeData$freetime)
gradeData$goout = factor(gradeData$goout)
gradeData$dalc = factor(gradeData$dalc)
gradeData$walc = factor(gradeData$walc)
gradeData$health = factor(gradeData$health)



lm(g3~school,data = gradeData)

unrestr = lm(g3~ sex + famsize, data = gradeData)
restr = lm(g3 ~ sex, data = gradeData)

waldtest(restr,unrestr, vcov = vcovHC(unrestr, type = "HC0"))

summary(gradeData$fedu)

?lm_robust

#data exploration

#correlation matrix
gradeDF = data.frame(g1 = gradeData$g1, g2 = gradeData$g2, g3 = gradeData$g3)
cor(gradeDF, method = "pearson") #very high multicollinearity, drop these from the model


#drop g1 and g2 from the dataset
gradeData = gradeData %>% 
  select(-c(g1,g2))


#create interaction variables
gradeData = gradeData %>% 
  mutate(failures_absences_inter = failures:absences) %>% 
  select(-c(studytime_sex_inter))




#Visualize grades and failures
gradeVSFailures = ggplot(data = gradeData, mapping = aes(x = failures, y = g3))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_y_continuous("Final Grades (G3)") +
  ggtitle("Final Grades (G3) versus No. of Class Failures")
ggsave("/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN140-Econometrics/gradesVsFailures.png",width=6,height=4,units='in')


#overall significance test: full model
full = lm(g3 ~ sex + age + medu + fedu + studytime + failures + higher + 
            internet + absences,data = gradeData)
summary(full)
coeftest(full, vcov = vcovHC(full, type = "HC0"))
plot(full)

hm = coeftest( full, vcov = vcovHC(full, type="HC0"))#robust standard errorrs, t test


waldtest(model, model_unres, vcov = vcovHC(model_unres, type = "HC0")) #robust standard errors, ftest


#step(full, direction = "backward")
?step

#Multiple Regression Table Visualization

robustLM = lm_robust(g3 ~ sex + age + medu + fedu + studytime + failures + higher + 
                       internet + absences + higher:schoolsup + higher:famsup,data = gradeData)
robustLM
class(robustLM)
str(robustLM)

robustLM$std.error

robustLM$statistic

regTable = data.frame( = empty, Estimate = robustLM$coefficients, Rob.Std.Errors = robustLM$std.error,
                   tValue = robustLM$statistic, p.value = robustLM$p.value,
                   CI_Lower = robustLM$conf.low, CI_Upper = robustLM$conf.high,
                   DF = robustLM$df)


regTable %>% 
  kbl(caption = "Multiple Regression Table") %>%
  kable_styling(font_size = 13) %>% 
  kable_material_dark()                   
?kbl
?kable_styling

  
#linear model 1:
#First I intend to look at how # of class failures affects final grade performance, after 
#controlling for # of absences and study time. I also intend to determine if its effect
#is dependent on study time (interaction between failures and study time?)




#pretty output:
dontInclude1 = c("schoolMS","sexM", "age","addressU","famsizeLE3", "pstatusT","medu", "fedu", "mjobhealth", 
                "mjobother","mjobservices", "mjobteacher","fjobhealth", "fjobother", "fjobservices", "fjobteacher",
                "reasonhome", "reasonother", "reasonreputation", "guardianmother", "guardianother", "traveltime",
                "schoolsupyes", "famsupyes", "paidyes","activitiesyes", "nurseryyes", "internetyes", "romanticyes",
                "famrel", "freetime", "goout", "dalc", "walc","health", "g1", "g2","higheryes", "studytime2", "studytime3","studytime4")

stargazer(hm,model1Simple,model1Interaction,type = "text",column.labels = c("robust se","simple","full"), omit = dontInclude1, add.lines = list(c("MODEL","full model","simple model","full model w/ interaction")))

?stargazer                                                                                                                                                                                             
stargazer(full,model1Simple,model1Interaction,type='latex',
          float=FALSE,digits=2,
          out="/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN140-Econometrics/model1_output.tex")

#run hypothesis tests here...

#test if failures is statistically significant, has non zero effect (include both terms ie failures and failures * absences)
#linearHypothesis(model1Interaction,c("failures = 0","failures:absences = 0"))

#test multiple restrictions
restr = lm(g3 ~ sex + age + medu + fedu + studytime + higher + 
             internet + absences,data = gradeData)
unrestr1 = lm(g3 ~ sex + age + medu + fedu + studytime + failures + higher + 
               internet + absences + failures:absences,data = gradeData)
multRestr = waldtest(restr,unrestr, vcov = vcovHC(unrestr1, type = "HC0")) 
multRestr


#looks like failures definitely has a nonzero effect on final grade performance
hm = coeftest(unrestr1, vcov = vcovHC(unrestr, type = "HC0")) 
hm




stargazer(restr,unrestr1,type = "text",column.labels = c("failures = failures:absences = 0","failures:absences = 0"), omit = dontInclude1, add.lines = list(c("MODEL","full model","simple model","full model w/ interaction")))


#test if the effect of failures on performance is dependent on study time (testing interaction)
#is there actually an interaction between failures and number of absences?
linearHypothesis(hm, c("failures_absences_inter = 0"))  #single restriction

#Yep. Our interaction term is statistically significant.The effect of class failures on final grade performance, holding all else constant, 
#is dependent on the number of absences.







#Linear Model 2
#How does study time effect final grade performance, and is there a difference between females
#and males after controlling for extra paid classes in math, as well as family as school support?

#visualize the data first 
ggplot(data = gradeData, mapping = aes(x = studytime, y = g3))+
  geom_point(colour = "red") +
  geom_smooth(method = "lm", se = FALSE)+
  scale_y_continuous("Final Grades (G3)") +
  ggtitle("Final Grades (G3) versus (levels of) Study Time")


model2Interaction = lm(g3 ~ . - g1 - g2 + studytime*sex, data = gradeData)


dontInclude2 = c("schoolMS", "age","addressU","famsizeLE3", "pstatusT","medu", "fedu", "mjobhealth", 
                 "mjobother","mjobservices", "mjobteacher","fjobhealth", "fjobother", "fjobservices", "fjobteacher",
                 "reasonhome", "reasonother", "reasonreputation", "guardianmother", "guardianother", "traveltime",
                 "schoolsupyes", "famsupyes", "paidyes","activitiesyes", "nurseryyes", "internetyes", "romanticyes",
                 "famrel", "freetime", "goout", "dalc", "walc","health", "g1", "g2","higheryes","failures","absences")


stargazer(full,model2Simple,model2Interaction,type = "text", omit = dontInclude2, add.lines = list(c("MODEL","full model","simple model","full model w/ interaction")))
stargazer(full,model2Simple,model2Interaction,type='latex',
          float=FALSE,digits=2,
          out="/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN140-Econometrics/model2_output.tex")

#run hypothesis tests

#interesting that study time does not appear to be a good predictor of grade performance
#unsurprisingly, effect of study time on grade performance does not appear to depend on the sex of the 
#student
                                                                                                     
                                                                                                                                                                                                        "main model w/ interaction")))
#hypothesis tests

restr = lm(g3 ~ sex + age + medu + fedu + failures + higher + 
             internet + absences,data = gradeData)
unrestr2 = lm(g3 ~ sex + age + medu + fedu + studytime + failures + higher + 
               internet + absences + studytime:age,data = gradeData)
multRestr = waldtest(restr,unrestr2, vcov = vcovHC(unrestr, type = "HC0")) 
multRestr


#test if study time is significant (so study time and the interaction)
linearHypothesis(model2Interaction, c("studytime = 0", "sexM:studytime = 0"))

#studytime doesn't appear to have a detectable affect on final grade performance.... hmm
#why? is this representative of the limitations of statistical testing


#test if interaction is significant alone if it is conclude that the effect of study time on 

?plogis




#=======================================================================================================
#Model 3
#=======================================================================================================
summary(lm(g3~higher))

model3Restr = lm(g3 ~ sex + age + medu + fedu + studytime + failures + higher + 
                   internet + absences,data = gradeData)
model3 = lm(g3 ~ sex + age + medu + fedu + studytime + failures + higher + 
              internet + absences + higher:schoolsup + higher:famsup,data = gradeData)
summary(model3)

multRestr = waldtest(restr,model3, vcov = vcovHC(model3, type = "HC0")) 
multRestr


#=====stargazer nice output==============================================================#
stargazer(full, unrestr1, unrestr2, model3, type = "text")


#========================================================================================================
#Logistic Regression Model:How well does a student's number of class failures predict their desire
#to pursue higher education
#=======================================================================================================

#convert higher variable to factor: 
gradeData$higher = factor(gradeData$higher)
contrasts(gradeData$higher)
str(gradeData$higher)



logisticFit = glm(higher ~ failures, data = gradeData, family = "binomial" )
summary(logisticFit)
#For every additional class failure, the log odds of the respective student wanting to pursue 
#higher education declines by about 1.0053.

exp(logisticFit$coefficients)
#In other words, the odds of wanting to pursue higher education decreases by a factor of about .36




