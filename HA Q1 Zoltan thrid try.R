require(lsr)
require(psych)
library(tidyverse)
require(lm.beta)
mydata= read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")
summary(mydata)
describe(mydata)
View(mydata)
mydata %>% summary()

#I can make those two answers I don't like as a NA's:
library(dplyr)
library(magrittr)
mydata[!is.na(mydata$STAI_trait) & mydata$STAI_trait <20, ] %<>%
  mutate(STAI_trait = NA)

#but I'm gonna remove two subjects: with the 3.5 answer and with the minus income 
mydata[-c(18, 49), ]
mydata_cleaned <- mydata[-c(18, 49), ]
summary(mydata_cleaned)
describe(mydata_cleaned)
#I renamed my data to mydata_cleaned after removing two subjects

#now I will create first model 
model1_mydata_cleaned <- lm(pain ~ age + sex, data = mydata_cleaned)
#now model2 with variables age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures
model2_mydata_cleaned <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = mydata_cleaned)
#now I will look at adj.r
summary(model1_mydata_cleaned)$adj.r.squared
summary(model2_mydata_cleaned)$adj.r.squared

############   NOW I HAVE TO CHECK THE ASSUMPTIONS OF BOTH MODELS    ####################
### FIRST MODEL'S ASSUMPTIONS  ###
##  OUTLIERS  ##
require(olsrr)
ols_plot_cooksd_bar(model1_mydata_cleaned)
# treshold for this model is 4/n (4/158) = 0.02, but also the treshold could be 1, according to mr. Cook, so I can actually keep it
# I'll keep it

## NORMALITY OF THE FIRST MODEL ##
hist( x = residuals(model1_mydata_cleaned),
      xlab = "Value of residual",
      main = "",
      breaks = 20
)  # here I get a graph from which I can see that it's pretty normal
# but let's continue:
require(ggpubr)
resid <- residuals(model1_mydata_cleaned) # pull the residuals 
hist( resid ) # draw a histogram
qqnorm( resid ) # draw a normal QQ plot
shapiro.test( resid )

## LINEARITY ##
#(Navaro p.484):
values <- fitted.values( object = model1_mydata_cleaned)
plot( x = values,
      y = mydata_cleaned$pain
)
#looks approximatelly linear from the graph
plot(x = model1_mydata_cleaned, which = 1)
require(car)
residualPlots( model = model1_mydata_cleaned)
#there is no significance so it's good. it shows that there are no violations

## HOMOGENEITY OF VARIANCE ##
plot(x = model1_mydata_cleaned, which = 3)
ncvTest(model1_mydata_cleaned)

## COLINEARITY ##
vif( mod = model1_mydata_cleaned)

#######  CHECKING MODEL 2  ##########
# model2_mydata_cleaned #
##  OUTLIERS  ##
require(olsrr)
ols_plot_cooksd_bar(model2_mydata_cleaned)
# treshold for this model is 4/n (4/158) = 0.02, but also the treshold could be 1, according to mr. Cook, so I can actually keep it
# I'll keep it

## NORMALITY OF THE SECOND MODEL ##
hist( x = residuals(model2_mydata_cleaned),
      xlab = "Value of residual",
      main = "",
      breaks = 20
)  # here I get a graph from which I can see that it's pretty normal
# but let's continue:
require(ggpubr)
resid <- residuals(model2_mydata_cleaned) # pull the residuals 
hist( resid ) # draw a histogram
qqnorm( resid ) # draw a normal QQ plot
shapiro.test( resid )

## LINEARITY ##
#(Navaro p.484):
values <- fitted.values( object = model2_mydata_cleaned)
plot( x = values,
      y = mydata_cleaned$pain
)
#looks approximatelly linear from the graph
plot(x = model2_mydata_cleaned, which = 1)
require(car)
residualPlots( model = model2_mydata_cleaned)
#there is no significance so it's good. it shows that there are no violations

## HOMOGENEITY OF VARIANCE ##
plot(x = model2_mydata_cleaned, which = 3)
ncvTest(model2_mydata_cleaned)

## COLINEARITY ##
vif( mod = model2_mydata_cleaned)   # serum and saliva are too big (intercorrelation), so let's see which one we should remove:
summary(model2_mydata_cleaned) 
# Saliva shows significant results. Serum is considered to be a more reliable measure (says in the exercise)


#########   REMOVING SALIVA   ###########
model2_final <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = mydata_cleaned)
summary(model2_final)$adj.r.squared

##  OUTLIERS  ##
require(olsrr)
ols_plot_cooksd_bar(model2_final)
# treshold for this model is 4/n (4/158) = 0.02, but also the treshold could be 1, according to mr. Cook, so I can actually keep it
# I'll keep it
## NORMALITY OF THE SECOND MODEL ##
hist( x = residuals(model2_final),
      xlab = "Value of residual",
      main = "",
      breaks = 20
)  # here I get a graph from which I can see that it's pretty normal
# but let's continue:
require(ggpubr)
resid <- residuals(model2_final) # pull the residuals 
hist( resid ) # draw a histogram
qqnorm( resid ) # draw a normal QQ plot
shapiro.test( resid )


## LINEARITY ##
#(Navaro p.484):
values <- fitted.values( object = model2_final)
plot( x = values,
      y = mydata_cleaned$pain
)
#looks approximatelly linear from the graph
plot(x = model2_final, which = 1)
require(car)
residualPlots( model = model2_final)
# So, two factors, STAI_trait and pain_cat, shows significance. But overall score of Tukey (which assumes all the variables),
# is not significant, that means that we can keep it how it is ( read more in Navaro's book about this)

## HOMOGENEITY OF VARIANCE ##
plot(x = model2_final, which = 3)
ncvTest(model2_final)

## COLINEARITY ##
vif( mod = model2_final)   # serum and saliva are too big (intercorrelation), so let's see which one we should remove:
summary(model2_final)


### NOW I CAN COMPARE TWO MODELS ###

anova(model1_mydata_cleaned, model2_final)

AIC(model1_mydata_cleaned)
AIC(model2_final)

summary(model1_mydata_cleaned)$adj.r.squared
summary(model2_final)$adj.r.squared

summary(model1_mydata_cleaned)
summary(model2_final)

confint(model1_mydata_cleaned)
lm.beta(model1_mydata_cleaned)

confint(model2_final)
lm.beta(model2_final)

coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}

coef_table(model1_mydata_cleaned)

coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}

coef_table(model2_final)

model2_final
