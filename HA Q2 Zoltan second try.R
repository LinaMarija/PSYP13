#Q2: loading the same data sample
datasample = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")
require(lsr)
require(psych)
library(tidyverse)
library(dplyr)
library(magrittr)
require(olsrr)
require(ggpubr)
require(car)
require(ggpubr)
summary(datasample)
describe(datasample)
View(datasample)
datasample %>% summary()
#as said in the Q2, excluding the participants:
datasample[-c(18, 49), ]
datasample_cleaned <- datasample[-c(18, 49), ]

#Now I will make a model with the variables that the lady from the assignment told to include:
model3 <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + IQ + household_income, data = datasample_cleaned)
#Now re-running data and model diagnostics as asjed in the Q2
#data diagnostics is for looking for missing or incorrenct data??  if so, I already did that then
summary(model3)$adj.r.squared
#now I'm looking at the outliers by using olsrr (or cooks distance) (I should have done that before comparing the models)
ols_plot_cooksd_bar(model3)

################  CHECKING ASSUMPTIONS FOR MODEL 3 ##########################

#normality:
hist( x = residuals( model3),
      xlab = "Value of residual",
      main = "",
      breaks = 20
)

# residuals
resid <- residuals(model3) # pull the residuals 
hist( resid ) # draw a histogram
qqnorm( resid ) # draw a normal QQ plot
shapiro.test( resid )
# Linearity (Navaro p.484):
values <- fitted.values(model3)
plot( x = values,
      y = datasample_cleaned$pain
)

plot(x = model3, which = 1)
require(car)
residualPlots( model = model3)
#there is a significant result (pain_cat = 0.03174*), but in Navaro says that it's still pretty close to normality,
#so it's oki, we can leave it like that

#Now I will check the homogeneity of variance
plot(x = model3, which = 3)
ncvTest( model3)

#checking collinearity

vif( mod =  model3)
summary( model3)

#everything is close to 1 so there's no intercorrelation

#so, everything is pretty normal, let's move on to regression 

step( object = model3, # start at the full model
      direction = "backward") # allow it remove predictors but not add them +)
#look at AIC. combination with the lowest AIC is the best
#every collumn shows the results without a particular variable - which is in the first column.
#"Call" shows the best results after all the deletions (in my case just one - STAI_trait) that can improve AIC model.


## checking backward model assumptions ##
#running regression with the best variables 
backward_model <- lm( formula = pain ~ weight + age + sex + pain_cat + mindfulness + cortisol_serum, data = datasample_cleaned)
print(backward_model)
summary(backward_model)

#normality:
hist( x = residuals(backward_model),
      xlab = "Value of residual",
      main = "",
      breaks = 20
)

# residuals
resid <- residuals(backward_model) # pull the residuals 
hist( resid ) # draw a histogram
qqnorm( resid ) # draw a normal QQ plot
shapiro.test( resid )
# Linearity (Navaro p.484):
values <- fitted.values(backward_model)
plot( x = values,
      y = datasample_cleaned$pain
)

plot(x = backward_model, which = 1)
require(car)
residualPlots( model = backward_model)
#there is a significant result (pain_cat), but in Navaro says that it's still pretty close to normality,
#so it's oki, we can leave it like that

#Now I will check the homogeneity of variance
plot(x = backward_model, which = 3)
ncvTest(backward_model)

#checking collinearity

vif( mod =  backward_model)
summary(backward_model)

# test statistics:
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

coef_table(backward_model)
#""Run a new regression model now only using the predictors that were retained 
#in the end of the backward regression, and save this model in a new R object. 
#We will refer to this model as the “backward model”.""

# comparing initial and backward models:
AIC(model3)
AIC(backward_model)
anova(model3, backward_model)

#""Run the full regression model you arrived at in the end of assignment 1 again, 
#and save this model in another R object. 
#We will refer to this model as the “theory-based model”.""
theory_based_model<- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = datasample_cleaned) # data here is the same as in question 1, just named differently
print(theory_based_model)
#""Compare the backward model and the theory-based model based on AIC 
#(and using the anova() function if appropriate).""
AIC(backward_model)
AIC(theory_based_model)
# anova is not applicable here because the two models are not nested

####### MOVING ON ####### LOADING NEW DATA ########
data_sample_2= read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
summary(data_sample_2)

#predicting
data_sample_2 $ prediction_backwardmodel <- predict(backward_model, data = data_sample_2) ## ERROR replacement has 158 rows, data has 160 ???
data_sample_2 $ prediction_backwardmodel <- predict(backward_model, newdata = data_sample_2) # created new column for predictions for the first model
data_sample_2 $ prediction_theory_based_model <- predict(theory_based_model, newdata = data_sample_2) # created new column for predictions for the second model

#comparing two models of predicted values with actual values using t-test:

t.test(data_sample_2$pain, data_sample_2$prediction_backwardmodel)
t.test(data_sample_2$pain, data_sample_2$prediction_theory_based_model)

#comparing two models of predicted values with actual values using sum of squares:

sum((data_sample_2$pain - data_sample_2$prediction_backwardmodel)^2)
sum((data_sample_2$pain - data_sample_2$prediction_theory_based_model)^2)
# the smaller the better - theory based is better predictor model

backward_model
theory_based_model




