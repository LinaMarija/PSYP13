require(psych)
require(car)
require(lmtest)
require(sandwich)
require(boot)
require(tidyverse)
require(cAIC4)
require(r2glmm)
require(lme4)
require(lmerTest)
require(influence.ME)
require(lattice)
source("GraphPlot.R")

sample_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")
sample_4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")

View(sample_3)
summary(sample_3)
# change mindfulness - delete that case which is wrong (because max here is 6.05)
sample_3[-c(195), ]
sample_3_without195<- sample_3[-c(195), ]
summary(sample_3_without195)
describe(sample_3_without195)

#change Female to female
sample_3_cleaned <- sample_3_without195 %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")))
summary(sample_3_cleaned)

describe(sample_3_cleaned) 

# checking data 4 
summary(sample_4)
describe(sample_4)

# asign hospital as a group factor
sample_3_cleaned = sample_3_cleaned %>% mutate(hospital = factor(hospital))

# Building random intercept model:

mod_rnd_int_3 = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = sample_3_cleaned)	

# coefficients and the confidence intervals of the coefficients:

coef_table(mod_rnd_int_3)
summary(mod_rnd_int_3)
confint(mod_rnd_int_3)
require(lmfor)
merMod(mod_rnd_int_3) # doesn't work

require(sjstats)
std_beta(mod_rnd_int_3)


sample_3_cleaned = sample_3_cleaned %>% 	
  mutate(resid = residuals(mod_rnd_int_3)) # We also create a copy of our data object and save the residuals in a variable we call resid.

## ASSUMPTIONS OF THE MODEL:
library(psych) # for pairs.panels	
library(tidyverse) # for tidy code and ggplot		
library(influence.ME) # for influence (this will also load the lme4 package)	
library(lattice) # for qqmath	
library(lme4) # for mixed models	
library(lmerTest)
library(insight)

# Report the variance components for the fixed effects, the random intercept, 
#and the residuals (from the model on data file 3)” 
get_variance_fixed(mod_rnd_int_3)
get_variance_intercept(mod_rnd_int_3)
get_variance_residual(mod_rnd_int_3)
### 1 # Outliers

influence_observation = influence(mod_rnd_int_3, obs = T)$alt.fixed # this can take a minute or so	
influence_group = influence(mod_rnd_int_3, group = "hospital")$alt.fixed

data_plot_inflience = as_tibble(influence_group) %>% 	
  gather(colnames(influence_group), value = coefficient, key = predictor)	

data_plot_inflience %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free") # so what does it mean?

### 2 # Normality

openGraph()
qqmath(mod_rnd_int_3, id=0.05)
qqmath(ranef(mod_rnd_int_3), col="lightcoral")

### 3 # Linearity
openGraph()
plot(mod_rnd_int_3, arg = "pearson", col = "mediumvioletred")

# residuals for sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness

plot(cars)
par(mfrow=c(2,2))
plot(cars)
dev.off()

sample_3_cleaned %>% 	
  ggplot() +	
  aes(x = pain, y = resid, col = "aquamarine3") +	
  geom_point()

sample_3_cleaned %>% 	
  ggplot() +	
  aes(x = sex, y = resid, col = "aquamarine3") +	
  geom_point()	

sample_3_cleaned %>% 	
  ggplot() +	
  aes(x = age, y = resid, col = "aquamarine3") +	
  geom_point()	

sample_3_cleaned %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = resid, col = "aquamarine3") +	
  geom_point()	

sample_3_cleaned %>% 	
  ggplot() +	
  aes(x = pain_cat, y = resid, col = "aquamarine3") +	
  geom_point()

sample_3_cleaned %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = resid, col = "aquamarine3") +	
  geom_point()

sample_3_cleaned %>% 	
  ggplot() +	
  aes(x = mindfulness, y = resid, col = "aquamarine3") +	
  geom_point()

### 4 # Homoscedasticity	

plot(mod_rnd_int_3, arg = "pearson")	

homosced_mod = lm(resid^2 ~ hospital, data = sample_3_cleaned)	
summary(homosced_mod)
# Check the complete model F-test p-value. If it is < 0.05, 
# heteroscedasticity on the cluster level might be problematic. In my case p = 0.4242, so there should be no problems
# and since p is sooo far from significance, it's not necessary to do the next step in exercise.
# So we can continue with multicolinearity.


### 5 # Multicollinearity	

openGraph()
pairs.panels(sample_3_cleaned[,c("pain", "sex", "age", "STAI_trait", "pain_cat","cortisol_serum", "mindfulness")], col = "red", lm = T)
# if non of them is above 0.8, it's good.

# Once the model is built, note the model coefficients and the confidence 
# intervals of the coefficients for all fixed effect predictors, 
# and compare them to the ones obtained in assignment 1.

# confidence interval:
confint(mod_rnd_int_3)
coef(mod_rnd_int_3)

# marginal R squared
library(nlme)
library(lme4)
r2beta(mod_rnd_int_3, method = "nsj", data = sample_3_cleaned)	
# another way - marginal and conditional R squared:
require(MuMIn)
r.squaredGLMM(mod_rnd_int_3) 

###

## equation regression:
#y = 3,41 + 0,30*sexmale + (-0,06)*age + (-0,01)*STAI_trait + 0,08*pain_cat + 0,47 * cortisol_serum + (-0,18)*mindfulness


View(sample_4)
summary(sample_4) # there are some negative income values, but since we're not gonna use income, we can just leave it like that
#negative household_income is weird but we will keep it since the models to not include the variable of household income and the other variables of those participants seemed fine


#predict pain with equation of data file 3 on data file 4
pred_sample_4 <- predict(mod_rnd_int_3, newdata = sample_4, allow.new.levels =TRUE)
pred_sample_4


#RSS: you have real values and predicted values, and you look how big is the difference
RSS = sum((sample_4$pain - predict(mod_rnd_int_3, sample_4, allow.new.levels =TRUE))^2)
#pred_data_sample_4)^2)
RSS
#There you look at the means.
mod_mean <- lm(pain ~ 1, data = sample_3_cleaned) # if we want to compare, we put data 3
mod_mean


TSS = sum((sample_3$pain - predict(mod_mean, sample_3_cleaned, allow.new.levels = TRUE))^2)
TSS


R2 = 1 - (RSS/TSS)
R2 # R^2 the same model, but the variance which is explained on the data set 4
#we compare it with marginal and conditional R2 of the model.


#new linear model: based on the variable that has the highest value from the old model, we make a new model:
mod_rnd_int_3 # we see that cortisol_serum has the highest value

#linear mixed model
mod_rnd_int_4 = lmer(pain ~ cortisol_serum +( cortisol_serum | hospital), data = sample_3_cleaned)
mod_rnd_int_4

#plotting
pred_slope = predict(mod_rnd_int_4)

#how cortisol predicts pain in each hospital
openGraph()
sample_3_cleaned %>% ggplot() + aes(y = pain, x = cortisol_serum,
                           group = hospital) + geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(y = pred_slope, x = cortisol_serum)) +
  facet_wrap(~hospital, ncol = 2)










######
# exploring clustering in the data (this is last step?):
sample_3_cleaned %>% ggplot() + aes(y = pain, x = age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness, weight) + 
  geom_point(aes(color = hospital), size = 10) + geom_smooth(method = "lm", se = F)

int_plot = sample_3_cleaned %>% ggplot() + aes(y = pain,
x = age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness, weight, color = hospital) + 
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)
int_plot
openGraph()


# this is just something:

########################################################################################################################################
########################################################################################################################################
# ## Check the dataset	

# In the following section we visualize the change of average pain over time (using geom_point), 
# with the confidence interval of the mean estimate included (using geom_errorbar). 
# (in the code below we first calculate the means and confidence intervals before plugging them in to ggplot) 

# change sex, ID and hospital into numeric variables?
home_assignment_data_3$sex <- as.numeric(as.factor(home_assignment_data_3$sex))
home_assignment_data_3$hospital <- as.numeric(as.factor(home_assignment_data_3$hospital))

# designate which are the repeated varibales	?? which ones ??
repeated_variables = c ("sex", "age",	"STAI_trait", "pain_cat",	"cortisol_serum", "mindfulness", "weight")

# fit a random intercept model including the random intercept of hospital-ID, 
# and the fixed effect predictors you used in assignment 1.
mod_rep_int = lmer(pain ~ sex +age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + (1|hospital), data = home_assignment_data_3)

########################  FROM EXERCISE 17  ###################

mod_rnd_int = lmer(pain ~ sex +age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + (1|hospital), data = home_assignment_data_3)	










