library(pwr)

# For a one-way ANOVA comparing 4 groups, calculate the
# sample size needed in each group to obtain a power of
# 0.80, when the effect size is moderate (0.35) and a
# significance level of 0.05 is employed.

pwr.anova.test(k=4,f=.35,sig.level=.05,power=.8)


groupmeans <- c(-.05, 0, 0, 0.5)

pwr.anova.test(k = 4, n = 0, f = 0.35, sig.level = 0.5, power = 0.8)



p <- power.anova.test(groups = length(groupmeans), 
                      between.var = var(groupmeans), within.var = 6400, 
                      power=0.823,sig.level=0.05,n=NULL)
p