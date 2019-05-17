# we'll use the insect dataset
# to compare means between groups   

str(InsectSprays)

#optional test for noramality
library(lattice)
#visualise the spray distribution
histogram(~count | spray, data = InsectSprays)

with(InsectSprays, tapply(count, spray, shapiro.test))
# p value tells us the chances that the sample comes from
# a normal distribution
# in this example, p is clearly greater than 0.05
# so it is normally distributed (this is the null hypothesis)

# build the ANOVA model
# We want to model the means of variable count as a function
# of variable spray

aov_model <- aov(count ~ spray, data = InsectSprays )
# count ~ spray reads as "count as a function of spray"

aov_model

# Evaluate the differences 
summary(aov_model)

#the ANOVA test is significant
# as p < 0.001, providing evidence that the 5 sprays
# are not the same (this was the null hypothesis)

# model.tables() function examines the resut of individual 
# levels of factors
model.tables(aov_model, type = 'effects')
# spray E on average had 6 times fewer bugs than the average overall fields
# on fields where spray A was sued, farmers found on average
# 5 bugs more than compared to the overall mean
model.tables(aov_model, type = 'means')
# means of each field compared to overall mean

# spray A would not be bought, but what about D verses E and C

# Test the pairwise differences between sprays
# use the TukeyHSD() function (Honest Significant Differences)
comparisons <- TukeyHSD(aov_model)
comparisons$spray['D-C',]
# compares means and gives p value for this difference
# p-value greater than 0.05 means there's no while difference
comparisons$spray['F-E',]
comparisons$spray['B-A',]

# plot the differences - las = 1 sets axis labels
# to horizontal
plot(comparisons, las = 1)
# each line represents the mean difference between
# both groups with the according default confidence level
# whenever the Confidence Interval (CI) doesnt include zero (the vertical line)
# the difference between groups is significant (postive sign. or negative sign.)

# dataset is in multcomp
install.packages('multcomp')
library(multcomp)

str(cholesterol)
attach(cholesterol)

aov_model <- aov(response ~trt)
aov_model
summary(aov_model)
# The ANOVA F test for treatment (trt) is signif.
# (P <0.001), providing evidence that the 5 treatments 
# aren't equally different
detach(cholesterol)        

install.packages('gplots')
library(gplots)
plotmeans(response ~ trt,
          data = cholesterol,
          xlab = "Treatment",
          ylab = 'Response',
          main = "Mean plot\nwith 95% CI")

# lets examine the output from TukeyHSD() for pairwise differences
# between group means
aov_model <- aov(response ~trt, data = cholesterol)
plot_info <- TukeyHSD(aov_model)

par(mar = c(5,8,4,2))
plot(plot_info, las = 2)

#using qq plot to determine normality
install.packages('car')
library(car)
qqplot(lm(response ~ trt, data = cholesterol),
       simulate = TRUE,
       main = "QQ Plot",
       labels = FALSE) ##doesn't work 


# anova assumes equal variance across groups
# The bartlett test is used to verify this assumption
bartlett.test(response ~ trt, data = cholesterol)
# this test indicates that the variances in the 5 groups
#  don't differ significantly - p = 0.97

# anova is also sensitive to outliers
# test for outliers using the outlierTest() function
outlierTest(aov_model)
# p < 0.05 so reject null hypothesis (that there are outliers)