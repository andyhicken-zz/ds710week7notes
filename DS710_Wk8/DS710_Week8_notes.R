# week 8 notes from DS710

# Hypothesis testing: using the data you have 
# from a representative sample
# to make an inference about a larger population.

# getting a sample of 3 random numbers in a range:
sample(1:10, 3)


# 4 parts of a hypothesis test:
#    Null and alternative hypotheses
#    Method
#    P-value
#    Conclusion

# Examples of null hypothesis:
#    Population parameter equals an established value
#    Two populations have the same value of a 
#        a parameter

# Alternative hypothesis: something that diverges
#     from null. H-sub-a or H-sub-1
# Where the null hypothesis is often = <= or >=, 
# the alternative hypothesis is often != < or >

# Method
# How you will test the hypothesis
# We'll use four common methods:

# T-test
# for comparing mean of a quantitative variable

# Test for proportions
# like a t-test but for categorical variables

# Chi-squared goodness of fit test
# Do we have a legit idea of the proportions 
# or probabilities for 
# count data in different categories?

# Test of slope in linear regression
# Are two quantitative variables associated
# with each other?

# Example: do a majority of voters support raising
#   the minimum wage? 

# Note: we are comparing a proportion to a fixed
# value of .5 - so we want the test for proportions.

# Let's say we survey 100 people and find 61 who
# support raising the minimum wage:
prop.test(61, 100, p = .5, alternative = "greater")


# Part 3: P-value:
# Probability of observing an outcome as extreme or
# more than what we observed, if the null hypothesis
# were true. 


# Part 4: Conclusion
# We reject the null hypothesis if p <= .05 or .01 . Easy! 



# _______________________

# T-tests
# Data should be large (>= 30), bell-shaped, or both
# Used for: one sample, two independent samples, or two paired samples
# There's a claim that one-year-old used cars have never been higher than $24,721.86 
# (in Cars.csv - adjusted dollars). Does the data support that claim? 

# What's innocent until proven guilty?
# Null hypothesis: mean price of cars >= $24,721.86. In other words, the opposite of the claim.
# Alternative hypothesis: mean price of cars < $24,721.86. 



library(readr)
cars <- read_csv("Cars 2005.csv")

# look at the data.
length(cars$Price)

library(ggformula)
cars %>% 
  gf_histogram(~ Price)


# Do the t-test
# mu says we're testing the true mean? alternative = "less" says our alternative hypothesis
# is that the mean is less than the true mean? 
t.test(cars$Price, mu = 24721.86, alternative = "less")


# Here's an example of a two-sample t-test: 
# Is there a difference in mean price between cars with two doors and cars with 4 doors? 
# Null hypothesis: there's no difference; they're equal.
# H-sub-1: there's a different. Not equal. 
# t-test:
# First variable is the one whose mean we're testing, and the second variable says how to divide
# the observations into two groups.
t.test(cars$Price ~ cars$Doors, alternative = "two.sided")

# The 95 percent confidence interval is the confidence interval for the difference in the means 
# of the two groups.
# You can change the percentage confidence interval you use with the argument conf.level. 

# This does the same thing, because two.sided is the default value for alternative:
t.test(cars$Price ~ cars$Doors)

# Here's an alternative way to do this, with the groups as different vectors
TwoDoor = cars$Price [ which(cars$Doors == 2)]
FourDoor = cars$Price [ which(cars$Doors ==4)]
t.test(TwoDoor, FourDoor)
# ^ the key when there are different vectors is to use a comma instead of a tilda. Shruggie. 


# Paired t-test
# Used when there are two groups, but each element of group A has "a natural pairing" with 
# each element of group B.

# Example: Did unemployment rates in the US decrease from 2013-2014?
# We happen to have a sample of county unemployment rates right here:
unemp <- read_csv("Unemployment_rates.csv")

# Then here's the paired samples t.test:
t.test(unemp$Rate_2013, unemp$Rate_2014, paired = T, alternative = "greater")
summary(unemp)



# _________________________________
# Tests for Proportion
# Testing the proportion of values of a categorical variable that fall into one category.

# Size of data set - guideline: 
# n * p-sub-0 (the hypothesized proportion) should be >=10 , and the same for n * 1 - p-sub-0

# In 2005, did a majority of 1-year-old used cars have cruise control? 
# p = true proportion that had cruise control
# Compare to p-sub-0 = 0.5

# Checking appropriateness...
length(cars$Cruise)
804 * .5 
# yup, >= 10

# First have to know how many cars had cruise control, and how many total cars there are 
# in the sample:
numCruise = length(which( cars$Cruise == 1))
numCruise
totalCars = length(cars$Cruise)
totalCars

# now do the prop.test
prop.test(numCruise, n = totalCars, p = .5, alternative = "greater")

# Conclusion in words:
# Because the p-value is <2.2???10(-16), it is less than the significance level of .01. 
# So, there is enough evidence to claim that a majority of 1-year-old used cars
# had cruise control in 2005.

# An alternatve approach using a matrix:
numWithout = length(which (cars$Cruise == 0))
numWithout
CruiseMatrix = matrix (c(605, 199), nr = 1)
prop.test(CruiseMatrix, p = .5, alternative = "greater")

# ---------------
# Two-sample test for a difference of proportions

# Example: is there a difference between the proportion of 2-door cars with cruise control
# and 4-door cars with cruise control? 

length( which ( cars$Doors == 2 & cars$Cruise == 1))
length( which ( cars$Doors == 2 ))

length( which ( cars$Doors == 4 & cars$Cruise == 1))
length( which ( cars$Doors == 4 ))

# returns 150, 190, 455, 614
prop.test( c(150, 455), n = c(190, 614), alternative = "two.sided")


# -------------------
# Chi-squared goodness of fit tests
# 
# Good for categorical variables with more than two categories 
#   Is a particular distribution (set of probabilities or proportions) a good fit
#   for the data? 
# Should be used when the expected counts in each category >= 5. If they are multiple
# that are less than that, then combine categories.

# Example: given this distribution of car types:
#    Convertible 2.4%
#    Coupe 7.3 % 
#    Hatchback 11.8 %
#    Sedan 76.1%
#    Wagon 2.6%
# Is that a good fit for the distribution in cars.csv? 

# Null hypothesis: yep, that distribution is a good fit.
# Alternative hypothesis: At least one of the proportions is different from that.

# First find the n in data that falls into each category
typecount = summary (cars$Type)
typecount
summary(cars$Type)
?summary
cars$Type

library(dplyr)

summarise(cars$Type)
cars <- cars %>% 
  mutate(Type = as.factor(Type))
summary(cars$Type)
summary(cars)

prop2014 = c(.022, .073, .118, .761, .026)
prop2014

# mutliply the expected proportions by the sample size, to determine if they are >=5
prop2014 * 804

# good to go; let's run it
chisq.test(typecount, p = prop2014)
# the low p-value means that the null hypothesis is rejected. Not a good fit.

# are cars engines _equally likely_ to have 4, 6, or 8 cylinders?

# here's a cool way to sub-total up the values we have for cylinder:
counts = table(cars$Cylinder)

# when we hypothesis that they're all equally likely, chisq is really easy to use:
chisq.test(counts)


# --------------
# Linear regression
# 
# testing relationship between two variables

# first do a scatterplot. Any reason not to think linear might work?
plot(cars$Mileage, cars$Price)

# and here we go. 
model = lm (cars$Price ~ cars$Mileage)
model

# plot it on the scatterplot like this:
abline ( model, col = "red", lwd = 2)
# (lwd is line width)

# then we can use this fancy model to make predictions,
# such as the price of a one-year old used car with 20,000 miles on it:
predict(model, list(cars$Mileage = 20000))
# the above isn't working for me.
# The second argument is supposed to be a list of predictor variables.

model

# residuals = difference between true y-val and predicted.
# we can use residuals to evaluate appropriateness of linear regression. 
# using ``residual diagnostic plots.'' 
par ( mfrow = c(2,2))
plot (model)

# focus on the top 2. The bottom 2, shrug, who nose.

# see examples of how to interpret on slide 14. Screenshotted in the notes folder.