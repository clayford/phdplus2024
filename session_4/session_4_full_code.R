# PhD Plus: Data Literacy in R (2024)
# Session 4: Essential Statistics
# Clay Ford


# Load packages we'll use today. 
library(ggplot2)
library(Hmisc)
library(epitools)
library(presize)
library(ggeffects)

# load("session_4.Rdata")
load(url("https://github.com/clayford/phdplus2024/raw/main/session_4/session_4.Rdata"))

# Insert code section 	Ctrl+Shift+R (Win) 	Cmd+Shift+R (Mac)

# Cross tabulations and Proportions ---------------------------------------

# 5-year randomized study testing whether regular intake of aspirin
# reduces mortality from cardiovascular disease.

# Final Report on the Aspirin Component of the Ongoing Physicians'
# Health Study. NEJM, 321:129-135 (1989).
mi

# proportion who experienced mi in each group
mi_tab <- proportions(mi, margin = "group")
mi_tab

# absolute difference in proportions: about -0.008 (risk difference)
mi_tab["aspirin", "yes"] - mi_tab["placebo","yes"] 

# relative difference in proportions: about 0.55 (risk ratio)
mi_tab["aspirin", "yes"] / mi_tab["placebo","yes"]

# about a 45% reduction
1 - (mi_tab["aspirin", "yes"] / mi_tab["placebo","yes"])

# we can go the other direction; effect of placebo
# Increases risk by about 82% 
mi_tab["placebo", "yes"] / mi_tab["aspirin","yes"] 

# odds ratio
fisher.test(mi)
fisher.test(mi[2:1,])

# cross tabs using the homes data

# cross tabulation of hsdistrict and fp
xtabs(~ hsdistrict + fp, data = homes)
proportions(xtabs(~ hsdistrict + fp, data = homes))

# Insert pipe operator 	Ctrl+Shift+M (Win)	Cmd+Shift+M (Mac)
xtabs(~ hsdistrict + fp, data = homes) |>
  proportions()

# proportions within hsdistrict
xtabs(~ hsdistrict + fp, data = homes) |>
  proportions(margin = "hsdistrict") |> 
  round(2)

# proportions within fp
xtabs(~ hsdistrict + fp, data = homes) |>
  proportions(margin = "fp")

# we can save a table
tab <- xtabs(~ hsdistrict + fp, data = homes) |>
  proportions(margin = "hsdistrict")
tab

# absolute difference in proportions between Albemarle and Monticello
tab[1, 2] - tab[2, 2]

# 3D tables (i.e., stratified by cooling)
xtabs( ~ hsdistrict + fp + cooling, data = homes)

# proportion across rows, within each strata
xtabs( ~ hsdistrict + fp + cooling, data = homes) |>
  proportions(margin = c("hsdistrict", "cooling")) 

# proportion across rows, within each strata
tab <- xtabs( ~ hsdistrict + fp + cooling, data = homes) |>
  proportions(margin = c(1, 3)) 
tab

# extract elements
tab[1,2,1]
tab["Albemarle", "Yes", "No Central Air"]
tab[,,"No Central Air"]

# convert to data frame so we can create plot
DF <- as.data.frame(tab, responseName = "Prop")
ggplot(DF) +
  aes(x = hsdistrict, y = Prop, fill = fp) +
  geom_col(position = "dodge") +
  facet_wrap(~ cooling) +
  scale_y_continuous(labels = scales::label_percent(), 
                     limits = c(0,1))



# Uncertainty -------------------------------------------------------------

# Let's pretend for a moment we don't have all the data for Albemarle
# County homes. Instead, we can only sample 200 homes. And we have to
# use that sample to make our best estimate of the mean totalvalue of
# homes in Albemarle County.

# We can use the `sample()` function to randomly sample elements of a
# vector. If you run this code will get a different sample than I get.
# Then we take the mean of our sample.
true_mean <- mean(homes$totalvalue)
samp <- sample(homes$totalvalue, size = 200)
mean(samp)

# We will all get different means. How uncertain are our estimates of
# the population mean? This is one of the fundamental aims of
# statistical inference: estimating the _standard error_ of an
# estimate.
abs(true_mean - mean(samp))

# A theoretical estimate of the standard error of the sample mean is
# to take the standard deviation of the sample and divide by the
# square root of the sample size.
sd(samp)/sqrt(200)  # standard error of the mean


# Again, if you're following along you got a different standard error.

# Our estimate of the population mean is our sample mean give or take
# the standard error. To be on the safe side, we usually give or take
# about two standard errors (ie, that's how we make a confidence
# interval).

mean(samp) + c(-1,1)*2*(sd(samp)/sqrt(200))
mean(homes$totalvalue)

# R allows us to easily simulate the process of sampling data and
# calculating the mean. Below we use the `replicate()` function to
# replicate our code above 10,000 times.

means <- replicate(n = 10000, expr = {
  samp <- sample(homes$totalvalue, size = 200)
  mean(samp)
})

# The standard deviation of our 10,000 sample means is the estimated
# standard error. In theory, standard error is the _standard deviation
# of the sample means_.
sd(means) # standard error based on 10,000 means


# Due to the Central Limit Theorem, the distribution of the sample
# means is fairly symmetric, so the standard deviation is a good
# measure of spread. This is called the sampling distribution.
hist(means)
# compare to population distribution
hist(homes$totalvalue, breaks = 100)

# We can add and subtract 2 standard errors from mean to form an
# approximate 95% confidence interval
SE_m <- sd(means)
mean(samp) + c(-1, 1)*2*SE_m


# The above was for educational purposes. In practice, we don't get to
# take 10,000 random samples. Instead we would take our _single
# sample_ and estimate a 95% confidence interval using a function such
# as `smean.cl.normal()` from the {Hmisc} package.
Hmisc::smean.cl.normal(samp)

# R's built-in `t.test()` function can also calculate 95% confidence
# intervals.
tout <- t.test(samp)
tout$conf.int
# attr() means attribute

# It also calculates standard error.
tout$stderr


# Again, if you're running this code, you're getting a different
# answer because we all took a different sample.

# As our samples get bigger, our standard error decreases. We have
# more information, therefore there is less uncertainty in our
# estimate.

# NOTE: To reduce the standard error by 1/2, we need 4 times the
# sample size. So doubling our sample size of 200 to 400 _does not_
# reduce our uncertainty by half. We would need a sample size of 200*4
# = 800.


# illustration of confidence intervals
# Called "confidence" because we're confident in the method.
# Not a probability interval!

true_mean <- mean(homes$totalvalue)

# check one sample; does 95% CI capture true value?
ci <- t.test(sample(homes$totalvalue, 200))$conf.int
true_mean > ci[1] & true_mean < ci[2]

# repeat 1000 times
ci_intervals <- replicate(n = 1000, expr = {
  ci <- t.test(sample(homes$totalvalue, 0200))$conf.int
  true_mean > ci[1] & true_mean < ci[2]
})

# proportion of times the 95% CI captures true mean
sum(ci_intervals)
mean(ci_intervals)

# the stated coverage of 95% is actually lower since the raw data is
# so skewed.

# The rats data contains a sample of captured kangaroo rats (Dipodomys
# merriami) that were used to estimate population size and density.

# Mabry, Karen; Hurtado, Gizelle; Mayer, Ghislaine (2022). Data from:
# Does urbanization ameliorate the effect of endoparasite infection in
# kangaroo rats? [Dataset]. Dryad.
# https://doi.org/10.5061/dryad.8pk0p2nns


# The variable "Mass.g" measures the mass of the rat in grams at the
# time of sampling. Calculate the mean and a 95% confidence interval
# for "mass"
summary(rats$Mass.g)
sum(!is.na(rats$Mass.g))
hist(rats$Mass.g)
t.test(rats$Mass.g)
Hmisc::smean.cl.normal(rats$Mass.g)

# The variable "Pterygodermatites.dipodomis" indicates whether a
# sample was positive for the presence of Pterygodermatites dipodomis
# (1 = positive, 0 = negative). Calculate the proportion of rats with
# this parasite and a 95% confidence interval using the `prop.test()`
# function.

table(rats$Pterygodermatites.dipodomis)
table(rats$Pterygodermatites.dipodomis) |> proportions()
prop.test(x = 70, n = 70 + 131)$conf.int

# a study measured serotonin in captured locusts for 0, 1, and 2 hours
# to see how it affects social behavior.

# Anstey et al. (2009) Serotonin mediates behavioral gregarization
# underlying swarm formation in desert locusts. Science 323: 627-630.

summary(locusts)
ggplot(locusts) +
  aes(x = treatmentTime, y = serotoninLevel) +
  geom_jitter(height = 0, width = 0.1)

aggregate(serotoninLevel ~ treatmentTime, data = locusts, 
          Hmisc::smean.cl.normal)

tapply(locusts$serotoninLevel, locusts$treatmentTime, Hmisc::smean.cl.normal)

# plot standard errors
ggplot(locusts) +
  aes(x = treatmentTime, y = serotoninLevel) +
  geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun.data = mean_cl_normal, colour = "red") 

# model-based approach to calculating standard errors
m <- lm(serotoninLevel ~ treatmentTime, data = locusts)
library(ggeffects)
ggpredict(m, "treatmentTime")
ggpredict(m, "treatmentTime") |> plot(add.data = TRUE, jitter = TRUE)


## bootstrap

# treat your random sample like a surrogate for the population and
# resample from it with replacement, and then calculate your
# statistic(s) on the new sample. Repeat many times to generate many
# replications and then use the replications to estimate standard
# error and/or a confidence interval.

# one bootstrap replicate 
bstrap <- sample(rats$Mass.g, replace = TRUE)
mean(bstrap, na.rm = TRUE)

# bootstrap with 10000 replications
bs_out <- replicate(n = 10000, expr = {
  bstrap <- sample(rats$Mass.g, replace = TRUE)
  mean(bstrap, na.rm = TRUE)
  })

# bootstrap standard error
sd(bs_out)
# distribution of bootstrap replicates
hist(bs_out)
# percentile CI
quantile(bs_out, probs = c(0.025, 0.975))
# compare to theoretical confidence interval
Hmisc::smean.cl.normal(rats$Mass.g)

# yours will look different from mine!

# Hmisc has a function to quickly do this
Hmisc::smean.cl.boot(rats$Mass.g)

# use set.seed() to make reproducible
set.seed(99)
Hmisc::smean.cl.boot(rats$Mass.g)

# using the boot package
library(boot)
# need to write a function with two arguments: one for the original
# data (x), and one to select the bootstrap sample (i).
md <- function(x, i)median(x[i])
bs_out2 <- boot(data = samp, statistic = md, R = 999)
boot.ci(bs_out2, type = c("perc", "bca"))



# Hypothesis testing ------------------------------------------------------

# 1. assume some null hypothesis is true

# 2. calculate some statistic on data (eg, compare means)

# 3. calculate probability of getting a statistic that big, or bigger
# (or that small, smaller) if null hypothesis is true

# 4. if the probability (the p-value) is small, then we have evidence
# against the null hypothesis

# That's it. It's just a probability. p < 0.05 is not a threshold for
# certainty.

# create a null distribution
# Flip a coin 20 times, 10000 times
flips <- rbinom(n = 10000, size = 20, prob = 0.5)
barplot(table(flips))

table(flips) |> proportions() 

# say I flip a coin and get 16 heads. What is the probability of
# getting 16 or more heads if the coin is fair?

# A p-value is the probability of 16 or more heads assuming coin is
# fair
mean(flips >= 16)
binom.test(x = 16, n = 20, p = 0.5, alternative = "greater")

### Comparing two means

# Do long spikes on a horned lizard (mm) help protect them from being
# eaten by shrikes, a type of bird that skewers its victims on thorns
# to eat later? The lizards data set contains measurements of the
# length of lizard horns for two groups: those that were alive and
# those that had been killed and skewered.

# Young, et al. (2004). How the horned lizard got its horns. Science
# 304: 65.

summary(lizards)
boxplot(horn_length ~ survival, data = lizards)
aggregate(horn_length ~ survival, data = lizards, mean)
t.test(horn_length ~ survival, data = lizards)

### Comparing more than two means

aggregate(serotoninLevel ~ treatmentTime, data = locusts, 
          function(x)c("mean" = mean(x), "sd" = sd(x)))
boxplot(serotoninLevel ~ treatmentTime, data = locusts)
stripchart(serotoninLevel ~ treatmentTime, data = locusts, method = "jitter")

# ANOVA: analysis of variance
# null: all means are equal
m <- aov(serotoninLevel ~ treatmentTime, data = locusts)
summary(m)

# Calculating sum of squares
# total sum of squares
gmean <- mean(locusts$serotoninLevel)
TSS <- sum((locusts$serotoninLevel - gmean)^2)

# Between sum of squares
lst <- split(locusts$serotoninLevel, locusts$treatmentTime)
SS <- lapply(lst, function(x)sum((mean(x) - gmean)^2) * length(x))
TreatSS <- sum(unlist(SS))

# residual sum of squares
TSS - TreatSS

### Comparing two categorical variables

# Is habitat related to whether or not a rat has Pterygodermatites
# dipodomis?
tab <- table(rats$Habitat, rats$Pterygodermatites.dipodomis)
proportions(tab, margin = 1)

# null: habitat and Pterygodermatites dipodomis are independent
chisq.test(tab, correct = F)
chi_out <- chisq.test(tab, correct = F)
chi_out$residuals

# compare proportions
# null: proportions are no different
addmargins(tab)
prop.test(x = c(45, 25), n = c(109, 92), correct = F)


# Is daily aspirin associated with lower incidence of heart attacks?
chisq.test(mi)
addmargins(mi)

# risk difference
prop.test(x = c(104, 189), n = c(11037, 11034), correct = FALSE)

# risk ratio
epitools::riskratio(mi)
epitools::riskratio(mi[2:1,])

# Power and sample size ---------------------------------------------------

curve(dnorm(x, mean = 30, sd = 5), from = 30 - 15, to = 30 + 15)

# simulate data
time1 <- rnorm(15, mean = 30, sd = 5)
time2 <- rnorm(15, mean = 35, sd = 5)
t.test(time1, time2)$p.value
t.test(time1, time2)$p.value < 0.05

tests <- replicate(n = 1000, expr = {
  time1 <- rnorm(15, mean = 30, sd = 5)
  time2 <- rnorm(15, mean = 35, sd = 5)
  t.test(time1, time2)$p.value < 0.05
})

# estimate of power: 
# probability of making correct decision (rejecting null hypothesis)
mean(tests) 

# Notice the assumptions: normality and constant standard deviation

# using built-in power.t.test function
# estimate power for = 15
power.t.test(n = 15, delta = 5, sd = 5, sig.level = 0.05)
# estimate sample size for power = 0.9
power.t.test(power = 0.9, delta = 5, sd = 5, sig.level = 0.05)

# Assumes no drop out or missing data.

# Estimate sample size for desired precision (i.e., width of 95% CI)
presize::prec_meandiff(delta = 5, sd1 = 5, conf.width = 4, var = "equal")

# comparing proportions

# design a survey to see if the proportion of students who floss their teeth
# differs between male and female students.
power.prop.test(p1 = 0.15, p2 = 0.20, sig.level = 0.01, power = 0.9)


# Parametric vs Nonparametric ---------------------------------------------

# Hypothesis testing makes assumptions about the data. For example,
# the two-sample t-test assumes each group is normally distributed
# (approximately) and has the same standard deviation. Same for ANOVA.

# If these assumptions are not satisfied, we may want to entertain a
# non-parametric method.

# Copy Lines Up/Down 	Shift+Alt+Up/Down(Win) 	Cmd+Option+Up/Down(Mac)
# A normal distribution has two parameters: mean and standard deviation
curve(dnorm(x, mean = 0, sd = 1), from = -3, to = 3)
curve(dnorm(x, mean = 1, sd = 1), from = -3, to = 3, add = TRUE, col="red")
curve(dnorm(x, mean = 0, sd = 2), from = -3, to = 3, add = TRUE, col="blue")

# Non-parametric methods do not assume that the data has a specific
# distribution. Sometimes they're called "distribution free" methods.

# The non-parametric version of the t-test is called the Wilcoxon or
# Mann-Whitney test. It assumes both groups have the same distribution.
# Rejecting the test implies the distributions are different. It does not
# necessarily mean the two distributions have different means/medians.

# going back to the lizard data
ggplot(lizards) +
  aes(x = horn_length) +
  geom_density() +
  facet_wrap(~survival)

wilcox.test(horn_length ~ survival, data = lizards, conf.int = TRUE)

# The CI estimates the median of the difference between a sample from
# x and a sample from y. Calculated manually: calculate the difference
# between all pairs and then find the median of those differences.
median(outer(lizards$horn_length[lizards$survival=="killed"],
             lizards$horn_length[lizards$survival=="living"],"-"))

# lizards data
# recall earlier result
summary(m)

# check assumption of constant variance
boxplot(serotoninLevel ~ treatmentTime, data = locusts)
# non-parametric ANOVA
# null: location parameters of the distribution of x are the same in each group
kruskal.test(serotoninLevel ~ treatmentTime, data = locusts)

# correlation and regression ----------------------------------------------

# The lions data set contains the age and proportion of dark
# pigmentation on their noses for 32 lions. Can we use the proportion
# of black to estimate a lion's age?

# Whitman, et al (2004). Sustainable trophy hunting of African lions.
# Nature 428: 175-178.

ggplot(lions) +
  aes(x = proportion, y = age) +
  geom_point() 


# correlation: strength of linear association
cor(lions$proportion, lions$age)
cor.test(lions$proportion, lions$age)

# dat1 and dat2 have two variables, x and y
# not apparently correlated...
cor.test(dat1$x, dat1$y)
cor.test(dat2$x, dat2$y)

# ...but clearly they have association
plot(dat1$x, dat1$y)
plot(dat2$x, dat2$y)

# fit a regression model with lm()
m <- lm(age ~ proportion, data = lions)
summary(m)

# result is a math formula for predicting age of lion
# age = 0.8790 + 10.6471 * proportion

ggplot(lions) +
  aes(x = proportion, y = age) +
  geom_point() +
  geom_smooth(method = "lm")


ggpredict(m) |> 
  plot(add.data = TRUE)

# slope: age increases by about 10.65 years per unit increase in
# proportion or, age increases by about 1.065 years for each 0.1
# increase in proportion

# confidence intervals for coefficients
confint(m)

# age increases by about .756 to 1.37 years for each 0.1 increase in
# proportion

# regression assumptions: residuals are normally distributed and have
# constant variance (i.e., they're not systematically too high or too
# low)
lions$age
fitted(m)

# residuals: obs - fitted
lions$age - fitted(m)
residuals(m)

# plot residuals versus fitted values
plot(m, which = 1)

lions[30,]

# prediction
predict(m, newdata = data.frame(proportion = 0.2))
# 95% CI for predicted mean
predict(m, newdata = data.frame(proportion = 0.2), interval = "confidence")
# 95% CI for predicted value
predict(m, newdata = data.frame(proportion = 0.2), interval = "prediction")

ggpredict(m, terms = "proportion[0.1:0.8 by=0.1]")
ggpredict(m, terms = "proportion[0.1:0.8 by=0.1]") |> plot()

# centering data to make intercept interpretable;
# when data is centered, 0 is the mean of the data
lions$proportionC <- lions$proportion - mean(lions$proportion)
m2 <- lm(age ~ proportionC, data = lions)
summary(m2)

# Now the intercept is the expected age of a lion with the mean
# proportion, which is about 0.3
mean(lions$proportion)

# Fitting non-linear effects

# Regression splines allows us to fit non-linear effects. Use the
# `ns()` function from the {splines} package that comes with R. Set
# `df` to determine wiggle of line, or number of times the line
# changes direction.

m2 <- lm(age ~ ns(proportion, df = 3), data = lions)
summary(m2)

# Summary is hopeless to interpret

# Fitted line
ggpredict(m2) |> 
  plot(add.data = TRUE)

# Compare models using a partial F test
# anova(small model, big model)
# small model needs to be "nested" in big model
anova(m, m2)

# or using AIC (lower is better)
AIC(m, m2)

# straight line model appears to be just as good as more complex model

# quantiles/percentiles ---------------------------------------------------

# Given the quantile/percentile, what's the value? What is 75th
# percentile of magnitude of seismic events?
quantile(eq$mag, probs = 0.75, na.rm = T)

# The default returns min/max and quartiles.
quantile(eq$mag)

# Here's a trick to get deciles. Set `probs = 1:9/10`
quantile(eq$mag, probs = 1:9/10)

### Empirical cumulative distribution (ECD)

# The ECD is the inverse of quantile.

# Given the value, what's the quantile/percentile? What percentile is
# a 2.5 seismic event in? Use the `ecdf()` function to create a
# function from the data to return quantiles.
Fn <- ecdf(eq$mag)
Fn(2.5) 

# What quantile is an event with magnitude 4.0?
Fn(4.0)   

# It's worth noting this is the same as finding the proportion of
# events with magnitude <= 4.0.

mean(eq$mag <= 4.0)

# But an ECDF function allows us to calculate lots of quantiles at once.

# From 1 to 4 in steps of 0.5
Fn(seq(1,4,0.5))

# can also plot the ECDF
plot(Fn)

# or using ggplot2
ggplot(eq) +
  aes(x = mag) +
  stat_ecdf()


# Log transformations -----------------------------------------------------


# The idea: Take positive, skewed data and hopefully make it more
# symmetric.

# Values clumped together get spread apart, values far away are moved
# closer to the rest of the data.

hist(homes$totalvalue)


# log transformed histogram
hist(log(homes$totalvalue))

# To undo a log transformation we take the "anti-log", or exponentiate
# using the `exp()` function.

x <- 1200000
logx <- log(x)
logx
exp(logx) # the anti-log


# Mean of untransformed data
mean(homes$totalvalue)

# Mean of log transformed data
mean(log(homes$totalvalue))

# When we take the anti-log of the mean of the log-transformed values,
# we get the "geometric mean". Notice this mean is closer to the
# median and is less affected by the outliers.
exp(mean(log(homes$totalvalue)))

### two sample t-test with log transformation

# Does totalvalue differ between homes with and without fireplaces?
# Could investigate with a t-test. We can use R's formula syntax
# `totalvalue ~ fp` that basically says in this case "compare
# totalvalue grouped by fp"

t.test(totalvalue ~ fp, data = homes)

# A small p-value provides evidence against the null of no difference,
# but does not tell us about the difference. Instead look at 95%
# confidence interval on the difference.

# The formal null hypothesis of a two-sample t-test is that both
# samples came from the same normal distribution.

# We should look at the distribution of totalvalue by fp.
ggplot(homes) +
  aes(x = totalvalue, fill = fp) +
  geom_density(alpha = 1/3)


# Ideally we would like to see the distributions of each group looking
# roughly symmetric. One option is a log transformation

ggplot(homes) +
  aes(x = log(totalvalue), fill = fp) +
  geom_density(alpha = 1/3)

# Let's try a t-test on the log-transformed data.
tout <- t.test(log(totalvalue) ~ fp, data = homes)
tout

# We get the same basic result as far as the hypothesis test goes, but
# the estimated means and confidence interval are on the log scale.

# We can use the `exp()` function to undo the log transformation (ie,
# take the anti-log) and see the expected mean values, aka the
# geometric means.

exp(tout$estimate)


# The confidence limits for the difference between means cannot be
# transformed back to the original scale

tout$conf.int

# Instead, if we exponentiate, we get a 95% CI for the _ratio of the
# geometric means_.
exp(tout$conf.int)

# Subtract from 1 to make it a little more interpretable.

1 - exp(tout$conf.int) # about 44% - 43% lower

# The geometric mean of homes without fireplaces is about 44% - 43%
# lower than homes with fireplaces. (ie, $320,321 is about 43-44%
# lower than $566,437)

# We can calculate the ratio of the geometric means. It's about 0.56.
# The 95% CI is about [0.56, 0.57]

# homes without fireplaces are less expensive than homes without by a
# factor of about 0.56.
exp(12.67708)/exp(13.24712 )

# subtract from 1 to express as percent decrease
1 - exp(12.67708)/exp(13.24712 )

# This demonstrates how a log-transformation results in an analysis of
# relative differences instead of absolute differences.

# we could also run this as a linear model
m <-  lm(log(totalvalue) ~ fp, data = homes)

# the coefficient is relative to fp = No
coef(m)[2]

# homes with fireplaces have totalvalue that are 74% to 79% higher
# than the totalvalue of homes without fireplaces.
round(exp(confint(m)), 3)



# Permutation test --------------------------------------------------------

# In a permutation test, we hold the rows and group labels fixed while
# we shuffle the observed values. If the null hypothesis is true -
# that both groups were drawn from the sample population - then all
# permutations of the data are equally likely. The p-value is the
# proportion of them for which the chosen statistic, say difference in
# means, is at least as extreme as the observed statistic.

# permutation test example using the lizards data

# create group labels
killed <- lizards$survival == "killed"  # group 1
living <- !killed                       # group 2

# calculate observed statistic: difference in means
obs_diff <- mean(lizards$horn_length[living]) - 
  mean(lizards$horn_length[killed])

# create a vector to store statistic calculated from permutations
meandiffs <- numeric(9999)

# for loop that samples all data without replacement (ie, shuffles the
# data), and then calculates difference in means. Each result is
# stored in the meandiffs vector.
for(i in 1:length(meandiffs)){
  s_hl <- sample(lizards$horn_length)
  meandiffs[i] <- mean(s_hl[living]) - mean(s_hl[killed])
}

# distribution of permuted difference in means
hist(meandiffs)

# proportion of permuted mean diffs that are at least as big as the
# observed difference in means. This is the permutation test p-value
mean(abs(meandiffs) > obs_diff)

# This can also be carried out using the coin package
# install.packages("coin")
library(coin)
independence_test(horn_length ~ survival, 
                  data = lizards, distribution = exact())

save(dat1, dat2, homes, lions, lizards, locusts, rats, mi, eq,
     file = "session_4.Rdata")
