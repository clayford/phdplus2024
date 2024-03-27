# PhD Plus: Data Literacy in R
# session 5
# Modeling and Machine Learning

# Load packages we'll use today
library(car)
library(ggeffects)
library(emmeans)
library(rpart)
library(randomForest)

# load data
# homes = homes that feed into Woodbrook elementary school
# ah = sample of homes that feed into Agnor-Hurt elementary school
load(url("https://github.com/clayford/phdplus2024/raw/main/session_5/session_5.Rdata"))

# Explore data ------------------------------------------------------------

Hmisc::describe(homes$totalvalue)
# Info: how continuous the variable is
# Gmd: sophisticated version of standard deviation (measure of spread)
hist(homes$totalvalue, breaks = 50)
summary(homes$totalvalue)

Hmisc::describe(homes$finsqft)
hist(homes$finsqft, breaks = 50)
hist(log(homes$finsqft), breaks = 50)

hist(homes$lotsize, breaks = 50)

hist(homes$age)

table(homes$condition)
table(homes$cooling)
table(homes$fp)
table(homes$bedroom)
table(homes$fullbath)
table(homes$remodeled)


# Models ------------------------------------------------------------------


# MODEL 1: three predictors
m1 <- lm(totalvalue ~ finsqft + lotsize + bedroom, data = homes)
summary(m1)
coef(summary(m1)) 

### Residuals

# _Residuals_ are the difference between what we observed and what the model
# predicted. We would like these to be symmetric and small.

summary(residuals(m1))

### Coefficients

# - The model coefficients are in the _Estimate_ column. 
# - The _Std. Error_ column shows the standard error of the coefficients. This summarizes the uncertainty of the estimate.
# - The _t value_ is the ratio of the Estimate and Std Error. A bigger ratio means more certainty.
# - The _Pr(>|t|)_ column shows the probability of getting a t value as large or larger (in absolute value) if the t value is really 0.

### Residual standard error

# The residuals for a linear model are assumed to be random draws from a Normal
# distribution. This is the estimated standard deviation of the distribution

### Degrees of Freedom

# Number of observations (1770) minus the number of estimated coefficients (4)

### R-squared

# Proportion of variance explained. Of all the variability in totalvalue, this
# model explains about 74% of it.

# _Multiple R-squared_ is simply the _squared correlation_ between what we
# observed and what the model predicts.

plot(homes$totalvalue, fitted(m1))
abline(0,1)
cor(homes$totalvalue, fitted(m1))^2 # multiple R-squared

### F-statistic

# This tests the null hypothesis that all coefficients except the Intercept are
# 0. Small p-values provide evidence against this hypothesis. A large p-value
# would suggest the model is no better at predicting values than simply using
# the overall mean as our predicted value.

# How coefficients are estimated (in theory; not really when using lm)
# (X'X)^-1(X'Y)
X <- model.matrix(m1)
Y <- homes$totalvalue
B <- solve(t(X) %*% X) %*% t(X) %*% Y
round(B, 4)

# how standard errors are estimated
# MSE * (X'X)^-1
MSE <- sum(residuals(m1)^2)/(nrow(homes)- 4)
# or equivalently
MSE <- sigma(m1)^2
# take the square root of the diagonal elements
sqrt(diag(MSE * solve(t(X) %*% X)))

# predictors for row 1
model.matrix(m1)[1,]
coef(m1)

# prediction for row 1
coef(m1) %*% model.matrix(m1)[1,]

predict(m1, newdata = data.frame(finsqft = 3125, lotsize = 3.69, 
                                 bedroom = 6))
fit_values <- fitted(m1)
fit_values[1]

# compare to observed value
homes$totalvalue[1]

# residual
homes$totalvalue[1] - fit_values[1]
residuals(m1)[1]

# Any home with 3125 finsqft, 6 bedrooms and 3.69 acres will get the same
# predicted value.

# Incorporate uncertainty into this prediction using residual standard error
summary(m1)
sigma(m1)
fit_values[1] + rnorm(1, mean = 0, sd = sigma(m1))

# Use model to simulate data using all observations
# does model simulate data similar to the observed data?
sim <- simulate(m1, nsim = 50)
plot(density(homes$totalvalue))
for(i in 1:50)lines(density(sim[[i]]), col = "grey80")

# model diagnostics
plot(m1, id.n = 5)

homes[1529,]
homes[76,]  # big lotsize

# sensitivity analysis
m1a <- update(m1, subset = -1529)
car::compareCoefs(m1, m1a)


# MODEL 2: log-transformed response 

# log-transformation of response can be helpful when the response is severely
# skewed and a range that spans orders of magnitude;
# can also log transform predictors
m2 <- lm(log(totalvalue) ~ log(finsqft) + lotsize + bedroom, data = homes)
summary(m2)

# check diagnostics
plot(m2)

# simulate data
sim <- simulate(m2, nsim = 50)
plot(density(log(homes$totalvalue)))
for(i in 1:50)lines(density(sim[[i]]), col = "grey80")

# Interpreting coefficients
round(coef(m2), 4)
# each 1% increase in finsqft increases total value by about 0.6%

# For x percent increase, calculate 1.x to the power of the coefficient,
# subtract 1, and multiply by 100. Example: For every 10% increase in the
# independent variable, our dependent variable increases by a factor of 1.062, or about 6.2 percent.
1.10 ^ 0.6317

# or do this way
(1.10 ^ 0.6317 - 1) * 100

# each additional acre increases totalvalue by about 5%

# each additional bedroom increases totalvalue by about 8%


# CI of coefficients
round(confint(m2), 4)

# With finsqft and fullbath in the model, it doesn't appear the cooling status
# adds much value to explaining totalvalue.



# MODEL 3: add fullbath

m3 <- lm(log(totalvalue) ~ log(finsqft) + lotsize + bedroom + fullbath, 
         data = homes)
summary(m3)
round(confint(m3), 4)

# each additional fullbath increases totalvalue by about 7%

plot(log(homes$totalvalue), fitted(m3))
abline(0,1)
cor(log(homes$totalvalue), fitted(m3))^2

# MODEL 4: add interaction between finsqft and fullbath

# Perhaps the effect of finsqft depends on number of fullbaths?

m4 <- lm(log(totalvalue) ~ log(finsqft) + lotsize + bedroom + fullbath +
           log(finsqft):fullbath, 
         data = homes)
summary(m4)
round(confint(m4), 4)

# compare models via hypothesis test:
# NULL = both models equally good
anova(m3, m4) # reject null, m4 seems "better" than m3

# compare models by information criteria (AIC)
# smaller is better
AIC(m3, m4)  # m4 seems "better" than m3 by virtue of a smaller AIC

sim <- simulate(m4, nsim = 50)
plot(density(log(homes$totalvalue)))
for(i in 1:50)lines(density(sim[[i]]), col = "lightblue")

# Visualize model
ggpredict(m4, terms = c("finsqft", "fullbath")) |> plot()
table(homes$fullbath)
summary(homes$finsqft)

# nature of plot does not change, but simply shifts up
ggpredict(m4, terms = c("finsqft[1200:2200 by=100]", "fullbath[1:4]"), 
          condition = c(lotsize = 0.5, bedroom = 3)) |> plot()

ggpredict(m4, terms = c("finsqft[1200:2200 by=100]", "fullbath[1:4]"), 
          condition = c(lotsize = 0.5, bedroom = 3)) |> 
  plot(labels = scales::dollar)

# Don't really need the interaction despite the "significance";
# Let's go with model m3


# Making predictions with model

# predicted mean for homes with those predictors
predict(m3, newdata = data.frame(finsqft = 1500, lotsize = 0.3, 
                                 bedroom = 3, fullbath = 2), 
        interval = "confidence") |> exp()

# predicted value for a home with those predictors
predict(m3, newdata = data.frame(finsqft = 1500, lotsize = 0.3, 
                                 bedroom = 3, fullbath = 2), 
        interval = "prediction") |> exp()



# Using model to make further inferences:

# Expected increase in totalvalue if I add a 300 finsqft addition to a house with 1500 finsqft?


# type = "response": predictions made on log scale and then transformed to the
# response scale (ratio)
emmeans(m3, specs = "finsqft", at = list(finsqft = c(1800, 1500),
                                         bedroom = 3,
                                         bathroom = 2, 
                                         lotsize = 0.3), 
        type = "response") |> 
  pairs() |> 
  confint()

# about a 10-11%% increase in total value


# regrid = "response": predictions made on the original scale (difference)
emmeans(m3, specs = "finsqft", at = list(finsqft = c(1800, 1500),
                                         bedroom = 3,
                                         bathroom = 2, 
                                         lotsize = 0.3), 
        regrid = "response") |> 
  pairs() |> 
  confint()

# about $38,000 to $42,000 increase in total value


# MSE and Cross validation ------------------------------------------------

# MSE = Mean Square Error
# SSE = Sum of Squares Error
# Lower values (closer to zero) indicate better fit.
# the mean square error can be considered as the variance of the residuals, i.e.
# the variation in the outcome the model doesn't explain.
mean(residuals(m3)^2) # MSE
sum(residuals(m3)^2) # SSE

# error: obs - predicted
residuals(m3)[1:5]

# square error
(residuals(m3)^2)[1:5]

# mean square error
mean(residuals(m3)^2)

# mean square error for intercept-only model;
# in other words, just use overall mean as predicted value
mean((log(homes$totalvalue) - mean(log(homes$totalvalue)))^2)


# Cross Validation

# Testing how well our model performs on the same data used to build it may lead
# to overly optimistic performance. We would like to estimate how it will
# perform on new data.

# let's hold out 200 observations and compare MSE for models built with and
# without those observations.

# random sample of 200 row numbers
set.seed(1)
i <- sample(nrow(homes), size = 200)
# when obs used to build model
mean(residuals(m3)[i]^2)

# update model without those observations
m3a <- update(m3, subset = -i)
p <- predict(m3a, newdata = homes[i,])
mean((log(homes$totalvalue)[i] - p)^2)
# MSE is a bit higher

# Let's divide our data into 10 sections and do this for each section

# 10-fold Cross validation
k <- 10
n <- nrow(homes)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- numeric(k)
for(i in 1:k){
  cvm <- update(m3, data = homes[folds != i,])
  p <- predict(cvm, newdata = homes[folds == i,])
  cv.errors[i] <- mean((log(homes$totalvalue[folds == i]) - p)^2)
}

# cross validated error is slightly higher
mean(cv.errors)


# The boot package has a function to do this: cv.glm()
# Model needs to be fit with glm()
library(boot)
m_glm <- glm(log(totalvalue) ~ log(finsqft) + fullbath + fp, 
            data = homes, family = gaussian)
cv.err <- cv.glm(homes, m_glm, K = 10)

# The first component is the raw cross-validation estimate of prediction error.
# The second component is the adjusted cross-validation estimate.
cv.err$delta

# Machine Learning --------------------------------------------------------


### Regression trees

# With machine learning (ML) we _let the machine learn from the data_ using an
# algorithm and come up with its own model. There are many ML algorithms. We'll
# look at classification and regression trees (CART), also called recursive
# partitioning.
#
# We can use the {rpart} package to build a single tree. Below we let the
# machine build a single tree to predict totalvalue using all variables in the
# homes data frame.

rt1 <- rpart(log(totalvalue) ~ ., data = homes)

# If we print the object we get a text representation of the tree:

rt1

# let's plot the tree. Setting `xpd = TRUE` ensures all plotting is clipped to
# the figure region. Adding `use.n = TRUE` adds the number of observations
# falling in each node. `cex = 0.8` makes text 80% of the normal size. 

plot(rt1)
text(rt1, xpd = TRUE, use.n = TRUE, cex = 0.8)

# another plotting methods for trees
library(partykit)
plot(as.party(rt1), tp_args = list(id = FALSE))


# The summary() function displays the tree constructions process. Beware, it can
# be quite long! Can use the file= argument to write the output to a given file
# name.

summary(rt1)

# Variable importance
# From the Introduction to Rpart:

# "A variable may appear in the tree many times, either as a primary or a
# surrogate variable. An overall measure of variable importance is the sum of
# the goodness of split measures for each split for which it was the primary
# variable, plus goodness * (adjusted agreement) for all splits in which it was
# a surrogate. In the printout these are scaled to sum to 100 and the rounded
# values are shown, omitting any variable whose proportion is less than 1%."

# Node number 1:
# mean=12.9599, MSE=0.08372591 
mean(log(homes$totalvalue))
mean((log(homes$totalvalue) - mean(log(homes$totalvalue)))^2)

# finsqft  < 1710.5  to the left,  improve=0.4690441, (0 missing)
SS_parent <- sum((log(homes$totalvalue) - mean(log(homes$totalvalue)))^2)
SS_left <- sum((log(homes$totalvalue[homes$finsqft < 1710.5]) - 
                  mean(log(homes$totalvalue[homes$finsqft < 1710.5])))^2)
SS_right <- sum((log(homes$totalvalue[homes$finsqft >= 1710.5]) - 
                   mean(log(homes$totalvalue[homes$finsqft >= 1710.5])))^2)
1 - ((SS_left + SS_right)/SS_parent)

# Node number 2:
# mean=12.74732, MSE=0.04551221 
mean(log(homes$totalvalue[homes$finsqft < 1710.5]))
mean((log(homes$totalvalue[homes$finsqft < 1710.5]) - 
        mean(log(homes$totalvalue[homes$finsqft < 1710.5])))^2)

# Compute R-squared for tree and compare to R-squared for last linear model
# above (0.7648).

p_tree <- predict(rt1)
plot(log(homes$totalvalue), p_tree)
cor(log(homes$totalvalue), p_tree)^2

plot(log(homes$totalvalue), predict(m3))
cor(log(homes$totalvalue), predict(m3))^2


### Pruning and Complexity

# The algorithm can theoretically build a huge, deep tree that will closely
# predict the data used the build the tree. But then it won't work well for data
# not used to build a tree. It was _overfit_ to the sample.

# Likewise, the algorithm can build a small tree to avoid overfitting, but then
# it also won't work well for data not used to build the tree. It's too simple.

# The `rpart()` function uses a _complexity parameter_ (or penalty) to determine
# how big to grow a tree. Higher complexity means a higher "cost" to growing a
# tree, and trees end up small. Lower complexity means a "lower" cost to growing
# a tree, and trees end up bigger.

# By default, the `rpart()` function uses 10-fold cross validation to estimate
# the optimal size tree using a complexity parameter of 0.01. (This says the
# overall R-squared must increase by at least 0.01 at each split.) But it also
# builds progressively smaller trees using bigger complexity parameters that we
# can select via _pruning_.

# We can view these trees using `printcp()`. The `xerror` column shows the cross
# validation error. Smaller is better. The `CP` column shows the complexity
# parameter.

printcp(rt1)
# Root node error: MSE at root (before any splits)
# 148.19/1770 (SSE/nrow(homes))
sum((log(homes$totalvalue) - mean(log(homes$totalvalue)))^2)/1770
mean((log(homes$totalvalue) - mean(log(homes$totalvalue)))^2)

# We can also plot these values using `plotcp()`. The dotted line is drawn 1
# standard error above the minimum of the curve. Rule of thumb: select the
# smallest tree within one standard error of the smallest CV error.

plotcp(rt1)


# If we like we can prune the tree using the `prune()` function.
rt1_prune <- prune(rt1, cp=0.014)
plot(rt1_prune)
text(rt1_prune, xpd = T, cex = 0.8)

# We can let the tree grow bigger (or deeper) if we change the _complexity
# parameter_. This is set with the argument `control = rpart.control(cp =
# 0.01)`. By default is 0.01. This says the overall R-squared must increase by
# cp at each step. If we decrease this we can grow a deeper tree. Let's try cp =
# 0.001.


rt2 <- rpart(log(totalvalue) ~ ., data = homes, 
             control = rpart.control(cp = 0.0001))

plot(rt2)
text(rt2, xpd = T, cex = 0.5, use.n = TRUE)
printcp(rt2)
plotcp(rt2)
i <- which.min(rt2$cptable[,"xerror"])
round(rt2$cptable[i,], 4)

# run this a few times;
# notice the optimal number of splits varies
rt2 <- rpart(log(totalvalue) ~ ., data = homes, 
             control = rpart.control(cp = 0.0001))
i <- which.min(rt2$cptable[,"xerror"])
round(rt2$cptable[i,], 4)

# implement in a for loop
opt_split <- numeric(30)
for(i in 1:30){
  cp <- rpart(log(totalvalue) ~ ., data = homes, 
              control = rpart.control(cp = 0.0001))$cptable
  min <- which.min(cp[,"xerror"])
  opt_split[i] <- cp[min, "nsplit"]
}
table(opt_split)  


# What's a good complexity parameter? What's a good size tree? How will the tree
# work for new data? These are hard questions.


### Random Forests

# One way to work around this is to bootstrap the data and build lots of deep
# trees using a random selection of variables to determine splits. The result
# will be many trees that we can use to make many predictions, and then take the
# average of those predictions. This is called a _random forest_.

# Let's fit a random forest using 30 trees. Usually we do 500, but we have a
# big data set and not much time. Set `do.trace = TRUE` to see progress. 

library(randomForest)
# homes$logfinsqft <- log(homes$finsqft)
rf1 <- randomForest(log(totalvalue) ~ ., 
                    data = homes, 
                    ntree=500, do.trace = TRUE)

# No. of variables tried at each split: 9 variables/3 = 3
rf1

# Calling `plot()` on the random forest object displays the MSE for prediction
# as we grow the forest:
plot(rf1)

# The drawback of random forests is that we cannot describe or display how the
# predictors are being used to make predictions.

# However we can use the `varImpPlot()` function to see the most important
# predictors based on the total decrease in mean square error that results from
# splits over the variable.
varImpPlot(rf1)

# variable importance
importance(rf1)

# R-squared
prf <- predict(rf1)
plot(prf, log(homes$totalvalue))
cor(log(homes$totalvalue), prf)^2

# Use cross validation to determine mtry
cvr <- rfcv(homes[,-2], log(homes[,2]), step = -1, scale = "step")
cbind(cvr$n.var, cvr$error.cv)
with(cvr, plot(n.var, error.cv, log="x", type="o", lwd=2))

# looks like mtry = 5 would be good

# increase mtry
rf2 <- randomForest(log(totalvalue) ~ ., 
                    data = homes, mtry = 5,
                    ntree=500, do.trace = TRUE)
rf2
plot(rf2)

# variable importance
importance(rf2)
varImpPlot(rf2)

# partial dependence plot: a graphical depiction of the marginal effect of a
# variable on response.
# rug: deciles of x.var.
partialPlot(rf2, homes, "finsqft")
partialPlot(rf2, homes, "lotsize")
partialPlot(rf2, homes, "age")


# partial dependence plot "by hand"
finsqft <- seq(min(homes$finsqft), max(homes$finsqft), length.out = 51)
partial_tv <- numeric(length(finsqft))
tmp <- homes
for(i in 1:length(finsqft)){
  tmp$finsqft <- finsqft[i]
  partial_tv[i] <- mean(predict(rf2, newdata = tmp))
}
plot(finsqft, partial_tv, type = "l")

# The `getTree()` function can be used to extract a single tree from the forest.
# For example, `getTree(rf1, k = 1)` will display the first tree.
tree1 <- getTree(rf2, k = 1, labelVar=TRUE)

# Let's look at the R-squared

prf <- predict(rf2)
cor(log(homes$totalvalue), prf)^2

# Let's see how the random forest predictions compare to what we observed:

cbind("obs" = homes$totalvalue[1:5], 
      "pred" = exp(predict(rf2, newdata = homes[1:5,])))

# Let's use our random forest to make a prediction for data not in our data set.
# Use Agnor-Hurt data.
p_ah <- predict(rf2, newdata = ah)
cbind(obs = ah$totalvalue, pred = exp(p_ah))
plot(ah$totalvalue, exp(p_ah))


# Can also make a single prediction on-the-fly, but it's tedious;
# Need to provide every variable.
nd <- data.frame(lotsize = 0.5,
                 condition = factor("Good", 
                                    levels = levels(homes$condition), 
                                    ordered = TRUE),
                 finsqft = 2000,
                 cooling = factor("Central Air", 
                                  levels = levels(homes$cooling)),
                 bedroom = 3,
                 fullbath = 2,
                 age = 0,
                 remodeled = 0,
                 fp = factor("Yes", levels = levels(homes$fp)))
exp(predict(rf1, newdata = nd))

