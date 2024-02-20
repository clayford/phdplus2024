# session 1
# Intro to R

# R is a calculator
# math: +, - , *, /, ^
sqrt(2)

# assignment
# Alt + - (Win) or Option + - (Mac)
s2 <- sqrt(2)
s2
s2 + 3
sqrt(s2)

# function with arguments
round(s2, digits = 4)
round(s2, 4)
s2 <- round(s2, 4)

# incomplete function results in "+" at prompt; hit Esc or complete function

# clear console: Ctrl + L (win/mac)
# arrow up/down to recall commands (navigate command history)
# removing objects from the global environment
rm(x)


# Naming rules
# R is case sensitive
# You can name an object in R almost anything you want, but it cannot 
# start with a number and it cannot use some special symbols, like ^, !, $, @, +, -, /, or *. 
# However you can use underscores and periods.


# save file as R script, with comments
# create an R script
# File...New File...R script
# Run code in a R script by putting your cursor in the code and hitting `Ctrl + Enter` (Win) or `Cmd + Return` (Mac)

# Enter comments using a `#`

# Play with data using trees
# Girth	numeric	Tree diameter (rather than girth, actually) in inches
# Height	numeric	Height in ft
# Volume	numeric	Volume of timber in cubic ft

data(trees)
head(trees)
tail(trees)
tail(trees, n = 1)
str(trees)
summary(trees)

nrow(trees)
ncol(trees)
dim(trees)
plot(trees)

#	Ctrl+1: Move focus to Source Editor
#	Ctrl+2: Move focus to Console
# Copy Lines Up/Down	Shift+Alt+Up/Down	(Win) Cmd+Option+Up/Down (Mac)

# work with columns in a data frame
trees$Volume
length(trees$Volume)
hist(trees$Volume)
summary(trees$Volume)
mean(trees$Volume)
sum(trees$Volume)/nrow(trees)
sum(trees$Volume)/length(trees$Volume)
median(trees$Volume)
min(trees$Volume)
max(trees$Volume)
quantile(trees$Volume, probs = 0.9)

# example of why we might use min() or max()
# normalization or feature scaling
# (trees$Volume - min(trees$Volume))/(max(trees$Volume) - min(trees$Volume))

sort(trees$Volume)
head(sort(trees$Volume, decreasing = TRUE), n = 5)
sort(trees$Volume, decreasing = TRUE) |> head(n = 5)

# Girth	 inches
# Height	 ft
# Volume	 cubic ft

# convert to meters^3
trees$Volume / 35.315

# creating new columns
# new names so we don't overwrite old columns
trees$Volume_m3 <- trees$Volume/35.315
trees$Height_m <- trees$Height/3.281
trees$Girth_cm <- trees$Girth * 2.54
View(trees)

# work with two columns in data frame
plot(trees$Girth, trees$Volume)
# lines(lowess(trees$Girth, trees$Volume))
# lowess(trees$Girth, trees$Volume) |> lines()
# plot(trees$Volume, trees$Volume_m3)
cor(trees$Girth, trees$Volume)

# m2 <- lm(Volume ~ poly(Girth, 2), data = trees)
# plot(m2, which = 1)
# plot(trees$Girth, trees$Volume)
# lines(8:25, predict(m2, newdata = data.frame(Girth = 8:25)))

# comparison
# FALSE = 0, TRUE = 1
trees$Girth > 13
sum(trees$Girth > 13)
sum(trees$Girth > 13)/length(trees$Girth)
mean(trees$Girth > 13)
sum(trees$Height > 70 & trees$Height < 80)
mean(trees$Height > 70 & trees$Height < 80)
sum(trees$Height == 80)
mean(trees$Height == 80)
sum(trees$Height != 80)
mean(trees$Height != 80)

# which rows are T/F
# which rows are T/F
any(trees$Height < 69)
all(trees$Height < 69)
which(trees$Height < 69)
i <- which(trees$Height < 69) # vector

# vectors; one dimension data of same type
# columns of data frames are (usually) vectors
# manually create vectors with c()
ages <- c(29, 25, 28, 45, 33)
gender <- c("m", "m", "f", "f", "m")
d <- data.frame(ages, gender)
d

# This is also a vector (logical vector)
trees$Height < 69

# subsetting brackets
trees[i,]
trees[trees$Height < 69,]
trees[trees$Height < 69, c("Height", "Volume")]

# can use with vectors
trees$Volume[trees$Volume > 50]
trees[trees$Volume > 50, c(1, 3)]
trees[trees$Volume > 50, c("Girth", "Volume")]

trees$Volume[c(2, 5, 7)]
trees$Volume[c(1, 1, 1)]
trees[c(2, 5, 7), c(1, 3)] # select columns 1 and 3

# using subset
subset(trees, Height < 69)
subset(trees, Height < 69, c(1,3))
trees2 <- subset(trees, Height < 69, c("Girth", "Volume"))

# load data with NAs
data(airquality)
help(airquality) # notice the example
summary(airquality) # missing data
table(airquality$Month)
airquality$Year <- 1973
airquality$Year <- NULL # how to delete column
mean(airquality$Temp)
mean(airquality$Ozone)
mean(airquality$Ozone, na.rm = TRUE)

head(airquality$Ozone)
is.na(airquality$Ozone)
sum(is.na(airquality$Ozone))
mean(is.na(airquality$Ozone))
sum(!is.na(airquality$Ozone)) # ! = negation (NOT TRUE, NOT FALSE)
mean(!is.na(airquality$Ozone))

subset(airquality, is.na(Solar.R))

# mean Temp by month
tapply(airquality$Temp, airquality$Month, mean) # returns vector
tapply(airquality$Ozone, airquality$Month, mean, na.rm = TRUE) 

# aggregate uses formula notation
aggregate(Temp ~ Month, data = airquality, mean) # data frame
aggregate(Ozone ~ Month, data = airquality, mean) # na.rm = T be default

# plots with formulas
boxplot(Temp ~ Month, data = airquality)
stripchart(Temp ~ Month, data = airquality, method = "jitter")

# Insert assignment operator: 	Alt+- (Win)	Option+- (Mac)
mean_temp <- aggregate(Temp ~ Month, data = airquality, mean)
plot(Temp ~ Month, data = mean_temp, type = "b", ylab = "Mean Temp", 
     main = "Mean Temp by Month")

# plot(Volume ~ Girth, data = trees)
# m <- loess(Volume ~ Girth, data = trees)
# lines(trees$Girth, predict(m))


# with CI bars
# m <- lm(Temp ~ factor(Month), data = airquality)
# library(ggeffects)
# plot(ggpredict(m))

# data with a factor
data(InsectSprays)
help(InsectSprays)
summary(InsectSprays)
str(InsectSprays)


# a factor is a categorical variable with fixed levels; numbers with labels
size <- c("XL", "L", "M", "L", "M")
table(size)
sizeF <- factor(size, levels = c("S", "M", "L", "XL"))
table(sizeF)

boxplot(count ~ spray, data = InsectSprays)
stripchart(count ~ spray, data = InsectSprays, method = "jitter")
aggregate(count ~ spray, data = InsectSprays, mean)
aggregate(count ~ spray, data = InsectSprays, sd)

mean_count <- aggregate(count ~ spray, data = InsectSprays, mean)
stripchart(count ~ spray, data = InsectSprays, method = "jitter", pch = 1)
points(mean_count$count, mean_count$spray, col = "red", pch = 16)
help(points)

## BREAK ##

# importing data and setting working directory

# source: https://catalog.data.gov/dataset/popular-baby-names
# set working directory
# https://bit.ly/baby_names_nyc

baby_names <- read.csv("popular_baby_names_nyc.csv")
str(baby_names)
sort(unique(baby_names$Year.of.Birth))
unique(baby_names$Ethnicity)

head(baby_names)
names(baby_names)
names(baby_names)[c(1,4)] <- c("Year", "Name")
names(baby_names)

# 2012 vs 2015 for names (all caps versus sentence case)
subset(baby_names, Rank == 1) 
subset(baby_names, Rank == 1 & Year == 2019) 
subset(baby_names, Rank %in% 1:5 & Year == 2019 & Ethnicity == "HISPANIC") 
subset(baby_names, Rank %in% 1:5 & Year == 2019 & Ethnicity == "WHITE NON HISPANIC") 

# most counts
max(baby_names$Count)
subset(baby_names, Count == max(baby_names$Count))
subset(baby_names, Count == min(baby_names$Count))

# longest name
max(nchar(baby_names$Name))
table(nchar(baby_names$Name))
barplot(table(nchar(baby_names$Name)))
which.max(nchar(baby_names$Name))
baby_names[which.max(nchar(baby_names$Name)),]

# find your name
subset(baby_names, Name %in% c("Addison", "ADDISON"))
addison <- subset(baby_names, Name %in% c("Addison", "ADDISON") & 
                    Ethnicity == "WHITE NON HISPANIC")
addison <- addison[order(addison$Year),]
plot(addison$Year, addison$Rank, type = "l")


# regular expression to find names with Capital letters in other position
# besides first position.
i <- grep("(?<=[a-z ])[A-Z]", baby_names$Name, perl = TRUE)
baby_names$Name[i]




# Saving objects

# 1. save a single object
# 2. save entire workspace
# 3. save two or more objects

# save an object and allow it to be restored with a different name
saveRDS(trees, file = "trees.rds")
tree_data <- readRDS("trees.rds")

# save selected objects
save(trees, trees2, file = "tree_data.Rdata")
load("tree_data.Rdata")

# notice these are restored with the same names

# save workspace using save.image(), give it a .Rdata extension
# Be aware of where you save it!
# session...set working directory...choose directory...
# Or use Tab inside quotes to navigate to location
save.image(file = "/Users/jcf2d/Desktop/my_workspace.RData")
load("my_workspace.Rdata")



# R packages
# all functions belong to packages
# R comes with 30 packages
# Can install packages from CRAN

install.packages("car")
library(car)
scatterplot(Ozone ~ Temp, data = airquality)
scatterplot(Volume ~ Girth, data = trees)
scatterplotMatrix(airquality[,1:4])
scatterplotMatrix(trees[,1:3])
# scatterplot(Ozone ~ Temp, data = airquality, smooth = list(style = "none"))

install.packages("corrplot")
library(corrplot)
corrplot(cor(trees[,1:3]))
corrplot(cor(airquality[,1:4], use = "pairwise"))
corrplot(cor(airquality[,1:4], use = "pairwise"), method = 'number')
corrplot(cor(airquality[,1:4], use = "pairwise"), type = 'upper')
corrplot(cor(airquality[,1:4], use = "pairwise"), type = 'upper', diag = F)

install.packages("readr")
library(readr)
x <- c("$23,000", "$34,000", "$12,000")
x <- readr::parse_number(x)

## writing a function
# remember f(x) from algebra? Similar concept.
# f(x) = x^2 + 2x + 7
fn <- function(x) x^2 + 2*x + 7
fn(300)

# turn this code into a function
head(sort(trees$Height, decreasing = TRUE), n = 5)

# use the function() function
top5 <- function(x){
  head(sort(x, decreasing = TRUE), n = 5)
  }

top5(trees$Height)
top5(airquality$Temp)

tapply(airquality$Temp, airquality$Month, top5)

# allow n to be an argument
top_n <- function(x, n){
  head(sort(x, decreasing = TRUE), n)
}

top_n(trees$Height, n = 5)
top_n(airquality$Temp, n = 3)

tapply(airquality$Temp, airquality$Month, top_n, n = 3)


# creating/simulating data
die <- 1:6
die
sample(x = die, size = 1)
sample(x = die, size = 10, replace = TRUE)
rolls <- sample(x = die, size = 10000, replace = TRUE)
table(rolls)

rolls <- sample(x = die, size = 10000, replace = TRUE, 
                prob = c(1,1,2,2,1,1))
table(rolls)
barplot(table(rolls))

# two dice
roll1 <- sample(x = die, size = 10000, replace = TRUE)
roll2 <- sample(x = die, size = 10000, replace = TRUE)
roll <- roll1 + roll2
table(roll)
barplot(table(roll))
mean(roll >= 9)
