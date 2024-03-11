# PhD Plus: Data Literacy in R (2024)
# Session 3: Visualizing Data
# Clay Ford

# Albemarle County Homes Data

# In this session we learn how to use ggplot2 to explore data visually.

# install.packages("hexbin")

homes <- readRDS(url("https://github.com/clayford/phdplus2024/raw/main/data/albemarle_homes_2024-02-28.rds"))

# How does the total value of a home relate to its size in finished square feet?
# Plot totalvalue versus finsqft to create a scatter plot.

# First we need to load the ggplot2 package. Only do this once per R session.
library(ggplot2)
ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() 

# What just happened?
#   
# -   The `ggplot()` function creates a plotting area for a data frame
# -   The `aes()` function maps variables in the data frame to aesthetic 
#     properties of geometric shapes
# -   The `geom_point()` function specifies we want to use points
# -   combine these functions with `+`, which adds components to a plot
# 
# Don't forget the parentheses on `geom_point()`!

# - Try entering `alpha = 1/10` in `geom_point()`.
# - Try entering `shape = "."` in `geom_point()`. 
# - Try entering `shape = 1` in `geom_point()`.  
#   (see `?points` shapes and their numeric codes)

ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point(shape = ".") 

ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point(shape = 1) # open circles 

ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point(alpha = 0.2) 

# look at census tract 106.03
ggplot(subset(homes, censustract == "106.03")) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() 

# points can have color
ggplot(subset(homes, censustract == "106.03")) +
  aes(x = finsqft, y = totalvalue, color = cooling) +
  geom_point() 

# points can have different sizes
ggplot(subset(homes, censustract == "106.03")) +
  aes(x = finsqft, y = totalvalue, color = cooling, size = lotsize) +
  geom_point() 

# pathological example
ggplot(subset(homes, censustract == "106.03")) +
  aes(x = finsqft, y = totalvalue, 
      color = cooling, size = lotsize, shape = fp) +
  geom_point() 

# not restricted to one geom
ggplot(subset(homes, censustract == "106.03")) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() +
  geom_smooth()

# by hand using base R
m <- loess(totalvalue ~ finsqft, data = homes, subset = censustract == "106.03")
plot(m)
x <- seq(500,3500,1)
lines(x, predict(m, data.frame(finsqft = x)))

# add color for fp
ggplot(subset(homes, censustract == "106.03")) +
  aes(x = finsqft, y = totalvalue, color = fp) +
  geom_point() +
  geom_smooth(se = F)


# this is the basic framework for ggplot2
# ggplot(data) +
#   aes() +
#   geom_point()

# Introduce faceting

ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() +
  geom_smooth(se = F) 

# by hand using base R
library(mgcv)
m <- gam(totalvalue ~ s(finsqft, bs = "cs"), data = homes)
x <- 1:20000
pred_y <- predict(m, newdata = data.frame(finsqft = x))
plot(homes$finsqft, homes$totalvalue)
lines(x, pred_y, col = "blue")


ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~msdistrict)

ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~cooling)

# facet with two categorical variables
ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() +
  geom_smooth(se = F) +
  facet_grid(fp ~ cooling, labeller = label_both)

# Modifying the coordinate system
# zoom in plot
ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(xlim = c(0,4000), ylim = c(0,1e6))

ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point(shape = ".") +
  geom_smooth() +
  coord_cartesian(xlim = c(0,4000), ylim = c(0,1e6))


# update the scales
ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point(shape = ".") +
  geom_smooth() +
  coord_cartesian(xlim = c(0,4000), ylim = c(0,1e6)) +
  scale_y_continuous(labels = scales::label_currency(), n.breaks = 10, 
                     minor_breaks = FALSE) +
  scale_x_continuous(labels = scales::label_comma())

# color scales
ggplot(subset(homes, censustract == "106.03"))  +
  aes(x = finsqft, y = totalvalue, color = fp) +
  geom_point() +
  scale_color_manual("Fireplace", values = c("blue", "red"))

# run colors()

# color-blind friendly palettes
# To see all color blind friendly palette's, run the following code. It shows
# three types of palettes:
#
# 1. Sequential: suited to ordered data that progress from low to high. 

# 2. Qualitative: suited to representing nominal or categorical data. 

# 3. Diverging: suited to data with mid-range critical values and extremes at
#    both ends of the data range.

library(RColorBrewer)
display.brewer.all(colorblindFriendly=TRUE)
# or 
RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)

ggplot(subset(homes, censustract == "106.03"))  +
  aes(x = finsqft, y = totalvalue, color = fp) +
  geom_point() +
  scale_color_brewer("Fireplace", palette = "Set2")

# Themes and labels
# labs()
# ?theme_gray
ggplot(subset(homes, censustract == "106.03"))  +
  aes(x = finsqft, y = totalvalue, color = fp) +
  geom_point() +
  scale_color_brewer("Fireplace", palette = "Set2") +
  scale_y_continuous(labels = scales::label_currency()) +
  labs(x = "Finished Sq Ft", y = "Total Value", title = "Census Tract 106.03") +
  theme_classic()

# example of modifying theme
ggplot(subset(homes, censustract == "106.03"))  +
  aes(x = finsqft, y = totalvalue, color = fp) +
  geom_point() +
  scale_color_brewer("Fireplace", palette = "Set2") +
  scale_y_continuous(labels = scales::label_currency()) +
  labs(x = "Finished Sq Ft", y = "Total Value", title = "Census Tract 106.03") +
  theme_classic() + 
  theme(legend.position = "bottom")


# Begin special topics, more geoms and scales

# transforming scales using log10() transformation
ggplot(homes) +
  aes(x = finsqft, y = totalvalue) +
  geom_point(shape = ".") +
  geom_smooth() +
  scale_y_log10(labels = scales::label_currency()) +
  scale_x_log10() +
  labs(caption = "Note: Both variables on log (base 10) scale.")

# summarize relationship using regression
m <- lm(log(totalvalue) ~ log(finsqft), data = homes)
summary(m)
coef(m)
# 1% increase in finsqft increases totalvalue by 1.14 %
# 10% increase in finsqft increases totalvalue by 11.4 %
pred <- predict(m, newdata = data.frame(finsqft = c(2000, 2000*1.10)))
# undo the log transformation
exp(pred)
exp(pred)[2]/exp(pred)[1] # expected increase equals 1.114, or about 11.4%


# can save portions of syntax for repeated use
p <- ggplot(homes) +
  aes(x = finsqft, y = totalvalue)

p + 
  geom_point() + 
  geom_smooth()

p + 
  geom_bin2d() 

p + 
  geom_hex()

p + 
  geom_density_2d()

p + 
  geom_smooth() +
  geom_rug()

# write a function with ggplot2

ggplot(subset(homes, censustract == "106.03"))  +
  aes(x = finsqft, y = totalvalue, color = fp) +
  geom_point() +
  labs(title = "106.03")
ggsave(filename = "census_106.03.jpg")

# use the function() function to create a function.
# ct = censustract (can choose your own argument name)
splot <- function(ct){
  ggplot(subset(homes, censustract == ct))  +
    aes(x = finsqft, y = totalvalue, color = fp) +
    geom_point() +
    labs(title = ct)
  ggsave(filename = paste0("census_",ct,".jpg"))
}

splot("108.02")
splot("106.02")
splot("102.01")

# apply function to all census tracts
lapply(levels(homes$censustract), splot)


## Line plots over time

# How many homes have been built each year in Albemarle county? What year saw
# the most homes built? We can use the yearbuilt variable in our data to help
# answer this. We just need to count up the number of homes per yearbuilt and
# save the result as a data frame. 

homes_year <- as.data.frame(xtabs(~ yearbuilt, data = homes))
homes_year$yearbuilt <- as.numeric(as.character(homes_year$yearbuilt))

# Let's plot n vs yearbuilt using a line.
ggplot(homes_year) +
  aes(x = yearbuilt, y = Freq) +
  geom_line()

# There was a boom sometime after 1750. Which year was it? Around 1950 it seems
# the number of homes built per year really started to take off. When was the
# peak? It also looks like we saw a dip sometime after 2000. What year was that?

# The above plot shows some interesting trends and events, but it's hard to
# get precise years and numbers. Let's make it interactive using `ggplotly()`:

library(plotly)
ggplot(homes_year) +
  aes(x = yearbuilt, y = Freq) +
  geom_line()
ggplotly()

# Now we can interact with the plot to see information of interest. Hovering
# over points reveals precise information. We can also zoom in on portions of
# the plot. (Double-click in the plotting region to return to the full plot.)
#
# We can also save our plot and call `ggplotly()` on it.

p <- ggplot(homes_year) +
  aes(x = yearbuilt, y = Freq) +
  geom_line()
ggplotly(p)

# export...save as web page... to save interactive version of plot

# using R code
htmlwidgets::saveWidget(
  widget = ggplotly(p), #the plotly object
  file = "figure.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)


# How has the mean number of fullbaths in a home changed over time (yearbuilt)?
#
# The following code creates a data frame of mean fullbath by yearbuilt for 1950
# to present, but only homes that have not been remodeled.

homes_mean_fb <- aggregate(fullbath ~ yearbuilt, data = homes, mean, 
                           subset = remodeled == 0 & yearbuilt >= 1950)
head(homes_mean_fb)
ggplot(homes_mean_fb) +
  aes(x = yearbuilt, y = fullbath) +
  geom_line()
ggplotly()

## A package that extends ggplot2: ggridges

# There are many packages that extend ggplot, mainly by adding additional geoms
# or themes. One that is useful for our data is the {ggridges} package.

# First notice the challenge of making comparisons of density histograms between
# 20 census tracts. (Census tracts are small subdivisions of a county that are
# used by the USA Census Bureau for statistical reporting.)

ggplot(homes) +
  aes(x = totalvalue) +
  geom_density() +
  facet_wrap(~censustract) +
  coord_cartesian(xlim = c(0, 1e6))

# The {ggridges} package allows us to create ridgeline plots, which are density
# plots arranged in a staggered fashion. Notice the new geom
# `geom_density_ridges()`.

# install.packages("ggridges")
library(ggridges)
ggplot(homes) +
  aes(x = totalvalue, y = censustract) + 
  geom_density_ridges() +
  coord_cartesian(xlim = c(0, 1e6))

# The ggridges package provides a custom theme specifically for use with
# ridgeline plots. Just tack on the function `theme_ridges()` to use it.

ggplot(homes) +
  aes(x = totalvalue, y = censustract) + 
  geom_density_ridges() +
  coord_cartesian(xlim = c(0, 1e6)) +
  theme_ridges()


# We can reorder the levels of censustract by a summary statistic such as the
# median of totalvalue. Notice we use the base R function `reorder` to
# accomplish this. That will produce a ridgeline plot where the distributions
# are ordered by median totalvalue of each census tract. Below we use the `labs`
# function to add a title and update the axis labels.

ggplot(homes) +
  aes(x = totalvalue, y = reorder(censustract, totalvalue, median)) + 
  geom_density_ridges() +
  coord_cartesian(xlim = c(0, 1e6)) +
  scale_x_continuous(labels = scales::label_currency()) +
  labs(y = "Census Tract", x = "Total Value", 
       title = "Distribution of Total Value by Census Tract") +
  theme_ridges()

# NOTE: `ggplotly` does not work with plots made by {ggridges}.


# visualize association between bedroom and fullbath. Looks like a peg board
# since the values are discrete.
ggplot(homes) +
  aes(x = bedroom, y = fullbath) +
  geom_point()

# A good option in these scenarios is to "jitter" the data.
ggplot(homes) +
  aes(x = bedroom, y = fullbath) +
  geom_jitter(shape = ".")

# use width and height to tighten up the jitter and zoom in
ggplot(homes) +
  aes(x = bedroom, y = fullbath) +
  geom_jitter(shape = ".", width = 0.2, height = 0.2) +
  coord_cartesian(xlim = c(1,6), ylim= c(1,6)) +
  scale_x_continuous(breaks = 1:6, minor_breaks = FALSE) +
  scale_y_continuous(breaks = 1:6, minor_breaks = FALSE)

sum(homes$bedroom == 3 & homes$fullbath == 2)
mean(homes$bedroom == 3 & homes$fullbath == 2)

# a more complex plot that uses two data frames
ggplot(homes) +
  aes(x = hsdistrict, y = totalvalue) +
  geom_boxplot() +
  scale_y_log10(labels = scales::label_currency())

# calculate medians
med_value <- aggregate(totalvalue ~ hsdistrict, data = homes, median)
# add dollar-formatted values to data frame;
med_value$tv_label <- scales::dollar(med_value$totalvalue)

# add medians to boxplot
# notice each geom can take their own data set
ggplot() +
  geom_boxplot(mapping = aes(x = hsdistrict, y = totalvalue), 
               data = homes) +
  geom_text(mapping = aes(x = hsdistrict, y = totalvalue*1.2, 
                          label = tv_label), 
            data = med_value, size = 3) +
  scale_y_log10(labels = scales::label_currency()) 


# another plot that uses two data frames
ggplot(homes, aes(x=yearbuilt, y=totalvalue)) + 
  geom_jitter(width = 0.2, height = 0, shape=".") +
  coord_cartesian(xlim=c(1950,2024), ylim=c(0,2e6))

homeValues <- aggregate(totalvalue ~ yearbuilt, data = homes, median)
head(homeValues)
names(homeValues) <- c("year", "median_value")

ggplot() + 
  geom_jitter(mapping = aes(x = yearbuilt, y = totalvalue), 
              data = homes, 
              width = 0.2, height = 0, shape=".") +
  # Notice the next geom uses a new data frame
  geom_line(mapping = aes(x = year, y = median_value), 
            data = homeValues, linewidth = 1, color = "blue") +
  coord_cartesian(xlim=c(1950,2024), ylim=c(0,1e6)) 


## Visualizing counts

# Recall the condition variable. We can quickly get counts of each condition using the `table()` function:
  
table(homes$condition)

# One way to visualize counts is with a bar plot. We can create a bar plot in
# ggplot2 with `geom_bar()`. Notice it automatically generates the counts for
# us.

ggplot(homes) +
  aes(x = condition) +
  geom_bar()

# One of the aesthetics of bars is their "fill" color. We can map a variable
# from our data frame to the fill aesthetic. For example, let's see counts of
# conditions for homes with and without central air (cooling).

ggplot(homes) +
  aes(x = condition, fill = cooling) +
  geom_bar()

# The default result is to stack the bars. We can set them side-by-side setting
# the position argument to "dodge".

ggplot(homes) +
  aes(x = condition, fill = cooling) +
  geom_bar(position = "dodge") 

# The large number of Average homes makes it difficult to see the counts for the
# other categories. We could use `coord_cartesian()` to zoom in on the y-axis.
# An alternative approach is to view the _proportion_ of homes with and without
# central air within each condition. We can do this by setting the position
# argument to "fill".

ggplot(homes) +
  aes(x = condition, fill = cooling) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")

# The y-axis indicates proportions instead of counts. 

# use a different color palette (from RColorBrewer)
ggplot(homes) +
  aes(x = condition, fill = hsdistrict) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Proportion", x = "Condition", fill = "HS District")



## Creating maps

# When we talk about real estate and census tracts, it's natural to want to see
# visualizations that incorporate maps. Where is census tract 110 in Albemarle
# county? How big is it geographically?

# To create maps we usually need latitude and longitude values so we an draw
# shapes, like the borders of counties, states and census tracts. Our data
# doesn't have that information so we need to get it. Fortunately there is an R
# package that helps us quickly get this data. The tigris package allows us to
# download TIGER/Line shapefiles from the United States Census Bureau and load
# into R as "sf" objects. SF stands for "simple features" which is standardized
# way of encoding spatial vector data in computers.

# I already downloaded the census tract borders and saved as an rds file for us
# to use to help save time.

# Below is a quick example of how we can draw a map of Albemarle county and its
# census tracts using the {leaflet} package. Run the code below. An interactive
# map should be created in the RStudio Viewer that you can double-click or
# scroll to zoom in on locations. You can also click-and-drag to move around.
# Try it.


# install.packages("tigris")
# library(tigris)
# 
# this is going to download about 15 MB of data; may take a second
# alb <- tracts(state = "VA", county = "Albemarle")
# saveRDS(alb, file = "alb.Rds")

alb <- readRDS(url("https://github.com/clayford/phdplus2024/raw/main/data/alb.Rds"))
library(leaflet)
library(sf)

# The |> is known as a "pipe". It takes the output of one function and sends it
# as the input to the nest function.
alb |> 
  st_transform('+proj=longlat +datum=WGS84') |> 
  leaflet() |> 
  addProviderTiles(providers$OpenStreetMap) |> 
  addPolygons()

# That's nice but it would help if the census tracts were labeled. We can do
# that with `addMarkers()` function. We need to map the NAME column of the alb
# data frame to the label aesthetic.

alb |> 
  sf::st_transform('+proj=longlat +datum=WGS84') |> 
  leaflet() |> 
  addProviderTiles(providers$OpenStreetMap) |>
  addPolygons() |>
  addMarkers(lng = ~as.numeric(INTPTLON), 
             lat = ~as.numeric(INTPTLAT), label = ~NAME)

# We can shade the census tract regions according to a summary statistic such as
# median total value of a home. Let's do that. First we calculate median
# totalvalue by census tract, and then join with the alb data frame.


med_tv <- aggregate(totalvalue ~ censustract, data = homes, median)
alb <- merge(alb, med_tv, by.x = "NAME", by.y = "censustract", all.x = TRUE)


# To shade the census tract regions using leaflet requires a fair amount of
# work. We add a `fillColor` argument to `addPolygons()` to color the census
# tracts by median home value. We also add a legend using `addLegend()`. In both
# of those functions we use a custom color palette created by the `colorBin()`
# function. I pieced together this code using Google and the leaflet
# documentation. It took some trial and error to get it right.

bins <- seq(2e5, 9e5, 1e5)
pal <- colorBin("Reds", domain = alb$totalvalue, bins = bins)
p <- alb |> 
  sf::st_transform('+proj=longlat +datum=WGS84') |> 
  leaflet() |> 
  addProviderTiles(providers$OpenStreetMap) |>
  addPolygons(fillColor = ~pal(totalvalue), 
              fillOpacity = 0.5) |>
  addMarkers(lng = ~as.numeric(INTPTLON), 
             lat = ~as.numeric(INTPTLAT), 
             label = paste0(alb$NAME, ": ", scales::dollar(alb$totalvalue))) |> 
  addLegend(pal = pal, values = ~totalvalue, 
            title = NULL,
            position = "bottomright")

# export...save as web page... to save interactive version of plot

# using R code
htmlwidgets::saveWidget(
  widget = p, #the plotly object
  file = "map.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)


# plotting means and confidence limits
data(foster, package = "HSAUR3")
head(foster)

ggplot(foster) +
  aes(x = motgen, y = weight) +
  geom_jitter(height = 0, width = 0.05)

# bootstrap confidence intervals
ggplot(foster) +
  aes(x = motgen, y = weight) +
  geom_jitter(height = 0, width = 0.05, alpha = 0.2) +
  stat_summary(fun.data = mean_cl_boot, color = "red")

# traditional confidence intervals
ggplot(foster) +
  aes(x = motgen, y = weight) +
  geom_jitter(height = 0, width = 0.05, alpha = 0.2) +
  stat_summary(fun.data = mean_cl_boot, color = "red", 
               fun.args = list(conf.int = 0.95))

# changing arguments
ggplot(foster) +
  aes(x = motgen, y = weight) +
  geom_jitter(height = 0, width = 0.05, alpha = 0.2) +
  stat_summary(fun.data = mean_cl_boot, color = "red", 
               fun.args = list(conf.int = 0.90, B=500))



## Earthquakes

# plotting seismic events on am interactive map
eq <- readRDS(url("https://github.com/clayford/phdplus2024/raw/main/data/eq.Rds"))
str(eq)

ggplot(eq) +
  aes(x = mag) +
  geom_density()

table(eq$state)

eq_ks <- subset(eq, state == "Arizona")
leaflet(eq_ks) |>
  addTiles() |> 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                   popup = ~as.character(mag))
