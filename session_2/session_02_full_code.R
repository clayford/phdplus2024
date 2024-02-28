# PhD Plus: Data Literacy in R
# Session 2: Preparing Data for Analysis
# Clay Ford

# Set working directory or start a new R Project

# Albemarle county real estate --------------------------------------------

# Albemarle county
# Office of Geographic Data Services
# https://www.albemarle.org/government/information-technology/geographic-information-system-gis-mapping/gis-data

# Real Estate Information - Parcel Level Data

# This file contains information about the parcel itself such as owner
# information, deed acreage value, and assessed value. This file can be joined
# to the Parcels GIS layer via the GPIN field.
# https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip

link <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip"
download.file(link, destfile = basename(link))
unzip(basename(link)) # extract file to working directory


# Real Estate Information - Card Level Data

# Card Level Data refers to property information organized by residential
# dwellings or commercial units (e.g. building details and outbuilding
# information) on a given property. this file includes data such as year built,
# finished square footage, number of rooms, and condition.
# https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip

link2 <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip"
download.file(link2, destfile = basename(link2))
unzip(basename(link2)) # extract file to working directory


# Other Parcel Characteristics
 
# This file contains other parcel information that is managed in our development
# tracking system (e.g. Zoning, School Districts, Jurisdictional Areas, etc.).
# This file can be joined to the Parcel Level Data via the ParcelID field or to
# the Parcels GIS layer via the GPIN field.

link3 <- "https://gisweb.albemarle.org/gisdata/CAMA/CityView_View_OtherParcelCharacteristics_TXT.zip"
download.file(link3, destfile = basename(link3))
unzip(basename(link3)) # extract file to working directory

# tidy up
file.remove("GIS_View_Redacted_ParcelInfo_TXT.zip")
file.remove("GIS_CardLevelData_new_TXT.zip")
file.remove("CityView_View_OtherParcelCharacteristics_TXT.zip")
rm(link, link2, link3)

# begin live coding

# we'll use this library
library(lubridate)

# import data files
card_level <- read.csv("GIS_CardLevelData_new.txt", 
                       na.strings = "NULL")
parcel_level <- read.csv("GIS_View_Redacted_ParcelInfo.txt", 
                         na.strings = "NULL")
other_parcel <- read.csv("CityView_View_OtherParcelCharacteristics.txt", 
                         na.strings = c("NULL", "N/A"))

# Look at Kala Somerville's data in View; explain card numbers

# card_level list of variables to keep
card_vars <- c("TMP", "CardType", "YearBuilt", "YearRemodeled", 
               "UseCode", "Condition", "FinSqFt", "Cooling", 
               "FP_Open", "Bedroom", "FullBath")
card <- card_level[,card_vars]

# parcel_level list of variables to keep
parcel_vars <- c("ParcelID", "LotSize", "TotalValue", "LastSalePrice",
                 "LastSaleDate1", "Cards")
parcel <- parcel_level[,parcel_vars]

# other_parcel list of variables to keep
other_vars <- c("ParcelID", "ESDistrict", "MSDistrict", "HSDistrict",
                "CensusTract")
other <- other_parcel[,other_vars]

# clean up
rm(card_level, parcel_level, other_parcel, card_vars, other_vars, parcel_vars)

# Merge other data with parcel data 
# all.x = TRUE means keep all parcel records
parcel <- merge(x = parcel, y = other, by = "ParcelID", all.x = TRUE)

# Merge card data with parcel data
# This keeps record that have matching ParcelID/TMP in both data frames
homes <- merge(x = parcel, y = card, by.x = "ParcelID", by.y = "TMP")

# Done with merging

# make names lower case
names(homes) <- tolower(names(homes))

# look at use code
sort(table(homes$usecode), decreasing = TRUE)

# keep only detached residential home records 
# (e.g., not businesses, apartment complexes)
res <- c("Single Family", "Doublewide", "Duplex")
homes <- subset(homes, usecode %in% res & cardtype == "R")

# keep records with only one card associated with a parcel
homes <- subset(homes, cards < 2)

# keep homes with totalvalue greater than 0 and not NA
summary(homes$totalvalue)
homes <- subset(homes, totalvalue > 0 & !is.na(totalvalue))

# keeps homes with finsqft greater than 0 and not missing
summary(homes$finsqft)
homes <- subset(homes, finsqft > 0 & !is.na(finsqft))

# keeps homes with fullbath not missing
table(homes$fullbath, useNA = "ifany")
homes <- subset(homes, !is.na(fullbath))

# Keep homes with an assigned census tract
table(homes$censustract, useNA = "ifany")
homes <- subset(homes, !is.na(censustract))

# keep homes with yearbuilt not missing
summary(homes$yearbuilt)
homes <- subset(homes, !is.na(yearbuilt))

# create age of home
homes$age <- lubridate::year(Sys.Date()) - homes$yearbuilt
summary(homes$age)

# look at school districts
table(homes$hsdistrict, useNA = "ifany")
subset(homes, hsdistrict == "Unassigned")

# keep homes that are assigned to a school district
homes <- subset(homes, hsdistrict != "Unassigned")

# set some columns to factor
table(homes$hsdistrict)
facvar <- c("esdistrict", "msdistrict", "hsdistrict", "censustract")
homes[,facvar] <- lapply(homes[,facvar], factor)
summary(homes$hsdistrict)

# sale date should be date
# "%m/%d/%Y" = date pattern
# see help(strptime)
homes$lastsaledate1 <- lubridate::mdy(homes$lastsaledate1)
# date is stored as number of days since 1/1/1970
as.numeric(head(homes$lastsaledate1))

# Create month sold column
homes$month_sold <- lubridate::month(homes$lastsaledate1, label = TRUE)

# condition
table(homes$condition, useNA = "ifany") 

# re-order levels of factor
# set order of condition levels
cond_levels <- c("Very Poor", "Poor", "Fair", "Average", 
                 "Average Plus", "Good", "Excellent") 

homes$condition <- factor(homes$condition, levels = cond_levels, ordered = TRUE)
head(homes$condition)
summary(homes$condition)

# keep rows with condition not missing
homes <- subset(homes, !is.na(condition))

# yearremodeled -> remodel indicator
summary(homes$yearremodeled) # NA not remodeled; 
homes$remodeled <- ifelse(!is.na(homes$yearremodeled), 1, 0)
table(homes$remodeled)

# cooling
table(homes$cooling, useNA = "ifany") 
# fix factor -- assume 00 and "" are no air

homes$cooling <- factor(homes$cooling)
summary(homes$cooling)
levels(homes$cooling)
levels(homes$cooling) <- c("No Central Air", "No Central Air", "Central Air", 
                           "Central Air", "Central Air", "No Central Air")
summary(homes$cooling)

# drop rows with missing cooling
homes <- subset(homes, !is.na(cooling))

# fp_open (these are characters)
# make a binary indicator, 0 and Null are none
table(homes$fp_open, useNA = "ifany")
homes$fp <- ifelse(homes$fp_open > 0, 1, 0)
homes$fp <- factor(homes$fp, labels = c("No", "Yes"))
summary(homes$fp)

# drop rows with missing fp
homes <- subset(homes, !is.na(fp))

# drop selected variables
homes$parcelid <- NULL
homes$cards <- NULL
homes$fp_open <- NULL
homes$cardtype <- NULL

# reset row numbers
rownames(homes) <- NULL

# tidy up
rm(res, facvar, cond_levels)

# save everything to working directory
date <- Sys.Date()
save.image(file = paste0("albemarle_homes_", date, ".Rdata")) 

# save just the homes data frame 
saveRDS(homes, file = paste0("albemarle_homes_", date, ".rds")) 

# save a csv file of the homes data; will lose factor levels!
write.csv(homes, file = paste0("albemarle_homes_", date, ".csv"), 
          row.names = FALSE) 

## To run from command line

# Note that Rscript is not by default in the PATH on Windows
# Rscript session_02.R



# Earthquakes -------------------------------------------------------------


# The following web page allows the public to download all earthquakes recorded by the US Geological Survey (USGS) in the past 30 days. Updated every minute.
 
# https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php

# The data has 22 variables which are documented on the following web page: 
# https://earthquake.usgs.gov/data/comcat/index.php#event-terms

URL <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
eq <- read.csv(URL)

library(lubridate)
library(stringr)
eq$time <- ymd_hms(eq$time)
eq$updated <- ymd_hms(eq$updated)

# create column for location
head(eq$place)

# For example, if a row has place 
# 52 km W of Anchor Point, Alaska

# we want location to be 
# Anchor Point, Alaska

# And if a row has place "South Sandwich Islands region" we want location to
# remain "South Sandwich Islands region".

eq$location <- sub(pattern = ".+ of ", replacement = "", x = eq$place)
# View(eq[,c("place","location")])

# Create a column to indicate if location is in the USA
# most states are spelled out; but some use abbreviations
state.name
state.abb

states <- c(state.name, state.abb)
# create a regular expression for all US states 
sre <- paste0("(", paste(states, collapse = "|"), ")$")
eq$US <- grepl(pattern = sre, x = eq$place)
View(eq[,c("place","location", "US")])

# create State column
eq$state <- str_extract(eq$location, sre)
table(eq$state)
eq$state <- ifelse(eq$state == "CA", "California", eq$state)
# eq$state <- ifelse(eq$state == "NV", "Nevada", eq$state)
# eq$state <- ifelse(eq$state == "OR", "Oregon", eq$state)
# View(eq[,c("place","location", "US", "state")])

table(eq$state)

# Calculate elapsed time between earthquakes in minutes
eq$time[1] - eq$time[2]
eq$time[2] - eq$time[3]

eq$time[1:5] - eq$time[2:6]

time1 <- head(eq$time, n = nrow(eq)-1)
time2 <- tail(eq$time, n = nrow(eq)-1)
elapsed_time <- time1 - time2
eq$elapsed_time <- NA
# add to data frame and convert to minutes
eq$elapsed_time[1:(nrow(eq) - 1)] <- elapsed_time/60

saveRDS(eq, file = "eq.Rds")
