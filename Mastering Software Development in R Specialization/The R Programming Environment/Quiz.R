# Final quiz for The R Programming Environment
# Author: Adam Yu Wen
# Date: June 12, 2018

# Quiz Instructions

# The goal of this assignment is to take datasets that are either messy or simply not tidy and to make them
# tidy datasets. The objective is to gain some familiarity with the functions for reading in data into R
# and calculating basic summary statistics on the data. In particular, we will make use of the following
# packages: dplyr, tidyr, readr, and readxl. You can install these packages with the install.packages()
# function in R, using
#   install.packages(c("dplyr", "tidyr", "readr", "readxl"))

# Running install.packges() may also install a host of other packages on which these two depend so it might
# take a minute or two.

# Before staring the quiz you will need to download the data for the quiz, which can be found in the file
# quiz_data.zip. The zip archive file contains two files:
  
# daily_SPEC_2014.csv.bz2: a compressed CSV file containing daily measurements of particulate matter
# chemical constituents in the United States for the year 2014. Note that you should NOT have to decompress
# this file. The data are measured at a network of federal, state, and local monitors and assembled by the
# EPA. In this dataset, the "Arithmetic Mean" column provides the level of the indicated chemical
# constituent and the "Parameter.Name" column provides the name of the chemical constituent. The combination
# of a "State Code", a "County Code", and a "Site Num", uniquely identifies a monitoring site (the location
# of which is provided by the "Latitude" and "Longitude" columns).

# aqs_sites.xlsx: An excel spreadsheet containing metadata about each of the monitoring sites in the
# United States where pollution measurements are made. In particular, the "Land Use" and "Location Setting"
# variables contain information about what kinds of areas the monitors are located in (i.e. "residential"
# vs. "forest").

# Once the data have been downloaded to your working directory, you can begin the quiz assignment. For
# this assignment, you may want to review Sections 1.2 through 1.5 of Mastering Software Development in R.

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("readr")
usePackage("readxl")
usePackage("tidyr")
usePackage("dplyr")
usePackage("lubridate")

library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)

path <- paste("/Users/yu_wen/Documents/Coursera/Mastering Software Development in R Specialization/", 
              "The R Programming Environment/data", sep = "")
setwd(path)

# Reading data for Q1 to Q5
daily_SPEC <- read_csv("daily_SPEC_2014.csv.bz2")

# Question 1
# Use the readr package to read the daily_SPEC_2014.csv.bz2 data file in to R. This file contains daily
# levels of fine particulate matter (PM2.5) chemical constituents across the United States. The data
# are measured at a network of federal, state, and local monitors and assembled by the EPA.
# In this dataset, the "Sample.Value" column provides the level of the indicated chemical constituent
# and the "Parameter.Name" column provides the name of the chemical constituent. The combination of a
# "State.Code", a "County.Code", and a "Site.Num", uniquely identifies a monitoring site (the location
# of which is provided by the "Latitude" and "Longitude" columns).
# For all of the questions below, you can ignore the missing values in the dataset, so when taking
# averages, just remove the missing values before taking the average (i.e. you can use na.rm = TRUE
# in the mean() function)
# What is average Arithmetic.Mean for "Bromine PM2.5 LC" in the state of Wisconsin in this dataset?
Q1 <- daily_SPEC %>%
  select(c(`State Name`, `Parameter Name`, `Arithmetic Mean`)) %>%
  filter(`State Name` == "Wisconsin", `Parameter Name` == "Bromine PM2.5 LC") %>%
  summarize(res = mean(`Arithmetic Mean`, na.rm = TRUE))
Q1

# Question 2
# Calculate the average of each chemical constituent across all states, monitoring sites and all time
# points. 
Q2 <- daily_SPEC %>%
  select(c(`State Name`, `Parameter Name`, `Arithmetic Mean`)) %>%
  group_by(`Parameter Name`) %>%
  summarize(res = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(res)) %>%
  filter(`Parameter Name` %in% c("Sodium PM2.5 LC", 
                                 "OC CSN Unadjusted PM2.5 LC TOT",
                                 "Sulfur PM2.5 LC",
                                 "EC2 PM2.5 LC")) %>%
  head(1)
Q2

# Question 3
# Which monitoring site has the highest average level of "Sulfate PM2.5 LC" across all time?
# Indicate the state code, county code, and site number.
Q3 <- daily_SPEC %>%
  select(c(`State Code`, `County Code`, `Site Num`, `Parameter Name`, `Arithmetic Mean`)) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarize(res = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(res)) %>%
  head(1)
Q3

# Question 4
# What is the absolute difference in the average levels of "EC PM2.5 LC TOR" between the states California
# and Arizona, across all time and all monitoring sites?
Q4 <- daily_SPEC %>%
  select(c(`State Name`, `Parameter Name`, `Arithmetic Mean`)) %>%
  filter(`State Name` == "California" | `State Name` == "Arizona") %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR") %>%
  group_by(`State Name`) %>%
  summarize(res = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  spread(`State Name`, res) %>%
  mutate(abs_diff = abs(Arizona - California))
Q4

# Question 5
# What is the median level of "OC PM2.5 LC TOR" in the western United States, across all time? Define
# western as any monitoring location that has a Longitude LESS THAN -100.
Q5 <- daily_SPEC %>%
  select(c(`Longitude`, `Parameter Name`, `Arithmetic Mean`)) %>%
  filter(`Longitude` < -100 & `Parameter Name` == "OC PM2.5 LC TOR") %>%
  summarize(res = median(`Arithmetic Mean`, na.rm = TRUE))
Q5

# Reading data for Q6 to Q10
aqs_sites <- read_excel("aqs_sites.xlsx")

# Question 6
# Use the readxl package to read the file aqs_sites.xlsx into R (you may need to install the package
# first). This file contains metadata about each of the monitoring sites in the EPA's monitoring
# system. In particular, the "Land Use" and "Location Setting" variables contain information about
# what kinds of areas the monitors are located in (i.e. "residential" vs. "forest").
# How many monitoring sites are labelled as both RESIDENTIAL for "Land Use" and SUBURBAN for
# "Location Setting"?
Q6 <- aqs_sites %>%
  select(c(`State Name`, `County Name`, `Site Number`, `Land Use`, `Location Setting`)) %>%
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
  group_by(`State Name`, `County Name`, `Site Number`) %>%
  nrow()
Q6

# Question 7
# What is the median level of "EC PM2.5 LC TOR" amongst monitoring sites that are labelled as both
# "RESIDENTIAL" and "SUBURBAN" in the eastern U.S., where eastern is defined as Longitude greater
# than or equal to -100?
Q7 <- aqs_sites %>%
  left_join(daily_SPEC, by = c("Latitude", "Longitude")) %>%
  select(c(`Parameter Name`, `Arithmetic Mean`, `Land Use`, `Location Setting`, `Longitude`)) %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR" &
           `Land Use` == "RESIDENTIAL" &
           `Location Setting` == "SUBURBAN" &
           `Longitude` >= -100) %>%
  summarize(res = median(`Arithmetic Mean`, na.rm = TRUE))
Q7

# Question 8
# Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", which month of the year
# has the highest average levels of "Sulfate PM2.5 LC"?
Q8 <- aqs_sites %>%
  left_join(daily_SPEC, by = c("Latitude", "Longitude")) %>%
  select(c(`Parameter Name`, `Arithmetic Mean`, `Land Use`, `Date Local`)) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC" &
           `Land Use` == "COMMERCIAL") %>%
  mutate(Month = months(`Date Local`)) %>%
  group_by(Month) %>%
  summarize(res = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(res)) %>%
  head(1)
Q8

# Question 9
# Take a look at the data for the monitoring site identified by State Code 6, County Code 65, and
# Site Number 8001 (this monitor is in California). At this monitor, for how many days is the sum
# of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" greater than 10? 
# For each of the chemical constituents, there will be some dates that have multiple `Arithmetic
# Mean` values at this monitoring site. When there are multiple values on a given date, take the
# average of the constituent values for that date.
Q9 <- aqs_sites %>%
  left_join(daily_SPEC, by = c("Latitude", "Longitude")) %>%
  select(c(`State Code.x`, `County Code.x`, `Site Number`, `Parameter Name`,
           `Arithmetic Mean`, `Date Local`)) %>%
  filter(`State Code.x` == 6 &
           `County Code.x` == 65 &
           `Site Number` == 8001 &
           (`Parameter Name` == "Sulfate PM2.5 LC" |
              `Parameter Name` == "Total Nitrate PM2.5 LC")) %>%
  group_by(`Parameter Name`, `Date Local`) %>%
  summarize(res = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  group_by(`Date Local`) %>%
  summarize(res = sum(res)) %>%
  filter(`res` > 10) %>%
  nrow()
Q9

# Question 10
# Which monitoring site in the dataset has the highest correlation between "Sulfate PM2.5 LC" and
# "Total Nitrate PM2.5 LC" across all dates? Identify the monitoring site by it's State, County,
# and Site Number code.
# For each of the chemical constituents, there will be some dates that have multiple Sample.Value's
# at a monitoring site. When there are multiple values on a given date, take the average of the
# constituent values for that date.
# Correlations between to variables can be computed with the cor() function.
Q10 <- aqs_sites %>%
  left_join(daily_SPEC, by = c("Latitude", "Longitude")) %>%
  select(c(`State Code.x`, `County Code.x`, `Site Number`, `Parameter Name`,
           `Arithmetic Mean`, `Date Local`)) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC" | `Parameter Name` == "Total Nitrate PM2.5 LC") %>%
  # Some dates that have multiple Sample.Values at a monitoring site
  group_by(`State Code.x`, `County Code.x`, `Site Number`, `Parameter Name`, `Date Local`) %>%
  summarize(Average = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  spread(`Parameter Name`, Average) %>%
  group_by(`State Code.x`, `County Code.x`, `Site Number`) %>%
  summarize(Corr = cor(`Sulfate PM2.5 LC`, `Total Nitrate PM2.5 LC`)) %>%
  arrange(desc(Corr)) %>%
  head(1)
Q10