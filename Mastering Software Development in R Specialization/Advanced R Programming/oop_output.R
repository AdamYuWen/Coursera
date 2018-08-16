library(readr)
library(magrittr)
source("oop_code.R")

setwd("C:/Users/yu_wen/Documents/Coursera/Mastering Software Development in R Specialization/Advanced R Programming")
data <- read_csv("data/MIE.csv")

# Test LongitudionalData Class ################################################
df <- make_LD(data)
print(class(df))
print(df)

# Test Subject Class ##########################################################
subject_df <- subject(df, 14)
print(class(subject_df))
print(subject_df)
summary(subject_df)

# Test Visit Class ############################################################
visit_df <- visit(subject_df, 0)
print(class(visit_df))
print(visit_df)

# Test Room Class #############################################################
room_df <- room(visit_df, "bedroom")
print(class(room_df))
print(room_df)
summary(room_df)

# Test Summary Class ##########################################################
print(room_df %>% summary)