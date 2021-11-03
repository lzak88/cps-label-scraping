library(tidyverse)
library(jsonlite)

#Import json file
json_raw <- fromJSON(file = "https://api.census.gov/data/2021/cps/asec/mar/variables.json") 

#convert to a dataframe
cps_raw <- enframe(unlist(json_raw))
head(cps_raw, 50)

#separate out variable name and data type into separate columns
cps_raw <- cps_raw %>% 
  separate(name, 
           into = c(paste0("x", 1:3)), 
           sep = '([.])')

head(cps_raw, 30)

#check number of unique variables in data set (1044)
cps_unique <- unique(cps_raw$x2)
length(cps_unique)

#filter so you are only getting variables and their labels
cps_df <- cps_raw %>%
  select(x2, x3, value) %>%
  filter(x3 == "label")

#rename columns
cps_df <- cps_df %>% 
  select(x2, value)%>% 
  rename(
    Variable = x2,
    Label = value
  )
#check results
print(head(cps_df, 10))

#Check number of variables in dataframe to make sure it matches 1044
length(cps_df$Variable)

#check that no null labels
sum(is.na(cps_df$Label))

#Which variables not in label df (should be zero)
cps_missing <- subset(cps_raw, !(x2 %in% cps_df$Variable))
cps_missing <- unique(cps_missing$x2)
cps_missing
