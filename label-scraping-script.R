library(tidyverse)
library(jsonlite)
library(glue)

#Create json url
year = 2021
url = glue("https://api.census.gov/data/{year}/cps/asec/mar/variables.json")

#Import json file
json_raw <- jsonlite::fromJSON(url) 

#convert to a dataframe
cps_raw <- enframe(unlist(json_raw))
head(cps_raw, 50)

#separate out variable name and data type into separate columns
cps <- cps_raw %>% 
  rename(label = value) %>%
  separate(name, 
           into = c("scrap", "variable", 
                    "metadata_type", "more_scrap", "value"), 
           sep =  "\\.",
           extra = "merge") %>%
  mutate(variable = tolower(variable)) %>% 
  filter(!(variable %in% c("for", "in", "ucgid", "yyyymm"))) %>%
  select(-matches("scrap"))
  

#check number of unique variables in data set 
# (1044 minus 3: 'for', 'in', and 'ucgid' which are API predicates, not variables)
cps_unique <- unique(cps$variable)
length(cps_unique)


#check results
print(head(cps, 10))

#Check number of variables in dataframe to make sure it matches 1041
length(unique(cps$variable))

#check that no null labels
sum(is.na(cps$label))

#filter so you are only getting variables and their labels
cps_variable_labels <- cps %>%
  filter(metadata_type == "label") %>%
  select(variable, label)

# create .do file text for variable labels
do_variable_labels <- cps_variable_labels %>%
  str_glue_data("label variable {variable} \"{label}\"")

#filter so you are only getting variables, values, and their labels
cps_value_labels <- cps %>% 
  filter(metadata_type == "values", 
         !(value %in% c("min", "max", "description"))) %>% 
  select(variable, value, label)

# create .do file text for variable values and labels
do_value_labels <- cps_value_labels %>%
  str_glue_data("label define {variable}_lbl {value} \"{label}\", add") 

apply_value_labels <- cps %>%
  distinct(variable) %>%
  str_glue_data("label values {variable} {variable}_lbl") 

#create do file text
do_file_data <- c(glue("* Generated {Sys.Date()}"),
                  "",
                  "// variable labels",
                  do_variable_labels,
                  "",
                  "// value labels",
                  do_value_labels,
                  apply_value_labels)


write_lines(do_file_data, glue("cps_variable_labels_{year}.do"))


