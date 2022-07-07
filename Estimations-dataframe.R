#packages
library(readxl)
library(tidyxl)
library(tidyr)
library(tidyverse)
library(janitor)
library(dplyr, warn = F)
library(tidyr)


# creates a function that wrangles the data in a way where all of the sections are listed.
tidy_data <- function(x) { x %>% 
    # remove ll empy rows and columns
  remove_empty() %>%
    # get a logical to see if the row is a header
  mutate(is_header_row = grepl("^Quan", `...5`),
         # logical to see  if there are any NA's in row 1
         is_row_empty = is.na(`...1`),
         # this is to get the NA's in row 5 to remove tthem later
         is_value_done = is.na(`...5`),
         # this names the rows headers by their header names
section = ifelse(is_header_row, `...1`, NA_character_)) %>% 
        # filter out all of the empty rows, because the headers should not be in the dataset
        # this takes away all the NA's at the end (After DG)
  filter(!is_row_empty, !is_value_done) %>%
        # this fills the whole column according to the section that they are in
  fill(section) %>%
        # remove any of the rows in the section column that are NA
        # remove any of the rows where the header is because you do not want headers in your rows 
  filter(!is.na(section), !is_header_row) %>%
        # deselect all of the created columns
  select(-is_row_empty, -is_header_row, -is_value_done) %>%
        # remove completely empty rows or completely empty columns
  remove_empty() %>%
    # rename first four columns
  rename(Item = 1, Quantity = 2, Price = 3, Totals = 4) %>% 
    # remove totals that are not there
    filter(!is.na(Totals))
}
#set the correct working directory
setwd("./ESTIMATIONS FOR ALEX")
# get the length of all of the files
length(list.files("."))
# create a vector to store all of the dataframes
estimates_list <- vector(mode = "list", length = length(list.files(".")))
split_list <- vector(mode = "list", length = length(list.files(".")))


# create a

for(i in 1:length(list.files("."))){
  estimates_list[[i]] <- read_excel(list.files(".")[i], col_names = F)
  if(!grepl("^Prices", estimates_list[[i]][,1])[1]){
 split_list[[i]] <- tidy_data(estimates_list[[i]]) } else{
   file.remove(list.files()[i])
 }
}
split_list
list.files()[24]  
 grepl("Prices", estimates_list[[24]])[1]
