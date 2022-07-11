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
getwd()
setwd("./ESTIMATIONS FOR ALEX")
# get the length of all of the files
length(list.files("."))
# create a vector to store all of the dataframes
estimates_list <- vector(mode = "list", length = length(list.files(".")))
split_list <- vector(mode = "list", length = length(list.files(".")))
# set the names for the column to be dropped
five_columns <- c("Item", "Quantity", "Price", "Totals", "section")
#set the conditions for the the files to be deleted

for(i in 1:length(list.files("."))){
  # reads in all the excel files
  estimates_list[[i]] <- read_excel(list.files()[i], col_names = F)
  # if there is a prices string in the first column or there are less than 6 columns, 

  if(any(grepl("Prices", estimates_list[[i]][,1])) | ncol(estimates_list[[i]])< 6){
    # it gets deleted
    file.remove(list.files()[i])
  
 }else{ 
   #apply function from above
  
   split_list[[i]] <- as.data.frame(tidy_data(estimates_list[[i]]))
   # take away the columns that are named ...9
   split_list[[i]] <- split_list[[i]][,names(split_list[[i]]) %in%  five_columns]
   
 }
}

# use the lapply function to remove items from the list that do not have the 5 necessary columns


split_list <- keep(split_list, function(x) ncol(x) == 5)

# row bind all of the data frames
estimates_df <- do.call("rbind", split_list) 
# make the correct columns numeric
sapply(estimates_df, class)
# get the names of the columns that are supposed to be integers
cols.num <- names(estimates_df[, c(2:4)])
# namke those columns numeric
estimates_df[cols.num] <- sapply(estimates_df[cols.num], as.numeric)

# remove all NA's
estimates_df <- estimates_df %>%
  na.omit() %>%
  filter(section != "Annuals")
# exploratory analysis
estimates_df %>%
  select(Item, Quantity) %>%
  group_by(summarise( sum = sum(Quantity)))
# get all of the distinct names to rename all of them
estimates_df %>%
   distinct(section) %>%
  as.data.frame()

estimates_df %>% filter(section == "Shrubs/Accents/Groundcovers")
# tidying up the data set
estimates_df <- estimates_df %>%
  # changing the values in grasses
  mutate(section = ifelse(grepl("Sod", estimates_df$Item) | 
                            grepl("Bermuda", estimates_df$Item) | 
                            grepl("Bermua", estimates_df$Item) |
                            grepl("Turf", estimates_df$Item)
                          , "Turf", section),
         section = ifelse(grepl("Screened", Item) | grepl("Minus", Item), "Decomposed Granite", section),
         section = ifelse(grepl("Lantana", Item), "Groundcovers", section)) %>%
         
  filter(section != "Vegetable / Herb Garden" & section != "Irrigation" & section != "Pitchers Mound") 

#rename all of the sections

estimates_df <- estimates_df %>%
  mutate(section = recode(section,
                          `Ground Covers` = "Groundcovers",
                          Groundcover = "Groundcovers",
                          GrounCovers = "Groundcovers",
                          Groudcovers = "Groundcovers",
                          Gorundcover = "Groundcovers",
                          Grundcovers = "Groundcovers",
                          `Groundcover and Shrubs` = "Shrubs/Groundcovers",
                          `Groundcover/Perenials` = "Groundcovers",
                          `Groundcovers & Shrubs` = "Shrubs/Groundcovers",
                          Grass = "Grasses",
                          `Grass/ Turf` = "Turf",
                          `Groundcover and Vines` = "Groundcovers/Vines",
                          `Groundcovers and Vines` = "Groundcovers/Vines",
                          GroundCovers = "Groundcovers",
                          `DG Path` = "DG",
                          `DG Trail` = "DG",
                          `DG Dust Control` = "Dust Control",
                          `Concrete Header` = "Curb",
                          `Curbing` = "Curb",
                          `Concrete Curb` = "Curb",
                          `Extruded Curb` = "Curb",
                          `Cacti / Succulents` = "Cacti",
                          `Cacti/ Accents` = "Cacti/Accents",
                          Cactus = "Cacti",
                          `Cacti/Succulents` = "Cacti",
                          DG = "Decomposed Granite",
                          `Decorative Rock` = "Decomposed Granite",
                          `Entry Way DG` = "Decomposed Granite",
                          `Entry Way Rip Rap` = "Rip Rap",
                          `Seed Mix` = "Seed",
                          Seeding = "Seed",
                          `River Rock` = "Rip Rap",
                          `Crushed Rock` = "Rip Rap",
                          `Palm Trees` = "Palms",
                          `Small Palms` = "Palms",
                          `Medium and Small Shrubs` = "Shrubs",
                          `Median DG` = "Decomposed Granite",
                          `Large Shrubs` = "Shrubs",
                          `Large Trees` = "Trees",
                          `Mexican Beach Pebble` = "Decomposed Granite",
                          `Extra Large Shrubs` = "Shrubs",
                          `Evergreen Trees` = "Trees",
                          `Boulder` = "Boulders",
                          `Beach Pebble` = "Decomposed Granite",
                          `Artficial Turf` = "Artificial Turf",
                          `Artifical Turf` = "Artificial Turf",
                          `Artificial Pet Turf` = "Artificial Turf",
                          `Alternate Parking Lot DG` = "Decomposed Granite",
                          Accetns = "Accents",
                          `Accent Plants/Cacti/Suculents` = "Cacti/Accents",
                          Accent= "Accents",
                          `Accents & Grasses` = "Accents/Grasses",
                          Vine = "Vines",
                          `Turf (SOD)` = "Turf",
                          `Temp DG` = "Decomposed Granite",
                          `Synthetic Turf` = "Artificial Turf",
                          Succulents = "Cacti",
                          `Stabilized DG` = "Decomposed Granite",
                          `Header` = "Curb",
                          `Trees and Palms` = "Trees/Palms",
                          `Small Shrubs` = "Shrubs",
                          `Small Trees` = "Trees",
                          Sod = "Turf",
                          SOD = "Turf",
                          `Stabilized Path` = "Decomposed Granite",
                          `Shrubs/ Accents` = "Shrubs/Accents",
                          `Shrubs and Accents` = "Shrubs/Accents",
                          `Shrubs, Grasses, Accents` = "Shrubs/Grasses/Accents",
                          `Shrubs & Accents` = "Shrubs/Accents",
                          `Shrubs & Vines` = "Shrubs/Vines",
                          `Shade Trees` = "Trees",
                          `Ornamental Trees` = "Trees",
                          `Medium Trees` = "Trees",
                          `Inert Gorundcovers` = "Groundcovers",
                          `Hydro-Seed` = "Seed",
                          `Hydroseed` = "Seed",
                          Header = "Curb",
                          `DG and Boulders` = "Decomposed Granite/Boulders",
                          `DG/Rip Rap` = "Decomposed Granite/Rip Rap",
                          `Courtyard Trees` = "Trees",
                          `Compacted DG` = "Decomposed Granite",
                          `Accents and Vines` = "Accents/Vines",
                          `Accents/ Vines` = "Accents/Vines",
                          `410` = "Trees",
                          `Accent and Grasses` = "Accents/Grasses",
                          `Accent/Ornamental Trees` = "Accents/Trees",
                          `Accents and Grasses` = "Accents/Grasses",
                          `Accents and Groundcovers` = "Accents/Groundcovers",
                          `Alternate Tree Pricing` = "Trees",
                          `Cobble` = "Rip Rap",
                          `Inert Groundcovers` = "Groundcovers",
                          `Granite Cobble` = "Rip Rap",
                          `FG` = "Decomposed Granite",
                           `Fractured Granite` = "Decomposed Granite",
                          `Accents/Ornamental Trees` = "Accents/Trees",
                           `TRUE`= "Turf",
                          `Succulent Garden` = "Cacti",
                          `Shrubs /  Groundcovers` = "Shrubs/Groundcovers",
                          `Medium Shrubs` = "Medium Shrubs",
                          `Inert Groundcovers`= "Groundcovers",
                          `FALSE` = "Seed",
                          Track = "Decomposed Granite",
                          `Groundcovers/ Mass Planters` = "Groundcovers",
                          `Gorilla Snot` = "Dust Control"))






