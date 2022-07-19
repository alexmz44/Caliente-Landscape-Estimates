#packages
library(readxl)
library(tidyxl)
library(tidyr)
library(tidyverse)
library(janitor)
library(dplyr, warn = F)
library(tidyr)
library(rvest)
library(stringr)
library(stringi)
library(jsonlite)
library(RODBC)
library(DBI)
library(sqldf)
library(odbc)

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
  group_by(Item) %>%
  summarize(total = sum(Quantity),
            avg_price = mean(Price)) 
# i need to acquire a list of names, so I can put the plants in their respective groups
# i need to use rvest to scrape the webpage from an online dataset
# html link
cactus_web <- read_html("http://www.cactus-mall.com/names.html")
# read the table and put it into a datframe
cactus_df <- as.data.frame(html_table(cactus_web, header = T)[[4]])
#capitlize the firs tletter of each string? not exactly what I wanted
cactus_df$New_Names<-sub("(.)", "\\U\\1",cactus_df$`Common name`,perl=TRUE)
# extract column of just the new sstrings
cactus_names <- cactus_df$New_Names
# create a pattern for the cactus_names
# uppercase for all first letters
cactus_names <- stri_trans_totitle(cactus_names)
pattern <- paste(cactus_names, collapse = "|")
  group_by(Item)%>%
  summarize(avg_price = max(Price)) %>%
  select(Item, avg_price) %>% 
  as.data.frame()



# getting the list of all the vines from internet
# need javascript but there is a workaround to get the vines table
# Inspect > Network
json_data <- jsonlite::fromJSON("https://api.bugwood.org/rest/api/subject/.json?fmt=datatable&include=count&cat=51&systemid=2&draw=2&columns%5B0%5D%5Bdata%5D=0&columns%5B0%5D%5Bsearchable%5D=false&columns%5B0%5D%5Borderable%5D=false&columns%5B0%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B1%5D%5Bdata%5D=1&columns%5B1%5D%5Bsearchable%5D=true&columns%5B1%5D%5Borderable%5D=true&columns%5B1%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B2%5D%5Bdata%5D=2&columns%5B2%5D%5Bsearchable%5D=true&columns%5B2%5D%5Borderable%5D=true&columns%5B2%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B3%5D%5Bdata%5D=3&columns%5B3%5D%5Bsearchable%5D=false&columns%5B3%5D%5Borderable%5D=true&columns%5B3%5D%5Bsearch%5D%5Bvalue%5D=&order%5B0%5D%5Bcolumn%5D=1&order%5B0%5D%5Bdir%5D=asc&start=163&length=126&search%5Bvalue%5D=&_=1657572710039")
vine_table <- as.data.frame(json_data$data)
vine_names <- vine_table$V2 %>% 
  stri_trans_totitle()
vine_pattern <- paste(vine_names, collapse = "|")
# tidying up the data set
estimates_df <- estimates_df %>%
  rename(Type= section)

estimates_df <- estimates_df %>%
  mutate(Type = ifelse(grepl("Sod", estimates_df$Item) | 
                            grepl("Bermuda", estimates_df$Item) | 
                            grepl("Bermua", estimates_df$Item) |
                            grepl("Turf", estimates_df$Item)
                          , "Turf", Type),
         Type = ifelse(grepl("seed", Item) | grepl("Seed", Item), "Seed", Type ),
         Type = ifelse(grepl("Rip Rap", Item), "Rip Rap", Type),
         Type = ifelse(grepl("Screened", Item) | grepl("Minus", Item), "Decomposed Granite", Type),
         Type = ifelse(grepl("Palm", Item), "Palms", Type),
         Type = ifelse(grepl("Trees", Item) | grepl('24" Box', Item) | grepl('36" Box', Item) |grepl('48" Box', Item) | grepl('60" Box', Item), "Trees", Type),
         Type = ifelse(grepl("Grass", Item), "Shrubs", Type),
         Type = ifelse(grepl("Agave", Item) | grepl("Aloe", Item), "Cacti/Succulents", Type),
         Type = ifelse(str_detect(Item, vine_pattern),"Vines", Type),
         Type = ifelse(grepl("Bougainvillea", Item) | grepl("Vine", Item), "Vines", Type),
         Type = ifelse(str_detect(Item, pattern)| grepl("Cactus", Item)| grepl("Mexican Fence Post", Item) , "Cacti/Succulents", Type)) %>%
  filter(Type != "Vegetable / Herb Garden" & Type != "Irrigation" & Type != "Pitchers Mound") %>%
  mutate(Type = recode(Type,
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
                          Succulents = "Cacti/Succulents",
                          `Stabilized DG` = "Decomposed Granite",
                          `Header` = "Curb",
                          `Trees and Palms` = "Trees",
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
                          `Shrubs /  Groundcovers` = "Shrubs",
                          `Medium Shrubs` = "Shrubs",
                          `Inert Groundcovers`= "Groundcovers",
                          `FALSE` = "Seed",
                          Track = "Decomposed Granite",
                          `Groundcovers/ Mass Planters` = "Groundcovers",
                          `Gorilla Snot` = "Dust Control",
                          `Accents/Cacti` = "Cacti/Accents",
                          `Trees/Palms` = "Trees"))
estimates_df <- estimates_df %>%
  mutate(Type = recode(Type,
                          `Decomposed Granite/Rip Rap` = "Rip Rap",
                          `Decomposed Granite/Boulders` = "Boulders",
                          `Shrubs/Accents/Cacti` = "Cacti/Succulents",
                          `Accents/Vines` = "Shrubs",
                          `Shrubs/Accents/Groundcovers` = "Shrubs",
                          `Cacti/Accents` = "Shrubs",
                          `Shrubs/Accents` = "Shrubs",
                          `Accents/Cacti/Succulents` = "Shrubs",
                          `Accents/Trees` = "Shrubs",
                          `Shrubs/Vines` = "Shrubs",
                          `Accents/Groundcovers` = "Shrubs",
                          `Shrubs/Grasses/Accents` = "Shrubs",
                          `Accents/Grasses` = "Shrubs",
                          `Groundcovers/Vines` = "Shrubs",
                          `Accents/Shrubs/Cacti` = "Shrubs",
                          `Shrubs/Groundcovers` = "Shrubs",
                           Cacti = "Shrubs",
                       Grasses = "Shrubs"))



# exploratory analysis
estimates_df %>%
group_by(Item) %>%
  summarize(total = sum(Quantity))

estimates_df %>%
  filter(Type == "Cacti")
  
  estimates_df %>%
    select(Type) %>%
    distinct()
    

# take off (Caliper) at the end of every single row
estimates_df$Item <-  sub("\\s*\\(.*?\\)$", "", estimates_df$Item)

# I want to separate it by section

list_dfs <- split(estimates_df, estimates_df$Type)
for(i in 1:length(names(list_dfs))){
  assign(paste(names(list_dfs[i]),".df", sep = ""), as.data.frame(list_dfs[i]))
}

# connecting to sql
my_connection <- dbConnect(drv = odbc::odbc(),
                         Driver = "SQL Server",
                         server = "calientelandscape.database.windows.net",
                         database = "Estimates",
                         uid = "alexmz44",
                         pwd = "Alexm0307!")


for(i in 1:length(list_dfs)){
dbWriteTable(conn = my_connection,
             name = paste(names(list_dfs)[i]),
             value = list_dfs[[i]],
             row.names = F)
}
list_dfs[1]































#########################################################################################################################################################################################################













# create a function that automatically gives you the prices of that specific plant this will be used 
# in a shiny app that I create in the future
price <- function(i) {
  setwd("C:/Users/alexa/OneDrive/Documents/Projects with Data Science/Caliente-Landscape-Estimates/ESTIMATIONS FOR ALEX")
  j <- as.data.frame(tidy_data(read_excel(i, col_names = F)))
  j[,1] <-  sub("\\s*\\(.*?\\)$", "", j[,1])
  jpattern <- paste(j[,1], collapse = "|")
  k <- estimates_df %>% 
    mutate(Item = ifelse(str_detect(estimates_df[,1],jpattern),estimates_df$Item, NA))%>%
    filter(!is.na(Item)) 
  k.num <- names(k[,c(2:4)])
  k <- sapply(k[k.num], is.numeric)
  k <- k %>% group_by(Item) %>% slice(which.max(Price))
  orders <- j$section
 k[order(match(j$Type, unique(orders))),]  
  }
price(list.files()[2])


tidy_data(read_excel(list.files()[1], col_names  = F))















# test it

j <- as.data.frame(tidy_data(read_excel(list.files()[1], col_names = F)))
j[,1] <-  sub("\\s*\\(.*?\\)$", "", j[,1])
jpattern <- paste(j[,1], collapse = "|")
k <- estimates_df %>% 
  mutate(Item = ifelse(str_detect(estimates_df[,1],jpattern),estimates_df$Item, NA))%>%
  filter(!is.na(Item)) 
k.num <- names(k[,c(2:4)])
k[k.num] <- sapply(k[k.num], as.numeric)
k <- k %>% group_by(Item) %>% slice(which.max(Price))
orders <- unique(j$section)
k[order(match(k$Type, unique(orders))),]  %>% as.data.frame()

unique(orders)
k
