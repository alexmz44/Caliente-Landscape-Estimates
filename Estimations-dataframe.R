#packages
library(readxl)
library(tidyxl)
library(tidyr)
library(tidyverse)

#set the correct working directory
setwd("./ESTIMATIONS FOR ALEX")
# get the length of all of the files
length(list.files("."))
# create a vector to store all of the dataframes
estimates_list <- vector(mode = "list", length = 248)
output_list <- vector(mode= "list", length = 248)


for(i in 1:length(list.files("."))){
  estimates_list[[i]] <- xlsx_cells(list.files(".")[i])
  output_list[[i]] <- estimates_list[[i]][estimates_list[[i]]$data_type == "character",
                                          c("address", "character", "local_format_id")]
  output_list[[i]] <- output_list[[i]] %>% mutate(frame = cumsum(local_format_id == 3))
}

output_list

x <- xlsx_cells(list.files()[1])
output <- x[x$data_type == "character", c("address", "character",
                                          "local_format_id")]
output[output$local_format_id == 6, ]
output
