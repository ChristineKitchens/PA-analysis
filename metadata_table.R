#' R Code for Creating CSMI L Michigan Inventory
#' Contains functions for extracting identifying information
#' into a table.
#' 
#' Christine Kitchens
#' University of Michigan
#' School for Environment and Sustainability
#' Cooperative Institute for Great Lakes Research
#' chknight@umich.edu
#' August 2022
  

# 0 - Install Libraries ---------------------------------------------------


# install.packages("googledrive")
# install.packages("googlesheets4")
# install.packages("tidyverse")

# 1 - Load Libraries ------------------------------------------------------


library("googledrive")
library("googlesheets4")
library("tidyverse")

# 2 - Set Working Directory -----------------------------------------------


# Generate file path to working directory based on OS in use. OS.type will
# return "unix" if on Mac and "windows" if on PC.
if(.Platform$OS.type == "unix") {
  file_path20 <- file.path("/volumes",
                         "GoogleDrive",
                         "Shared drives",
                         "CSMI 2020 L Michigan Productivity and Optics",
                         "CSMI L Michigan Optics Experiments",
                         fsep = "/")
  file_path22 <- file.path("/volumes",
                          "GoogleDrive",
                          "Shared drives",
                          "CSMI 2022 Primary Production and Optics",
                          fsep = "/")
} else {
  file_path20 <- file.path("G:",
                         "Shared drives",
                         "CSMI 2020 L Michigan Productivity and Optics",
                         "CSMI L Michigan Optics Experiments",
                         fsep = "/")
  file_path22 <- file.path("G:",
                         "Shared drives",
                         "CSMI 2022 Primary Production and Optics",
                         fsep = "/")
}

setwd(file_path20)

# 3 - Find All Files That Need Data Extracted -----------------------------


# List files in current working directory
list.files(path = ".")

# After identifying files containing PA waveguide data, use indexes to extract those
# folders to a new vector.
date_folders <- list.files(path = ".")[c(2:6)]
date_folders

# Create vector of different method folders of interest
method_folders <- c("Particulate Reflectance Measurements",
                    "Particulate Transmission Measurements")

# Create a dataframe that is a combination of the vectors
df <- expand.grid(file_path20, date_folders, method_folders)

# Create a new data frame that contains one column with the full path name
df2 <- df %>%
  mutate(full_path_name = file.path(Var1, Var2, Var3, fsep = "/")) %>%
  select(full_path_name)

# Create a new data frame of full path names of Flame files
df3 <- as.data.frame(lapply(df2, FUN = list.files, full.names = TRUE))

# 4 - Extract Details from File Names -------------------------------------


# Extract information from file name and get values for relevant columns
metadata_df <- df3 %>% 
  mutate(file = full_path_name,
         sample.date = as.Date(str_extract(full_path_name,
                                           regex("[:punct:][:digit:]+[:punct:][:digit:]+[:punct:](202[:digit:]{1}|21|22)[:punct:]")),
                               format = case_when( str_detect(full_path_name, regex("[:punct:][:digit:]+[:punct:][:digit:]+[:punct:]202[:digit:]{1}[:punct:]")) ~ "_%m_%d_%Y",
                                                   str_detect(full_path_name, regex("[:punct:][:digit:]+[:punct:][:digit:]+[:punct:](21|22)[:punct:]")) ~ "_%m_%d_%y")),
         station = case_when(str_detect(full_path_name, "_M15_") ~ "M15",
                             str_detect(full_path_name, "_M45_") ~ "M45",
                             str_detect(full_path_name, "_M110_") ~ "M110",
                             str_detect(full_path_name, "_SB02_") ~ "SB02",
                             str_detect(full_path_name, "_SB10_") ~ "SB10",
                             str_detect(full_path_name, "(?i)_FB_|_FOR_") ~ "FOR",
                             str_detect(full_path_name, "(?i)_EOR_") ~ "EOR"),
         depth.m = 2,
         spec.method = case_when(str_detect(full_path_name, "_R_") ~ "reflectance",
                                 str_detect(full_path_name, "_T_") ~ "transmission"),
         raw.or.bleached = case_when(str_detect(full_path_name, "(?i)_(r|R)aw_") ~ "raw",
                                     str_detect(full_path_name, "(?i)_(b|B)leached_") ~ "bleached"),
         fraction = case_when(str_detect(full_path_name, "_Whole_") ~ "Whole",
                              str_detect(full_path_name, "_53_") ~ "<53",
                              str_detect(full_path_name, "_20_") ~ "<20",
                              str_detect(full_path_name, "_(10|10\\.0)_") ~ "<10",
                              str_detect(full_path_name, "_(5|5\\.0)_") ~ "<5",
                              str_detect(full_path_name, "_(2|2\\.0)_") ~ "<2",
                              str_detect(full_path_name, "_(12|1\\.2)_") ~ "<1.2",
                              str_detect(full_path_name, "(?i)_FB_|_FOR_|_EOR_") ~ "blank"),
         replicate = case_when(str_detect(full_path_name, "_A_") ~ "A",
                               str_detect(full_path_name, "_B_") ~ "B",
                               str_detect(full_path_name, "_C_") ~ "C"),
         path.length.nominal = 10) %>%
  select(-full_path_name)

metadata_df

# 5 - Obtain Depth and Volume Filtered Info From Google Sheets ------------


inventory_tbl <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1uB1jW_x6XcULsd6NkRogOMB1Aon-lBY7tV0xxwNqd0I/edit?usp=sharing"))

# Perform left join
metadata_df2 <- metadata_df %>%
  merge(y = inventory_tbl,
        by.x = c("sample.date", "fraction", "station"),
        by.y = c("date.collected", "size.fraction", "site"),
        all.x = TRUE)

write.csv(metadata_df2, "R/app/data/metadata20_df.csv", row.names = FALSE)