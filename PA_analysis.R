#' R Code for Analyzing Particulate Absorption OceanView Data
#' Contains functions for reading raw text files from OceanView
#' and puts out graphs of data
#' 
#' Christine Kitchens
#' University of Michigan
#' Cooperative Institute for Great Lakes Research
#' chknight@umich.edu
#' October 2022


# 0 - Install Libraries ---------------------------------------------------

# install.packages("data.table")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("plotly")
# install.packages("reshape")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("tidyverse")


# 1 - Load Libraries ------------------------------------------------------

library("data.table")
library("janitor")
library("lubridate")
library("plotly")
library("reshape")
library("shiny")
library("shinythemes")
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


# 3 - Load in Metadata Table ----------------------------------------------

metadata_df <- read.csv(file = "R/app/data/metadata20_df.csv")

#' Read in all the data files and convert to a dataframe. Due to some files
#' missing header data, read in files with header = FALSE argument and then
#' elevate first row to column names after dataframe creation.
raw_data <- rbindlist(lapply(metadata_df$file,
                             fread,
                             check.names = TRUE,
                             header = FALSE,
                             colClasses = 'character',
                             na.strings = ""),
                      idcol = "file")

raw_data2 <- raw_data %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  filter(!is.na(v1))

#' Change id column to file path name, then merge raw_data2 with metadata_df
raw_data3 <- raw_data2 %>%
  mutate(x1 = metadata_df$file[x1]) %>%
  left_join(metadata_df, by = c('x1' = 'file')) %>%
  rename(file = x1,
         read.time = v1,
         unknown = v2)

#' Clean up column names
names(raw_data3) <- gsub(x = names(raw_data3), pattern = "\\_", replacement = "\\.")
names(raw_data3) <- gsub(x = names(raw_data3), pattern = "^x", replacement = "")

#' Melt data table
raw_data4 <- melt(raw_data3,
                  id = c('read.time', 'unknown', colnames(metadata_df)),
                  variable_name = "wavelength")

#' Write full data set to csv
write.csv(raw_data4,
          file = "R/app/data/full_dataset.csv",
          row.names = FALSE)

# 4 - Aggregate Raw Data --------------------------------------------------


#' Group raw data by file so all 10 reads are averaged as one record. Note that
#' the 'value' variable refers to the absorbance reading at 'variable' wavelength
averaged_data <- raw_data4 %>%
  mutate(value = as.numeric(value)) %>%
  group_by(file, wavelength, ymd_hms(read.time)) %>%
  summarise(mean_value =  mean(value),
            sd_value = sd(value),
            
            .groups = 'drop') %>%
  ungroup() %>%
  as.data.frame()

averaged_data2 <- averaged_data %>%
  left_join(metadata_df, by = c("file"))

#' Write averaged data set to csv
write.csv(averaged_data2,
          file = "R/app/data/averaged_dataset.csv",
          row.names = FALSE)
