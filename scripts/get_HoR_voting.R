#### Preamble ####
# Purpose: This file gets Australian HoR voting from Adam Carr's Psephos
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 21 December 2021
# Prerequisites: -
# To do: -


#### Set up workspace ####
# Load libraries
library(tidyverse)
library(heapsofpapers)
# update.packages()


#### Load the lists of URLs to scrape and file names to save the PDF as ####
data_to_scrape <- read_csv("inputs/scraping/URLs_for_adam_carr.csv")


#### Invoke the function that will visit address_to_visit and save to save_name files ####
heapsofpapers::get_and_save(
  data = data_to_scrape,
  links = "URL",
  save_names = "file_name",
  dir = "inputs/voting_data",
  delay = 10,
  dupe_strategy = "ignore"
)

#### Check what's missing ####
hmmm <- 
heapsofpapers::check_for_existence(data = data_to_scrape,
                                   save_names = "file_name",
                                   dir = "inputs/voting_data")
hmmm %>% 
  filter(got_this_already != 1)

