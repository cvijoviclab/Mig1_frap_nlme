library(tidyverse)
library(stringr)
library(ggthemes)

# Append new observations to the Data_tidy file


# Read in files
data_tot <- read_csv("../../Intermediate/Data_files/Data_tidy.csv", 
                 col_types = cols(id = col_factor(), 
                                  exp_tag = col_factor()))
data_new <- read_csv("../../Intermediate/Data_files/Data_tidy_201125.csv", 
                 col_types = cols(id = col_factor(), 
                                  exp_tag = col_factor()))

## Aggregate into one-large data-file with tidy-data 
# Ensure that each cell gets a unique id 
n_ind_data_tot <- length(unique(data_tot$id))
n_ind_data_new <- length(unique(data_new$id))

data_old <- data_tot %>% 
  mutate(id = as.numeric(id)) %>%
  select(time, BG, ROI_raw, REF_raw, id, exp_tag, ROI, REF, intensity)
data_new <- data_new %>% 
  mutate(id = as.numeric(id)) %>%
  mutate(id = id + n_ind_data_tot) %>%
  select(time, BG, ROI_raw, REF_raw, id, exp_tag, ROI, REF, intensity)

data_tot <- data_old %>%
  bind_rows(data_new) %>%
  mutate(id = as.factor(id)) %>%
  mutate(exp_tag = as.factor(exp_tag))

# Ensure that directory to save result in exists 
dir_save <- "../../Intermediate/Data_files/"
if(!dir.exists(dir_save)) dir.create(dir_save, recursive = T)
path_save <- str_c(dir_save, "Data_tidy.csv")
write_csv(data_tot, path_save)
