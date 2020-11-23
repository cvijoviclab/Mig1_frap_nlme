library(tidyverse)
library(stringr)
library(readxl)
library(ggthemes)


# Processing of the frap-data (which is in xlsx and .txt files) into 
# a single tidy data-set with columns:
#     time, time-point for experiment
#     BG, background intensity 
#     ROI_raw, ROI-intensity where BG has not been subtracted 
#     REF_raw, REF-intensity where BG has not been subtracted 
#     id, individual id 
#     exp_tag, experimental tag, e.g glocse_2 = 2 % glucose 
#     ROI, ROI-intensity with background intensity removed 
#     REF, REF-intensity with background intensity removed 
#     intensity, calculated intensity which the exponential 
#       models are fitted against 
# The output data-file is stored in intermediate folder 


# ----------------------------------------------------------------------------
# Functions 
# ----------------------------------------------------------------------------


# Function ensuring that the correct columns is named BG, REF and ROI. 
# The measurment with smalles mean is backgroudn. 
# The measurment with largest mean is ROI  
# The remaning is REF
# Args:
#     tibble where the measurment columns are named col1, col2, col3
# Returns:
#    tibble where the measurment columns now are named bg, ref and roi correctly
name_cols <- function(data)
{
  # Ensure correct columns 
  data_vals <- data %>%
    select(col1, col2, col3)
  col_means <- sapply(data_vals, mean)
  col_var <- sapply(data_vals, var)
  
  i_bg <- as.integer(which.min(col_means))
  i_roi <- as.integer(which.max(col_means))
  i_ref <- setdiff(c(1, 2, 3), c(i_bg, i_roi))
  # Note plus 1 to match with data-tibble format 
  i_vec <- c(i_bg, i_ref, i_roi) + 1
  colnames(data)[i_vec] <- c("BG", "REF_raw", "ROI_raw")
  
  return(data)
}


# Function that takes the raw ROI, REF values and convert them to 
# ROI - BG and REF - BG, and given these calculate the intensity 
# for each individual. When calculating the intensity (ROI * REF)
# the intensity is normalised using the average of the first 
# ten data-points for ROI and REF. 
# Args:
#     data, a tibble with columns id, ROI_raw and REF_raw and also exp_tag
# Returns:
#     tibble where ROI, REF and intensity has been added to the original tibble 
#       for each individual 
calc_intensity <- function(data)
{
  id_list <- unique(data$id)
  data_new_list <- lapply(id_list, function(i){
    # Calculate intensity for an individual 
    data_ind <- data %>%
      filter(id == i) 
    mean_bg <- mean(data_ind$BG)
    
    # Add ROI and REF with background removed 
    data_ind <- data_ind %>%
      mutate(ROI = ROI_raw - mean_bg) %>%
      mutate(REF = REF_raw - mean_bg)
    
    # Extract 10 first values of ROI and REF 
    data_ind_ten <- data_ind[1:10, ]
    mean_roi_ten <- mean(data_ind_ten$ROI)
    mean_ref_ten <- mean(data_ind_ten$REF)
    
    data_ind <- data_ind %>%
      mutate(intensity = (ROI * REF) /(mean_roi_ten*mean_ref_ten))
    
    # Remove first 10 data-points and rescale time 
    data_ind <- data_ind[-(1:10), ]
    t_min <- data_ind$time[1]
    data_ind <- data_ind %>%
      mutate(time = time - t_min)
    
    return(data_ind)}) 
  data_new <- do.call(rbind, data_new_list)
  
  return(data_new)
}


# ----------------------------------------------------------------------------
# Processing the data into tidy-format  
# ----------------------------------------------------------------------------


# Regex for getting which individual data-set we are looking at 
pattern1 <- "([:digit:]+).xlsx$"
pattern2 <- "([:digit:]+).txt$"

# Column-name for each data-file
col_name_gen <- c("time", "col1", "col2", "col3")

## Process 2% glucose 
dir_data <- "../../Data/Glucose_2/Files_for_import/"
exp_tag <- "glucose_2"
files <- list.files(dir_data)
data_glc2 <- tibble()

# Loop through all the files 
for(i in 1:length(files)){
  file_read <- str_c(dir_data, files[i])
  id <- str_match(file_read, pattern1)[2]
  data <- read_excel(file_read)
  # Give generic column-names, which column is 
  # decided given the entire data 
  colnames(data) <- col_name_gen
  data <- data %>%
    mutate(id = id) %>%
    mutate(exp_tag = exp_tag)
  
  data_glc2 <- data_glc2 %>%
    bind_rows(data)
}

data_glc2 <- name_cols(data_glc2)


## Process 0.05% glucose 
dir_data <- "../../Data/Glucose_005/Files_for_import/"
exp_tag <- "glucose_005"
files <- list.files(dir_data)
data_glc005 <- tibble()

# Loop through all the files
for(i in 1:length(files)){
  file_read <- str_c(dir_data, files[i])
  id <- str_match(file_read, pattern2)[2]
  data <- read_tsv(file_read, col_types = cols()) %>%
    drop_na() %>%
    select(-Time)
  colnames(data) <- col_name_gen
  data <- data %>%
    mutate(id = id) %>%
    mutate(exp_tag = exp_tag)
  
  data_glc005 <- data_glc005 %>%
    bind_rows(data)
}

data_glc005 <- name_cols(data_glc005)


## Process glycerol 2% 
dir_data <- "../../Data/Glycerol_2/Files_for_import/"
exp_tag <- "glycerol_2"
files <- list.files(dir_data)
data_gly2 <- tibble()

# Loop through all the files
for(i in 1:length(files)){
  file_read <- str_c(dir_data, files[i])
  id <- str_match(file_read, pattern1)[2]
  data <- read_excel(file_read) 
  colnames(data) <- col_name_gen
  data <- data %>%
    mutate(id = id) %>%
    mutate(exp_tag = exp_tag)
  
  data_gly2 <- data_gly2 %>%
    bind_rows(data)
}

data_gly2 <- name_cols(data_gly2)


## Process NC 
dir_data <- "../../Data/NC/Files_for_import/"
exp_tag <- "NC"
files <- list.files(dir_data)
data_nc <- tibble()

# Loop through all the files
for(i in 1:length(files)){
  file_read <- str_c(dir_data, files[i])
  id <- str_match(file_read, pattern2)[2]
  data <- read_tsv(file_read, col_types = cols()) %>%
    drop_na() %>%
    select(-Time)
  colnames(data) <- col_name_gen
  data <- data %>%
    mutate(id = id) %>%
    mutate(exp_tag = exp_tag)
  
  data_nc <- data_nc %>%
    bind_rows(data)
}

data_nc <- name_cols(data_nc)


## Aggregate into one-large data-file with tidy-data 
# Ensure that each cell gets a unique id 
n_ind_glc2 <- length(unique(data_glc2$id))
n_ind_glc005 <- length(unique(data_glc005$id))
n_ind_gly2 <- length(unique(data_gly2$id))
data_glc2 <- data_glc2 %>%
  mutate(id = as.numeric(id)) %>%
  select(time, BG, ROI_raw, REF_raw, id, exp_tag)
data_glc005 <- data_glc005 %>%
  mutate(id = as.numeric(id)) %>%
  mutate(id = id + n_ind_glc2) %>%
  select(time, BG, ROI_raw, REF_raw, id, exp_tag)
data_gly2 <- data_gly2 %>%
  mutate(id = as.numeric(id)) %>%
  mutate(id = id + n_ind_glc2 + n_ind_glc005) %>%
  select(time, BG, ROI_raw, REF_raw, id, exp_tag)
data_nc <- data_nc %>% 
  mutate(id = as.numeric(id)) %>%
  mutate(id = id + n_ind_glc2 + n_ind_glc005 + n_ind_gly2) %>%
  select(time, BG, ROI_raw, REF_raw, id, exp_tag)

data_tot <- data_glc2 %>%
  bind_rows(data_glc005) %>%
  bind_rows(data_gly2) %>%
  bind_rows(data_nc) %>%
  mutate(id = as.factor(id)) %>%
  mutate(exp_tag = as.factor(exp_tag))

# Calculate intensity 
data_tot <- calc_intensity(data_tot)

# Ensure that directory to save result in exists 
dir_save <- "../../Intermediate/Data_files/"
if(!dir.exists(dir_save)) dir.create(dir_save, recursive = T)
path_save <- str_c(dir_save, "Data_tidy.csv")
write_csv(data_tot, path_save)

