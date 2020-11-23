library(tidyverse)
library(stringr)


# Process the Glucose 2% and 0.05%, Glycerol 2% and NC data into monolix-format. 
# That is, each experimental condition is transformed to a tibble with the 
# columns time (time), id (individual id) and observation (intensity in the original 
# data set). For each condition a file is generated, and stored in the intermediate 
# folder under Monolix_data


# ---------------------------------------------------------------------------------
# Process the experiments into monolix format 
# ---------------------------------------------------------------------------------


# Read and plot each data-case 
data <- read_csv("../../Intermediate/Data_files/Data_tidy.csv", 
                 col_types = cols(id = col_factor(), 
                                  exp_tag = col_factor()))

dir_save <- "../../Intermediate/Monolix_data/"
if(!dir.exists(dir_save)) dir.create(dir_save)

# Write the data to file 
exp_tags <- unique(data$exp_tag)
for(i in 1:length(exp_tags)){
  tag_save <- as.character(exp_tags[i])
  data_save <- data %>%
    filter(exp_tag == tag_save) %>%
    rename("observation" = "intensity") %>%
    select(time, observation, id)
  
  file_save <- str_c(dir_save, tag_save, ".csv")
  write_csv(data_save, file_save)
}
