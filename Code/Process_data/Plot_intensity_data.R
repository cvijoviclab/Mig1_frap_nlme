library(tidyverse)
library(stringr)
library(ggthemes)


# Plotting the data for each experimental condition (glucose 2 and 0.05%, glycerol 
# 2% and NC) for all individuals. The created svg-plots are stored in result-folder. 


# General plotting parameters 
my_theme <- theme_tufte(base_size = 16) + theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"), 
                                                plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title=element_text(size=18))
my_colors <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 20)
BASE_HEIGHT <- 5
BASE_WIDTH <- 7.0


# --------------------------------------------------------------------------------------
# Plot the data 
# --------------------------------------------------------------------------------------


# Read and plot each data-case 
data <- read_csv("../../Intermediate/Data_files/Data_tidy.csv", 
                 col_types = cols(id = col_factor(), 
                                  exp_tag = col_factor()))

# Ensure that directory where to save results exist 
dir_save <- "../../Result/Data_set/"
if(!dir.exists(dir_save)) dir.create(dir_save, recursive = T)

# Plot all conditions 
exp_tags <- unique(data$exp_tag)
for(i in 1:length(exp_tags)){

  tag_plot <- as.character(exp_tags[i])
  data_plot <- data %>%
    filter(exp_tag == tag_plot)
  
  p <- ggplot(data_plot, aes(time, intensity, color = id)) + 
    geom_line() + 
    geom_rangeframe(color="black") + 
    labs(x = "Time [seconds]", y = "Intensity [A.U]", title = tag_plot) + 
    scale_color_manual(values = my_colors) + 
    my_theme + 
    theme(legend.position = "none")
  
  file_save <- str_c(dir_save, tag_plot, ".svg")
  ggsave(file_save, p, width = BASE_WIDTH, height = BASE_HEIGHT)
}
