#############################################################################
#
# This script creates charts and graphs looking at the difference in
# majors based on student demographic information
#
#############################################################################

library(tidyverse)
library(glue)

source('recode_values.R')
source('functions.R')

df <- read_csv('data/raw_data.csv',
               col_types = cols(.default = col_character())) %>%
  # recode values with descriptive labels
  recode_demo_vars()

# custom color mapping; make STEM black and all other majors gray
non_stem_color <- 'gray'

custom_colors <- c(
  "Social Science" = non_stem_color, 
  "Independent" = non_stem_color,
  "Arts" = non_stem_color,
  "Humanities" = non_stem_color,
  "Business" = non_stem_color,
  "STEM" = 'black'
)

# list to store plots to save them
eda_plots <- list()

# vecotr to store order of images for filenames
file_name_order <- c()

# integer for list location
i <- 1

# year ------------------

perc_plot_title <- 'Percentage of matriculating students'

eda_plots[[i]] <- demo_table(df, 'YEAR', 'MAJOR') %>%
  # create plot
  ggplot(aes(YEAR, perc, color = MAJOR, group = MAJOR)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .5)) +
    scale_colour_manual(values = custom_colors) +
    labs(title = 'Black line is STEM majors',
         x = 'Year of matriculation',
         y = perc_plot_title) +
    cf_plot_theme()

file_name_order <- c(file_name_order, 'year')
i <- i + 1

# gender ---------------------------

# create table of percentages
eda_plots[[i]] <- demo_table(df, 'MAJOR', 'GENDER') %>%
  # create plot
  point_plot(perc, MAJOR, GENDER, custom_colors, 'Percentage of STEM Majors by Gender', perc_plot_title, NULL, 3)

file_name_order <- c(file_name_order, 'gender')
i <- i + 1

# race ---------------------------

# create table of percentages
eda_plots[[i]] <- demo_table(df, 'MAJOR', 'RACETHN') %>%
  # create plot
  point_plot(perc, MAJOR, RACETHN, custom_colors, 'Percentage of STEM Majors by Race', perc_plot_title, NULL, 1) +
  theme(axis.text.y=element_blank())

file_name_order <- c(file_name_order, 'race')
i <- i + 1

# Distance -------------------------
no_dist_desc <- 'No distance information'

levels(df$DISTANCE) <- c(levels(df$DISTANCE), no_dist_desc)

# create table of percentages
eda_plots[[i]] <- df %>%
  replace_na(list(DISTANCE = no_dist_desc)) %>%
  demo_table('MAJOR', 'DISTANCE') %>%
  # create plot
  point_plot(perc, MAJOR, DISTANCE, custom_colors, 'Percentage of STEM Majors by Disance', 
             perc_plot_title, NULL, 1) +
  theme(axis.text.y=element_blank())

file_name_order <- c(file_name_order, 'distance')
i <- i + 1

# HS GPA ---------------------------------------
no_grade_desc <- 'No grade information'

levels(df$HS_GPA) <- c(levels(df$HS_GPA), no_grade_desc)

eda_plots[[i]] <- df %>%
  replace_na(list(HS_GPA = no_grade_desc)) %>%
  demo_table('MAJOR', 'HS_GPA') %>%
  # create plot
  point_plot(perc, MAJOR, HS_GPA, custom_colors, 'Percentage of STEM Majors by HS GPA', 
             perc_plot_title, NULL, 2) +
  theme(axis.text.y=element_blank())

file_name_order <- c(file_name_order, 'hs_gpa')
i <- i + 1

# first generation ---------------------------------------

eda_plots[[i]] <- df %>%
  demo_table('MAJOR', 'FIRST_GEN') %>%
  drop_na(FIRST_GEN) %>%
  # create plot
  point_plot(perc, MAJOR, FIRST_GEN, custom_colors, 'Percentage of STEM Majors by First Generation', 
             perc_plot_title, NULL, 3)

file_name_order <- c(file_name_order, 'first_gen')
# save all images --------------------

# filenames for images
file_names <- glue("plot_eda_{file_name_order}.png")

# list of parameters to iterate through with ggsave
plot_save_params <- list(
  filename = file_names,
  plot = eda_plots
)

pwalk(plot_save_params, ggsave, path = 'eda/plots_demo', dpi = 'retina',
      width=unit(7, "in"), height=unit(5, "in"))
