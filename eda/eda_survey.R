#############################################################################
#
# This script creates charts and graphs looking at the difference in
# majors based on survey results
#
#############################################################################

library(tidyverse)
library(rstanarm)
library(glue)

source('recode_values.R')
source('functions.R')

custom_colors <- c(
  "Non-STEM" = '#9E7E38',
  "STEM" = 'black'
)

df <- read_csv('data/raw_data.csv',
               col_types = cols(.default = col_character())) %>%
  # recode values with descriptive labels
  recode_demo_vars()

# list to store plots for saving
survey_plot_list <- list()

i <- 1

# vector storing order of filenames for saving plots
file_name_order <- c()

# ACT questions ----------------------

act_question_title <- 'In the past year, how often did you:'
act_subtitle <- "From 'Not At All' (top) to 'Frequently' (bottom)"

act_table <- survey_table(df, ACT01:ACT13)
act_table$Question <- recode_act_ques(act_table$Question)

survey_plot_list[[i]] <- survey_plot(act_table, act_question_title, act_subtitle)

file_name_order <- c(file_name_order, 'act')

i <- i + 1

# RATE questions -----------------------

rate_title <- 'How would you rate yourself in the following areas:'
rate_subtitle <- "From 'A Major Weakness' (top) to 'A Major Strength' (bottom)"

rate_table <- survey_table(df, RATE1:RATE7)
rate_table$Question <- recode_rate_ques(rate_table$Question)

survey_plot_list[[i]] <- survey_plot(rate_table, rate_title, rate_subtitle)

file_name_order <- c(file_name_order, 'rate')

i <- i + 1

# AGREE questions -----------------------

agree_title <- 'Please rate the extent to which you agree or disagree with the following items:'
agree_subtitle <- "From 'Strongly Disagree' (top) to 'Strongly Agree' (bottom)"

agree_table <- survey_table(df, AGREE_01:AGREE_09)
agree_table$Question <- recode_agree_ques(agree_table$Question)

survey_plot_list[[i]] <- survey_plot(agree_table, agree_title, agree_subtitle)

file_name_order <- c(file_name_order, 'agree')

survey_plot_list[[3]]

# save plots ---------------------------------

# filenames for images
file_names <- glue("plot_survey_{file_name_order}.png")

# list of parameters to iterate through with ggsave
plot_save_params <- list(
  filename = file_names,
  plot = survey_plot_list
)

pwalk(plot_save_params, ggsave, path = 'eda/plots_survey', dpi = 'retina',
      width=unit(10, "in"), height=unit(6, "in"))