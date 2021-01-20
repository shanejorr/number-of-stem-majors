#####################################################################################
#
# Models to help understand the relationship between demographic factors and stem majors
#
#####################################################################################

library(tidyverse)
library(rstanarm)
library(bayesplot)
library(glue)

options(mc.cores = parallel::detectCores())

source('recode_values.R')
source('functions.R')

df <- read_csv('data/raw_data.csv',
               col_types = cols(.default = col_character())) %>%
  # recode values with descriptive labels
  recode_demo_vars() %>%
  combine_demo_vars()

# all HS_GPA that are missing are first generate
# all the first geneartion students have missing hs_gpa

# model demographic factors ---------------------------------

# must create different models for HS GPA and all other demgoraphic variables
# because all first gen have missing high school gpa

# all demographics except HS GPA
mod_demo <- stan_glmer(STEM_bern ~ (1 | YEAR) + (1 | DISTANCE) + (1 | RACETHN_recode) + GENDER + FIRST_GEN, 
                      data = df,
                      family = binomial(link = "logit"), 
                      prior = student_t(df = 7, location = 0, scale = 1.5), 
                      iter = 3000,
                      seed = 126, 
                      adapt_delta = 0.99)

# calculate HS GPA by creating model with first gen removed
mod_hs_gpa <- stan_glmer(STEM_bern ~ (1 | YEAR) + (1 | DISTANCE) + (1 | RACETHN_recode) + (1 | HS_GPA) + GENDER, 
                           data = df,
                           family = binomial(link = "logit"), 
                           prior = student_t(df = 7, location = 0, scale = 1.5), 
                           iter = 3000,
                           seed = 126, 
                           adapt_delta = 0.99)

# plot demographic factors ----------------------------------

main_x_axis_lab <- "Positive values signify positive relationship with majoring in STEM"

demo_regex <- c(".*Intercept.*YEAR.*", ".*Intercept.*RACE.*", ".*Intercept.*DISTANCE.*", 
                "GENDERMale", "FIRST_GENYes", ".*Intercept.*HS_GPA*")

x_axis_labs <- c(main_x_axis_lab, main_x_axis_lab, main_x_axis_lab, 
                 "Positive values signify more males than females in STEM majors",
                 "Positive values signify more first gen than non-first gen in STEM majors",
                 main_x_axis_lab)

y_labels <- list(
  year = c("2012", "2013", "2104", "2015"),
  race = c("Asian", "Black or African American", "Hispanic / Latino",
           "Other", "Two or More Races", "White, non-Hispanic"),
  distance = c("5 or less", "6 - 10", "11 - 50", "51 - 100", "101 - 500", "Over 500"),
  gender = "Male / Female",
  first_gen = "First Gen / Not First Gen",
  hs_gpa = c('A or A+', 'A-', 'B+', 'B', 'B-')
)

mods <- list(mod_demo,mod_demo,mod_demo,mod_demo,mod_demo,mod_hs_gpa)

custom_scheme <-  c("#ffebcc", "#ffcc80",
                    "#ffad33", "#e68a00",
                    "#995c00", "#663d00")
color_scheme_set(custom_scheme)

demo_plot_params <- list(
  mod = mods,
  col_regex = demo_regex,
  y_labels = y_labels,
  x_axis = x_axis_labs
)

# create plots for all demographic groups
demo_plots <- pmap(demo_plot_params, demo_coef_plot)

# save all demographic plots ---------------------

# demographic category for filenames
demo_plot_file_name <- c('year', 'race', 'distance', 'gender', 'first_gen', 'hs_gpa')

# filenames for images
file_names <- glue("plot_coef_demo_{demo_plot_file_name}.png")

# list of parameters to iterate through with ggsave
plot_save_params <- list(
  filename = file_names,
  plot = demo_plots
)

pwalk(plot_save_params, ggsave, path = 'modeling/demo_coef_plots', dpi = 'retina',
      width=unit(7, "in"), height=unit(5, "in"))