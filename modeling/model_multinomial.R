########################################################
#
# multinomial model predicting all majors
#
#########################################################

library(MASS)
library(glue)
library(psych)
library(brms)
library(rstanarm)
library(tidybayes)
library(tidyverse)
library(ggridges)

options(mc.cores = parallel::detectCores())

source('recode_values.R')
source('functions.R')

# import and clean data -------------------------------

df <- read_csv('data/raw_data.csv',
               col_types = cols(.default = col_character())) %>%
  # recode values with descriptive labels
  recode_demo_vars()   %>%
  # make agree survey items 1 if in disagree categories, 0 if in agree categories
  mutate(across(.cols = contains('AGREE'), .fns = ~ifelse(. >= 3, 1, 0), .names = "dichot_{.col}"))


# find column with missing values and allocate most common value for variable and model selection ------------

n_rows <- nrow(df)

# columns with missing values
missing_values <- df %>%
  summarize_all(~sum(is.na(.))) %>%
  t() %>%
  as.data.frame() %>%
  filter(V1 != 0) %>%
  rownames_to_column(var = 'col_name') %>%
  mutate(perc = V1 / n_rows) %>%
  rename(missing = V1)

# tables with breakdown of values for the two columns with missing values ---------
table_missing <- map(missing_values$col_name, function(x) {
  df %>%
    # most hs grade values are 
    group_by_at(x) %>%
    count() %>%
    mutate(perc = n / n_rows)
})

# impute missing values ---------------

dist_max <- table_missing[[1]][table_missing[[1]]$n == max(table_missing[[1]]$n), 1][1,1]
hs_grade_max <- table_missing[[2]][table_missing[[2]]$n == max(table_missing[[2]]$n), 1]

# combine categories
df <- df %>%
  recode_demo_vars() %>%
  combine_demo_vars()

df$HS_GPA <- impute_missing(df, 'HS_GPA')
df$DISTANCE <- impute_missing(df, 'DISTANCE')

# combine arts and independent since they do not have many people

# majors to combine
combine_majors <- c("Independent", "Arts")

df$MAJOR <- ifelse(df$MAJOR %in% combine_majors, 'Arts / Independent', df$MAJOR)

# create models -----------------

#########
mod_multi <-brm(
  formula = MAJOR ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + AGREE_01 + AGREE_02 + AGREE_03 + AGREE_05 + AGREE_06 + AGREE_07 + AGREE_08,
  family = categorical(link = logit),
  prior = set_prior(horseshoe(df = 3, par_ratio = 0.25)),
  data = df, 
  iter = 2000, warmup = 1000, cores = 4, chains = 4,
  seed = 22
)

# write_rds(mod_multi, 'modeling/mod_multi.rds')

# predict from model --------------------------

mod_multi <- read_rds('modeling/mod_multi.rds')

# dataset for predictions ---------------------------------
# sample equally from gender and year

num_students <- 1300

pred_sample <- df %>%
  group_by(YEAR) %>%
  sample_n(size = num_students/4, replace = F) %>%
  ungroup() %>%
  mutate(year = '2016')

# get total number of stem majors ---------------------------------
n_draws <- 1000

preds <- rstanarm::posterior_predict(mod_multi, newdata = pred_sample, draws = n_draws, seed = 295)

preds_test <- preds %>%
  t() %>%
  as.data.frame()

# list to contain all vectors of counts
num_pred_majors <- list(
  arts = c(),
  bus = c(),
  hum = c(),
  soc = c(),
  stem = c()
)

for (col_name in colnames(preds_test)) {
  
  # get count for each major for each draw from posterior
  count_table <- table(preds_test[[col_name]])
  
  # place the count for each major in a vector
  num_pred_majors$arts <- c(num_pred_majors$arts, unname(count_table[(names(count_table)) == '1']))
  num_pred_majors$bus <- c(num_pred_majors$bus, unname(count_table[(names(count_table)) == '2']))
  num_pred_majors$hum <- c(num_pred_majors$hum, unname(count_table[(names(count_table)) == '3']))
  num_pred_majors$soc <- c(num_pred_majors$soc, unname(count_table[(names(count_table)) == '4']))
  num_pred_majors$stem <- c(num_pred_majors$stem, unname(count_table[(names(count_table)) == '5']))
  
}

# make counts a dataframe in long form (each row is a draw / major)

num_pred_majors <- num_pred_majors %>%
  as.data.frame() %>%
  mutate(.draw = row_number())

# recode major names
major_recode <- c(
  stem = 'STEM',
  soc = "Social Science",
  arts = 'Arts / Independent',
  hum = "Humanities",
  bus = "Business"
)

# make long form for plotting
num_pred_majors_long <- num_pred_majors %>%
  pivot_longer(cols = -.draw, names_to = 'major', values_to = 'total') %>%
  mutate(major = recode(major, !!!major_recode))

# plot number of majors ----------------------

plot_quantiles <- c(0.05, .5, 0.95)

plot_titles <- 'Estimated Number of Students by Major - 2020'
plot_x_title <- 'Number of students'

plot_total_major <- ggplot(num_pred_majors_long, aes(x = total, y = major, fill = major)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = plot_quantiles, 
                      alpha = 0.6, fill = '#9E7E38') +
  #scale_fill_brewer(type = 'qual', palette = 'Set3') +
  labs(title = plot_titles,
       y = 'Major',
       x = plot_x_title) +
  cf_plot_theme()

ggsave(filename = 'plot_multi_total.png', plot = plot_total_major, path = 'modeling', 
       dpi = 'retina', width=unit(8, "in"), height=unit(6, "in"))

# numbers from plots ------------------------

n <- nrow(num_pred_majors)

# probability stem will have the most majors
num_pred_majors %>%
  mutate(stem_max = ifelse(stem > arts & stem > bus & stem > hum & stem > soc, 1, 0)) %>%
  summarize(stem_largest = sum(stem_max) / n)

# 0.6625

# hdi of STEM and humanities
num_pred_majors_long %>%
  filter(major %in% c('STEM', 'Humanities')) %>%
  group_by(.draw) %>%
  summarize(total = sum(total)) %>%
  median_hdi(total)
# 272    257    287   0.95 median hdi 

# probability that STEM and business have combined enrollment > 330
thresh <- 330

num_pred_majors_long %>%
  filter(major %in% c('STEM', 'Business')) %>%
  group_by(.draw) %>%
  summarize(total = sum(total)) %>%
  mutate(greater_threshold = ifelse(total > !!thresh, 1, 0)) %>%
  summarize(greater_perc = sum(greater_threshold) / n)

# 0.144
