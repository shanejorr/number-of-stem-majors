##############################################################
#
# Model and variable selection
#
#################################################################

library(MASS)
library(miscTools)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(glue)
library(psych)

options(mc.cores = parallel::detectCores())

source('recode_values.R')
source('functions.R')

# import and clean data -------------------------------

df <- read_csv('data/raw_data.csv',
               col_types = cols(.default = col_character())) %>%
  # recode values with descriptive labels
  recode_demo_vars()   %>%
  # make agree survey items 1 if in disagree categories, 0 if in agree categories
  mutate(across(.cols = contains('AGREE'), .fns = ~ifelse(. >= 3, 1, 0), .names = "dichot_{.col}")) %>%
  # add factor scores for dim reduction
  bind_cols(factor_scores(., 'AGREE')) %>%
  bind_cols(factor_scores(., 'RATE')) %>%
  bind_cols(factor_scores(., 'ACT'))


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

# calculate median for each survey question

df <- df %>%
  mutate(ACT_median = combine_survey_questions(., 'ACT'),
         RATE_median = combine_survey_questions(., 'RATE'),
         AGREE_median = combine_survey_questions(., 'AGREE')) %>%
  # make them characters so that they are categorical in model
  mutate(across(contains("_median"), as.character))

# create models -----------------
df$STEM_bern <- as.integer(df$STEM_bern)

# list of formulas for models we want to compare
pred_cols <- list(
  
  # model with all questions for AGREE
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + AGREE_01 + AGREE_02 + AGREE_03 + AGREE_04 + AGREE_05 + AGREE_06 + AGREE_07 + AGREE_08 + AGREE_09"),
  
  # model where agree is transformed to dichotomous -- bad
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + dichot_AGREE_01 + dichot_AGREE_02 + dichot_AGREE_03 +  + dichot_AGREE_04 + dichot_AGREE_05 + dichot_AGREE_06 + dichot_AGREE_07 + dichot_AGREE_08 + dichot_AGREE_09"),
  
  # model with factor scores for all surveys
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + MR1_AGREE + MR2_AGREE + MR3_AGREE + MR1_RATE + MR1_ACT"),
  
  # model with factor scores for AGREE
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + MR1_AGREE + MR2_AGREE + MR3_AGREE"),
  
  # model with median values for all survey responses
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + AGREE_median")
  
)

# priors for the number of relevant variables for each model
prior_rel_vars <- c(7, 7, 7, 7, 7)

# number of predictor columns
num_predictors <- c(61, 24, 21, 19, 21)

mod_params <- list(
  mod_form = pred_cols, 
  prior_relevant_vars = prior_rel_vars, 
  num_predictor_cols = num_predictors
)

# iterate through each model and create
mods <- pmap(mod_params, mod_varsel, df = df, prior_form = 'horseshoe')

write_rds(mods, 'modeling/mods_var_sel.rds')

# compare all models ---------------

mod_loo1 <- map(mods, waic)

loo_compare(x = list(a = mod_loo1[[1]], b = mod_loo1[[2]], c = mod_loo1[[3]],
                     d = mod_loo1[[4]], e = mod_loo1[[5]]))

##############################

# based on the preceeding, tweak models and recompare

# list of formulas for models we want to compare
pred_cols <- list(
  
  # model with all questions
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + ACT01 + ACT02 + ACT03 + ACT04 + ACT05 + ACT06 + ACT07 + ACT08 + ACT09 + ACT10 + ACT11 + ACT12 + ACT13 + RATE1 + RATE2 + RATE3 + RATE4 + RATE5 + RATE6 + RATE7 + AGREE_01 + AGREE_02 + AGREE_03 + AGREE_04 + AGREE_05 + AGREE_06 + AGREE_07 + AGREE_08 + AGREE_09"),
  
  # model with limited questions for AGREE
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN + AGREE_01 + AGREE_02 + AGREE_03 + AGREE_05 + AGREE_06 + AGREE_07 + AGREE_08"),
  
  # models with just demographics
  c("STEM_bern ~ (1 | YEAR) + (1 | RACETHN_recode) + GENDER + HS_GPA + FIRST_GEN")
  
)

# priors for the number of relevant variables for each model
prior_rel_vars <- c(15, 11, 6)

# number of predictor columns
num_predictors <- c(115, 51, 16)

mod_params <- list(
  mod_form = pred_cols, 
  prior_relevant_vars = prior_rel_vars, 
  num_predictor_cols = num_predictors
)

# iterate through each model and create
mods2 <- pmap(mod_params, mod_varsel, df = df, prior_form = 'horseshoe')

write_rds(mods2, 'modeling/mods_var_sel2.rds')

mod_loo2 <- map(mods2, waic)

compare_list <- list(a1 = mod_loo1[[1]], b1 = mod_loo1[[2]], c1 = mod_loo1[[3]], d1 = mod_loo1[[4]], e1 = mod_loo1[[5]],
                     a2 = mod_loo2[[1]], b2 = mod_loo2[[2]], c2 = mod_loo2[[3]])

write_rds(compare_list, 'modeling/model_waics.rds')

loo_compare(x = compare_list)

# model b2 is best
