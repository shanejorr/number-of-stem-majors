##############################################################
#
# Predict the number of stem majors
#
#################################################################

library(MASS)
library(rstanarm)
library(glue)
library(ggridges)
library(patchwork)
library(tidybayes)
library(tidyverse)

options(mc.cores = parallel::detectCores())

source('recode_values.R')
source('functions.R')

# clean data ------------------------------

df <- read_csv('data/raw_data.csv',
               col_types = cols(.default = col_character())) %>%
  # recode values with descriptive labels
  recode_demo_vars() %>%
  combine_demo_vars()

# impute missing values
df$HS_GPA <- impute_missing(df, 'HS_GPA')
df$DISTANCE <- impute_missing(df, 'DISTANCE')

# read in model we're going to use -----------------------

mod <- read_rds('modeling/mods_var_sel2.rds')[[2]]

# dataset for predictions ---------------------------------
# sample equally from gender and year

num_students <- 1300

set.seed(8227)
pred_sample <- df %>%
  group_by(YEAR) %>%
  sample_n(size = num_students/4, replace = T) %>%
  ungroup() %>%
  mutate(year = '2016')

table(pred_sample$STEM)

# get total number of stem majors ---------------------------------
preds <- posterior_predict(mod, newdata = pred_sample, draws = 1000, seed = 295)

# dataframe with the number and percentage of stem students
stem_preds <- tibble(
  num_stem = rowSums(preds),
  perc_stem = num_stem / !!num_students
)

plot_quantiles <- c(0.05, .25, .5, .75, 0.95)

plot_titles <- 'Estimated Number of Matriculating STEM Majors - 2020'
plot_x_title <- 'Number of STEM students'

stem_quantiles <- quantile(stem_preds$num_stem, probs = plot_quantiles) %>%
  round(0)

perc_quantiles <- quantile(stem_preds$perc_stem, probs = plot_quantiles)

labels_stem <- c('5%', '25%', '50%', '75%', '95%')

text_labels <- glue("{labels_stem}: {stem_quantiles}")

# plot total number of students w/ 50% and 95% credible intervals
plot_num_students <- ggplot(stem_preds, aes(x = num_stem)) +
  geom_density(fill = '#9E7E38', alpha = .3) +
  # most likely value
  geom_vline(xintercept = stem_quantiles[[3]], size = 1) +
  # 50% interval
  geom_vline(xintercept = stem_quantiles[c(2, 4)], linetype = 2) +
  # 95% ci
  geom_vline(xintercept = stem_quantiles[c(1, 5)], linetype = 3) +
  geom_text(data = data.frame(stem_quantiles = stem_quantiles,
                              stem_labels = text_labels),
            aes(x = stem_quantiles + 1, y = .04, label = stem_labels),
            angle = 270) +
  labs(title = plot_titles,
       x = plot_x_title,
       y = NULL) +
  cf_plot_theme() +
  theme(axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
plot_num_students

ggsave(plot = plot_num_students, 
       filename = 'post_pred_num_stem.png', path = 'modeling/post_pred_plots',
      width=unit(6, "in"), height=unit(4, "in"), dpi = 'retina')

# numbers of total stem students for presentation ------------------------

# most likely value
stem_quantiles[[3]]

# only 5% chance there will be more than this amount
stem_quantiles[[5]]

# the probability that there will be more than 175 is
total_over_175 <- stem_preds %>%
  mutate(over175 = ifelse(num_stem > 455, 1, 0)) %>%
  summarize(num_over_175 = sum(over175))

total_over_175 / nrow(stem_preds)

# breakdown by demographic ------------------------------

demo_cols <- c('GENDER', 'RACETHN_recode', 'FIRST_GEN')

preds_demo <- add_predicted_draws(newdata = pred_sample, model = mod, n = 1000, seed = 149) %>%
  ungroup() %>%
  select(.row, .chain, .iteration, .draw, .prediction, GENDER, RACETHN_recode, FIRST_GEN)

# percentage female and male
perc_gender <- preds_demo %>% 
  group_by(.draw, GENDER) %>%
  summarize(total_STEM = sum(.prediction)) %>%
  ungroup() %>%
  group_by(.draw) %>%
  mutate(total_students = sum(total_STEM),
         perc = total_STEM / total_students)

# hdi by gender
perc_gender %>%
  group_by(GENDER) %>%
  median_hdi(total_STEM)

# estimate quantiles per group
gender_quant <- perc_gender %>% 
  group_by(GENDER) %>% 
  summarise(x = quantile(total_STEM, plot_quantiles), q = plot_quantiles) %>%
  pivot_wider(id_cols = GENDER, names_from = 'q', values_from = x)

# plot total number of students w/ 50% and 95% credible intervals
plot_gender <- ggplot(perc_gender, aes(x = total_STEM, fill = GENDER)) +
  geom_density(alpha = .3) +
  # most likely value
  geom_vline(data = gender_quant, aes(xintercept = `0.5`), size = 1) +
  # 95% ci
  geom_vline(data = gender_quant, aes(xintercept =`0.05`), size = 1, linetype = 3) +
  geom_vline(data = gender_quant, aes(xintercept =`0.95`), size = 1, linetype = 3) +
  scale_fill_brewer(type = 'qual', palette = 'Set2') +
  labs(title = 'Estimated Number of Matriculating STEM Majors by Gender - 2020',
       x = "Percentage of STEM students",
       y = NULL,
       color = NULL) +
  cf_plot_theme() +
  theme(axis.text.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.position = 'bottom')

ggsave(plot = plot_gender, filename = 'post_pred_num_stem_demo.png', path = 'modeling/post_pred_plots',
      width=unit(6, "in"), height=unit(4, "in"), dpi = 'retina')

# plot coefficients ----------------------------------

mod_coef <- mod %>%
  gather_draws(`^GENDERM.*|^HS.*|^FIRST.*|^AGREE.*|.*Intercept.*RACE.*|.*Intercept.*YEAR.*`, regex = T) %>%
  group_by(.variable) %>%
  median_hdi(.value) %>%
  mutate(.variable = str_remove_all(.variable, "^b.*[)] "),
         .variable = str_remove_all(.variable, "\\]"),
         .variable = str_remove_all(.variable, "THN."),
         .variable = str_replace_all(.variable, "^AGREE.*0", "AGREE:"),
         .variable = str_replace_all(.variable, "_", " "),
         .variable = str_replace_all(.variable, "HS.GPA", "HS GPA:"),
         .variable = str_replace_all(.variable, "FIRST.GEN", "FIRST GEN:"),
         .variable = str_replace_all(.variable, "GENDER", "GENDER:"),
         .variable = str_replace_all(.variable, "RACErecode", "RACE")) %>%
  separate(.variable, into = c('group', 'subgroup'), sep = ':') %>%
  mutate(group = str_to_title(group),
         group = str_replace(group, "^Hs.*", "HS GPA ('A+' as reference category)"),
         group = str_replace(group, "^First.*", "First Generation ('No' as reference category)"),
         group = str_replace(group, "^Gender", "Gender ('Female' as reference category)"))

# create different dataset and plot for survey --------------------------

# recode question numbers
agree_recode <- c(
  `1` = 'I have a strong sense of belonging to a community of scientists',
  `2` = 'I think of myself as a scientist',
  `3` = 'I feel like I belong in the field of science',
  `4` = 'Being very well off financially is important to me',
  `5` = 'Becoming successful in my own business is important to me',
  `6` = 'Developing a meaningful philosophy of life is important to me',
  `7` = 'Becoming a community leader is important to me',
  `8` = 'Making a theoretical contribution to science is important to me',
  `9` = 'Influencing social values is important to me'
)

survey_hdi <- mod_coef %>%
  filter(group == 'Agree') %>%
  mutate(group = "Survey - Agree",
         ques_num = str_extract(subgroup, '^[0-9]'),
         likert = str_extract(subgroup, '[0-9]$')) %>%
  mutate(ques_num = recode(ques_num, !!!agree_recode))

survey_categories <- c('Disagree', 'Slightly disagree', 
                       'Slightly agree', 'Agree', 'Strongly agree')

for (i in seq_along(survey_categories)) {
  
  
  reg_ex <- glue("[{i+1}]$")
  survey_hdi$likert <- str_replace(survey_hdi$likert, reg_ex, survey_categories[i])
  
}

wf_color <- 'black'#9E7E38'

x_axis_plot <- 'Coefficient value (log-odds ratio)'
plot_subtitle = 'Positive value means stronger association with STEM'

plot_survey <- survey_hdi %>%
  mutate(likert = factor(likert, levels = survey_categories)) %>%
  ggplot(aes(.value, likert)) +
  geom_point(color = wf_color) +
  geom_segment(aes(x=.lower, xend=.upper, y=likert, yend=likert), color = wf_color) +
  facet_wrap(~str_wrap(ques_num, 35), ncol = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(title = 'Modeled Relationship of Survey Responses and STEM',
       subtitle = plot_subtitle,  
       x = x_axis_plot,
       y = NULL) +
  cf_plot_theme()

ggsave(filename = 'post_pred_coef_survey.png',
       plot = plot_survey,
       path = 'modeling/post_pred_plots',
       width=unit(7, "in"), height=unit(7, "in"), dpi = 'retina')

# plot demographic coefficients -----------------------

# change order of rows so they plot better
a_minus <- mod_coef %>%
  filter(subgroup == 'A-')

mod_demo <- mod_coef %>%
  filter(group != 'Agree',
         subgroup != 'A-') %>%
  bind_rows(a_minus) %>%
  # 2014 is in twice, for some reason
  filter(!(subgroup == '2014' & .lower > 0))
  
plot_demo <- mod_demo %>%
  mutate(subgroup = factor(subgroup, levels = mod_demo$subgroup)) %>%
  ggplot(aes(.value, subgroup)) +
    geom_point(color = wf_color) +
    geom_segment(aes(x=.lower, xend=.upper, y=subgroup, yend=subgroup), color = wf_color) +
    facet_wrap(~group, ncol = 1, scales = 'free_y') +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(title = 'Modeled Relationship of Demographics and STEM',
         subtitle = plot_subtitle,   
         x = x_axis_plot,
         y = NULL) +
    cf_plot_theme()

ggsave(filename = 'post_pred_coef_demo.png', 
       plot = plot_demo,
       path = 'modeling/post_pred_plots', 
       width=unit(6, "in"), height=unit(8, "in"), dpi = 'retina')
