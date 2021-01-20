# Custom functions for WFU presentation

# data cleaning -----------------------

demo_table <- function(df, var1, var2) {
  
  # create a table that is the percentage of matriculants for a 
  # demographic broken down by major
  
  # Parameters:
  #   df: dataframe with data to create percentages for
  #   var1: the first variable to segment by
  #   var2: the second variable to segment by
  
  df %>%
    group_by_at(c(var1, var2)) %>%
    count() %>%
    group_by_at(var1) %>%
    mutate(total_students = sum(n),
           perc = n / total_students)
}


survey_table <- function(df, survey_cols) {
  
  # creates table of survey results by question and degree group (STEM, Non-Stem)
  
  # Parameters:
  #   df: dataframe with survey results
  #   survey_cols: range of survey columns using tidyselect (ACT01:ACT04)
  
  df %>%
    select(STEM, {{survey_cols}}) %>%
    pivot_longer(cols = {{survey_cols}}, 
                 names_to = 'Question', values_to = 'Response') %>%
    group_by(STEM, Question, Response) %>%
    count() %>%
    group_by(STEM, Question) %>%
    mutate(total = sum(n),
           perc = n / total) %>%
    ungroup() 
  
}

combine_survey_questions <- function(df, survey_cols) {
  
  # collapse all questions in a single survey into one survey question
  # by taking the median of all survey questions
  
  df <- df %>%
    select(contains('ACT')) %>%
    mutate_all(as.numeric) %>%
    mutate('{survey_cols}_median' := rowMedians(.))
  
  df[[glue('{survey_cols}_median')]] 
  
}

# analysis ---------------------------------------

mod_varsel <- function(df, mod_form, prior_relevant_vars, num_predictor_cols, prior_form = 'horseshoe') {
  
  if (prior_form == 'horseshoe') {
    
    # create hyperparametres for horseshoe prior
    n <- nrow(df) # number of rows
    D <- num_predictor_cols # number of columns
    # prior guess for the number of relevant variables, 
    # based on looking at plots of relationships between STEM and demo / survey questions
    p0 <- prior_relevant_vars
    tau0 <- p0/(D-p0) * 1/sqrt(n) # scale for tau 
    coef_prior <- hs(df=1, global_df=1, global_scale=tau0)
    
  }
  
  else if (prior_form == 'student_t') {
    
    coef_prior <- student_t(df = 7, location = 0, scale = 1.5)

  }
  
  int_prior <- student_t(df = 7, location = 0, scale = 2.5)
  
  mod <- stan_glmer(formula(mod_form), 
                    data = df,
                    family = binomial(link = "logit"), 
                    prior = coef_prior, 
                    prior_intercept = int_prior,
                    seed = 126, 
                    adapt_delta = 0.95)
  
  return(mod)
}

factor_scores <- function(df, survey_name) {
  
  # use factor analysis for dimensionality reduction of survey items
  
  # dataframe that only contains survey questions
  survey <- df %>%
    select(starts_with(!!survey_name)) %>%
    mutate_all(as.numeric)
  
  # get the optimal number of factors
  num_fas <- fa.parallel(survey, plot = F)$nfact
  
  # get the factor scores
  survey_fa <- fa(survey, num_fas)
  
  # clean up dataframe of factor scores
  as.data.frame(survey_fa$scores) %>%
    rename_with(~paste0(., '_', survey_name))
  
}

impute_missing <- function(df, missing_col) {
  
  # impute missing values
  
  # give each row a distinct number so we can match
  # predictions with full dataset
  df[['row_num']] <- seq(1, nrow(df))
  
  # predictors for model predicting missing values
  matching_vars <- c('YEAR + GENDER + RACETHN_recode + MAJOR + COLLEGE_GPA')
  
  # formula for model predicting missing value
  mod_form <- glue("{missing_col} ~ {matching_vars}")
  
  # create model to predict missing values (ordinal regression)
  model <- polr(formula(mod_form), data=df)
  
  # get dataset of missing values to make predictions
  predict_data <- df[is.na(df[[missing_col]]),]
  
  # make predictions of missing values
  pred_values <- predict(model, newdata = predict_data)
  
  # clean up dataframe of predicted missing values, so it can be combines with full dataset
  pred_data <- predict_data %>%
    mutate(.prediction = pred_values,
           .prediction = as.character(.prediction)) %>%
    select(row_num, .prediction)
  
  # join predictions with full dataset
  df <- df %>%
    left_join(pred_data, by = 'row_num')
  
  # if the row of the missing value column in the full dataset is missing, 
  # replace with the predicted value
  impute_missing_values <- as.character(df[[missing_col]])
  impute_missing_values <- ifelse(is.na(impute_missing_values), df$.prediction, impute_missing_values)
  
  return(impute_missing_values)
  
}

# visualizations -------------------------------

cf_plot_theme <- function() {
  
  # ggplot theme for wfu job talk
  
  # themes cover items like font size, font, grid lines, whether there are axis labels, etc.
  
  # start with the stock theme_minimal as the base
  theme_minimal() + 
    theme(
      axis.text = element_text(colour = "black"), # text color black
      #panel.grid.major.x = element_blank(), # no major horzontal grid lines
      #panel.grid.minor.x = element_blank(), # no minor horizontal grid lines
      legend.position = 'none', # place legends at the bottom of the plot; use 'right' if needed
      plot.title = element_text(size = 11), # reduce font size of plot titles
      plot.subtitle = element_text(size = 10), # reduce font size of plot subtitles
      axis.title = element_text(size = 10), # reduce font size of axis titles
      legend.key.size = unit(3, 'mm') # decrease the size of the legend boxes
    )
}

point_plot <- function(df, x_var, y_var, facet_var, custom_colors,
                       plot_title, x_title, y_title, point_size = 3) {
  
  # function creates a point plot with the lines running horizontal
  
  facet_enq <- rlang::enquo(facet_var)
  
  # create dataset for vline that is the average of the entire demographic 
  # (percentage of females / percentage of males)
  vline <- df %>%
    ungroup() %>%
    group_by(!!facet_enq) %>%
    summarize(n = sum(n)) %>%
    ungroup() %>%
    mutate(total = sum(n),
           perc = n / total) #%>%
    #rename('vline_var' = 'GENDER')
  
  ggplot(df, aes({{x_var}}, {{y_var}}, color = {{y_var}})) +
    geom_vline(data = vline, aes(group = {{facet_var}}, xintercept = perc), linetype = 2, alpha = .7) +
    geom_point(size = point_size) +
    geom_segment(aes(x = 0, y = {{y_var}}, xend = {{x_var}}, yend = {{y_var}}, colour = {{y_var}})) +
    facet_wrap(vars(!! facet_enq), ncol = 1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_colour_manual(values = custom_colors) +
    labs(title = 'Black line is STEM majors | Vertical line is all students',
         subtitle = NULL,
         x = x_title,
         y = y_title) +
    cf_plot_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  
}

survey_plot <- function(survey_tbl, plot_title, plot_subtitle) {
  
  # create a plot of survey results by question and degree grouping
  
  # parameters:
  #   survey_tbl: a table of survey results made with survey_table
  #   plot_title: title of plot
  
  survey_tbl %>%
    ggplot(aes(perc, rev(Response), fill = STEM)) +
    geom_col(position=position_dodge(.75), width = .7, alpha = .7) +
    facet_wrap(~Question, ncol = 4) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_fill_manual(values = custom_colors) +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         x = 'Percentage of total responses for degree grouping',
         y = 'Question',
         fill = NULL) +
    cf_plot_theme() +
    theme(axis.text.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(legend.position = 'bottom')
}

demo_coef_plot <- function(mod, col_regex, y_labels, x_axis) {
  
  # plot each coefficient of demographic variables

  mcmc_areas(as.matrix(mod),
             regex_pars = col_regex,
             prob = c(.5, .95)) + 
    scale_y_discrete(labels= y_labels) +
    xlim(c(-3, 3)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = .7) +
    labs(title = "With medians, 50% intervals, and 95% intervals of log-odds ratio",
         x = x_axis) +
    cf_plot_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50"))

}
