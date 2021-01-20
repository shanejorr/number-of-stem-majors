################################################################
#
# Series of functions to recode the dataset from integers to 
# their string descriptions
#
##################################################################

# recode all demographic and personal attributes ---------

recode_demo <- function(df) {

  race_recode <- c(
    `1` = 'American Indian or Alaska Native',
    `2` = 'Asian',
    `3` = 'Black or African American, not Hispanic',
    `4` = 'Hispanic/Latino',
    `5` = 'Native Hawaiian or Other Pacific Islander, not Hispanic',
    `6` = 'White, not Hispanic',
    `7` = 'Two or more races',
    .default = 'not matched'
  )
  
  major_recode <- c(
    `1` = 'Independent',
    `2` = 'Arts',
    `3` = 'Humanities',
    `4` = 'Social Science',
    `5` = 'Business',
    `6` = 'STEM',
    .default = 'not matched'
  )
  
  distance_recode <- c(
    `1` = '5 or less',
    `2` = '6 - 10',
    `3` = '11 - 50',
    `4` = '51 - 100',
    `5` = '101 - 500',
    `6` = 'Over 500',
    .default = 'not matched'
  )
  
  # HS gpa only has 4 and above
  hs_gpa_recode <- c(
    `1` = 'D or lower',
    `2` = 'C',
    `3` = 'C+',
    `4` = 'B-',
    `5` = 'B',
    `6` = 'B+',
    `7` = 'A-',
    `8` = 'A or A+',
    .default = 'not matched'
  )
  
  college_gpa_recode <- c(
    `1` = 'D or lower',
    `2` = 'C',
    `3` = 'C+',
    `4` = 'B',
    `5` = 'B',
    `6` = 'B+',
    `7` = 'A',
    `8` = 'A or A+',
    .default = 'not matched'
  )
  
  first_gen_recode <- c(
    `1` = 'No',
    `2` = 'Yes',
    .default = 'not matched'
  )
  
  # vector of the column names for the demographic columns that will be recoded
  demo_cols <- c('RACETHN', 'MAJOR', 'DISTANCE', 'HS_GPA', 'COLLEGE_GPA', 'FIRST_GEN')
  
  # list of recode mappings, to iterate through
  demo_recodes <- list(race_recode, major_recode, distance_recode, hs_gpa_recode, college_gpa_recode, first_gen_recode)
  
  # iterate through columns and recodings, recoding columns
  for (i in seq_along(demo_cols)) {
      df[[demo_cols[i]]] <- recode(df[[demo_cols[i]]], !!!demo_recodes[[i]])
  }
  
  return(df)

}


recode_demo_vars <- function(df) {
  
  # function to recode all variables at once
  # wrapper for other recode functions
  # also, create a new variable that labels students as either stem major or non-stem
  # and capitalize male and female
  
  # order of levels for distance as a factor
  distance_levels <- c("5 or less", "6 - 10", "11 - 50", "51 - 100", "101 - 500", "Over 500") 
  hs_gpa_levels <- c('A or A+', 'A-', 'B+', 'B', 'B-')
  
  
  df %>%
    recode_demo() %>%
    mutate(STEM = if_else(MAJOR == 'STEM', 'STEM', 'Non-STEM'),
           GENDER = str_to_title(GENDER),
           DISTANCE = factor(DISTANCE, levels = distance_levels),
           HS_GPA = factor(HS_GPA, levels = hs_gpa_levels),
           STEM_bern = if_else(MAJOR == 'STEM', '1', '0'),
           STEM_bern = as.integer(STEM_bern))
}

combine_demo_vars <- function(df) {
  
  # This function creates objects to recode demographics by
  # combining groups (combining different races)
  
  # must run recode_demo_vars first
  
  recode_race <- c(
    "Native Hawaiian or Other Pacific Islander, not Hispanic" = "Other",
    "American Indian or Alaska Native" = "Other"
  )
  
  recode_distance <- c(
    "5 or less" = "10 or less",
    "6 - 10" = "10 or less"
  )
  
  # column names of columns are are recoding
  col_names <- c('RACETHN', 'DISTANCE')
  
  # column names of new columns to make
  col_names_new <- glue("{col_names}_recode")
  
  # list of recode mappings, to iterate through
  demo_recodes <- list(recode_race, recode_distance)
  
  # iterate through columns and recodings, recoding columns
  for (i in seq_along(col_names)) {
    df[[col_names_new[i]]] <- recode(df[[col_names[i]]], !!!demo_recodes[[i]])
  }
  
  return(df)
  
}

# Recode survey likert values ------------------------------

recode_survey_one_vars <- function(df) {

  survey_one_recode <- c(
    `1` = 'Not at all',
    `2` = 'Occasionally',
    `3` = 'Frequently',
    .default = 'missing'
  )
  
  # survey one columns
  col_names <- colnames(df)
  survey_one_cols <- col_names[str_detect(col_names, '^ACT')]
  
  # iterate through columns and recodings, recoding columns
  for (col in survey_one_cols) {
    df[[col]] <- recode(df[[col]], !!!survey_one_recode)
  }
  
  return(df)
  
}

recode_survey_two_vars <- function(df) {
  
  survey_two_recode <- c(
    `1` = 'A major weakness',
    `2` = 'Somewhat weak',
    `3` = 'Average',
    `4` = 'Somewhat strong',
    `5` = 'A major strength'
  )
  
  col_names <- colnames(df)
  survey_two_cols <- col_names[str_detect(col_names, '^RATE')]
  
  # iterate through columns and recodings, recoding columns
  for (col in survey_two_cols) {
    df[[col]] <- recode(df[[col]], !!!survey_two_recode)
  }
  
  return(df)
  
}
  
recode_survey_three_vars <- function(df) {
  
  survey_three_recode <- c(
    `1` = 'Strongly disagree',
    `2` = 'Disagree',
    `3` = 'Slightly disagree',
    `4` = 'Slightly agree',
    `5` = 'Agree',
    `6` = 'Strongly agree'
  )
  
  col_names <- colnames(df)
  survey_three_cols <- col_names[str_detect(col_names, '^AGREE')]
  
  # iterate through columns and recodings, recoding columns
  for (col in survey_three_cols) {
    df[[col]] <- recode(df[[col]], !!!survey_three_recode)
  }
  
  return(df)

}

# recode survey questions --------------------------------


recode_act_ques <- function(col_vector) {
  
  act_recode <- c(
    ACT01 = 'Perform volunteer work', 
    ACT02 = 'Vote in a student election',
    ACT03 = 'Discuss politics',
    ACT04 = 'Skip school/class',
    ACT05 = 'Fall asleep in class',
    ACT06 = 'Ask a teacher for advice after class',
    ACT07 = 'Tutor another student',
    ACT08 = 'Seek alternative solutions to a problem',
    ACT09 = 'Look up scientific research articles and
    resources',
    ACT10 = 'Explore topics on your own, even though
  it was not required for a class',
    ACT11 = 'Accept mistakes as part of the learning
  process',
    ACT12 = 'Seek solutions to problems and explain
  them to others',
    ACT13 = 'Write computer code'
  )

  recode(col_vector, !!!act_recode)

}

recode_rate_ques <- function(col_vector) {
  
  rate_recode <- c(
    RATE1 = 'Tolerance of others with different beliefs',
    RATE2 = 'Openness to having my own views
challenged',
    RATE3 = 'Ability to work cooperatively with
diverse people',
    RATE4 = 'Critical thinking skills ',
    RATE5 = 'Mathematical ability',
    RATE6 = 'Academic ability ',
    RATE7 = 'Writing ability '
  )
  
  recode(col_vector, !!!rate_recode)
}

recode_agree_ques <- function(col_vector) {
  
  agree_recode <- c(
    AGREE_01 = 'I have a strong sense of belonging to
a community of scientists',
    AGREE_02 = 'I think of myself as a scientist',
    AGREE_03 = 'I feel like I belong in the field of
science',
    AGREE_04 = 'Being very well off financially is
important to me',
    AGREE_05 = 'Becoming successful in my own
business is important to me',
    AGREE_06 = 'Developing a meaningful philosophy
of life is important to me',
    AGREE_07 = 'Becoming a community leader is
important to me',
    AGREE_08 = 'Making a theoretical contribution to
science is important to me',
    AGREE_09 = 'Influencing social values is important
to me'
  )
  
  recode(col_vector, !!!agree_recode)
  
}
