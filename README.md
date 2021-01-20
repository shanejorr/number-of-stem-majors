# Number of STEM Majors

This repo predicts the number of STEM majors from an entering class. Data is not included.

Repo folders:

- `eda`: Exploratory data analysis. Looks at the relationship between majoring in STEM and various demographic factors and survey answers.
- `modeling`: Creates a model to predict whether a student will major in a STEM degree. The model is then applied to predict the total number of STEM majors. The file `model_multinomial.R` predicts the number of majors in four different majors.

The file `functions.R` and `recode_values.R` contain general custom functions to clean and model the data.
