
# OLD EXAMPLE function/loop example from previous work on AP Test Upset Plots
```{r}
library(dplyr)
library(rlang)

# sample data frame
student_id <- c('111', '112', '113', '114', '115', '116', '117', '118', '119', '120')
names_exams_passed <- c(
  'NA|NA|Spanish Language and Culture|NA|NA|NA|NA|NA|Spanish Literature and Culture|Studio Art: 2-D Design Portfolio|NA', 
  'Spanish Language and Culture|Spanish Literature and Culture|World History|English Language and Composition|NA|NA|Calculus AB|NA|NA',
  'NA|Spanish Language and Culture|NA|Calculus AB|NA|NA|Spanish Literature and Culture|NA|NA|NA|NA|Microeconomics|NA|NA',
  'NA|NA|English Language and Composition|NA|United States History|Biology|Calculus AB|English Literature and Composition|Microeconomics|United States Government and Politics',
  'NA|NA|Spanish Language and Culture|NA|NA|English Literature and Composition|NA|NA|NA|NA|United States Government and Politics',
  'Human Geography|World History|NA|English Language and Composition|NA|NA|NA|NA|NA|NA|NA|NA',
  'Human Geography|Spanish Language and Culture|Spanish Literature and Culture|World History|NA|NA|United States History|Biology|English Language and Composition|NA|NA|NA|United States Government and Politics',
  'NA|Spanish Language and Culture|NA|NA|Spanish Literature and Culture|United States History|Biology|English Language and Composition|United States Government and Politics',
  'NA|World History|NA|English Language and Composition|NA|NA|United States History|Biology|NA|English Literature and Composition|NA|NA|United States Government and Politics',
  'NA|World History|NA|English Language and Composition|NA|NA|United States History'
)
df <- data.frame(student_id, names_exams_passed)

# shorter list of test names for proof-of-concept of function + loop approach
uniq_exam_names <- c('Biology', 'Calculus AB', 'Spanish Language and Culture', 'Spanish Literature and Culture', 'Studio Art: 2-D Design Portfolio', 'United States History', 'World History')

# helper function creates a new column with the provided exam_name and puts TRUE/FALSE if that exam is listed in the "names_exams_passed" column
addExamPassedCol <- function(df, exam_name) {
  mutate(df, !!exam_name :=  grepl(exam_name, df$names_exams_passed, fixed=TRUE))
  
  # reference for dynamically named column: https://stackoverflow.com/a/26003971
  # grepl() reference example: https://www.statology.org/string-contains-r/
}


df_exam_cols <- df

for (exam_name_i in uniq_exam_names)
{
  df_exam_cols <- addExamPassedCol(df_exam_cols, exam_name_i)
}
df_exam_cols 
```