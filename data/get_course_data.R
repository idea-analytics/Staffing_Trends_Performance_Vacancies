library(tidyverse)
library(janitor)
library(ideadata)




## Too much data to pull from the DWH and too slow. Save into parquet file
## the data is very dirty so to keep it simple(MATH, ELA, SS, SCI)
# courses_dwh <-  get_table(.table_name = "StudentClassEnrollment", .database_name = "PROD1", .schema = "Schools", .server_name = "RGVPDSD-DWPRD1") %>%
#   filter(AcademicYear == "2023-2024") %>%
#   collect() %>%
#   janitor::clean_names() %>%
#   filter(!(course_credit_type %in% c('NA', 'LC','CTE','EL','FA','LOTE','PE','RE','TA','HL','HLTH','EC','SP','ELE'))) %>%
#   filter(!is.na(course_credit_type)) %>%
#   mutate(course_credit_type = case_when(course_credit_type == "SOCSTUD" ~ "SS",
#                                         course_credit_type == "MA" ~ "MATH",
#                                         course_credit_type == "English" ~ "ELA",
#                                         course_credit_type == "ENGL" ~ "ELA",
#                                         course_credit_type == "ENG" ~ "ELA",
#                                         course_credit_type == "SC" ~ "SCI",
#                                         TRUE ~ course_credit_type))
# # save course data to file
# write_parquet(courses_dwh, "data/courses_dwh.parquet")

#load courses
courses_dwh <- read_parquet("data/courses_dwh.parquet")

# School data
course_schools <- ideadata::get_schools() %>%
  collect() %>%
  filter(IsDeprecated == 0) %>%
  mutate(campus_type = case_when(SchoolNumber == 41 ~ "College Prep",
                                 str_detect(SchoolName, regex("Academy", ignore_case = T)) ~ "Academy",
                                 TRUE ~ "College Prep")) %>%
  mutate(campus = str_trim(SchoolShortName)) %>%
  mutate(power_school_number = SchoolNumber) %>%
  select(SchoolNumber = StateSchoolNumber, # !!! Using the State School ID as the School Number !!!
         power_school_number,
         campus,
         campus_type) %>%
  filter(!is.na(campus_type)) %>%
  janitor::clean_names() %>%
  distinct() 




#staff courses data (course that came from power school)
staffing_courses <- courses_dwh %>%
  filter(!grepl("\\D", name_id)) %>%
  mutate(employee_id = as.integer(name_id)) %>%
  filter(employee_id != 0) %>%
  select(employee_id,
         course_number,
         course_name,
         course_credit_type,
         power_school_number = school_number) %>%
  distinct() %>%
  left_join(course_schools, join_by(power_school_number))


#distinct staff courses data (course that came from power school)
staffing_w_courses <- staffing_courses %>%
  select(employee_id) %>%
  distinct() 