library(tidyverse)
library(janitor)
library(ideadata)




#tie together tyler_munis and staffing_w_courses to start and end dates for each emp
tm_staffing_w_course <- tyler_munis %>%
  select(employee_id, work_start_date, work_end_date) %>%
  inner_join(staffing_w_courses, join_by(employee_id)) %>%
  group_by(employee_id) %>%
  summarise(work_start_date = min(work_start_date),
            work_end_date = max(work_end_date)) %>%
  ungroup()



fmla_staffing_w_course <- fmla %>%
  select(employee_id, first_date_of_leave, rtw_date_date, fmla_days_out) %>%
  inner_join(staffing_w_courses, join_by(employee_id)) %>%
  filter(!is.na(first_date_of_leave)) %>%
  filter(fmla_days_out >= 45) %>%
  group_by(employee_id) %>%
  summarise(leave_start_date  = min(first_date_of_leave ),
            leave_end_date = max(rtw_date_date),
            fmla_days_out = sum(fmla_days_out)) %>%
  ungroup()


courses_tm_fmla_staff <- staffing_courses %>%
  left_join(tm_staffing_w_course, join_by(employee_id)) %>%
  left_join(fmla_staffing_w_course, join_by(employee_id)) %>%
  mutate(missing_begining_start_date = if_else(work_start_date > ymd("2023-09-01"), ymd("2023-09-01"), NA),
         missing_begining_end_date = if_else(!is.na(missing_begining_start_date),  work_start_date - days(1), NA)) %>%
  mutate(missing_begining_days_out = as.numeric(difftime(ymd(missing_begining_end_date), 
                                        ymd(missing_begining_start_date), units = "days"))) %>%
  mutate(missing_ending_start_date = if_else(work_end_date < ymd("2024-05-01"),work_end_date + days(1), NA),
         missing_ending_end_date = if_else(!is.na(missing_ending_start_date), ymd("2024-05-27"), NA)) %>%
  mutate(missing_ending_days_out = as.numeric(difftime(ymd(missing_ending_end_date), 
                                                         ymd(missing_ending_start_date), units = "days"))) %>%
  mutate(total_days_out = if_else(is.na(fmla_days_out), 0, fmla_days_out)  + 
           if_else(is.na(missing_begining_days_out), 0, missing_begining_days_out) + 
           if_else(is.na(missing_ending_days_out), 0, missing_ending_days_out)) 



jobvite_staffing_vacancies <- tyler_munis %>%
  inner_join(jobvite, join_by(position_control_number)) %>%
  filter(job_filled_on < work_start_date) %>%
  filter(job_most_recent_open_date <= work_start_date) %>%
  filter(work_end_date > job_most_recent_open_date) %>%
  left_join(staffing_courses %>% mutate(staffing_courses = employee_id) , join_by(employee_id)) %>%
  mutate(vacancy_begining_start_date = if_else(work_start_date > job_most_recent_open_date, job_most_recent_open_date, work_start_date)) %>%
  mutate(vacancy_begining_end_date = if_else(!is.na(vacancy_begining_start_date),  work_start_date, job_filled_on)) %>%
  mutate(total_days_vacancy = as.numeric(difftime(ymd(vacancy_begining_end_date), 
                                                         ymd(vacancy_begining_start_date), units = "days"))) %>%
  mutate(total_days_vacancy = if_else(total_days_vacancy == 0, as.numeric(difftime(ymd(job_filled_on), 
                                                  ymd(job_most_recent_open_date), units = "days")), total_days_vacancy)) %>%
  filter(total_days_vacancy > 0)

