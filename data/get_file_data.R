## Purpose : Load, clean and integrate school data for Jobvite, Tyler-Munis, FMLA


library(arrow)
library(janitor)
library(tidyverse)
library(lubridate)


#Schools data
school_matching <- ideadata::get_schools() %>%
  collect() %>%
  filter(SchoolName != "IDEA Academy") %>% 
  filter(IsDeprecated == 0) %>%
  mutate(campus_type = case_when(str_detect(SchoolName,"College") ~ "College Prep",
                                 str_detect(SchoolName, regex("Academy", ignore_case = T)) ~ "Academy",
                                 TRUE ~ NA)) %>%
  mutate(campus = str_trim(SchoolShortName)) %>%
  select(SchoolNumber = StateSchoolNumber, # !!! Using the State School ID as the School Number !!!
         campus,
         campus_type) %>%
  filter(!is.na(campus_type)) %>%
  janitor::clean_names() %>%
  distinct()

#Jobvite data
jobvite <- read.csv("data/jobvite_07012023_06302024.csv") %>%
  janitor::clean_names() %>%
  filter(school_year == "2023-2024") %>%
  mutate(position_control_number = strsplit(position_control_number,",")) %>%
  unnest(position_control_number) %>%
  mutate(position_control_number =  suppressWarnings(as.integer(position_control_number)))%>%
  mutate(location = if_else(position_control_number == 101810, "IDEA Yukon College Prep", location)) %>% #the one teaching position that is not in a CP/A in Yukon 
  mutate(campus_type = case_when(str_detect(location,"College") ~ "College Prep",
                                 str_detect(location,"Academy") ~ "Academy",
                                 TRUE ~ NA)) %>%
  mutate(campus = gsub(" College Prep| Academy|IDEA ", "",location)) %>%
  mutate(campus = case_when(campus == "University Prep" ~ "UP",
                            campus == "Harvey E. Najim" ~ "Najim",
                            campus == "A.W. Brown" ~ "A W Brown",
                            TRUE ~ campus)) %>%
  left_join(school_matching, join_by(campus, campus_type)) %>%
  inner_join(school_matching %>%  select(campus) %>% distinct(), 
            join_by(campus)) %>%
  mutate(job_filled_on = as.Date(job_filled_on, format("%m/%d/%Y"))) %>%
  mutate(job_most_recent_open_date = as.Date(job_most_recent_open_date, format("%m/%d/%Y")))  %>% 
  mutate(internal_role = iconv(internal_role, "latin1", "ASCII", sub=""),
         job_title = iconv(department, "latin1", "ASCII", sub=""),
         department = iconv(internal_role, "latin1", "ASCII", sub="")) %>%
  filter(grepl("Teacher|Principal",internal_role) | 
           grepl("Teacher|Principal",job_title) | 
           grepl("Teach",department), 	
         !grepl("Co-|Interventionist",internal_role),
         !grepl("Substitute|PE",department),
         job_title %in% c("Teaching - English Language Arts", "Teaching - Science",
                          "Teaching - Social Studies", "Teaching - Math", "Executive Campus Leadership")) 
  
  
# Tylyer Munis data
tyler_munis <- read.csv("data/tyler_munis_07012023_06302024.csv") %>%
  janitor::clean_names() %>%
  rename(position_control_number = position_control) %>%
  mutate(campus_type = case_when(str_detect(location,"ICP ") ~ "College Prep",
                                 str_detect(location,"IA ") ~ "Academy",
                                 TRUE ~ NA)) %>%
  mutate(campus_upper = gsub("ICP |IA |IDEA ", "", location)) %>%
  #dirty campus names
  mutate(campus_upper = case_when(campus_upper == "BATON ROUGE UNIPREP" ~ "UP",
                                  campus_upper == "INNOVATION PARK" ~ "Innovation",
                                  campus_upper == "AW BROWN" ~ "A W Brown",
                          TRUE ~ campus_upper)) %>%
  mutate(campus_upper = toupper(campus_upper)) %>%
  #merge in campus information
  left_join(school_matching %>% mutate(campus_upper = toupper(campus)) %>% select(-campus), 
            join_by(campus_upper, campus_type)) %>%
  left_join(school_matching %>% mutate(campus_upper = toupper(campus)) %>% select(campus_upper, campus) %>% distinct(), 
            join_by(campus_upper)) %>%
  mutate(work_start_date = as.Date(work_start_date, format("%m/%d/%Y"))) %>%
  mutate(work_end_date = as.Date(mdy(work_end_date), format("%m/%d/%Y"))) 


# FMLA data
fmla <- read.csv("data/loa_08292024.csv", stringsAsFactors=FALSE, fileEncoding="latin1") %>%
  janitor::clean_names() %>%
  rename(first_day_of_leave = x1st_day_of_leave) %>%
  rename(employee_id = eid) %>%
  #only need close (already happened) or active (on going) records
  filter(status %in% c('Closed','Active')) %>%
  #dirty data with  employee ids
  filter(!grepl("\\D", employee_id)) %>%
  mutate(employee_id = as.integer(employee_id)) %>%
  mutate(first_date_of_leave = as.Date(first_day_of_leave, format("%m/%d/%Y"))) %>%
  mutate(rtw_date_date = as.Date(mdy(rtw_date), format("%m/%d/%Y"))) %>%
  #only need records that 
  filter(first_date_of_leave < ymd("2024-06-30") & (rtw_date_date > ymd("2023-07-01") | is.na(rtw_date_date))) %>%
  mutate(first_date_of_leave = if_else(first_date_of_leave < ymd("2023-07-01"), ymd("2023-07-01"), first_date_of_leave)) %>% ##limit date to within SY
  mutate(rtw_date_date = if_else(rtw_date_date > ymd("2024-06-30"), ymd("2024-06-30"), rtw_date_date)) %>% ##limit date to within SY
  mutate(fmla_days_out = as.numeric(difftime(ymd(rtw_date_date), 
                                          ymd(first_date_of_leave), units = "days")))

