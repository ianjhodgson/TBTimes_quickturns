### Florida FAFSA completion rates ###

# Header ----

library(tidyverse)
library(readxl)

rm(list = ls())

# Read data ---- 

## Grade 12 enrollment
gr12_2223 <- read_xlsx("data/FAFSA/2223MembBySchoolByGrade.xlsx",
                       sheet = "School", 
                       skip = 2,
                       na = "*") %>% 
  janitor::clean_names() %>% 
  select(1:4, x12) %>% 
  rename(students = x12) %>% 
  filter(!is.na(students)) %>% 
  mutate(class = "2023")

gr12_2324 <- read_xlsx("data/FAFSA/2324MembBySchoolByGrade.xlsx",
                       sheet = "School", 
                       skip = 2,
                       na = "*") %>% 
  janitor::clean_names() %>% 
  select(1:4, x12) %>% 
  rename(students = x12) %>% 
  filter(!is.na(students)) %>% 
  mutate(class = "2024")

gr12_enroll <- bind_rows(gr12_2223, 
                         gr12_2324)

## latest FAFSA completion data
fafsa_files <- list.files("data/FAFSA/2024 reports/") %>% 
  sort(decreasing = T)
fafsa <- read_xls(paste0("data/FAFSA/2024 reports/", fafsa_files[1]),
                   sheet = "FL School Level Data",
                   skip = 3, 
                   na = "<5") %>% 
  select(1:8) %>% 
  mutate(date = str_extract(fafsa_files[1], "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>% 
           ymd())
names(fafsa) <- c("nces_school_id", 
                  "school_name", 
                  "city", 
                  "state", 
                  "submitted_2024", 
                  "completed_2024",
                  "submitted_2023",
                  "completed_2023",
                  "date")

## state to NCES code crosswalk 
nces_xwalk <- read_xlsx("data/FAFSA/ncesdata_441DE4E0.xlsx",
                        skip = 11,
                        na = "†") %>% 
  janitor::clean_names() %>% 
  mutate(school_number = str_extract(state_school_id, "[0-9]{4}$") %>% 
           as.numeric,
         district_number = str_extract(state_district_id, "[0-9]{2}$") %>% 
           as.numeric)

## School lunch status
lunch_status <- read_xlsx("data/FAFSA/2324FS2-Lunch-Status-School.xlsx",
                          sheet = 1, skip = 2, na = "*") %>% 
  janitor::clean_names() %>% 
  mutate(district_number = as.numeric(dist_number),
         school_number = as.numeric(schl_number)) %>% 
  select(-school_name)

# Clean and combine data ----
fafsa_clean <- fafsa %>% 
  select(1:6, 9) %>% 
  mutate(class = "2024") %>% 
  rename(submitted = submitted_2024,
         completed = completed_2024) %>% 
  bind_rows(fafsa %>% 
              select(1:4, 7:9) %>% 
              mutate(class = "2023") %>% 
              rename(submitted = submitted_2023, 
                     completed = completed_2023)) %>% 
  left_join(nces_xwalk %>% 
              select(nces_school_id, 
                     state_school_id,
                     charter), 
            by = "nces_school_id") %>% 
  mutate(district_number = str_extract(state_school_id, "-[0-9]{2}-") %>% 
           str_remove_all("-") %>% as.numeric(),
         school_number = str_extract(state_school_id, "[0-9]{4}") %>% as.numeric()) %>% 
  left_join(gr12_enroll, by = c("district_number", "school_number", "class")) %>% 
  left_join(lunch_status %>% 
              mutate(district_number = as.numeric(dist_number),
                     school_number = as.numeric(schl_number)) %>% 
              select(district_number, school_number, number_of_students_denominator, rate_wo_multiplier)) %>% 
  filter(!is.na(state_school_id),
         charter == "No")


## Statewide submission and completion rates ----

submission_rates <- fafsa_clean %>% 
  group_by(district, class) %>% 
  summarise(grade_12_students = sum(as.numeric(students), na.rm = T),
            submitted = sum(as.numeric(submitted), na.rm = T),
            completed = sum(as.numeric(completed), na.rm = T)) %>% 
  mutate(share_submitted = submitted/grade_12_students, 
         share_uncompleted = 1-completed/submitted) %>% 
  arrange(district, class) %>% 
  bind_rows(fafsa_clean %>% 
              group_by(class) %>% 
              summarise(grade_12_students = sum(as.numeric(students), na.rm = T),
                        submitted = sum(as.numeric(submitted), na.rm = T),
                        completed = sum(as.numeric(completed), na.rm = T)) %>% 
              mutate(share_submitted = submitted/grade_12_students, 
                     share_uncompleted = 1-completed/submitted,
                     share_completed = completed/grade_12_students,
                     district = "Florida"))


