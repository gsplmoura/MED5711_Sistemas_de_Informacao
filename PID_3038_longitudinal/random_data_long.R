library(tidyverse)
library(janitor)
library(lubridate)
library(stringi)

# import data dictionary and import template----

data_dictionary <- read_csv("PID_3038_longitudinal/data_dictionary_long.csv") %>% janitor::clean_names()
import_template <- read_csv("PID_3038_longitudinal/import_template_long.csv")

# demographics----

set.seed(126)
start_date_enrolled <- as.Date("2024-01-01")
end_date_enrolled <- as.Date("2024-12-31")
start_dob <- as.Date("1990-01-01")
end_dob <- as.Date("2000-12-31")

demographics <- import_template |>
  select(record_id:demographics_complete) %>% 
  dplyr::bind_rows(tibble::tibble(record_id = as.character(1:20))) %>% 
  mutate(
    first_name = stri_rand_strings(20, 3, pattern = "[A-Z]"), # stri_rand_strings(n, length, pattern = "[A-Za-z0-9]")
    date_enrolled = sample(seq(start_date_enrolled, end_date_enrolled, by = "day"), 20),
    dob = sample(seq(start_dob, end_dob, by = "day"), 20),
    ethnicity = sample(c(0:2), 20, replace = TRUE),
    race = sample(c(0:6), 20, replace = TRUE),
    sex = sample(c(0,1), 20, replace = TRUE),
    demographics_complete = "2",
    redcap_event_name = "visit_1_arm_1"
    )

write_csv(demographics, "PID_3038_longitudinal/import_demographics_long.csv",na = "")

# events----
# visit_1_arm_1
# visit_2_arm_1
# visit_3_arm_1

variables <- import_template %>% 
  select(stomach_pain_sc:sleep_sc) %>% 
  names()

phq15 <- import_template %>% 
  select(record_id, redcap_event_name, stomach_pain_sc:phq15_complete) %>%
  bind_rows(
    tibble(record_id = rep(as.character(1:20), 3))) %>% 
  mutate(
    across(all_of(variables), ~ sample(0:2, n(), replace = TRUE)),
    phq15_complete = "2"
  ) %>% 
  group_by(record_id) %>% 
  mutate(
    redcap_event_name = case_when(
      row_number() == 1 ~ "visit_1_arm_1",
      row_number() == 2 ~ "visit_2_arm_1",
      row_number() == 3 ~ "visit_3_arm_1"
    )
  ) %>% 
  ungroup()

write_csv(phq15, "PID_3038_longitudinal/import_phq15_long.csv",na = "")

data_export <- read_csv("PID_3038_longitudinal/data_export_long.csv")
