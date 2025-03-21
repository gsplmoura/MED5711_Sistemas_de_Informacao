library(tidyverse)
library(janitor)
library(lubridate)
library(stringi)

# import data dictionary and import template----

data_dictionary <- read_csv("PID_3034_repeating_instruments/data_dictionary.csv") %>% janitor::clean_names()
import_template <- read_csv("PID_3034_repeating_instruments/import_template.csv")

# demographics----

set.seed(123)
start_date_enrolled <- as.Date("2024-01-01")
end_date_enrolled <- as.Date("2024-12-31")
start_dob <- as.Date("1990-01-01")
end_dob <- as.Date("2000-12-31")

demographics <- import_template |>
  dplyr::bind_rows(tibble::tibble(record_id = as.character(1:20))) %>% 
  mutate(
    first_name = stri_rand_strings(20, 3, pattern = "[A-Z]"), # stri_rand_strings(n, length, pattern = "[A-Za-z0-9]")
    date_enrolled = sample(seq(start_date_enrolled, end_date_enrolled, by = "day"), 20),
    dob = sample(seq(start_dob, end_dob, by = "day"), 20),
    ethnicity = sample(c(0:2), 20, replace = TRUE),
    race = sample(c(0:6), 20, replace = TRUE),
    sex = sample(c(0,1), 20, replace = TRUE),
    demographics_complete = rep(2, 20)
    )

write_csv(import_template, "PID_3034_repeating_instruments/import_demographics.csv",na = "")

# repeating instrument: PHQ-15----

variables <- import_template %>% 
  select(stomach_pain_sc:sleep_sc) %>% 
  names()

phq15 <- import_template %>% 
  bind_rows(
    tibble(record_id = as.character(1:20)),
    tibble(record_id = as.character(sample(1:20, 40, replace = TRUE)))) %>% 
  mutate(
    redcap_repeat_instrument = "phq15",
    phq15_complete = "2",
    across(all_of(variables), ~ sample(0:2, n(), replace = TRUE)) # Use n() dynamically
  ) %>% 
  group_by(record_id) %>% 
  mutate(redcap_repeat_instance = row_number()) %>% # Assign sequential numbers within each record_id group
  ungroup()

write_csv(phq15, "PID_3034_repeating_instruments/import_phq15.csv",na = "")

data_export <- read_csv("PID_3034_repeating_instruments/data_export.csv")
