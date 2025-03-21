library(tidyverse)
library(janitor)
library(lubridate)
library(stringi)

# import data dictionary and import template----

data_dictionary <- read_csv("PID_3037_non-repeating_instruments/data_dictionary_nri.csv") %>% janitor::clean_names()
import_template <- read_csv("PID_3037_non-repeating_instruments/import_template_nri.csv")

# demographics----

set.seed(125)
start_date_enrolled <- as.Date("2024-01-01")
end_date_enrolled <- as.Date("2024-12-31")
start_dob <- as.Date("1990-01-01")
end_dob <- as.Date("2000-12-31")

demographics <- import_template |>
  select(
    record_id:demographics_complete
  ) %>% 
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

write_csv(demographics, "PID_3037_non-repeating_instruments/import_demographics_nri.csv",na = "")

# PHQ_1 & PHQ_2----

variables_phq <- import_template %>% 
  select(stomach_pain_sc_1:phq15_2_complete) %>% 
  names()

phq15 <- import_template %>%
  select(
    record_id, stomach_pain_sc_1:phq15_2_complete
  ) %>% 
  bind_rows(tibble(record_id = rep(as.character(1:20),2))
            ) %>% 
  mutate(
    across(all_of(variables_phq), ~ sample(0:2, n(), replace = TRUE)),
    phq15_1_complete = "2",
    phq15_2_complete = "2"
  )

write_csv(phq15, "PID_3037_non-repeating_instruments/import_phq15_nri.csv",na = "")

# PHQ_3----

variables_phq_3 <- import_template %>% 
  select(stomach_pain_sc_3:sleep_sc_3) %>% 
  names()

phq15_3 <- import_template %>% 
  select(
    record_id, stomach_pain_sc_3:phq15_3_complete
  ) %>% 
  bind_rows(
    tibble(record_id = as.character(sample(1:20, 10, replace = FALSE)))) %>%
  mutate(
    across(all_of(variables_phq_3), ~ sample(0:2, n(), replace = TRUE)),
    phq15_3_complete = "2",
  )

write_csv(phq15_3, "PID_3037_non-repeating_instruments/import_phq15_3_nri.csv",na = "")

data_export <- read_csv("PID_3037_non-repeating_instruments/data_export_nri.csv")
