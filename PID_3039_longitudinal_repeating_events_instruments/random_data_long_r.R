library(tidyverse)
library(janitor)
library(lubridate)
library(stringi)

# import data dictionary and import template----

data_dictionary <- read_csv("PID_3039_longitudinal_repeating_events_instruments/data_dictionary_long_r.csv") %>% janitor::clean_names()
import_template <- read_csv("PID_3039_longitudinal_repeating_events_instruments/import_template_long_r.csv")

# demographics----

set.seed(127)
start_date_enrolled <- as.Date("2024-01-01")
end_date_enrolled <- as.Date("2024-12-31")
start_dob <- as.Date("1990-01-01")
end_dob <- as.Date("2000-12-31")

demographics <- import_template |>
dplyr::bind_rows(tibble::tibble(record_id = as.character(1:5))) %>%
  mutate(
    redcap_event_name = "visit_1_arm_1",
    first_name = stri_rand_strings(5, 3, pattern = "[A-Z]"), # stri_rand_strings(n, length, pattern = "[A-Za-z0-9]")
    date_enrolled = sample(seq(start_date_enrolled, end_date_enrolled, by = "day"), 5),
    dob = sample(seq(start_dob, end_dob, by = "day"), 5),
    ethnicity = sample(c(0:2), 5, replace = TRUE),
    race = sample(c(0:6), 5, replace = TRUE),
    sex = sample(c(0,1), 5, replace = TRUE),
    demographics_complete = "2"
  ) %>% 
  select(record_id:demographics_complete)

write_csv(demographics, "PID_3039_longitudinal_repeating_events_instruments/import_demographics_long_r.csv",na = "")

# Events: visit_1_arm_1, visit_2_arm_1, visit_3_arm_1
## Repeating instruments
### medication_single
### adverse_events

start_medication <- as.Date("2010-01-01")
end_medication <- as.Date("2024-12-31")

medication_single <- import_template %>% 
  dplyr::bind_rows(tibble::tibble(record_id = as.character(1:5)),
                   tibble::tibble(record_id = as.character(sample(1:5, 40, replace = TRUE)))) %>% 
  mutate(
    medication_single_complete = "2",
    redcap_repeat_instrument = "medication_single",
    med = sample(c("losartana", "metformina", "captopril", "glibenclamida", "furosemida"), 45, replace = TRUE),
    med_start = sample(seq(ymd("2010-01-01"), ymd("2024-12-31"), by = "day"), 45),
    med_dose = sample(c(10,20,50,75,100), 45, replace = TRUE),
    med_frequency = sample(c("1x/d", "12/12h", "8/8h"), 45, replace = TRUE),
    redcap_event_name = sample(c("visit_1_arm_1", "visit_2_arm_1"), 45, replace = TRUE)
  ) %>% 
  group_by(record_id, redcap_event_name) %>%
  mutate(redcap_repeat_instance = row_number()) %>%
  ungroup()


medication_single %>% select(record_id:medication_single_complete) %>% 
write_csv(., "PID_3039_longitudinal_repeating_events_instruments/import_medication_single_long_r.csv",na = "")

# Adverse events ----

set.seed(128)
adverse_events <- import_template %>% 
  select(record_id:redcap_repeat_instance, aeyn:adverse_events_complete) %>% 
  dplyr::bind_rows(tibble::tibble(record_id = rep(as.character(1:5),2))) %>% 
  mutate(
    adverse_events_complete = "2",
    redcap_repeat_instrument = "adverse_events"
    ) %>%
  group_by(record_id) %>%
  mutate(
    redcap_event_name = case_when(
      row_number() == 1 ~ "visit_2_arm_1",
      row_number() == 2 ~ "visit_3_arm_1"
    )) %>% 
  ungroup() %>%
  mutate(
    aeyn = sample(c(0,1), 10, replace = TRUE)
  )

ae_no <- adverse_events %>% 
  filter(
    aeyn == 0
  ) %>% 
  mutate(
    redcap_repeat_instance = as.character("1")
  )

write_csv(ae_no, "PID_3039_longitudinal_repeating_events_instruments/import_adverse_events_no_long_r.csv",na = "")

ae_yes <- adverse_events %>% 
  filter(
    aeyn == 1
  ) %>% 
  dplyr::bind_rows(
    tibble::tibble(record_id = as.character(sample(unique(.$record_id), 20, replace = TRUE))) 
    )%>%
  mutate(
    aeyn = as.character("1"),
    adverse_events_complete = "2",
    redcap_repeat_instrument = "adverse_events",
    redcap_event_name = sample(c("visit_2_arm_1", "visit_3_arm_1"), n(), replace = TRUE)
  ) %>% 
  mutate(
    aeterm = sample(c("dor de cabeca", "nausea", "vomito", "diarreia", "tontura"), n(), replace = TRUE),
    aeoccur = as.character("1"),
    aeongo = as.character("1"),
    aestdat = sample(seq(ymd("2025-01-01"), ymd("2025-02-28"), by = "day"), n()),
    aesev = sample(c(1:3), n(), replace = TRUE),
    aerel = sample(c(1:4), n(), replace = TRUE),
    aedis = sample(c(0,0,0,0,1), n(), replace = TRUE)
  ) %>% 
  group_by(record_id, redcap_event_name) %>%
  mutate(redcap_repeat_instance = row_number()) %>%
  ungroup()
  
write_csv(ae_yes, "PID_3039_longitudinal_repeating_events_instruments/import_adverse_events_yes_long_r.csv",na = "")

