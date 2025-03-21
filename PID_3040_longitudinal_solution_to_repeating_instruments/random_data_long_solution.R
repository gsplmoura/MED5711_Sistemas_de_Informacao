library(tidyverse)
library(janitor)
library(lubridate)
library(stringi)

# import data dictionary and import template----

data_dictionary <- read_csv("PID_3040_longitudinal_solution_to_repeating_instruments/data_dictionary_long_solution.csv") %>% janitor::clean_names()
import_template <- read_csv("PID_3040_longitudinal_solution_to_repeating_instruments/import_template_long_solution.csv")

# demographics----

start_date_enrolled <- as.Date("2024-01-01")
end_date_enrolled <- as.Date("2024-12-31")
start_dob <- as.Date("1990-01-01")
end_dob <- as.Date("2000-12-31")

set.seed(130)
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

write_csv(demographics, "PID_3040_longitudinal_solution_to_repeating_instruments/import_demographics_long_solution.csv",na = "")


# Medications -----

medications <- bind_rows(
  # Ensure at least one row per record_id in Visit_1
  tibble(
    record_id = rep(1:5, each = 1),
    redcap_event_name = "visit_1_arm_1",
    med = sample(c("losartana", "metformina", "captopril", "glibenclamida", "furosemida"), 5, replace = TRUE),
    med_start = sample(seq(ymd("2010-01-01"), ymd("2024-12-31"), by = "day"), 5),
    med_dose = sample(c(10,20,50,75,100), 5, replace = TRUE),
    med_frequency = sample(c("1x/d", "12/12h", "8/8h"), 5, replace = TRUE)
  ),
  # Randomly distribute additional medications
  tibble(
    record_id = as.character(sample(1:5, 40, replace = TRUE)),
    redcap_event_name = sample(c("visit_1_arm_1", "visit_2_arm_1"), 40, replace = TRUE, prob = c(0.7, 0.3)),
    med = sample(c("losartana", "metformina", "captopril", "glibenclamida", "furosemida"), 40, replace = TRUE),
    med_start = sample(seq(ymd("2010-01-01"), ymd("2024-12-31"), by = "day"), 40),
    med_dose = sample(c(10,20,50,75,100), 40, replace = TRUE),
    med_frequency = sample(c("1x/d", "12/12h", "8/8h"), 40, replace = TRUE)
  )
)

# Assign a sequential med_num, but cap it at 6
medications <- medications %>%
  group_by(record_id, redcap_event_name) %>%
  mutate(med_num = row_number()) %>%
  filter(med_num <= 6) %>%  # Keep only up to 6 medications per record per visit
  ungroup()

# Pivot to Wide Format (Ensuring max 6 medications per record per visit)
medications_wide <- medications %>%
  pivot_wider(
    id_cols = c(record_id, redcap_event_name),
    names_from = med_num,
    values_from = c(med, med_start, med_dose, med_frequency),
    names_glue = "{.value}_{med_num}"
  ) %>%
  mutate(medications_complete = "2")  # Mark as complete for REDCap


write_csv(medications_wide, "PID_3040_longitudinal_solution_to_repeating_instruments/import_medications_long_solution.csv",na = "")


# Adverse Events ----

set.seed(131)

# Step 1: Generate Initial Adverse Events Data (Long Format)
adverse_events <- tibble(
  record_id = as.character(rep(as.character(1:5), 2)),
  redcap_event_name = rep(c("visit_2_arm_1", "visit_3_arm_1"), each = 5),
  aeyn = as.character(sample(c(0, 1), 10, replace = TRUE))
) %>%
  mutate(adverse_events_complete = "2")

# Step 2: Generate Adverse Events for Participants with aeyn == 1
ae_yes <- adverse_events %>%
  filter(aeyn == 1) %>%
  bind_rows(
    tibble(
      record_id = as.character(sample(unique(adverse_events$record_id), 20, replace = TRUE)),
      redcap_event_name = sample(c("visit_2_arm_1", "visit_3_arm_1"), 20, replace = TRUE),
      aeyn = as.character("1"),
      adverse_events_complete = "2"
    )
  ) %>%
  mutate(
    aeterm = sample(c("dor de cabeca", "nausea", "vomito", "diarreia", "tontura"), n(), replace = TRUE),
    aeoccur = "1",
    aeongo = "1",
    aestdat = as.character(sample(seq(ymd("2025-01-01"), ymd("2025-02-28"), by = "day"), n())),
    aesev = sample(1:3, n(), replace = TRUE),
    aerel = sample(1:4, n(), replace = TRUE),
    aedis = sample(c(0, 0, 0, 0, 1), n(), replace = TRUE)
  ) %>%
  group_by(record_id, redcap_event_name) %>%
  mutate(ae_num = row_number()) %>%  # Assign event numbers within each visit
  filter(ae_num <= 4) %>%  # Limit to max 4 adverse events per visit
  ungroup()

# Step 3: Convert to Wide Format for REDCap
ae_wide <- ae_yes %>%
  pivot_wider(
    id_cols = c(record_id, redcap_event_name),
    names_from = ae_num,
    values_from = c(aeterm, aeoccur, aeongo, aestdat, aesev, aerel, aedis),
    names_glue = "{.value}_{ae_num}"
  ) %>%
  right_join(adverse_events, by = c("record_id", "redcap_event_name")) %>%  # Keep all original rows
  mutate(across(starts_with("aeterm_"), ~ ifelse(aeyn == 0, NA, .)))  # Remove values if no adverse events

# Step 4: Save as CSV for REDCap Import
write_csv(ae_wide, "PID_3040_longitudinal_solution_to_repeating_instruments/import_adverse_events_long_solution.csv",na = "")

