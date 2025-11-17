library(here)
library(tidyverse)

data_raw <- here("data-raw", "JOAS.csv") |>
  read_csv()

weekends <- c("Saturday", "Sunday")
airports <- c("LTFM", "EIDW", "LGAV", "EDDF", "EGLL", "EKCH")

rand_nums <- function(n) sample(10000:99999, n, replace = TRUE)

filtered_data <- data_raw |>
  rename(
    PHASE = SRC_PHASE,
    ADEP = ADEP_ICAO,
    ADES = ADES_ICAO,
    RWY_TIME = MVT_TIME_UTC,
    BLOCK_TIME = BLOCK_TIME_UTC,
    SLOT_TIME = SCHED_TIME_UTC,
  ) |>
  mutate(
    day_of_week = lubridate::wday(SLOT_TIME, label = TRUE, abbr = FALSE),

    # Replace ADEP NOT in airports with random numbers
    ADEP = if_else(
      ADEP %in% airports,
      ADEP,
      as.character(rand_nums(n()))
    ),

    # Replace ADES NOT in airports with random numbers
    ADES = if_else(
      ADES %in% airports,
      ADES,
      as.character(rand_nums(n()))
    )
  ) |>
  filter(
    ADEP %in% airports | ADES %in% airports,
    !(day_of_week %in% weekends)
  ) |>
  select(
    PHASE,
    ADEP,
    ADES,
    SLOT_TIME,
    BLOCK_TIME,
    RWY_TIME,
  )

# Save into /data/ folder
write.csv(
  filtered_data,
  here("data", "Slot_data.csv"),
  row.names = FALSE
)
