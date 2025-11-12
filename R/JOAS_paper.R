#### LIBRARIES  ####
library(conflicted)
library(tidyverse)
library(readxl)
library(ggridges)
library(scales)
library(eurocontrol)
library(jsonlite)
library(withr)
library(slider)
library(sf)
library(knitr)
library(maps)
library(data.table)
library(forcats)
library(patchwork)
library(zoo)
library(broom)
library(caret)
library(factoextra) # for clustering visualization
library(randomForest)
library(cluster)
library(janitor)
library(ranger)
library(tidyverse)
library(pROC)
library(doParallel)
library(here)

#### Descriptive statistics ####

# source("R/helpers.R")

ds <- here("data-raw", "JOAS.csv") |>
  read_csv()

selected_airports <- c("LTFM", "EIDW", "LGAV", "EDDF", "EGLL", "EKCH")


daily_counts <- ds |>
  filter(
    (ADEP %in% selected_airports & PHASE == "DEP") |
      (ADES %in% selected_airports & PHASE == "ARR")
  ) |>
  mutate(
    AIRPORT = if_else(PHASE == "DEP", ADEP, ADES)
  )

compute_intervals <- function(df, time_col) {
  df |>
    arrange(!!sym(time_col)) |>
    mutate(
      INTERVAL_MIN = as.numeric(difftime(
        !!sym(time_col),
        lag(!!sym(time_col)),
        units = "mins"
      ))
    ) |>
    summarise(
      MEDIAN_INTERVAL = median(INTERVAL_MIN, na.rm = TRUE),
      MEAN_INTERVAL = mean(INTERVAL_MIN, na.rm = TRUE)
    ) |>
    mutate(METRIC = time_col)
}

metrics <- c("FIRST_SUBMITTED", "FIRST_ACCEPTED", "SLOT_TIME", "BLOCK_TIME")

summary_table <- daily_counts |>
  group_by(AIRPORT, PHASE) |>
  group_modify(
    ~ bind_rows(lapply(metrics, function(m) compute_intervals(.x, m)))
  ) |>
  ungroup() |>
  select(AIRPORT, PHASE, METRIC, MEDIAN_INTERVAL, MEAN_INTERVAL)

# --- 4. Nicely formatted summary table ---
summary_table |>
  arrange(AIRPORT, PHASE, METRIC) |>
  knitr::kable(
    digits = 2,
    caption = "Median and Mean Intervals (Minutes) per Airport and Phase"
  )


metric_labels <- c(
  FIRST_SUBMITTED = "Submitted (Airlines)",
  FIRST_ACCEPTED = "Accepted (NM)",
  SLOT_TIME = "Planned (Slot)",
  ACTUAL = "Operated (Actual)"
)

summary_table_plot <- summary_table |>
  mutate(
    METRIC = fct_recode(factor(METRIC), !!!metric_labels),
    PHASE = if_else(PHASE == "ARR", "Arrival", "Departure")
  )

ggplot(summary_table_plot, aes(x = METRIC, y = MEAN_INTERVAL, fill = PHASE)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_point(
    aes(y = MEDIAN_INTERVAL, color = PHASE),
    position = position_dodge(width = 0.8),
    size = 3
  ) +
  facet_wrap(~AIRPORT, ncol = 3) +
  scale_fill_manual(
    values = c("Arrival" = "#1b9e77", "Departure" = "#d95f02")
  ) +
  scale_color_manual(
    values = c("Arrival" = "#1b9e77", "Departure" = "#d95f02")
  ) +
  labs(
    title = "Median and Mean Intervals Between Flights (by Airport & Phase)",
    x = "Flight Stage",
    y = "Interval (minutes)",
    fill = "Phase",
    color = "Phase",
    caption = "Points show median interval; bars show mean interval."
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold")
  )
ggsave("intervals.png", width = 16, height = 9, dpi = 600)


delay_data <- ds |>
  filter(
    (ADEP %in% selected_airports & PHASE == "DEP") |
      (ADES %in% selected_airports & PHASE == "ARR")
  ) |>
  mutate(
    AIRPORT = if_else(PHASE == "DEP", ADEP, ADES),
    DELAY_MIN = as.numeric(difftime(BLOCK_TIME, SLOT_TIME, units = "mins")) # positive = delayed
  )

# --- REMOVE OUTLIERS ---
delay_data_clean <- delay_data |>
  filter(!is.na(DELAY_MIN)) |>
  group_by(AIRPORT, PHASE) |>
  mutate(
    lower_bound = quantile(DELAY_MIN, 0.05, na.rm = TRUE),
    upper_bound = quantile(DELAY_MIN, 0.95, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(DELAY_MIN >= lower_bound & DELAY_MIN <= upper_bound)

percentile_summary <- delay_data_clean |>
  group_by(AIRPORT, PHASE) |>
  summarise(
    MIN_DELAY_10TH = quantile(DELAY_MIN, 0.10, na.rm = TRUE),
    MAX_DELAY_90TH = quantile(DELAY_MIN, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# Common color setup
phase_colors <- c("ARR" = "#1b9e77", "DEP" = "#d95f02")

## ARRIVALS
ggplot(
  delay_data_clean |> filter(PHASE == "ARR"),
  aes(x = DELAY_MIN)
) +
  geom_histogram(
    binwidth = 2,
    fill = "#1b9e77",
    alpha = 0.6,
    color = "white"
  ) +
  geom_vline(
    data = percentile_summary |> filter(PHASE == "ARR"),
    aes(xintercept = MIN_DELAY_10TH),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    data = percentile_summary |> filter(PHASE == "ARR"),
    aes(xintercept = MAX_DELAY_90TH),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  facet_wrap(~AIRPORT, scales = "free_y", ncol = 3) +
  labs(
    title = "Arrival Delay Distribution per Airport",
    x = "Delay (minutes)",
    y = "Flight Count",
    caption = "Dashed blue = 10th percentile, red = 90th percentile"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave("arrival_buffer.png", width = 16, height = 9, dpi = 600)

## DEPARTURES
ggplot(
  delay_data_clean |> filter(PHASE == "DEP"),
  aes(x = DELAY_MIN)
) +
  geom_histogram(
    binwidth = 2,
    fill = "#d95f02",
    alpha = 0.6,
    color = "white"
  ) +
  geom_vline(
    data = percentile_summary |> filter(PHASE == "DEP"),
    aes(xintercept = MIN_DELAY_10TH),
    color = "blue",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    data = percentile_summary |> filter(PHASE == "DEP"),
    aes(xintercept = MAX_DELAY_90TH),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  facet_wrap(~AIRPORT, scales = "free_y", ncol = 3) +
  labs(
    title = "Departure Delay Distribution per Airport",
    x = "Delay (minutes)",
    y = "Flight Count",
    caption = "Dashed blue = 10th percentile, red = 90th percentile"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave("departure_buffer.png", width = 16, height = 9, dpi = 600)

percentile_summary |>
  arrange(AIRPORT, PHASE) |>
  mutate(
    MIN_DELAY_10TH = round(MIN_DELAY_10TH, 1),
    MAX_DELAY_90TH = paste0("+", round(MAX_DELAY_90TH, 1), " min")
  ) |>
  knitr::kable(
    caption = "10th and 90th Percentile Delay Thresholds per Airport and Phase",
    align = "lccc"
  )


delay_data <- ds |>
  filter(
    (ADEP %in% selected_airports & PHASE == "DEP") |
      (ADES %in% selected_airports & PHASE == "ARR")
  ) |>
  mutate(
    AIRPORT = if_else(PHASE == "DEP", ADEP, ADES),
    DELAY_MIN = as.numeric(difftime(BLOCK_TIME, SLOT_TIME, units = "mins")) # positive = delayed
  )

delay_data_clean <- delay_data |>
  filter(!is.na(DELAY_MIN)) |>
  group_by(AIRPORT, PHASE) |>
  mutate(
    lower_bound = quantile(DELAY_MIN, 0.05, na.rm = TRUE),
    upper_bound = quantile(DELAY_MIN, 0.95, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(DELAY_MIN >= lower_bound & DELAY_MIN <= upper_bound)


percentile_summary <- delay_data_clean |>
  group_by(AIRPORT, PHASE) |>
  summarise(
    MIN_DELAY_10TH = quantile(DELAY_MIN, 0.10, na.rm = TRUE),
    MAX_DELAY_90TH = quantile(DELAY_MIN, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

make_airport_plot <- function(airport, phase_color, phase_label) {
  dat <- delay_data_clean |> filter(AIRPORT == airport, PHASE == phase_label)
  percs <- percentile_summary |>
    filter(AIRPORT == airport, PHASE == phase_label)

  ggplot(dat, aes(x = DELAY_MIN)) +
    geom_histogram(
      binwidth = 2,
      fill = phase_color,
      alpha = 0.6,
      color = "white"
    ) +
    geom_vline(
      aes(xintercept = percs$MIN_DELAY_10TH),
      color = "blue",
      linetype = "dashed",
      size = 1.2
    ) +
    geom_vline(
      aes(xintercept = percs$MAX_DELAY_90TH),
      color = "red",
      linetype = "dashed",
      size = 1.2
    ) +
    labs(
      title = airport,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 26) +
    theme(
      plot.title = element_text(face = "bold", size = 32, hjust = 0.5),
      axis.text = element_text(size = 22),
      panel.grid.minor = element_blank()
    )
}

arrivals_plots <- lapply(
  selected_airports,
  make_airport_plot,
  phase_color = "#1b9e77",
  phase_label = "ARR"
)
departures_plots <- lapply(
  selected_airports,
  make_airport_plot,
  phase_color = "#d95f02",
  phase_label = "DEP"
)

combined_plot <- (wrap_plots(arrivals_plots, ncol = 6) /
  wrap_plots(departures_plots, ncol = 6)) +
  plot_annotation(
    title = "Distribution of Flight Delays per Airport",
    subtitle = "Top: Arrivals (green), Bottom: Departures (orange)\nDashed blue = 10th percentile, red = 90th percentile",
    theme = theme(
      plot.title = element_text(face = "bold", size = 42, hjust = 0.5),
      plot.subtitle = element_text(size = 26, hjust = 0.5)
    )
  )


percentile_summary |>
  arrange(AIRPORT, PHASE) |>
  mutate(
    MIN_DELAY_10TH = round(MIN_DELAY_10TH, 1),
    MAX_DELAY_90TH = paste0("+", round(MAX_DELAY_90TH, 1), " min")
  ) |>
  knitr::kable(
    caption = "10th and 90th Percentile Delay Thresholds per Airport and Phase",
    align = "lccc"
  )

cdm_airports <- c("EDDF", "EGLL", "EKCH")

allowed_buffer <- percentile_summary |>

  mutate(
    MIN_DELAY_10TH = as.numeric(MIN_DELAY_10TH),
    MAX_DELAY_90TH = as.numeric(MAX_DELAY_90TH)
  ) |>

  transmute(
    AIRPORT,
    PHASE,
    BUFFER_START_MIN = MIN_DELAY_10TH, # 10th percentile (may be negative)
    BUFFER_END_MIN = MAX_DELAY_90TH # 90th percentile (positive -> delayed)
  ) |>
  arrange(AIRPORT, PHASE) |>
  # add CDM flag
  mutate(
    CDM = if_else(AIRPORT %in% cdm_airports, "CDM", "non-CDM"),
    # pretty text for PPTX (e.g. "-9" and "+47")
    BUFFER_START_TEXT = paste0(
      ifelse(BUFFER_START_MIN > 0, "+", ""),
      round(BUFFER_START_MIN, 1)
    ),
    BUFFER_END_TEXT = paste0(
      ifelse(BUFFER_END_MIN > 0, "+", ""),
      round(BUFFER_END_MIN, 1),
      " min"
    )
  )

allowed_buffer_pptx <- allowed_buffer |>
  select(AIRPORT, PHASE, BUFFER_START_TEXT, BUFFER_END_TEXT, CDM)

kable(
  allowed_buffer_pptx,
  col.names = c("Airport", "Phase", "Buffer start", "Buffer end", "Group")
)

group_summary <- allowed_buffer |>
  group_by(CDM, PHASE) |>
  summarise(
    N_airports = n(),
    MEAN_BUFFER_START = round(mean(BUFFER_START_MIN, na.rm = TRUE), 2),
    MEDIAN_BUFFER_START = round(median(BUFFER_START_MIN, na.rm = TRUE), 2),
    MEAN_BUFFER_END = round(mean(BUFFER_END_MIN, na.rm = TRUE), 2),
    MEDIAN_BUFFER_END = round(median(BUFFER_END_MIN, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(CDM, PHASE)

kable(
  group_summary,
  col.names = c(
    "Group",
    "Phase",
    "N",
    "Mean start (min)",
    "Median start (min)",
    "Mean end (min)",
    "Median end (min)"
  )
)


# --- EDDF capacity test ---
airport_target <- "EDDF"
capacity_ref <- data.frame(
  window = c(10, 30, 60),
  capacity = c(20, 57, 106)
)

eddf_data <- ds |>
  filter(
    (ADEP == airport_target & PHASE == "DEP") |
      (ADES == airport_target & PHASE == "ARR")
  ) |>
  mutate(
    AIRPORT = airport_target
  )

make_binned_counts <- function(data, time_col, window_minutes) {
  data |>
    mutate(
      TIME_BIN = floor_date(
        !!sym(time_col),
        unit = paste0(window_minutes, " minutes")
      )
    ) |>
    group_by(TIME_BIN) |>
    summarise(FLIGHTS = n(), .groups = "drop") |>
    mutate(WINDOW = paste0(window_minutes, "min"))
}

slot_10 <- make_binned_counts(eddf_data, "SLOT_TIME", 10)
block_10 <- make_binned_counts(eddf_data, "BLOCK_TIME", 10)

slot_30 <- slot_10 |>
  mutate(FLIGHTS = FLIGHTS * 3, WINDOW = "30min")

slot_60 <- slot_10 |>
  mutate(FLIGHTS = FLIGHTS * 6, WINDOW = "60min")

block_30 <- block_10 |>
  mutate(FLIGHTS = FLIGHTS * 3, WINDOW = "30min")

block_60 <- block_10 |>
  mutate(FLIGHTS = FLIGHTS * 6, WINDOW = "60min")


slot_all <- bind_rows(slot_10, slot_30, slot_60) |> mutate(TYPE = "SLOT")
block_all <- bind_rows(block_10, block_30, block_60) |> mutate(TYPE = "BLOCK")
plot_data <- bind_rows(slot_all, block_all)

plot_data_daily <- plot_data |>
  mutate(HOUR = hour(TIME_BIN) + minute(TIME_BIN) / 60) |>
  group_by(TYPE, WINDOW, HOUR) |>
  summarise(MEDIAN_FLIGHTS = median(FLIGHTS, na.rm = TRUE), .groups = "drop")

# Base EDDF capacity and observed pattern ---
ggplot(
  plot_data_daily,
  aes(x = HOUR, y = MEDIAN_FLIGHTS, color = WINDOW, linetype = TYPE)
) +
  geom_line(size = 0.8) +
  geom_hline(
    data = capacity_ref,
    aes(yintercept = capacity, color = paste0(window, "min")),
    linetype = "dotted",
    size = 1
  ) +
  scale_color_manual(
    values = c("10min" = "green3", "30min" = "orange", "60min" = "steelblue"),
    name = "Window size"
  ) +
  scale_linetype_manual(
    values = c("SLOT" = "solid", "BLOCK" = "dashed"),
    name = "Time type"
  ) +
  labs(
    title = "EDDF Capacity Utilisation – Current Flights per 10/30/60 Min",
    x = "Hour of Day (UTC)",
    y = "Number of Flights",
    caption = "Solid = SLOT (planned), Dashed = BLOCK (actual); dotted = capacity reference"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave("EDDF_capacity.png", width = 16, height = 9, dpi = 600)

# Simulated tolerance window (-9 to +50 min) ---
simulated <- eddf_data |>
  mutate(
    SLOT_LOWER = SLOT_TIME - minutes(9),
    SLOT_UPPER = SLOT_TIME + minutes(47)
  )

time_seq <- seq(
  floor_date(min(simulated$SLOT_TIME, na.rm = TRUE), "10 minutes"),
  ceiling_date(max(simulated$SLOT_TIME, na.rm = TRUE), "10 minutes"),
  by = "10 min"
)

sim_counts <- tibble(TIME_BIN = time_seq) |>
  rowwise() |>
  mutate(
    FLIGHTS = sum(
      simulated$SLOT_LOWER <= TIME_BIN & simulated$SLOT_UPPER >= TIME_BIN,
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  mutate(
    WINDOW = "10min",
    TYPE = "Simulated",
    HOUR = hour(TIME_BIN) + minute(TIME_BIN) / 60
  )

# Impact with tolerance window ---
ggplot(sim_counts, aes(x = HOUR, y = FLIGHTS)) +
  geom_line(color = "#080", size = 1.3) +
  geom_hline(
    yintercept = capacity_ref$capacity[1],
    color = "green3",
    linetype = "dotted",
    size = 1
  ) +
  geom_hline(
    yintercept = capacity_ref$capacity[2],
    color = "orange",
    linetype = "dotted",
    size = 1
  ) +
  geom_hline(
    yintercept = capacity_ref$capacity[3],
    color = "steelblue",
    linetype = "dotted",
    size = 1
  ) +
  scale_x_continuous(breaks = 1:24, limits = c(1, 24)) +
  labs(
    title = "EDDF Capacity Stress Test (Assumed ± Window on Slot)",
    x = "Hour of Day (UTC)",
    y = "Number of Concurrent Flights",
    caption = "Assumption: Each flight may operate 9 min before to 47 min after slot. Dashed lines = capacity levels."
  ) +
  theme_minimal(base_size = 30) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave("EDDF_predicted_big.png", width = 20, height = 10, dpi = 600)


#### A-CDM and non-CDM ####

set.seed(2024)

data <- ds |> as_tibble()

data <- data |>
  mutate(
    AIRPORT = if_else(PHASE == "DEP", ADEP, ADES),
    # compute minutes since midnight for relevant times where possible
    SLOT_min = as.numeric(difftime(
      SLOT_TIME,
      floor_date(SLOT_TIME, "day"),
      units = "mins"
    )),
    BLOCK_min = as.numeric(difftime(
      BLOCK_TIME,
      floor_date(BLOCK_TIME, "day"),
      units = "mins"
    )),
    ACT_min = as.numeric(difftime(
      ACTUAL,
      floor_date(ACTUAL, "day"),
      units = "mins"
    ))
  )

data <- data |>
  mutate(DEV_MIN = as.numeric(difftime(BLOCK_TIME, SLOT_TIME, units = "mins")))

cdm_airports <- c("EDDF", "EGLL", "EKCH")
data <- data |>
  mutate(CDM = if_else(AIRPORT %in% cdm_airports, "CDM", "non-CDM"))

#ACCEPTED deviations
#Change to outliers and 80%
if (!"ACCEPTED" %in% names(data)) {
  airport_bounds <- data |>
    filter(!is.na(DEV_MIN)) |>
    group_by(AIRPORT) |>
    summarise(
      p10 = quantile(DEV_MIN, 0.15, na.rm = TRUE),
      p90 = quantile(DEV_MIN, 0.85, na.rm = TRUE),
      .groups = "drop"
    )
  data <- data |>
    left_join(airport_bounds, by = "AIRPORT") |>
    mutate(
      ACCEPTED = case_when(
        is.na(DEV_MIN) ~ NA, # keep NA if no dev
        DEV_MIN >= p10 & DEV_MIN <= p90 ~ 1L,
        TRUE ~ 0L
      )
    ) |>
    select(-p10, -p90)
}

if (is.logical(data$ACCEPTED))
  data <- data |> mutate(ACCEPTED = as.integer(ACCEPTED))

# Descriptive summaries

summary_by_cdm <- data |>
  filter(!is.na(DEV_MIN)) |>
  group_by(CDM) |>
  summarise(
    n = n(),
    mean_dev = mean(DEV_MIN, na.rm = TRUE),
    median_dev = median(DEV_MIN, na.rm = TRUE),
    sd_dev = sd(DEV_MIN, na.rm = TRUE),
    mean_abs_dev = mean(abs(DEV_MIN), na.rm = TRUE),
    median_abs_dev = median(abs(DEV_MIN), na.rm = TRUE),
    accept_rate = mean(ACCEPTED, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(CDM)

summary_by_cdm |>
  select(CDM, n, mean_dev, median_dev, sd_dev, accept_rate) |>
  arrange(CDM, n, mean_dev, median_dev, sd_dev) |>
  knitr::kable(
    digits = 2,
    caption = "Summary statistics by group (CDM vs non-CDM)"
  )


write_csv(summary_by_cdm, "summary_by_cdm.csv")

# Violin
ggplot(data |> filter(!is.na(DEV_MIN)), aes(x = CDM, y = DEV_MIN)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.8) +
  labs(
    title = "Distribution of DEV_MIN by CDM status",
    y = "DEV_MIN (minutes)",
    x = "CDM"
  ) +
  theme_minimal(base_size = 18)

ggsave("DEV_min_dis.png", width = 16, height = 9, dpi = 600)


# Hypothesis testing: CDM vs non-CDM
# DEV_MIN: normality to decide t-test vs Wilcoxon
shapiro_by_group <- data |>
  filter(!is.na(DEV_MIN)) |>
  group_by(CDM) |>
  summarise(
    p_shapiro = tryCatch(
      shapiro.test(sample(DEV_MIN, min(length(DEV_MIN), 5000)))$p.value,
      error = NA_real_
    ),
    .groups = "drop"
  )
print(shapiro_by_group)
write_csv(shapiro_by_group, "shapiro_devmin_by_cdm.csv")

# Two-sample test for DEV_MIN (non-paired)
dev_cdm <- data |> filter(!is.na(DEV_MIN), CDM == "CDM") |> pull(DEV_MIN)
dev_noncdm <- data |> filter(!is.na(DEV_MIN), CDM == "non-CDM") |> pull(DEV_MIN)

# Wilcoxon if non-normal, otherwise t-test

wilcox_dev <- tryCatch(wilcox.test(dev_cdm, dev_noncdm), error = NULL)

wilcox_dev

# For absolute deviations (magnitude) - likely more relevant for "adherence"
abs_dev_cdm <- data |>
  filter(!is.na(DEV_MIN), CDM == "CDM") |>
  pull(abs(DEV_MIN))
abs_dev_noncdm <- data |>
  filter(!is.na(DEV_MIN), CDM == "non-CDM") |>
  pull(abs(DEV_MIN))

wilcox_abs <- tryCatch(wilcox.test(abs_dev_cdm, abs_dev_noncdm), error = NULL)

wilcox_abs

# Acceptance rate comparison (proportions)
prop_table <- data |>
  filter(!is.na(ACCEPTED)) |>
  group_by(CDM) |>
  summarise(
    n = n(),
    accepted = sum(ACCEPTED, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(prop = accepted / n)
prop_table
# 2-sample proportion test
prop_test <- prop.test(x = prop_table$accepted, n = prop_table$n)
prop_test

write_csv(prop_table, "acceptance_prop_by_cdm.csv")

data_sel <- data |>
  filter(AIRPORT %in% selected_airports)

# ANOVA: is 'airport' significant and does CDM explain variance?
lm1 <- lm(
  DEV_MIN ~ CDM + AIRPORT + PHASE,
  data = data_sel |> filter(!is.na(DEV_MIN))
)
summary(lm1)

anova_lm1 <- anova(lm1)
tidy(anova_lm1) |> write_csv("anova_devmin.csv")
anova_lm1


#  Regression models
# Linear regression predicting absolute deviation magnitude
lm2 <- lm(
  abs(DEV_MIN) ~ CDM + PHASE + factor(hour(SLOT_TIME)) + RWY_TIME + AIRPORT,
  data = data_sel |> filter(!is.na(DEV_MIN))
)
summary(lm2)
tidy(lm2) |> write_csv("lm_abs_dev_coefficients.csv")

# Logistic regression: predict ACCEPTED (1) vs NOT (0)
log_data <- data_sel |>
  filter(!is.na(ACCEPTED)) |>
  mutate(
    HOUR = hour(SLOT_TIME),
    PHASE = factor(PHASE),
    CDM = factor(CDM),
    AIRPORT = factor(AIRPORT)
  ) |>
  select(
    ACCEPTED,
    CDM,
    PHASE,
    HOUR,
    RWY_TIME,
    DAY_OF_OPERATION,
    AIRPORT,
    DEV_MIN
  ) |>
  drop_na()

glm1 <- glm(
  ACCEPTED ~ CDM + PHASE + HOUR + RWY_TIME,
  data = log_data,
  family = binomial
)
summary(glm1)
tidy(glm1) |> write_csv("glm_accepted_coeffs.csv")

# Predict & ROC for logistic
log_probs <- predict(glm1, type = "response")
roc_obj <- roc(log_data$ACCEPTED, log_probs)
auc_val <- auc(roc_obj)
print(auc_val)

roc_df <- tibble(
  sens = rev(roc_obj$sensitivities),
  spec = rev(roc_obj$specificities),
  thresh = rev(roc_obj$thresholds)
)
write_csv(roc_df, "glm_roc.csv")

# Clustering:
clust_df <- data_sel |>
  filter(!is.na(DEV_MIN) & !is.na(ACT_min)) |>
  mutate(
    abs_dev = abs(DEV_MIN),
    ACT_minus_BLOCK = ACT_min - BLOCK_min
  ) |>
  dplyr::select(AIRPORT, CDM, DEV_MIN, abs_dev, ACT_minus_BLOCK) |>
  drop_na()

clust_scaled <- clust_df |>
  dplyr::select(DEV_MIN, abs_dev, ACT_minus_BLOCK) |>
  scale() |>
  as_tibble()


set.seed(42)
sample_idx <- sample(nrow(clust_scaled), 2000)
clust_scaled_sample <- clust_scaled[sample_idx, ]

sil <- map_dbl(2:5, function(k) {
  km <- kmeans(clust_scaled_sample, centers = k, nstart = 25)
  ss <- cluster::silhouette(km$cluster, dist(clust_scaled_sample))
  mean(ss[, 3])
})

sil_df <- tibble(k = 2:5, sil = sil)
print(sil_df)

k_opt <- sil_df$k[which.max(sil_df$sil)]
k_opt

km_final <- kmeans(clust_scaled, centers = k_opt, nstart = 50)
clust_df <- clust_df |> mutate(cluster = factor(km_final$cluster))

fviz_cluster(
  list(data = clust_scaled, cluster = km_final$cluster),
  geom = "point"
) +
  ggtitle(paste("K-means clusters (k =", k_opt, ")")) +
  theme_minimal(base_size = 18)

ggsave("kmeans_clusters.png", width = 16, height = 9, dpi = 600)

# Outlier detection
outliers <- data_sel |>
  filter(!is.na(DEV_MIN)) |>
  group_by(AIRPORT) |>
  mutate(
    Q1 = quantile(DEV_MIN, 0.25, na.rm = TRUE),
    Q3 = quantile(DEV_MIN, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    outlier_IQR = (DEV_MIN < (Q1 - 1.5 * IQR)) | (DEV_MIN > (Q3 + 1.5 * IQR)),
    outlier_MAD = abs(DEV_MIN - median(DEV_MIN, na.rm = TRUE)) >
      3 * mad(DEV_MIN, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(outlier_IQR | outlier_MAD) |>
  dplyr::select(AIRPORT, ADEP, ADES, PHASE, DEV_MIN, outlier_IQR, outlier_MAD)

write_csv(outliers, "outliers_devmin.csv")
n_outliers <- outliers |> group_by(AIRPORT) |> summarise(n = n())
print(n_outliers)


# Random Forest: Does CDM pay off? (Optimized version)
rf_data <- data |>
  filter(!is.na(ACCEPTED)) |>
  mutate(
    ACCEPTED = factor(ACCEPTED, levels = c(0, 1)),
    HOUR = lubridate::hour(SLOT_TIME),
    PHASE = factor(PHASE),
    CDM = factor(CDM),
    AIRPORT = factor(AIRPORT)
  ) |>
  dplyr::select(
    ACCEPTED,
    CDM,
    PHASE,
    HOUR,
    RWY_TIME,
    DEV_MIN,
    ACT_min,
    AIRPORT
  ) |>
  drop_na()

# Train/Test Split (70/30)
set.seed(123)
train_index <- createDataPartition(rf_data$ACCEPTED, p = 0.7, list = FALSE)
train_rf <- rf_data[train_index, ]
test_rf <- rf_data[-train_index, ]

# caret expects "yes"/"no" factors for binary classification
train_rf <- train_rf |>
  mutate(
    ACCEPTED = if_else(ACCEPTED == "1", "yes", "no") |>
      factor(levels = c("no", "yes"))
  )
test_rf <- test_rf |>
  mutate(
    ACCEPTED = if_else(ACCEPTED == "1", "yes", "no") |>
      factor(levels = c("no", "yes"))
  )

# Parallelization
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# Control and Model Training
control <- trainControl(
  method = "cv", # simpler, faster cross-validation
  number = 3, # 3-fold CV is fine for large datasets
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

set.seed(42)

rf_fit <- ranger(
  ACCEPTED ~ .,
  data = train_rf,
  num.trees = 100,
  mtry = 3,
  importance = "impurity",
  probability = TRUE,
  respect.unordered.factors = TRUE
)

# Variable Importance -
varImp_rf <- data.frame(
  variable = names(rf_fit$variable.importance),
  importance = rf_fit$variable.importance
) |>
  arrange(desc(importance))

print(varImp_rf)

# Predictions and Performance
pred_probs <- predict(rf_fit, data = test_rf)$predictions[, "yes"]
pred_labels <- ifelse(pred_probs > 0.5, "yes", "no") |>
  factor(levels = c("no", "yes"))

# Confusion matrix
conf_mat <- confusionMatrix(pred_labels, test_rf$ACCEPTED, positive = "yes")
print(conf_mat)

# ROC and AUC
roc_rf <- roc(if_else(test_rf$ACCEPTED == "yes", 1, 0), pred_probs)
auc_val <- as.numeric(auc(roc_rf))


# Save Performance Summary
rf_perf <- tibble(
  Accuracy = conf_mat$overall["Accuracy"],
  Kappa = conf_mat$overall["Kappa"],
  Sensitivity = conf_mat$byClass["Sensitivity"],
  Specificity = conf_mat$byClass["Specificity"],
  AUC = auc_val
)

print(rf_perf)

# Plots
# Variable importance
ggplot(varImp_rf, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance", x = "", y = "Importance") +
  theme_minimal(base_size = 18)
ggsave("rf_variable_importance.png", width = 16, height = 9, dpi = 600)

# ROC curve
plot(roc_rf, col = "#2C7BB6", lwd = 3, main = "ROC Curve: Does CDM Pay Off?")
abline(a = 0, b = 1, lty = 2, col = "gray")
ggsave("rf_roc_curve.png", width = 16, height = 9, dpi = 600)

# Console Summary
cat("\n--- Random Forest (ranger) Results ---\n")
print(rf_perf)
cat("\nTop 10 Important Variables:\n")
print(head(varImp_rf, 10))
cat("\nAUC =", round(auc_val, 3), "\n")

# Interpretation / reporting helpers
results_summary <- list(
  descriptive = summary_by_cdm,
  t_test_dev = if (!is.null(t_test_dev)) tidy(t_test_dev) else NULL,
  wilcox_dev = if (!is.null(wilcox_dev)) broom::tidy(wilcox_dev) else NULL,
  t_test_abs = if (!is.null(t_test_abs)) tidy(t_test_abs) else NULL,
  wilcox_abs = if (!is.null(wilcox_abs)) broom::tidy(wilcox_abs) else NULL,
  proportion_test = tidy(prop_test),
  anova = tidy(anova_lm1),
  lm_abs = tidy(lm2),
  glm = tidy(glm1),
  rf_performance = rf_perf
)

# VISUALIZE ADHERENCE PERFORMANCE: CDM vs NON-CDM
theme_set(theme_minimal(base_size = 18))

plot_df <- data |>
  filter(!is.na(DEV_MIN), !is.na(ACCEPTED)) |>
  mutate(abs_dev = abs(DEV_MIN))

# Histogram / density: adherence deviation distribution
ggplot(plot_df, aes(x = DEV_MIN, fill = CDM)) +
  geom_histogram(
    aes(y = ..density..),
    alpha = 0.5,
    position = "identity",
    bins = 80
  ) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Adherence Distribution by A-CDM vs Non-CDM Airports",
    x = "Deviation from SLOT (BLOCK − SLOT) [min]",
    y = "Density"
  ) +
  scale_fill_manual(values = c("CDM" = "#1b9e77", "non-CDM" = "#d95f02")) +
  theme(legend.position = "top")
ggsave("CDM_vs_nonCDM_distribution.png", width = 14, height = 8, dpi = 600)

# Boxplot of absolute deviations
ggplot(plot_df, aes(x = CDM, y = abs_dev, fill = CDM)) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.size = 0.5) +
  labs(
    title = "Adherence Performance by CDM Category",
    x = "Airport Category",
    y = "|Deviation| (minutes)"
  ) +
  scale_fill_manual(values = c("CDM" = "#66c2a5", "non-CDM" = "#fc8d62")) +
  theme(legend.position = "none")
ggsave("CDM_vs_nonCDM_boxplot.png", width = 10, height = 7, dpi = 600)

# Acceptance rate comparison
accept_summary <- plot_df |>
  group_by(CDM) |>
  summarise(Acceptance_Rate = mean(ACCEPTED, na.rm = TRUE), n = n()) |>
  ungroup()

ggplot(accept_summary, aes(x = CDM, y = Acceptance_Rate, fill = CDM)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = percent(Acceptance_Rate, accuracy = 0.1)),
    vjust = -0.3,
    size = 5
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Acceptance Rate (within operational buffer)",
    x = "Airport Category",
    y = "Accepted Flights (%)"
  ) +
  scale_fill_manual(values = c("CDM" = "#1b9e77", "non-CDM" = "#d95f02")) +
  theme(legend.position = "none")
ggsave("CDM_vs_nonCDM_acceptance_rate.png", width = 10, height = 7, dpi = 600)

# Supplementary summary table for reporting
summary_plot_table <- plot_df |>
  group_by(CDM) |>
  summarise(
    n = n(),
    Mean_DEV = round(mean(DEV_MIN, na.rm = TRUE), 2),
    Median_DEV = round(median(DEV_MIN, na.rm = TRUE), 2),
    Mean_abs_DEV = round(mean(abs_dev, na.rm = TRUE), 2),
    Acceptance_Rate = round(mean(ACCEPTED, na.rm = TRUE) * 100, 1)
  )

knitr::kable(
  summary_plot_table,
  caption = "Adherence Performance Summary by CDM Category"
)
