library(dplyr)
library(DT)
library(scales)
library(shiny)
library(shinyWidgets)
library(tidyr)

load("www/data.RData")
source("www/functions.R")

# create state a loss triangle
state_a_triangle <- state_a_losses %>%
  pivot_wider(names_from = "MONTHS",
              values_from = "LOSS",
              names_prefix = "MONTHS_") %>%
  as.data.frame()

# create state a development triangle
state_a_triangle_dev <- state_a_triangle %>%
  transmute(YEAR,
            MONTHS_12_24 = round(MONTHS_24 / MONTHS_12, 3),
            MONTHS_24_36 = round(MONTHS_36 / MONTHS_24, 3),
            MONTHS_36_48 = round(MONTHS_48 / MONTHS_36, 3),
            MONTHS_48_60 = round(MONTHS_60 / MONTHS_48, 3),
            MONTHS_60_72 = round(MONTHS_72 / MONTHS_60, 3),
            MONTHS_72_84 = round(MONTHS_84 / MONTHS_72, 3),
            MONTHS_84_96 = round(MONTHS_96 / MONTHS_84, 3),
            MONTHS_96_108 = round(MONTHS_108 / MONTHS_96, 3),
            MONTHS_108_120 = round(MONTHS_120 / MONTHS_108, 3)) %>%
  filter(!(is.na(MONTHS_12_24) & YEAR == max(YEAR)))

# determine the heat map colors for state a
state_a_triangle_dev <- add_highlighting(state_a_triangle_dev)
state_a_triangle_dev$COLOR_USE <- FALSE

# create the state a average factors table
state_a_triangle_summ <- state_a_triangle %>%
  pivot_longer(cols = c("MONTHS_12", "MONTHS_24", "MONTHS_36", "MONTHS_48", "MONTHS_60", "MONTHS_72", "MONTHS_84", "MONTHS_96", "MONTHS_108", "MONTHS_120"),
               names_prefix = "MONTHS_",
               names_to = "MONTHS",
               values_to = "LOSS") %>%
  mutate(MONTHS = as.numeric(MONTHS)) %>%
  group_by(YEAR) %>%
  mutate(MONTHS_PRIOR = lag(MONTHS),
         LOSS_PRIOR = lag(LOSS)) %>%
  filter(!is.na(LOSS),
         MONTHS != 12) %>%
  mutate(FACTOR = round(LOSS / LOSS_PRIOR, 3)) %>%
  group_by(MONTHS) %>%
  mutate(PRD = rank(desc(YEAR))) %>%
  summarise(TWO_YEAR = round(mean(FACTOR[PRD <= 2]), 3),
            THREE_YEAR = round(mean(FACTOR[PRD <= 3]), 3),
            FIVE_YEAR = round(mean(FACTOR[PRD <= 5]), 3),
            ALL_YEAR = round(mean(FACTOR), 3)) %>%
  pivot_longer(cols = c("TWO_YEAR", "THREE_YEAR", "FIVE_YEAR", "ALL_YEAR"),
               names_to = "AVG_METHOD",
               values_to = "AVERAGE") %>%
  mutate(LINK = paste0("MONTHS_", MONTHS - 12, "_", MONTHS)) %>%
  select(-MONTHS) %>%
  pivot_wider(names_from = "LINK",
              values_from = "AVERAGE") %>%
  mutate(AVG_METHOD = c("Two Year", "Three Year", "Five Year", "All Year"))


# create state b loss triangle
state_b_triangle <- state_b_losses %>%
  pivot_wider(names_from = "MONTHS",
              values_from = "LOSS",
              names_prefix = "MONTHS_") %>%
  as.data.frame()

# create state b development triangle
state_b_triangle_dev <- state_b_triangle %>%
  transmute(YEAR,
            MONTHS_12_24 = round(MONTHS_24 / MONTHS_12, 3),
            MONTHS_24_36 = round(MONTHS_36 / MONTHS_24, 3),
            MONTHS_36_48 = round(MONTHS_48 / MONTHS_36, 3),
            MONTHS_48_60 = round(MONTHS_60 / MONTHS_48, 3),
            MONTHS_60_72 = round(MONTHS_72 / MONTHS_60, 3),
            MONTHS_72_84 = round(MONTHS_84 / MONTHS_72, 3),
            MONTHS_84_96 = round(MONTHS_96 / MONTHS_84, 3),
            MONTHS_96_108 = round(MONTHS_108 / MONTHS_96, 3),
            MONTHS_108_120 = round(MONTHS_120 / MONTHS_108, 3)) %>%
  filter(!(is.na(MONTHS_12_24) & YEAR == max(YEAR)))

# determine the heat map colors for state b
state_b_triangle_dev <- add_highlighting(state_b_triangle_dev)
state_b_triangle_dev$COLOR_USE <- FALSE

# create the state b average factors table
state_b_triangle_summ <- state_b_triangle %>%
  pivot_longer(cols = c("MONTHS_12", "MONTHS_24", "MONTHS_36", "MONTHS_48", "MONTHS_60", "MONTHS_72", "MONTHS_84", "MONTHS_96", "MONTHS_108", "MONTHS_120"),
               names_prefix = "MONTHS_",
               names_to = "MONTHS",
               values_to = "LOSS") %>%
  mutate(MONTHS = as.numeric(MONTHS)) %>%
  group_by(YEAR) %>%
  mutate(MONTHS_PRIOR = lag(MONTHS),
         LOSS_PRIOR = lag(LOSS)) %>%
  filter(!is.na(LOSS),
         MONTHS != 12) %>%
  mutate(FACTOR = round(LOSS / LOSS_PRIOR, 3)) %>%
  group_by(MONTHS) %>%
  mutate(PRD = rank(desc(YEAR))) %>%
  summarise(TWO_YEAR = round(mean(FACTOR[PRD <= 2]), 3),
            THREE_YEAR = round(mean(FACTOR[PRD <= 3]), 3),
            FIVE_YEAR = round(mean(FACTOR[PRD <= 5]), 3),
            ALL_YEAR = round(mean(FACTOR), 3)) %>%
  pivot_longer(cols = c("TWO_YEAR", "THREE_YEAR", "FIVE_YEAR", "ALL_YEAR"),
               names_to = "AVG_METHOD",
               values_to = "AVERAGE") %>%
  mutate(LINK = paste0("MONTHS_", MONTHS - 12, "_", MONTHS)) %>%
  select(-MONTHS) %>%
  pivot_wider(names_from = "LINK",
              values_from = "AVERAGE") %>%
  mutate(AVG_METHOD = c("Two Year", "Three Year", "Five Year", "All Year"))
