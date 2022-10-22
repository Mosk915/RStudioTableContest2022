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
  mutate(AVG_METHOD = c("Two Year", "Three Year", "Five Year", "All Year")) %>%
  as.data.frame()

print(state_a_triangle_summ, width = 180)
