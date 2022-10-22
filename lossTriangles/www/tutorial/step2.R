state_a_triangle <- state_a_losses %>%
  pivot_wider(names_from = "MONTHS",
              values_from = "LOSS",
              names_prefix = "MONTHS_") %>%
  as.data.frame()

print(state_a_triangle, width = 180)
