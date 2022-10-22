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

print(state_a_triangle_dev, width = 180)
