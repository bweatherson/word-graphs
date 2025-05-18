century_splits <- word_data |>
  mutate(century = str_sub(year, start = 1, end = 2)) |>
  group_by(word, type, century) |>
  summarise(value = mean(value), .groups = "drop") |>
  ungroup() |>
  group_by(word, type) |>
  mutate(surplus = value - mean(value)) |>
  arrange(-surplus)