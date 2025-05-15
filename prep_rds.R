require(tidyverse)

load("top20_bib.RData")
load("words_by_journal_year.RData")
load("top20_word_appear_by_year.RData")
load("top20_word_topic_by_year.RData")

words_each_year <- top20_bib |>
  group_by(year) |>
  summarise(all_words = sum(wordcount))

articles_each_year <- top20_bib |>
  group_by(year) |>
  tally(name = "all_articles")

word_data <- words_by_journal_year |>
  group_by(word) |>
  filter(sum(wordcount) >= 500) |>
  ungroup() |>
  group_by(word, year) |>
  summarise(count = sum(wordcount), .groups = "drop") |>
  left_join(words_each_year, by = "year") |>
  mutate(value = count * 1000/all_words) |>
  select(-count, -all_words) |>
  mutate(type = "frequency")

appear_data <- top20_word_appear_by_year |>
  filter(word %in% word_data$word) |>
  ungroup() |>
  complete(word, year, fill = list(appear = 0)) |>
  left_join(articles_each_year, by = "year") |>
  mutate(value = appear * 100 / all_articles) |>
  select(-appear, -all_articles) |>
  mutate(type = "appear")
  
topic_data <- top20_word_topic_by_year |>
  filter(word %in% word_data$word) |>
  ungroup() |>
  complete(word, year, fill = list(topic = 0)) |>
  left_join(articles_each_year, by = "year") |>
  mutate(value = topic * 100 / all_articles) |>
  select(-topic, -all_articles) |>
  mutate(type = "topic")

word_data <- bind_rows(
  word_data,
  appear_data,
  topic_data) |>
  filter(word %in% topic_data$word)

short_data <- word_data |>
  group_by(word) |>
  filter(sum(value) >= 4000) |>
  ungroup()

journal_summary <- top20_bib |>
  group_by(journal) |>
  summarise(Articles = n_distinct(id),
            Words = sum(wordcount)) |>
  ungroup() |>
  rename(Journal = journal)

word_list <- (word_data |>
                group_by(word) |>
                tally())$word

saveRDS(word_data, "t20-graphs/word_data.rds")
saveRDS(word_list, "t20-graphs/word_list.rds")