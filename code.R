# setwd("/home/agricolamz/work/articles/2025_russian_borrowings_in_andi_with_Vika_Chiara")
library(tidyverse)

df <- read_csv("avar_andic_rus_ranks.csv")

# ca analysis -------------------------------------------------------------

df |> 
  filter(is.na(derived)) |> 
  select(language, reference, russian_ipa, target_ipa) |>
  mutate(language_ref = str_c(language, ": ", reference),
         russian_ipa = str_to_lower(russian_ipa),
         russian_ipa = str_remove_all(russian_ipa, "'"),
         target_ipa = str_remove_all(target_ipa, "'"),
         russian_ipa = str_split(russian_ipa, "-"),
         target_ipa = str_split(target_ipa, "-")) |> 
  unnest(c(russian_ipa, target_ipa)) |> 
  count(language_ref, russian_ipa, target_ipa) |> 
  mutate(corresp = str_c(russian_ipa, ":", target_ipa)) |> 
  select(-russian_ipa, -target_ipa) |> 
  distinct() |> 
  na.omit() |> 
  pivot_wider(names_from = corresp, values_from = n, values_fill = 0) |> 
  column_to_rownames("language_ref") |> 
  ca::ca() ->
  ca

ca$rowcoord |> 
  as.data.frame() |> 
  rownames_to_column("settlement") |> 
  ggplot(aes(Dim1, Dim2, label = settlement))+
  geom_point()+
  ggrepel::geom_label_repel()+
  theme_minimal()


df |> 
  filter(is.na(derived)) |> 
  select(language, reference, russian_ipa, target_ipa, meaning_ru) |>
  distinct() |> 
  group_by(language, reference,  meaning_ru) |> 
  mutate(meaing_id = 1:n()) |> 
  slice_sample(n = 1) |> 
  mutate(language_ref = str_c(language, ": ", reference),
         russian_ipa = str_to_lower(russian_ipa),
         russian_ipa = str_remove_all(russian_ipa, "'"),
         target_ipa = str_remove_all(target_ipa, "'"),
         russian_ipa = str_split(russian_ipa, "-"),
         target_ipa = str_split(target_ipa, "-")) |> 
  unnest(c(russian_ipa, target_ipa)) |> 
  add_count(language_ref, reference, meaning_ru) |> 
  rename(total = n) |> 
  filter(russian_ipa != target_ipa) |> 
  add_count(language_ref, meaning_ru) |> 
  rename(changes = n) ->
  for_modeling

library(lme4)
for_modeling |> 
  mutate(ratio = changes/total) |> 
  group_by(language_ref) |> 
  mutate(median_value = median(ratio)) |> 
  ungroup() |> 
  mutate(language_ref = fct_reorder(language_ref, -median_value)) |> 
  ggplot(aes(ratio, language_ref, fill = language_ref))+
  ggridges::geom_density_ridges(alpha = 0.5, show.legend = FALSE)+
  theme_minimal()+
  labs(y = NULL)

# extract word frequency from Google ngrams -------------------------------

df |> 
  distinct(meaning_ru) |> 
  arrange(meaning_ru) |> 
  filter(meaning_ru != "алжир/ец, -ка") |>
# в пакете ограничение на 12 запросов, так что делим на группы по 12 лемм
  mutate(group = rep(1:ceiling(n()/12), each = 12)[1:n()]) ->
  words_samples

library(ngramr)

words_samples |> 
  distinct(group) |> 
  pull(group) |> 
  walk(.progress = TRUE, 
      function(i){
        words_samples |> 
          filter(group == i) |> 
          pull(meaning_ru) |> 
          ngram(corpus = "ru-2019", smoothing = 30, year_start = 1800) |> 
          mutate(group = i) |> 
          write_csv("google_ngram_frequency.csv", na = "", append = TRUE)
        Sys.sleep(5)
  })

ngrams <- read_csv("google_ngram_frequency.csv", col_names = c("year", "meaning_ru", "frequency", "corpus", "group"))

words_samples[!(words_samples$meaning_ru %in% unique(ngrams$meaning_ru)),] |> 
  filter(str_detect(meaning_ru, "ё")) |> 
  mutate(group = rep(1:ceiling(n()/12), each = 12)[1:n()],
         meaning_ru = str_replace(meaning_ru, "ё", "е")) ->
  research_with_yo2ye

research_with_yo2ye |> 
  distinct(group) |> 
  pull(group) |> 
  walk(.progress = TRUE, 
       function(i){
         research_with_yo2ye |> 
           filter(group == i) |> 
           pull(meaning_ru) |> 
           ngram(corpus = "ru-2019", smoothing = 30, year_start = 1800) |> 
           mutate(group = i) |> 
           write_csv("google_ngram_frequency.csv", na = "", append = TRUE)
         Sys.sleep(5)
       })

ngrams <- read_csv("google_ngram_frequency.csv", col_names = c("year", "meaning_ru", "frequency", "corpus", "group"))

ngrams |> 
  filter(meaning_ru %in% c("елка", "щетка", "пролетарий", "телефон", "вафля", "колхоз")) |> 
  distinct(year, meaning_ru, frequency, corpus) |> 
  group_by(meaning_ru) |> 
  mutate(sum = cumsum(frequency)) |> 
  ggplot(aes(year, sum))+
  geom_line()+
  facet_wrap(~meaning_ru, scales = "free")

ngrams |> 
#  filter(meaning_ru %in% sample(unique(ngrams$meaning_ru), 6)) |> 
  distinct(year, meaning_ru, frequency, corpus) |> 
  group_by(meaning_ru) |> 
  mutate(sum = cumsum(frequency),
         differ = c(NA, diff(sum)),
         differ2 = c(NA, diff(differ)),
         sd_differ2 = sd(differ2, na.rm = TRUE)) |> 
  na.omit() |> 
  ggplot(aes(sd_differ2))+
  geom_histogram()+
  scale_y_log10()

dfor_modeling |> 
  left_join(result |> distinct(meaning_ru, year_break)) |> 
  mutate(ratio = changes/total) |> 
  ggplot(aes(year_break, ratio))+
  #geom_point(size = 0.1)+
  geom_hex()+
  scale_fill_gradient(low = "navy", high = "tomato")
