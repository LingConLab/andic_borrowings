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

for_modeling |> 
  mutate(ratio = changes/total) |> 
  group_by(language_ref) |> 
  mutate(mean_value = mean(ratio)) |> 
  ungroup() |> 
  mutate(language_ref = fct_reorder(language_ref, -mean_value)) |> 
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
  mutate(group = rep(1:ceiling(n()/12), each = 12)[1:n()],
         meaning_ru = str_replace(meaning_ru, "ё", "е")) ->
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

for_modeling |> 
  left_join(result |> distinct(meaning_ru, year_break)) |> 
  mutate(ratio = changes/total) |> 
  ggplot(aes(year_break, ratio))+
  #geom_point(size = 0.1)+
  geom_hex()+
  scale_fill_gradient(low = "navy", high = "tomato")


ngrams |> 
  distinct(meaning_ru) |> 
  pull(meaning_ru) |> 
  walk(.progress = TRUE,
       function(meaning){
    ngrams |> 
      filter(meaning_ru == meaning) |> 
      distinct(year, meaning_ru, frequency, corpus) |> 
      group_by(meaning_ru) |> 
      mutate(sum = cumsum(frequency),
             differ = c(NA, diff(sum)),
             differ2 = c(NA, diff(differ)),
             sd_differ2 = sd(differ2, na.rm = TRUE)) |> 
      na.omit() |> 
      select(-corpus, -differ) |> 
      pivot_longer(names_to = "type", values_to = "value", frequency:differ2) |> 
      mutate(type = factor(type, levels = c("frequency", "sum", "differ2"))) ->
      for_plot
    
    for_plot |> 
      ggplot(aes(year, value))+
      geom_line()+
      facet_wrap(~type, scales = "free")+
      labs(title = meaning, subtitle = unique(for_plot$sd_differ2)) ->
      plot2save

    str_c(unique(for_plot$sd_differ2)*1000000 |> round(), "_", meaning, ".png") |>     
      ggsave(filename = _, 
             plot = plot2save, 
             path = "google_n_gram_pics",
             width = 9, 
             height = 7,
             bg = "white")
  })

ngrams |> 
  distinct(year, meaning_ru, frequency, corpus) |> 
  group_by(meaning_ru) |> 
  mutate(sum = cumsum(frequency)) |> 
  ggplot(aes(year, sum, group = meaning_ru))+
  geom_line(alpha = 0.1, linewidth = 0.1)+
  theme_minimal()+
  scale_y_log10()


ngrams |> 
  distinct(year, meaning_ru, frequency, corpus) |> 
  group_by(meaning_ru) |> 
  mutate(sum = cumsum(frequency)+0.000000000000001) ->
  for_plot

for_plot |> 
  ggplot(aes(year, sum, group = meaning_ru))+
  geom_line(alpha = 0.1, linewidth = 0.1)+
  geom_line(data = for_plot |> filter(meaning_ru == "колхоз"), color = "red")+
  theme_minimal()+
  scale_y_log10()+
  labs(title = "колхоз")

for_plot |> 
  group_by(meaning_ru) |> 
  filter(sum >= 1e-06) |> 
  slice(1) ->
  cross_range
  
ngrams |> 
  distinct(meaning_ru) |> 
  pull(meaning_ru) |> 
  walk(.progress = TRUE,
       function(meaning){
         cross_range |> 
           filter(meaning_ru == meaning) |> 
           pull(year) ->
           year_of_crossing
         
         for_plot |> 
           ggplot(aes(year, sum, group = meaning_ru))+
           geom_line(alpha = 0.1, linewidth = 0.1)+
           geom_line(data = for_plot |> filter(meaning_ru == meaning), color = "red")+
           geom_hline(yintercept = 1e-08, linetype = 2)+
           theme_minimal()+
           scale_y_log10()+
           labs(title = meaning,
                subtitle =  year_of_crossing) ->
           plot2save
         
         str_c(year_of_crossing, "_", meaning, ".png") |>     
           ggsave(filename = _, 
                  plot = plot2save, 
                  path = "google_n_gram_pics",
                  width = 9, 
                  height = 7,
                  bg = "white")
       })


# betareg

# regression --------------------------------------------------------------

ngrams <- read_csv("google_ngram_frequency.csv", col_names = c("year", "meaning_ru", "frequency", "corpus", "group"))

ngrams |> 
  distinct(year, meaning_ru, frequency, corpus) |> 
  group_by(meaning_ru) |> 
  mutate(sum = cumsum(frequency)) |> 
  filter(sum >= 1e-06) |> 
  group_by(meaning_ru) |> 
  slice(1) |> 
  select(year, meaning_ru) ->
  cross_range

# df |> 
#   mutate(r_m = str_count(russian_ipa, "-"),
#          t_m = str_count(target_ipa, "-")) |> 
#   filter(r_m != t_m) |> 
#   View()

df |> 
  filter(is.na(derived)) |> 
  select(language, reference, russian_ipa, target_ipa, meaning_ru, metathesis) |>
  distinct() |> 
  group_by(language, reference,  meaning_ru) |> 
  mutate(meaing_id = 1:n(),
         metathesis = if_else(is.na(metathesis), 0, 1)) |> 
  slice_sample(n = 1) |> 
  mutate(language_ref = str_c(language, ": ", reference),
         russian_ipa = str_remove_all(russian_ipa, "'"),
         russian_ipa = str_split(russian_ipa, "-"),
         target_ipa = str_split(target_ipa, "-")) |> 
  unnest(c(russian_ipa, target_ipa)) |> 
  add_count(language_ref, reference, meaning_ru) |> 
  rename(total = n) |> 
  # palatalisation
  mutate(russian_ipa = if_else(language %in% c("Godoberi", "Tindi"), russian_ipa, str_remove(russian_ipa, "ʲ")),
         target_ipa = if_else(language %in% c("Godoberi", "Tindi"), target_ipa, str_remove(target_ipa, "ʲ")),
         russian_ipa = if_else(str_detect(russian_ipa, "[kg]ʲ"), russian_ipa, str_remove(russian_ipa, "ʲ")),
         # expected correspondences
         russian_ipa = str_remove(russian_ipa, "ˌ"),
         target_ipa = str_remove(target_ipa, "^'"),
         russian_ipa = if_else(str_remove(target_ipa, "ː") == russian_ipa, target_ipa, russian_ipa),
         russian_ipa = if_else(str_remove(target_ipa, "ʷ") == russian_ipa, target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "ʐ") & str_detect(target_ipa, "ʒ"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "tɕ") & str_detect(target_ipa, "tʃ"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "x") & str_detect(target_ipa, "χ"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "ɕː") & str_detect(target_ipa, "ʃ"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "zv") & str_detect(target_ipa, "zʷ"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "sv") & str_detect(target_ipa, "sʷ"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "kv") & str_detect(target_ipa, "kʷ"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "v") & str_detect(target_ipa, "w"), target_ipa, russian_ipa),
         # assimilation
         russian_ipa = if_else(str_detect(russian_ipa, "[ZS]") & str_detect(target_ipa, "[zs]"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "[GK]") & str_detect(target_ipa, "[gk]"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "[DT]") & str_detect(target_ipa, "[dt]"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "B") & str_detect(target_ipa, "[bp]"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "V") & str_detect(target_ipa, "w"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "Ž") & str_detect(target_ipa, "[ʃʒ]"), target_ipa, russian_ipa),
         # vowel reduction
         russian_ipa = if_else(str_detect(russian_ipa, "[AO]") & str_detect(target_ipa, "[ao]"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "U") & str_detect(target_ipa, "u"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "O") & str_detect(target_ipa, "o"), target_ipa, russian_ipa),
         russian_ipa = if_else(str_detect(russian_ipa, "[IE]") & str_detect(target_ipa, "[ie]"), target_ipa, russian_ipa)) |> 
  filter(#str_detect(russian_ipa, "0", negate = TRUE),
         #str_detect(target_ipa, "0", negate = TRUE),
         #russian_ipa == str_to_upper(russian_ipa),
         russian_ipa != target_ipa) |> 
  #ungroup() |> count(russian_ipa, target_ipa, sort = TRUE) |> View()
  add_count(language_ref, meaning_ru) |>
  mutate(n = n - metathesis) |> 
  rename(changes = n) |> 
  left_join(cross_range) |> 
  distinct(language, reference, meaning_ru, metathesis, total, changes, year) |> 
  mutate(ratio = changes/total,
         language = str_c(language, ": ", reference),
         language = factor(language),
         language = fct_relevel(language, "Avar: Gimbatov 2006")) |> 
  na.omit() ->
  for_modeling

for_modeling |> 
  write_csv("check_me.csv")

for_modeling |> 
  group_by(language) |> 
  mutate(mean_ratio = mean(ratio)) |> 
  ungroup() |> 
  mutate(language = fct_reorder(language, mean_ratio)) |> 
  ggplot(aes(ratio, language))+
  ggridges::geom_density_ridges()

# df |> 
#   filter(is.na(derived)) |> 
#   select(language, reference, meaning_ru, match_count, total_segments) |>
#   mutate(changes = total_segments - match_count) |> 
#   rename(total = total_segments) |> 
#   group_by(language, reference,  meaning_ru) |> 
#   mutate(meaing_id = 1:n()) |> 
#   slice_sample(n = 1) |> 
#   left_join(cross_range) |> 
#   distinct(language, reference, meaning_ru, total, changes, year) |> 
#   mutate(ratio = changes/total,
#          language = str_c(language, ": ", reference),
#          language = factor(language),
#          language = fct_relevel(language, "Avar: Gimbatov 2006")) |> 
#   filter(changes > 0) |> 
#   na.omit() ->
#   for_modeling

for_modeling |> 
  betareg::betareg(I(changes/total) ~ year:language,
          data = _) ->
  fit

summary(fit)

library(ggeffects)

fit |> 
  ggpredict(terms = c("year [all]", "language")) |> 
  as_tibble() |> 
  # summarise(range(x))
  ggplot(aes(x, predicted, color = group))+
  geom_line()+
  geom_text(aes(label = group), 
            data = tibble(x = 1989, 
                          group = levels(for_modeling$language),
                          predicted = predict(fit,
                                              tibble(language = levels(for_modeling$language),
                                                     year = 1989))),
            show.legend = FALSE, hjust = 0) +
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("#F78716",
                                "#93898C",
                                "#3B8BC6",
                                "#35A532",
                                "#E62651",
                                "#E62651",
                                "#8D63B9",
                                "#8E5D4D",
                                "#EB88CD",
                                "#F3DD0D")) +
  xlim(1800, 2060)+
  labs(x = "year of start of active use of word in Russian",
       y = "estimated ratio\nof changes in borrowing")

read_tsv("/home/agricolamz/work/databases/TALD/data/tald_villages.csv") |>
  filter(aff == "Avar-Andic",
         lat > 41.9,
         lat < 43.3,
         lon < 46.7) ->
  filtered_langs

library(lingtypology)
map.feature(filtered_langs$lang,
            features = filtered_langs$lang,
            latitude = filtered_langs$lat,
            longitude = filtered_langs$lon,
            minimap = TRUE, 
            minimap.position = "bottomleft",
            legend.position = "topleft",
            tile = "Esri.WorldGrayCanvas")

  ggplot(aes(lon, lat, color = lang))+
  geom_point()
  ggrepel::geom_text_repel()
