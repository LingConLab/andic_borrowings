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
