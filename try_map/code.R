setwd("/home/agricolamz/work/articles/2025_russian_borrowings_in_andi_with_Vika_Chiara/try_map")
library(tidyverse)
library(sf)

list.files(pattern = "geoshape") |> 
  map(function(x){
    st_read(x, as_tibble = TRUE) |>
      st_crop(xmin=-45, xmax=47, ymin=41, ymax=42.9) |> 
      mutate(district = x)}) |> 
  list_rbind() ->
  districts

st_read("andiyskoye_koysu.geoline", as_tibble = TRUE) |>
  st_crop(xmin=-45, xmax=47, ymin=41, ymax=42.9) ->
  andiyskoye_koysu

read_tsv("/home/agricolamz/work/databases/TALD/data/tald_villages.csv") |>
  filter(aff == "Avar-Andic",
         lat > 41.9,
         lat < 42.9,
         lon < 46.7,
         !(village %in% c("Tivi", "Kenkhi", "Sachada", "Urukh-Sota", "Gochob", "Artlukh", "Imanaliroso", "Akhtachikan", "Inkvalita"))) |> 
  mutate(default_level = case_when(str_detect(default_level, "Andi") ~ "Andi",
                                   str_detect(default_level, "Akhvakh") ~ "Akhvakh",
                                   default_level == "Gigatli" ~ "Chamalal",
                                   default_level == "Karata" ~ "Karata-Tukita",
                                   default_level == "Tukita" ~ "Karata-Tukita",
                                   TRUE ~ default_level)) ->
  filtered_langs

centers <- tibble(name = c("Botlikh", "Karata", "Agvali"),
                  lat = c(42.66781, 42.5942, 42.54119),
                  lon = c(46.22145, 46.3419, 46.1218))

districts |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry), fill = "grey95", color = "grey40")+
  geom_sf(aes(geometry = geometry), data = andiyskoye_koysu, color = "lightblue", linewidth = 2)+
  geom_point(aes(lon, lat), size = 2, color = "black", data = filtered_langs)+
  geom_point(aes(lon, lat, color = default_level), data = filtered_langs)+
  ggrepel::geom_label_repel(aes(lon, lat, label = name), data = centers, lineheight = 0.3,
                           hjust = 0,
                           alpha = 0.5,
                           nudge_x = -0.42,
                           direction = "y",
                           segment.colour = "grey20")+
  theme_minimal()+
  labs(color = NULL, x = NULL, y = NULL)+
  scale_color_manual(values = c("#93898C",
                                "#3B8BC6",
                                "#F78716",
                                "#35A532",
                                "#E62651",
                                "#8D63B9",
                                "#8E5D4D",
                                "#EB88CD",
                                "#F3DD0D"))+
  ylim(42.2, 42.9)+
  xlim(45.7, 46.7) ->
  map

map

ggsave("map.png", plot = map, width = 9, height = 7, bg = "white")
