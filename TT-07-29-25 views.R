movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
shows <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')

library(tidyverse)

#made by Sara, Erin, and Oliver

top_10_shows <- shows %>%
  mutate(english_title = str_split_i(title, '//', i = 1)) %>%
  filter(str_detect(english_title, ": Season")) %>%
  mutate(season = str_sub(string = english_title, start = -2, end=-1), 
         season = as.integer(season),
         english_title = str_split_i(english_title, ':', i = 1)) %>%
  filter(!is.na(season)) %>%
  group_by(english_title) %>%
  summarize(total_views = sum(views)) %>%
  arrange(desc(total_views)) %>%
  slice_head(n = 10) %>%
  pull(english_title)

shows %>%
  mutate(english_title = str_split_i(title, '//', i = 1)) %>%
  filter(str_detect(english_title, ": Season")) %>%
  mutate(season = str_sub(string = english_title, start = -2, end=-1), 
         season = as.integer(season),
         english_title = str_split_i(english_title, ':', i = 1)) %>%
  filter(!is.na(season), report == "2025Jan-Jun") %>%
  group_by(english_title) %>%
  mutate(max_season = max(season)) %>%
  filter(max_season >= 3, english_title %in% top_10_shows) %>%
  ggplot(aes(x = season, y = views)) +
  geom_col(aes(color = english_title, fill = english_title), alpha = 0.3) +
  facet_wrap(~english_title, scales = 'free_x') +
  labs(x = 'Season', y = 'Views', color = 'Show Title', fill = 'Show Title') +
  scale_x_continuous(breaks = seq(1,20, by = 1)) +
  theme_bw() +
  theme(legend.position = 'none')
