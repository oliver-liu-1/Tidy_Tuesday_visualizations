library(tidyverse)
library(scales)
library(ggthemes)
library(NatParksPalettes)

#citation: replication of the lower half visualization of 
#https://bsky.app/profile/sponce1.bsky.social/post/3m4loeucfgs2y
#without looking at the code

flint_mdeq <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv')
flint_vt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv')

flint_mdeq_vt <- flint_mdeq %>%
  select(sample, lead) %>%
  mutate(source = "MDEQ") %>%
  rbind(flint_vt %>% mutate(source = "VT"))

#xlim will cut the data at the point and force the plot to still sum to 1
flint_mdeq_vt %>%
  ggplot(aes(x = lead, color = source))+
  stat_ecdf(linewidth = 1.1, alpha = 0.85) +
  geom_vline(xintercept = 15, linetype = "longdash") +
  coord_cartesian(xlim = c(0,30)) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(0,30,5)) +
  labs(x = "Lead Concentration Per Billions", y = "Cumulative Percentage of Samples") +
  NatParksPalettes::scale_color_natparks_d(name = "Charmonix", override.order = )  +
  theme_minimal() +
  theme(legend.justification = c("right","bottom"),
        legend.position = "inside",
        legend.background = element_rect(fill = "white", color = "white"))


