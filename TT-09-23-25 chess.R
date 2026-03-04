tuesdata <- tidytuesdayR::tt_load(2025, week = 38)

aug <- tuesdata$fide_ratings_august
sep <- tuesdata$fide_ratings_september

library(tidyverse)
library(ggbeeswarm)

#A recreation of Nicholas Vietto's Chess Tidy Tuesday plot by Oliver, Sara, and Erin
#https://bsky.app/profile/nvietto.bsky.social/post/3lzhd3m4o722w
#without looking the code

sep %>%
  filter(!is.na(foa)) %>%
  mutate(foa = case_when(foa == "AIM" ~ "Arena International Master",
                         foa == "AGM" ~ "Arena Grand Master",
                         foa == "AFM" ~ "Arena FIDE Master",
                         foa == "ACM" ~ "Arena Candidate Master"),
         foa = factor(foa, levels = c("Arena Candidate Master",
                                           "Arena FIDE Master",
                                           "Arena International Master",
                                           "Arena Grand Master"))) %>%
  mutate(foa = fct_rev(foa))%>%
  ggplot(aes(y = foa, x=rating, color = foa))+
  geom_beeswarm()+
  geom_vline(xintercept = 1000, color = "orange")+
  geom_curve(aes(x =900, xend = 1000, y = 2, yend = 1.8),
             arrow = arrow(length = unit(0.01, "npc")), curvature = 0,
             color = "cadetblue4")+
  annotate(x = 850, y = 2, geom = "label", label = "Not a noob anymore", fill = "orange", color = "white")+
  scale_x_continuous(limits = c(750,2500))+
  theme_minimal()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black"),
        panel.grid = element_line(color = "gray40"),
        axis.text = element_text(color = "white", face = "bold", family = "mono"),
        plot.title = element_text(color = "white", face = "bold", family = "mono", hjust = 0.5),
        plot.caption = element_text(color = "white", face = "bold", family = "mono", hjust = 0.5)) +
  labs(title = "How Your Chess Rating (Elo) Measures Up: \n A Comparison of a thousand elo milestone and arena master title",x="",y="",
       caption = "A recreation of Nicholas Vietto's Chess Tidy Tuesday plot by Oliver, Sara, and Erin")
