#load packages
library(tidyverse)
library(tidytext)
library(tidytuesdayR)
library(janitor)
library(tvthemes)
library(extrafont)
library(patchwork)
import_avatar()
loadfonts() #load fonts

#get data
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar
sceneInfo <- tuesdata$scene_description

#find the ten characters who speak the most
topTenCharacters <- avatar %>%
  count(character, sort = TRUE) %>%
  filter(character != "Scene Description") %>%
  top_n(10) %>%
  #hard code their nations
  mutate(nation = case_when(character %in% c("Sokka", "Katara") ~ "Water",
                            character %in% c("Zuko", "Iroh", "Azula", "Zhao") ~ "Fire",
                            character %in% c("Toph", "Jet", "Suki") ~ "Earth",
                            character == "Aang" ~ "Air"))

#get tokens for each character
avatarWords <- topTenCharacters %>%
  left_join(avatar) %>%
  unnest_tokens(word, character_words)

#get sentiment words for anger, trust, joy, and sadness.
sentimentWords <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("anger", "trust", "joy", "sadness"))

#get percentage of anger, trust, joy, and sadness words that each person spoke.
avatarSentimentWords <- avatarWords %>%
  left_join(sentimentWords) %>%
  count(character, nation, sentiment) %>%
  drop_na() %>%
  group_by(character) %>%
  mutate(percent = n / sum(n)) %>%
  select(-n) %>%
  ungroup()

# Creating Individual Nation Plots ----------------------------------------

fireNationPlot <- avatarSentimentWords %>%
  filter(nation == "Fire") %>%
  mutate(sentiment = fct_reorder(sentiment, percent)) %>%
  ggplot(aes(x = "", y = percent, fill = sentiment)) +
  scale_fill_avatar(palette = "FireNation") +
  geom_col() +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(y = "") +
  coord_polar("y") +
  facet_wrap(~character, nrow = 1) +
  theme_avatar(text.font = "Slayer",
               legend.font = "Slayer",
               title.font = "Slayer") +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12))

waterNationPlot <- avatarSentimentWords %>%
  filter(nation == "Water") %>%
  mutate(sentiment = fct_reorder(sentiment, percent)) %>%
  ggplot(aes(x = "", y = percent, fill = sentiment)) +
  scale_fill_avatar(palette = "WaterTribe") +
  geom_col() +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(y = "") +
  coord_polar("y") +
  facet_wrap(~character) +
  theme_avatar(text.font = "Slayer",
               legend.font = "Slayer",
               title.font = "Slayer") +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12))

earthNationPlot <- avatarSentimentWords %>%
  filter(nation == "Earth") %>%
  mutate(sentiment = fct_reorder(sentiment, percent)) %>%
  ggplot(aes(x = "", y = percent, fill = sentiment)) +
  scale_fill_avatar(palette = "EarthKingdom") +
  geom_col() +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(y = "") +
  coord_polar("y") +
  facet_wrap(~character) +
  theme_avatar(text.font = "Slayer",
               legend.font = "Slayer",
               title.font = "Slayer") +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12))

airNationPlot <- avatarSentimentWords %>%
  filter(nation == "Air") %>%
  mutate(sentiment = fct_reorder(sentiment, percent)) %>%
  ggplot(aes(x = "", y = percent, fill = sentiment)) +
  scale_fill_avatar(palette = "AirNomads") +
  geom_col() +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(y = "") +
  coord_polar("y") +
  facet_wrap(~character) +
  theme_avatar(text.font = "Slayer",
               legend.font = "Slayer",
               title.font = "Slayer") +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12))

# Combining Plots ---------------------------------------------------------

ggdraw() +
  draw_plot(fireNationPlot, y = -0.39, scale = 0.275) +
  draw_plot(earthNationPlot, y = -0.135, scale = 0.275) +
  draw_plot(waterNationPlot, y = 0.125, scale = 0.275) +
  draw_plot(airNationPlot, y = 0.38, scale = 0.275) +
  annotate("curve", x = 0.30, xend = 0.4, y = 0.85, yend = 0.65, arrow = arrow(length = unit(0.02, "npc"))) +
  annotate("text", x = 0.30, y = 0.9, label = "Each chart illustrates characters' \n usage of angry, sad, joyful, and trusting words.") +
  theme_avatar(text.font = "Slayer",
               legend.font = "Slayer",
               title.font = "Slayer") +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12)) +
  plot_annotation(title = "Avatar: The Last Airbender",
                  subtitle = "Ten Most Frequent Characters",
                  theme = theme_avatar(legend.font = "Slayer",
                                       title.font = "Slayer") +
                    theme(plot.title = element_text(hjust = 0.5),
                          plot.subtitle = element_text(hjust = 0.5),
                          plot.caption = element_text(hjust = 0.5, 
                                                      size = 10)),
                  caption = "@jdtrat | #tidytuesday Week 33")

