# Read in Data
library(tidyverse)
library(tidytuesdayR)

# load data
tuesdata <- tidytuesdayR::tt_load('2020-08-18')

plants <- tuesdata$plants


# Tutorial done for my lab:

finalPlot <- plants %>%
  pivot_longer(cols = contains("threat_"),
              names_to = "threat_type",
              values_to = "threat") %>% 
  group_by(country) %>%
  summarize(totalThreat = sum(threat),
            .groups = "drop") %>%
  filter(totalThreat > 20) %>%
  mutate(country = case_when(str_detect(country, "Saint Helena") ~ "Saint Helena",
                             TRUE ~ country),
         country = fct_reorder(country, totalThreat)) %>%
  ggplot(aes(x = country, y = totalThreat, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Total Threats to Plant Species",
       subtitle = "Countries with over 20 species threats",
       x = "Country",
       y = "Total Threat",
       caption = "Graph by @jdtrat | #tidytuesday Week 34 \n Data from the International Union for Conservation of Nature (IUCN) Red list of Threatened Species (Version 2020-1)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lavenderblush1",
                                        color = "lavenderblush2"),
        plot.background = element_rect(fill = "lavenderblush"),
        plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = 'totalThreatPlot.png', plot = finalPlot, path = '8-18-2020/')





