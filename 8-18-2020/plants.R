# Read in Data
library(tidyverse)
library(tidymodels)
library(janitor)
library(tidytuesdayR)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)

# Don't need: worldSF <- ne_countries(scale = "medium", returnclass = "sf")

# load data
tuesdata <- tidytuesdayR::tt_load('2020-08-18')

# get plants data
plants <- tuesdata$plants

# get world data
world <- map_data("world")


# create plants over time
originalPlantsOverTime <- plants %>% 
  count(country, time = year_last_seen, sort = T)


# create a data frame that has the taxonomy group of the extinct plants and the date they were last seen.
plantsOverTime <- plants %>% 
  drop_na() %>%
  mutate(year_last_seen = as_factor(year_last_seen),
         year_last_seen = fct_relevel(year_last_seen, 
                                      c("Before 1900", 
                                        "1900-1919", 
                                        "1920-1939", 
                                        "1940-1959",
                                        "1960-1979",
                                        "1980-1999",
                                        "2000-2020"))) %>%
  count(country, year_last_seen, taxonomy = group) %>%
  group_by(country) %>%
  mutate(extinctNums = cumsum(n)) %>%
  ungroup()

finale <-
  tibble("Before 1900", 
         "1900-1919", 
         "1920-1939", 
         "1940-1959",
         "1960-1979",
         "1980-1999",
         "2000-2020") %>% 
  bind_cols(plants %>% 
              drop_na() %>% 
              distinct(country)) %>%
  pivot_longer(cols = -country, 
               values_to = "time") %>%
  select(-name) %>% 
  left_join(originalPlantsOverTime, by = c("country", "time")) %>%
  mutate(n = replace_na(n, 0),
         time = as_factor(time),
         time = fct_relevel(time, 
                            c("Before 1900", 
                              "1900-1919", 
                              "1920-1939", 
                              "1940-1959",
                              "1960-1979",
                              "1980-1999",
                              "2000-2020"))) %>%
  arrange(time) %>%
  group_by(country) %>%
  mutate(extinctNums = cumsum(n)) %>%
  ungroup()


# get top 25 countries who have most extinct plant species
top25 <- finale %>% filter(time == "2000-2020") %>% 
  arrange(desc(extinctNums)) %>% 
  slice_head(n = 25) %>% 
  select(country) %>% 
  left_join(finale)


# create a data frame that has the mean long and lat for each country that will be plotted.
geomLocations <- plantsOverTime %>% 
  left_join(world %>% rename(country = region), by = "country") %>%
  group_by(country) %>%
  summarize(long = mean(long),
            lat = mean(lat),
            .groups = "drop")

# join the data with geom locations
finalePlotData <- left_join(top25, geomLocations)

#Find out which countries were not joined properly 
#and recode manually based on world dataset
finalePlotData %>% 
  filter(is.na(long)) %>% 
  count(country)

# Rerun this with fixed countries.
# Côte d'Ivoire does not exist in world data set
finalPlotData <- finale %>% 
  mutate(country = case_when(country == "United States" ~ "USA",
                             country == "Viet Nam" ~ "Vietnam",
                             country == "Pitcairn" ~ "Pitcairn Islands",
                             country == "Cabo Verde" ~ "Cape Verde",
                             country == "Congo" ~ "Democratic Republic of the Congo",
                             country == "Côte d'Ivoire" ~ "",
                             country == "Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena",
                             country == "Trinidad and Tobago" ~ "Trinidad",
                             country == "United Kingdom" ~ "UK",
                             TRUE ~ country))

# create a data frame that has the mean long and lat for each country that will be plotted.
geomLocations <- originalPlantsOverTime %>% 
  mutate(country = case_when(country == "United States" ~ "USA",
                             country == "Viet Nam" ~ "Vietnam",
                             country == "Pitcairn" ~ "Pitcairn Islands",
                             country == "Cabo Verde" ~ "Cape Verde",
                             country == "Congo" ~ "Democratic Republic of the Congo",
                             country == "Côte d'Ivoire" ~ "",
                             country == "Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena",
                             country == "Trinidad and Tobago" ~ "Trinidad",
                             country == "United Kingdom" ~ "UK",
                             TRUE ~ country)) %>%
  left_join(world %>% rename(country = region), by = "country") %>%
  group_by(country) %>%
  summarize(long = mean(long),
            lat = mean(lat),
            .groups = "drop")

# Rerun
top25finale <- finalPlotData %>% filter(time == "2000-2020") %>% 
  arrange(desc(extinctNums)) %>% 
  slice_head(n = 25) %>% 
  select(country) %>% 
  left_join(finalPlotData)

# finale stuff
finalePlotData <- left_join(top25finale, geomLocations)


# Plot

finalePlot <- finalePlotData %>%
  mutate(extinctNums = -extinctNums,
         colorCategory = case_when(extinctNums >= -100 & extinctNums < -80 ~ "1",
                                   extinctNums > -80 & extinctNums <= -60 ~ "2",
                                   extinctNums > -60 & extinctNums <= -40 ~ "3",
                                   extinctNums > -40 & extinctNums <= -20 ~ "4",
                                   extinctNums > -20 & extinctNums <= 0 ~ "5")) %>%
  ggplot(aes(x = long, y = lat)) +
  borders() +
  geom_point(aes(y = lat - 2.5, group = country, size = extinctNums), shape = 124, color = "goldenrod4") +
  geom_point(aes(group = country, size = extinctNums, color = colorCategory), shape = 17) +
  #geom_point(aes(y = lat - 1.5, group = country, size = extinctNums), shape = 17, color = "chartreuse4") +
  labs(title = "Extinct Plants Across the Planet",
       subtitle = "Subset of 25 Countries \n Last seen between {closest_state}",
       size = "Extinct Plant Count",
       caption = "Animation by @jdtrat | #tidytuesday Week 34 \n Data from the International Union for Conservation of Nature (IUCN) Red list of Threatened Species (Version 2020-1)") +
  scale_color_manual("Extinct Plant Count", values = c("darkseagreen1", "lightgreen", "darkolivegreen3", "chartreuse4", "forestgreen"), guide = "none") +
  scale_size("Extinct Plant Count", breaks = seq(-100,-0, by = 20), labels = c("100", "80", "60", "40", "20", "0"), guide = "none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "mintcream",
                                        color = "olivedrab3"),
        plot.background = element_rect(fill = "honeydew2"),
        legend.background = element_rect(fill = "honeydew2"),
        legend.key = element_rect(fill = "honeydew2"),
        plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(face = "bold")) +
  annotate("text", x = -40, y = 30, label = "The smaller and \n lighter the tree, \n the more plants \n have become extinct.", size = 2) +
  annotate("text", x = 47, y = -30, label = "Madagascar", size = 3, fontface = "bold") +
  annotate("curve", x = -36, xend = 47, y = 46, yend = -10, arrow = arrow(length = unit(0.02, "npc")), curvature = -0.7) +
  annotate("text", x = -122, y = 36, label = "United States", size = 3, fontface = "bold") +
  annotate("curve", x = -36, xend = -115, y = 46, yend = 50, arrow = arrow(length = unit(0.02, "npc")))


anim <- finalePlot + transition_states(time,
                                       transition_length = 2,
                                       state_length = 1) +
  ease_aes('cubic-in-out')



# Animate and save plot
animate(
  anim + enter_fade() + exit_fade(),
  width = 800, height = 400, res = 100
)


anim_save(filename = 'plantsOverTimeFinal3.gif', path = '8-18-2020/')
