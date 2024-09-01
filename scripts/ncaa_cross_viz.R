## Load Packages and Data ----

# Packages
library(janitor)  # cleaning
library(dplyr)  # cleaning
library(tidyr)  # cleaning
library(lubridate)  # cleaning dates and times
library(ggplot2)  # plotting
library(forcats)  # dealing with factors
library(stringr)  # wrapping text
library(showtext)  # fonts

# Read in the data
raw_data <- read.csv("data/team_champs.csv") %>% clean_names()


## Cleaning ----

# Update champion column to factor and get the most recent winning year
team_champs <- raw_data %>% 
  mutate(champion = as.factor(champion)) %>% 
  group_by(champion) %>% 
  mutate(max_year = max(year)) %>% 
  arrange(year)

# Create data frame that counts how many total championships
team_champs_count<- team_champs %>% 
  group_by(champion) %>% 
  summarise(counts = n()) %>% 
  arrange(desc(counts))


## Plotting ---- 

# Add fonts
font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()

# Create the plot
ncaa_cross_plot <- team_champs %>% 
  ggplot(
    mapping = aes(x = year, y = reorder(champion, max_year), 
                  label = substr(year, start = 3, stop = nchar(year)))) +
  theme_classic() +
  theme(
    text = element_text(family = "Roboto"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 14, face = "bold", family = "Roboto Condensed", color = "#2A3F4D", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 9, color = "#2A3F4D", margin = margin(b = 15)),
    plot.caption = element_text(size = 8, face = "italic", hjust = 0, margin = margin(t = 10)),
    panel.grid.major.y = element_line(color = "#2A3F4D", linewidth = 0.25, linetype = "dotted"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9, color = "#2A3F4D", margin = margin(t = 2)),
    axis.text.y = element_text(size = 10, color = "#2A3F4D")
  ) +
  labs(
    title = "NCAA Cross Country Champions",
    subtitle = str_wrap("While NAU has been dominant recently, with double three-peats to win six out of the last eight years, the legendary Arkansas program is still the standard, winning 11 times over 17 years in the 80s and 90s.", 104),
    caption = "Data: ncaa.com\nViz: Tim Fulton, PhD",
    x = "Age"
  ) +
  geom_point(
    shape = 95,
    size = 7, 
    color = "#466A80"
  ) +
  geom_text(
    size = 2.6,
    color = "#466A80",
    nudge_y = 0.3,
    fontface = "bold",
    family = "Roboto"
  ) +
  scale_x_continuous(
    breaks = seq(1940, 2020, 10),
    expand = expansion(mult = 0.015)
  ) 

# Save the plot
ggsave(
  "plots/ncaa_cross_plot.png",
  plot = ncaa_cross_plot,
  width = 13,
  height = 7,
  units = "in",
  dpi = 600
)

