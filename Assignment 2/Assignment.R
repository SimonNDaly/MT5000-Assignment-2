install.packages("tidyverse")
install.packages("plotly")
install.packages("maps")
install.packages("ggtext")
install.packages("gapminder")

library(tidyverse)
library(plotly)
library(maps)
library(ggtext)
options(scipen = 999)

unicef_metadata <- read_csv("unicef_metadata_a2.csv")
unicef_indicator <- read_csv("unicef_indicator_2.csv")

unicef_metadata_2 <- na.omit(unicef_metadata)

unicef_data <- full_join(unicef_metadata_2, unicef_indicator, 
                       by = c("country" = "country", 
                              "alpha_2_code" = "alpha_2_code", 
                              "alpha_3_code" = "alpha_3_code", 
                              "numeric_code" = "numeric_code",
                              "year" = "time_period"))

# Map Plot 1
map_chart_1 <- map_data("world")

map_plot_1 <- unicef_data %>%
  filter(year == 2021) %>%
  full_join(map_chart_1, by = c("country" = "region")) %>%
  ggplot(aes(long, lat, group = group, fill = obs_value,
             text = paste(country, "has", obs_value, "Observed Cases")))+ 
  scale_fill_gradient(name = "Number of Observed Cases", 
                      low = "blue", high = "red", na.value="#05f244") +
  geom_polygon(color = "black") +
  labs(title = "Number of Observed Cases per Country in 2021", 
       subtitle = "Countries in Grey lack Data or have no Observed Cases", 
       caption = "Source: Unicef Indicator") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.ticks = element_blank(), axis.title.x = element_blank(),
      axis.title.y = element_blank(), panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, 
                                face = "bold", size = "15", 
                                family = "NimbusSan"), 
      plot.subtitle = element_text(hjust = 0.2, face = "italic", 
                                   size = "10", family = "NimbusSan"), 
      plot.caption = element_text(face = "italic"),
      panel.background = element_rect(fill = "#90e9ff"),
      text = element_text(family = "serif"),
      panel.border = element_rect(colour = "black", fill=NA,  
                                  linewidth = "1"))

ggplotly(map_plot_1, tooltip = c("text"))

# Map Plot 2

map_chart_2 <- map_data("world")

map_plot_2 <- unicef_data %>%
  filter(year == 2021, alpha_2_code != "ZA") %>%
  full_join(map_chart_2, by = c("country" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = obs_value,
             text = paste(country, "has", obs_value, "Observed Cases"))) + 
  scale_fill_gradient(name = "Number of Observed Cases", 
                      low = "blue", high = "red", na.value = "#05f244") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), rect = element_blank(),
        text = element_text(family = "serif")) +
  geom_polygon(color = "black") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", size = "15", 
                                  family = "NimbusSan"), 
        plot.subtitle = element_text(hjust = 0.2, face = "italic", 
                                     size = "12", family = "NimbusSan"), 
        plot.caption = element_text(face = "italic"),
        panel.background = element_rect(fill = "#90e9ff"),
        text = element_text(family = "serif"),
        panel.border = element_rect(colour = "black", fill=NA,  
                                    linewidth = "1")) +
  labs(title = "Number of Observed Cases per Country (Excluding South Africa) in 2021",
       subtitle = "Countries in Grey lack Data or have no Observed Cases", 
       caption = "Source: Unicef Indicator")
 
ggplotly(map_plot_2, tooltip = c("text"))

# Scatter Plot 1

scatter_plot_1 <- filter(unicef_data, year > 2015, year < 2022) %>%
  ggplot(aes(Life_exp, obs_value, 
      fill = obs_value, size = pop_total, 
      text = paste(country, "has a Life Expectiancy of", 
                   Life_exp, "and", obs_value, "Observed Cases"))) +
    geom_point(shape = 4, size = 5, alpha = 0.5) +
  facet_wrap(~year) +
  scale_fill_gradient(name = "Number of Observed Cases", 
                      low = "blue", high = "red") +
  scale_x_continuous(breaks = c(50, 60, 70, 80)) +
  scale_y_continuous(limits = c(0, 80000), 
                     breaks = c(20000, 40000, 60000, 80000))+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size = "12"), 
        axis.text.y = element_text(size = "12"),
        axis.title.x = element_text(size = "15", face = "italic", family = "NimbusSan"),
        axis.title.y = element_text(size = "15", face = "italic", family = "NimbusSan"), 
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", size = "15", 
                                  family = "NimbusSan"), 
        plot.caption = element_text(face = "italic"),
        panel.border = element_rect(colour = "black", fill=NA,
                                    linewidth = "2"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        strip.text = element_text(face = "bold", 
                                  family = "NimbusSan", size ="15"),
        strip.background = element_rect(fill = "white")) +
  labs(title = "Life Expectancy by Country with Observable Cases 2016-2021",
       x = "Life Expectancy (at birth)", y = "Observable Cases",
       caption = "Source: Unicef Indicator")
  guides(size = "none")

ggplotly(scatter_plot_1, tooltip = c("text"))

# Scatter Plot 2
scatter_plot_2 <- filter(unicef_data, year > 2015, year < 2022, 
                         obs_value > 0) %>%
  ggplot(aes(GDP_per_cap, obs_value, 
      fill = obs_value, size = pop_total, 
      text = paste(country, "has a GDP per Capita of", 
                   GDP_per_cap, "and", obs_value, "Observed Cases"))) +
  geom_point(shape = 4, size = 5, alpha = 0.5) +
  facet_wrap(~year) +
  scale_fill_gradient(name = "Number of Observed Cases", 
                      low = "blue", high = "red") +
  scale_y_continuous(limits = c(0, 80000), 
                     breaks = c(20000, 40000, 60000, 80000)) +
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size = "12"), 
        axis.text.y = element_text(size = "12"),
        axis.title.x = element_text(size = "15", face = "italic", family = "NimbusSan"),
        axis.title.y = element_text(size = "15", face = "italic", family = "NimbusSan"), 
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", size = "15", 
                                  family = "NimbusSan"), 
        plot.caption = element_text(face = "italic"),
        panel.border = element_rect(colour = "black", fill=NA,
                                    linewidth = "2"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        strip.text = element_text(face = "bold", 
                                  family = "NimbusSan", size ="15"),
        strip.background = element_rect(fill = "white")) +
  labs(title = "GDP per Capita by Country with Observable Cases 2016-2021",
       x = "GDP per Capita", y = "Observed Cases",
       caption = "Source: Unicef Indicator")
guides(size = "none")

ggplotly(scatter_plot_2, tooltip = c("text"))

# Scatter Plot 3

scatter_plot_3 <- filter(unicef_data, year > 2015, year < 2022, 
                         obs_value > 0) %>%
  ggplot(aes(obs_value, GNI,
             fill = obs_value, size = pop_total, 
             text = paste(country, "has a GNI of", 
                          GNI, "and", obs_value, "Observed Cases"))) +
  geom_point(shape = 4, size = 5, alpha = 0.5) +
  facet_wrap(~year) +
  scale_fill_gradient(name = "Number of Observed Cases", 
                      low = "blue", high = "red") +
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size = "12"), 
        axis.text.y = element_text(size = "12"),
        axis.title.x = element_text(size = "15", face = "italic", family = "NimbusSan"),
        axis.title.y = element_text(size = "15", face = "italic", family = "NimbusSan"), 
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", size = "15", 
                                  family = "NimbusSan"), 
        plot.caption = element_text(face = "italic"),
        panel.border = element_rect(colour = "black", fill=NA,
                                    linewidth = "2"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        strip.text = element_text(face = "bold", 
                                  family = "NimbusSan", size ="15"),
        strip.background = element_rect(fill = "white")) +
  labs(title = "GNI by Country with Observable Cases 2016-2021",
       x = "Observed Cases", y = "GNI",
       caption = "Source: Unicef Indicator")
guides(size = "none")

ggplotly(scatter_plot_3, tooltip = c("text"))

# Scatter Plot 4

scatter_plot_4 <- filter(unicef_data, year > 2015, year < 2022, 
                         obs_value > 0, alpha_2_code != "ZA") %>%
  ggplot(aes(obs_value, GNI,
             fill = obs_value, size = pop_total, 
             text = paste(country, "has a GNI of", 
                          GNI, "and", obs_value, "Observed Cases"))) +
  geom_point(shape = 4, size = 5, alpha = 0.5) +
  facet_wrap(~year) +
  scale_fill_gradient(name = "Number of Observed Cases", 
                      low = "blue", high = "red") +
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size = "12"), 
        axis.text.y = element_text(size = "12"),
        axis.title.x = element_text(size = "15", face = "italic", family = "NimbusSan"),
        axis.title.y = element_text(size = "15", face = "italic", family = "NimbusSan"), 
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", size = "15", 
                                  family = "NimbusSan"), 
        plot.caption = element_text(face = "italic"),
        panel.border = element_rect(colour = "black", fill=NA,
                                    linewidth = "2"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        strip.text = element_text(face = "bold", 
                                  family = "NimbusSan", size ="15"),
        strip.background = element_rect(fill = "white")) +
  labs(title = "GNI by Country with Observable Cases 2016-2021 (Excluding South Africa)",
       x = "Observed Cases", y = "GNI",
       caption = "Source: Unicef Indicator")
guides(size = "none")

ggplotly(scatter_plot_4, tooltip = c("text"))

# Bar Chart 1

bar_plot_1 <- unicef_data %>%
  filter(year == 2021, obs_value > 1000) %>%
  group_by(country, year) %>%
  ggplot(aes(reorder(country, obs_value/pop_total * 100),
             obs_value/pop_total * 100, fill = obs_value, 
             text = paste(country, "had", obs_value, "Observed Cases,",
                          obs_value/pop_total * 100, "percent of the Total Population have been tested"))) +
  scale_fill_gradient(name = "Number of Observed Cases", low = "blue", high =
                        "red", na.value = "grey") +
  
  scale_y_continuous(expand = c(0,0), 
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme(rect = element_blank(), 
        text = element_text(family = "serif"),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_text(size = "12"),
        axis.title.x = element_text(size = "15", face = "italic", family = "NimbusSan"),
        axis.title.y = element_text(size = "15", face = "italic", family = "NimbusSan"), 
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", size = "15", 
                                  family = "NimbusSan"), 
        plot.caption = element_text(face = "italic"),
        panel.border = element_rect(colour = "black", fill=NA,
                                    linewidth = "2"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey")) +
  labs(title = "Number of Observed Cases (Over 1000) as percentage of a Country’s Total Population in 2021",
       x = "Country", y = "Cases as % of Total Population",
       caption = "Source: Unicef Indicator") +
  geom_col(color = "black", width = 1)
  
ggplotly(bar_plot_1, tooltip = c("text"))

# Time-series Graph 1

timeseries_plot_1 <- filter(unicef_data, year > 2010, 
                                 year < 2022, obs_value >1) %>%
  ggplot() +
  aes(year, Life_exp, group = country, colour = continent,
      text = paste(country, "has a Life Expectancy of", Life_exp, 
                   "and", obs_value, "Observed Values in", year)) +
  geom_line() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016,
                                2017, 2018, 2019, 2020, 2021)) +
  scale_colour_manual(values = c("#7D3C98", "#229954", 
                                 "#C3173E", "#3498DB", "#20B2AA")) +
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size = "12"), 
        axis.text.y = element_text(size = "12"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = "15", face = "italic", family = "NimbusSan"), 
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold", size = "15", 
                                  family = "NimbusSan"), 
        plot.caption = element_text(face = "italic"),
        panel.border = element_rect(colour = "black", fill=NA,
                                    linewidth = "2"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        strip.text = element_text(face = "bold", 
                                  family = "NimbusSan", size ="15"),
        strip.background = element_rect(fill = "white")) +  
    labs(title = "Life Expectancy by Country with Observable Cases 2011-2021",
       x = "Year", y = "Life Expectancy (at birth)",
       caption = "Source: Unicef Indicator")

ggplotly(timeseries_plot_1, tooltip = c("text"))
