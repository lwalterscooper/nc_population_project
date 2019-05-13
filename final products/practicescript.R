library(tidyverse)
blackncmigration1870 <- read_csv("data/blkncmigration1870.csv")
topten1870 <- blackncmigration1870 %>% 
  select(year, state_name, totblkpop, ncnatives) %>% 
  arrange(desc(ncnatives)) %>% 
  top_n(10, ncnatives)
topten1870

blackncmigration1880 <- read_csv("data/blkncmigration1880.csv")
topten1880 <- blackncmigration1880 %>% 
  select(year, state_name, totblkpop, ncnatives) %>% 
  arrange(desc(ncnatives)) %>% 
  top_n(10, ncnatives)
topten1880

blackncmigration <- bind_rows(topten1870, topten1880)
blackncmigration

##code for visualization #1 ##
migration <- bind_rows(blackncmigration1870, blackncmigration1880)   

migration %>% 
  filter(state_name != "North Carolina") %>% 
  mutate(year = fct_rev(as.factor(year))) %>% 
  mutate(state_name = if_else(ncnatives < 2000 & state_name != "Indiana", "Other", state_name)) %>% 
  group_by(state_name, year) %>% 
  summarize(ncnatives = sum(ncnatives)) %>% 
ggplot(aes(x=fct_reorder(state_name, ncnatives), y=ncnatives, color = year, fill = year)) + 
  geom_bar(stat="identity", color= "#2B101E", position = position_dodge()) + 
  labs(title="States of Residence for African Americans Born in North Carolina, 1870 & 1880")+
  coord_flip() +
  scale_fill_manual(values = c("#2B101E", "#845157")) +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = "African American residents born in North Carolina")

###color codes in scale_fill_manual from palitra (check Clio 2 bookmarks for color palettes) ##
### fixed outline issue for bars: simply defined the color in geom_bar()


##working leaflet map: Where people born in NC live in 1870

library(tidyverse)
library(USAboundaries)
library (ggplot2)
library(leaflet)
library (sf)
states_1870 <- us_states("1870-12-31")
ncmigration_1870 <- read_csv("data/blkncmigration1870.csv") 
ncmigration_1870 %>% 
  mutate(state_name = str_remove(state_name, " Territory"),
         (totncblack_percentage = ncmigper*100))
  
ncmigration_1870_gi <- states_1870 %>% 
  left_join(ncmigration_1870, by = "state_name") %>% 
  filter(state_name != "Alaska",
         state_name != "North Carolina")

ncmig1870_colors <- colorNumeric(palette = "YlOrRd",
                             domain = c(0, max(ncmigration_1870_gi$ncnatives, na.rm = TRUE)))

leaflet(ncmigration_1870_gi) %>% 
  addPolygons(fillColor = ~ncmig1870_colors(ncnatives), 
              fillOpacity = 1,
              color = "black",
              weight = 1,
              popup = ~str_c(state_name,"<br>",
                             "Black NC Natives: ", prettyNum(ncnatives, big.mark = ","), "<br>",
                             "% of all Black NC Natives: ", prettyNum(ncmigper ),
                               "%")) %>% 
  addLegend(position = "bottomleft", 
            pal = ncmig1870_colors, values = ~ncnatives,
            title = "1870:<br>Black Migration from <br>North Carolina and <br>Percentage of<br>Total Black<br> NC Nativity", 
            opacity = 1 )

##1880 map
states_1880 <- us_states("1880-12-31")
ncmigration_1880 <- read_csv("data/blkncmigration1880.csv") 
ncmigration_1880 %>% 
  mutate(state_name = str_remove(state_name, " Territory"),
         (totncblack_percentage = ncmigper*100))

ncmigration_1880_gi <- states_1880 %>% 
  left_join(ncmigration_1880, by = "state_name") %>% 
  filter(state_name != "Alaska",
         state_name != "North Carolina")

ncmig1880_colors <- colorNumeric(palette = "YlOrBr",
                                 domain = c(0, max(ncmigration_1880_gi$ncnatives, na.rm = TRUE)))

leaflet(ncmigration_1880_gi) %>% 
  addPolygons(fillColor = ~ncmig1880_colors(ncnatives), 
              fillOpacity = 1,
              color = "black",
              weight = 1,
              popup = ~str_c(state_name,"<br>",
                             "Black NC Natives: ", prettyNum(ncnatives, big.mark = ","), "<br>",
                             "% of all Black NC Natives: ", prettyNum(ncmigper ),
                             "%")) %>% 
  addLegend(position = "bottomleft", 
            pal = ncmig1880_colors, values = ~ncnatives,
            title = "1880:<br>Black Migration from <br>North Carolina and <br>Percentage of<br>Total Black<br> NC Nativity", 
            opacity = 1 )

# 1870: static map of black settlement in NC
library(tidyverse)
library(USAboundaries)
library (ggplot2)
library(leaflet)
library (sf)

nctotalblkpop_1870 <- read_csv("data/nctotalblkpop_1870.csv")
counties_nc_1870 <- us_counties("1870-12-31", resolution = "high", states = "NC")
nc1870blkpop <- counties_nc_1870 %>% 
  left_join(nctotalblkpop_1870, by = "name")
nc1870blkpop 

ggplot(nc1870blkpop) +
  geom_sf(aes(fill = totalblkpop)) +
  scale_fill_distiller("African American Population", palette = "YlGnBu", na.value = "white", direction = 1) +
  labs(title = "1870: Where African Americans Live in North Carolina") +
  theme(plot.title = element_text(size = 14, hjust = 0.7),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

# 1880: static map of black settlement in NC 

nctotalblkpop_1880 <- read_csv("data/nctotalblkpop_1880.csv")
counties_nc_1880 <- us_counties("1880-12-31", resolution = "high", states = "NC")
nc1880blkpop <- counties_nc_1880 %>% 
  left_join(nctotalblkpop_1880, by = "name")
nc1880blkpop 

ggplot(nc1880blkpop) +
  geom_sf(aes(fill = totalblkpop)) +
  scale_fill_distiller("African American Population", palette = "YlGnBu", na.value = "white", direction = 1) +
  labs(title = "1880: Where African Americans Live in North Carolina") +
  theme(plot.title = element_text(size = 14, hjust = 0.7),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())
