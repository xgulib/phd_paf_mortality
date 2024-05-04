library(maps)
library(dplyr)
library(stringr)
library(ggplot2)

world <- map_data("world")

world2 <- world %>%
  mutate(region2 = case_when(
    region == "Antigua" ~ "Antigua and Barbuda",
    region == "Barbuda" ~ "Antigua and Barbuda",
    region == "Bahamas" ~ "The Bahamas",
    region == "Ivory Coast" ~ "Cote d'Ivoire",
    region == "Republic of Congo" ~ "Congo", 
    region == "UK" ~ "United Kingdom",
    region == "Gambia" ~ "The Gambia",
    region == "North Macedonia" ~ "Macedonia",
    region == "Trinidad" ~ "Trinidad and Tobago",
    region == "Tobago" ~ "Trinidad and Tobago",
    region == "USA" ~ "United States",
    region == "Saint Vincent" ~ "Saint Vincent and the Grenadines",
    region == "Grenadines" ~ "Saint Vincent and the Grenadines",
    TRUE ~ region)) %>%
  rename(location_name = region2) %>%
  select(-region)

load("/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/out_phdi2018.Rdata")

phdi_map <- out_phdi2018%>%
  ungroup()%>%
  filter(type == "country") %>%
  select(region, location_name, 76:91) 

phdi_map2 <- world2 %>%
  left_join(phdi_map, by = "location_name") %>%
  mutate(catphdi = case_when(
    phdi <= 60 ~ "<=60",
    phdi >60 & phdi <=65 ~ "61-65",
    phdi >65 & phdi <=70 ~ "66-70",
    phdi >70 & phdi <=75 ~ "71-75",
    phdi >75 & phdi <=80 ~ "76-80",
    phdi >80 & phdi <=85 ~ "81-85",
    phdi >85 & phdi <=90 ~ "86-90",
    phdi >90 & phdi <=95 ~ "91-95",
    phdi >95 & phdi <=100 ~ "96-100",
    phdi >100 ~ ">100",
    TRUE ~ "No data"
  )) %>%
  mutate(facphdi = factor(catphdi, levels = c("No data", "<=60", "61-65", "66-70", "71-75", "76-80", "81-85", "86-90", "91-95", "96-100", ">100")))

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
)

#cols <- c("<=55" = "#a50026", "56-60" = "#d73027", "61-65" = "#f46d43", "66-70" = "#fdae61", "71-75" = "#fee08b", "76-80" = "#d9ef8b", "81-85" = "#a6d96a",
#          "86-90" = "#66bd63", "91-95" = "#1a9850", ">95" = "#006837")
cols2 <- c("No data" = "white", "<=60" = "#a50026", "61-65" = "#d73027", "66-70" = "#f46d43", "71-75" = "#fdae61", "76-80" = "#fee08b", "81-85" = "#ffffbf", "86-90" = "#d9ef8b",
          "91-95" = "#a6d96a", "96-100" = "#66bd63", ">100" = "#1a9850")
worldPHDI <- ggplot(data = phdi_map2, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(colour = "black", linewidth = 0.2, aes(fill = facphdi)) +
  scale_fill_manual(values = cols2) + # or direction=1
  plain +
  labs(fill = "PHDI") +
  theme(legend.text=element_text(size=32),
        legend.title = element_text(face = "bold", size=40),
        legend.position = "right",
        legend.key.size = unit(36, "pt"))
worldPHDI

