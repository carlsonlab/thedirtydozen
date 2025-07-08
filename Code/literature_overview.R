
library(tidyverse)
library(sf)
library(ggplot2)
library(scatterpie)
library(MetBrewer)
library(patchwork)

geometry_to_lonlat <- function(x) {
  if (any(sf::st_geometry_type(x) != "POINT")) {
    stop("Selecting non-points isn't implemented.")
  }
  coord_df <- sf::st_transform(x, sf::st_crs("+proj=longlat +datum=WGS84")) %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::select(X, Y) %>%
    dplyr::rename(lon = X, lat = Y)
  out <- sf::st_set_geometry(x, NULL) %>%
    dplyr::bind_cols(coord_df)
  return(out)
} # https://github.com/r-spatial/sf/issues/498

### Basic study counting

att <- read_csv("study_metadata.csv")
att <- att[c(1:20),] # Remove 2025 study
att %>% 
  select(Region, `Health impact`) %>% 
  rename(Impact = `Health impact`) %>%
  separate_rows(Region, sep=",") %>% 
  separate_rows(Impact, sep=",") %>%
  mutate(Region = recode(Region, !!!c('Africa' = 'AFRO',
                                      'Americas' = 'AMRO',
                                      'Europe' = 'EURO',
                                      'Eastern Mediterranean' = 'EMRO',
                                      'South-East Asia' = 'SEARO',
                                      'Western Pacific' = 'WPRO'))) -> att

att %>% 
  group_by(Region, Impact) %>%
  summarize(Count = n()) %>%
  ungroup() %>% 
  complete(Region, Impact, fill = list(Count = 0)) -> att 

att %>% group_by(Region) %>% summarize(RegionCount = sum(Count)) -> reg
att %>%
  left_join(reg) %>%
  mutate(Count = Count/RegionCount) %>%
  select(-RegionCount) %>%
  pivot_wider(names_from = Impact, values_from = Count) %>%
  left_join(reg) -> att

### Basic map setup

who <- read_sf("WHORegionsBoundary.shp")
who <- who %>%
  mutate(WHO_region = paste0(WHO_region, "O"))
who$WHO_region[who$WHO_region=='NAO'] <- NA

who %>% 
  st_centroid() -> centroids

who %>% 
  ggplot() + 
  geom_sf(aes(fill=WHO_region), color="black") +
  geom_sf(data = centroids, color = 'black', shape = 8, size = 3) + 
  theme_void()


### Turn this into scatterpie 

centroids %>% geometry_to_lonlat() %>%
  rename(Region = WHO_region) %>%
  left_join(att) -> atpts

# move the SEARO and WPRO just a tiny bit apart 
atpts$lon[5] <- atpts$lon[5] - 5
atpts$lon[6] <- atpts$lon[6] + 20

colors <- met.brewer("Renoir", n=15)
who %>% 
  ggplot() + 
  geom_sf(aes(fill = WHO_region), color=NA, alpha = 0.6) +
  scale_fill_manual(values = met.brewer("Pillement", n=7), guide = "none") + 
  theme_bw() +
  coord_sf(xlim=c(-180,180), ylim=c(-55,90)) + 
  ggnewscale::new_scale_fill() +
  geom_scatterpie(data = atpts[,-1], 
                  aes(x=lon, y=lat, r = 1.2*RegionCount), 
                  cols=c("Heat-related deaths",
                         "Heat-related morbidity",
                         "Cold-related deaths",
                         "Extreme weather-related deaths",
                         "Injury",
                         "Infectious diseases",
                         "Non-communicable diseases",
                         "Air pollution-related deaths",
                         "Maternal and child health",
                         "Migration and displacement"), size = 0.2) +
  scale_fill_manual(values = colors[c(10,9,8,11,12,14,1,3,5,6)]) + 
  guides(fill=guide_legend(title="Health impact")) + 
  theme(panel.background=element_blank(),
        panel.spacing = unit(c(0, 0, 0, 0), "cm"),
        plot.margin = margin(-2,0,-2,0,'cm')) -> g1; g1

### Bottom panels

att <- read_csv("study_metadata.csv")
att <- att[c(1:20),] # Remove 2025 study

att %>%
  select(Year, `Health impact`) %>%
  rename(Impact = `Health impact`) %>%
  separate_rows(Impact, sep=",") %>%
  mutate(Impact = factor(Impact, levels = c("Heat-related deaths",
                                            "Heat-related morbidity",
                                            "Cold-related deaths",
                                            "Extreme weather-related deaths",
                                            "Injury",
                                            "Infectious diseases",
                                            "Non-communicable diseases",
                                            "Air pollution-related deaths",
                                            "Maternal and child health",
                                            "Migration and displacement"))) %>%
  ggplot(aes(x = Year, fill = Impact)) +
  geom_bar() +
  scale_fill_manual(values = colors[c(10,9,8,11,12,14,1,3,5,6)]) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = 'none') + 
  xlab("") + ylab("Study topic") + 
  scale_x_continuous(breaks=c(2016,2018,2020,2022,2024)) -> g2; g2

att %>%
  count(`Health variable(s)`) %>%
  ggplot(aes(x = reorder(`Health variable(s)`,-n), y = n)) + 
  geom_bar(fill = 'black', stat = 'identity') + 
  theme_classic() + 
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + ylab("Health variable")  -> g3; g3

att %>%
  separate_rows(`Climate variable(s)`, sep = ',') %>%
  count(`Climate variable(s)`) %>%
  ggplot(aes(x = reorder(`Climate variable(s)`, -n), y = n)) + 
  geom_bar(fill = 'black', stat = 'identity') + 
  theme_classic() + 
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + ylab("Climate variable") -> g4; g4

att %>%
  separate_rows(Timescale, sep = ',') %>%
  count(Timescale) %>%
  ggplot(aes(x = reorder(Timescale,-n), y = n)) + 
  geom_bar(fill = 'black', stat = 'identity') + 
  theme_classic() + 
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + ylab("Study timescale") -> g5; g5

g6 <- g2+g3+g4+g5+plot_layout(ncol = 4, widths = c(9, 5, 7, 2)); g6


########### Broken

g1 / g6 + 
  plot_layout(height = c(1,0.4), width = c(1,0.8), guides = 'collect') + 
  plot_annotation(tag_levels = 'a') + 
  coord_sf()
ggsave('Figure 1.pdf', width = 11.74, height = 7.94, units = "in")
