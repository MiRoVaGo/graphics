#Not finished 

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
library(rgdal)

map_path = "../geodata/"

# read shapefiles
wmap <- readOGR(dsn = paste0(map_path, "ne_110m_land"), layer = "ne_110m_land")
wmap_df <- fortify(wmap)
bbox <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_wgs84_bounding_box") 
countries <- readOGR(paste0(map_path,"ne_110m_admin_0_countries"), layer = "ne_110m_admin_0_countries") 
grat <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_graticules_15") 
grat <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_graticules_30") 

countries_wintri <- spTransform(countries, CRS("+proj=wintri")) ##Change to wintri projection
bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
grat_wintri <- spTransform(grat, CRS("+proj=wintri"))

# create a blank ggplot theme for maps
theme_map <- list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_rect(fill = "white"),
                       panel.border = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_text(size = 22),
                       legend.key = element_rect(fill = "white")))

###############################################
#Old
###############################################

ggplot(bbox_wintri, aes(long, lat, group = group)) + 
  geom_polygon(fill = "grey70") +
  geom_polygon(data = countries_wintri, aes(long,lat, group = group, fill = hole)) + 
  geom_path(data = grat_wintri, aes(long, lat, group = group, fill = NULL), 
            linetype = "dashed", color="grey50") +
  scale_size_continuous(range = c(1, 10)) + 
  geom_polygon(fill = "transparent", col = 'black') +
  coord_equal(ratio = 1) + 
  theme_map +
  scale_fill_manual(values=c("black", "black"), guide = "none")

ggplot(bbox_wintri, aes(long, lat, group = group)) + 
  geom_polygon(data = wmap_wintri, aes(long,lat, group = group, fill = hole), col = 'grey30') + 
  geom_path(data = grat_wintri, aes(long, lat, group = group, fill = NULL), color="grey80") +
  geom_polygon(fill = "transparent", col = 'black') +
  scale_size_continuous(range = c(1, 10)) + 
  coord_equal(ratio = 1) + 
  theme_map +
  scale_fill_manual(values=c("white", "white"), guide = "none")

###############################################
#New: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
###############################################
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() 

ggplot(data = world[region_un == 'Europe'], aes(fill = economy)) +
  geom_sf() +
  coord_sf(xlim = c(-12, 40), ylim = c(34, 75), expand = FALSE) 

