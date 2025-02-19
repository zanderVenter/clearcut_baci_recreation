#### Snow cover inside clearcut and control ----------------------------
z <- 20000
b <- 1000
osmFinal_df <- st_read(paste0('./DATA/clearcut/clearcutControlZones/clearcutControlZones_', z, '.shp')) %>%
  mutate(treatment = 'clearcut') %>%
  bind_rows(st_read(paste0('./DATA/clearcut/clearcutImpactZones/clearcutImpactZones_', b, '.shp')) %>%
              mutate(treatment = 'control') %>% dplyr::select(treatment))
unique(osmFinal_df$treatment)

osmFinal_df %>%
  st_transform(st_crs(4326)) %>%
  st_write('./DATA/For_GEE/clearcuts_1000_control_20000.shp', append=F)

snowcover <- read_csv('./DATA/From_GEE/snow_cover.csv') %>%
  gather(year, coverPerc, snowDetecRate2017, snowDetecRate2019) %>%
  mutate(year = ifelse(year == 'snowDetecRate2017', 2017, 2019),
         coverPerc = coverPerc*100) %>%
  group_by(year, treatment)  %>%
  summarise(median = mean(coverPerc, na.rm=T),
            sd = sd(coverPerc),
            se = sd/n()) 
5.29-5.19
5.48-5.38

snowPlot <- snowcover %>%
  ggplot(aes(x=year, y=round(median,1), color=treatment)) +
  geom_point(position = position_dodge(width=0.1)) +
  geom_line(position = position_dodge(width=0.1))+
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd), 
                width=0.1,
                position = position_dodge(width=0.1)) + 
  scale_x_continuous(breaks  = c( 2017, 2019)) +
  labs(y = 'Snow detection rate (%)',
       x = 'Year') +
  theme(legend.position = 'top')
snowPlot

ggsave("figS7_forQGIS.png", snowPlot, width = 9, height=8, units='cm')


#### Winter alpine sports -----------------------------------------------
norway <- st_read('./DATA/Norge.shp') %>%
  st_transform(st_crs(4326))

library(osmdata)

# Define bounding box for Norway
norway_bbox <-  c(10.2, 59.6, 11.1, 60.3) #c(4.085, 57.8, 31.5, 71.4) # (min_lon, min_lat, max_lon, max_lat)

norway_bbox <-  c(4.085, 57.8, 31.5, 71.4) # (min_lon, min_lat, max_lon, max_lat)

# Create Overpass query for landuse=winter_sports
norway_winter_sports <- opq(bbox = norway_bbox) %>% 
  add_osm_feature(key = "landuse", value = "winter_sports") %>%
  osmdata_sf()

alpine <- rbind(norway_winter_sports$osm_polygons %>% dplyr::select("piste:type"), 
                              norway_winter_sports$osm_multipolygons %>% dplyr::select("piste:type")) %>%
  mutate(type = `piste:type`) %>%
  st_make_valid() %>%
  st_filter(norway)

nrow(alpine)

alpine %>%
  ggplot(aes(x= type)) +
  geom_bar()

ggplot() +
  geom_sf(data = alpine)

library(leaflet)

# Create an interactive Leaflet map
leaflet() %>%
  addTiles() %>%  # Add OpenStreetMap tiles
  addPolygons(data = alpine ,
              color = "blue", 
              weight = 2, 
              opacity = 0.8, 
              fillOpacity = 0.5,
              popup = ~type) # Add popup with 'name' attribute if available


# Clearcuts intersection


clearcutIntersects <- tibble()

for (b in impactZoneRadii){
  clearcuts <- st_read(paste0('./DATA/clearcut/clearcutImpactZones/clearcutImpactZones_', b, '.shp'))%>%
    st_transform(st_crs(4326))
  clearcutAlpine <- clearcuts %>%
    st_filter(alpine)
  clearcutIntersects <- clearcutIntersects %>%
    bind_rows(tibble(b = b, n = nrow(clearcutAlpine), perc = n/10781*100))
}
clearcutIntersects

map <- ggplot() +
  geom_sf(data = st_transform(norway, st_crs(25833)), fill='grey') +
  geom_sf(data =  st_transform(alpine, st_crs(25833)), color='red', fill='red') +
  theme_void() +
  labs(title = 'A) Alpine ski piste in Norway (red)')
map

graph <- clearcutIntersects %>%
  ggplot(aes(x = factor(b), y=perc)) +
  geom_bar(stat='identity') +
  geom_label(aes(label = paste0('n = ',n))) +
  labs(x='Clearcut proximity to alpine ski piste (m)',
       y = 'Percentage of clearcuts in our study (%)',
       title = 'B) Percentage clearcuts within or nearby') +
  ylim(c(0,1.5))+
  coord_flip()
graph

out <- grid.arrange(map, graph,   nrow=1,
             padding = unit(0, "line"),widths=c(1,1), newpage = T)
out

ggsave("figS6.png", out, width = 22, height=12, units='cm')
