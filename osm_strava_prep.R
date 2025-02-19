# Define source directory for Strava data
mastDir <- ''
list.files('')

# Import the OpenStreetMap base map for Strava activities
osm_raw <- st_read(paste0(mastDir, 'Shapefiles/norway_osm_20191217.shp')) %>%
  mutate(edge_id = id) %>%
  dplyr::select(edge_id, osm_id, km) %>%
  st_transform(projCrs)
names(osm_raw)

# Get some clearcut IDs to test with
clearcutExpZones_forFilter <- st_read('./DATA/clearcut/clearcutExpZones/clearcutExpZones_20000.shp')

set.seed(123)
testIDs <- sample(clearcutExpZones_forFilter$ID, 400)

z <- 20000
for (z in expZoneRadii){
  print(paste0('-x-----x------x------x-------x------x-------x doing control zone ', z))
  
  clearcutControlZones <- st_read(paste0('./DATA/clearcut/clearcutControlZones/clearcutControlZones_', z, '.shp'))
  
  osm_control <- osm_raw %>% 
    dplyr::select(edge_id, osm_id)%>%
    #st_join(clearcutControlZones, join = st_intersects) %>% 
    #filter(!is.na(ID)) %>%
    st_intersection(clearcutControlZones) %>%
    mutate(treatment = 'no clearcut')%>%
    dplyr::select(ID ,edge_id, osm_id, treatment)
  length(unique(osm_control$ID))
  
  
  as_tibble(osm_control) %>%
    dplyr::select(-geometry) %>%
    write_csv(paste0('./DATA/osm_clearcut/osmLookup_controlZone_',z,'.csv' ))
  
}

b <- 250
for (b in impactZoneRadii){
  
  
  print(paste0('-x-----x------x------x-------x------x-------x doing impact zone ', b))
  
  clearcutImpactZones <- st_read(paste0('./DATA/clearcut/clearcutImpactZones/clearcutImpactZones_', b, '.shp'))
  
  osm_clearcut <- osm_raw %>% 
    dplyr::select(edge_id, osm_id) %>%
    #st_join(clearcutImpactZones, join = st_intersects) %>% 
    #filter(!is.na(ID)) %>%
    st_intersection(clearcutImpactZones) %>%
    mutate(treatment = 'clearcut') %>%
    dplyr::select(ID ,edge_id, osm_id, treatment) 
  length(unique(osm_clearcut$ID))
  
  length(unique(osm_clearcut$ID))
  
  as_tibble(osm_clearcut) %>%
    dplyr::select(-geometry) %>%
    write_csv(paste0('./DATA/osm_clearcut/osmLookup_clearcutZone_',b,'.csv' ))
  
  
}

#### Full cover Strava data for GEE visualization -----------------------------------------------------------

ped2019full <- readStravaFull(paste0(mastDir, "/2019/norway_20190101_20191231_ped_edges/Edges/norway_20190101_20191231_ped_rollup_total.csv"),'ped' )
hist(ped2019full$activities)
quantile(ped2019full$activities, probs = c(0, 0.95, 1))

ride2019full <- readStravaFull(paste0(mastDir, "/2018/norway_20180101_20181231_ride_edges/Edges/norway_20180101_20181231_ride_rollup_total.csv"),'ride' )
hist(ride2019full$activities)
quantile(ride2019full$activities, probs = c(0, 0.95, 1))

ski2019full <- readStravaFull(paste0(mastDir, "/2019/norway_20190101_20191231_winter_edges/Edges/norway_20190101_20191231_winter_rollup_total.csv"), 'ski')
hist(ski2019full$activities)
quantile(ski2019full$activities, probs = c(0, 0.95, 1))

stravaFull <- ped2019full  %>%
  bind_rows(ride2019full ) %>%
  bind_rows(ski2019full) %>%
  group_by(actType) %>%
  mutate(act_lg_scaled = BBmisc::normalize(log(activities+0.1), method='range')) %>%
  pivot_wider(values_from = c('activities', 'act_lg_scaled'), names_from = actType, values_fill=0)
nrow(stravaFull)
length(unique(stravaFull$edge_id))

stravaFull %>%
  gather(key, val, act_lg_scaled_ped:act_lg_scaled_ski) %>%
  ggplot(aes(x=val)) +
  geom_histogram() +
  facet_grid(key~., scales='free')

stravaFull_spat <- st_read(paste0(mastDir, 'Shapefiles/norway_osm_20191217.shp')) %>%
  mutate(edge_id = id) %>%
  dplyr::select(edge_id, km) %>%
  filter(edge_id %in% unique(stravaFull$edge_id)) %>%
  left_join(stravaFull, by='edge_id')
head(stravaFull_spat)
stravaFull_spat %>%
  st_write('./DATA/For_GEE/strava_2019.csv', layer_options = "GEOMETRY=AS_WKT",delete_dsn=TRUE) 


#### Legacy code -------------------------------------------------------------------

getControlTrails <- function(id){
  #print(paste0('Getting control trails for....', id))
  
  # Define the edges in full experimental area
  osmSel <- osm_clipped %>%
    filter(ID == id)
  
  neighboringZones <- clearcutExpZones %>% 
    st_filter(osmSel, .predicate= st_intersects)%>%
    filter(ID != id) %>%
    pull(ID)
  
  ccontrol <- osmSel %>%
    filter(!ID %in% neighboringZones) %>%
    dplyr::select(-ID) %>%
    st_intersection(clearcutControlZones%>%
                      filter(ID == id))%>%
    mutate(treatment = 'no clearcut') %>%
    dplyr::select(ID ,edge_id, treatment) 
  
  
  return (ccontrol)
}
osm_control <- map(unique(osm_clearcut$ID), getControlTrails)
osm_control <- bind_rows(osm_control)




