
#### Import clearcut data ---------------------------------------------------------------

files <- list.files('./DATA/From_GEE/clearcuts/', full.names = T)
files

clearcutsRaw <- tibble()
for (i in files){
  clearcutsRaw <- clearcutsRaw %>% 
    bind_rows(read_csv(i))
}
nrow(clearcutsRaw)
clearcutsCleaned <- st_as_sf(bind_cols(clearcutsRaw,geojson_sf(clearcutsRaw$.geo))) %>%
  mutate(ID = `system:index`)%>%
  st_transform(projCrs) %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  filter(area > 20000) %>%
  dplyr::select(-'.geo', -'system:index') 
nrow(clearcutsCleaned)
length(unique(clearcutsCleaned$ID))
mean(clearcutsCleaned$area)
sum(clearcutsCleaned$area)/1000000


# Write out for GEE
clearcutsCleaned %>%
  st_centroid() %>%
  st_transform(st_crs(4326)) %>%
  dplyr::select(ID, area) %>%
  st_write('./DATA/For_GEE/clearcutsCleaned.csv', layer_options = "GEOMETRY=AS_WKT",delete_dsn=TRUE) 

# Write out metadata with municipalities
clearcutsMeta <- clearcutsCleaned%>%
  dplyr::select(ID, area) %>%
  st_centroid()  %>%
  st_transform(st_crs(4326))   %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])%>%
  as_tibble() %>%
  dplyr::select(ID, area, lat, lon ) 

# Import municipalities and intersect with clearcuts - will use as spatial random effect
municipalities <- st_read("/") %>%
  st_transform(st_crs(4326)) %>%
  mutate(municipality = navn) %>%
  dplyr::select(municipality)

clearcutsMeta <- st_as_sf(clearcutsMeta, coords = c("lon", "lat"), crs = 4326) %>%
  st_intersection(municipalities) %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])%>%
  as_tibble() %>%
  dplyr::select(ID, area, lat, lon, municipality) 

clearcutsMeta %>%
  write_csv('./DATA/clearcuts_cleaned_metadata.csv')

# Random selection
set.seed(123)
clearcuts <- clearcutsCleaned #%>% sample_n(500)

nrow(clearcuts)
length(unique(clearcuts$ID))

clearcuts %>% ggplot(aes(x=area)) + geom_histogram()

#### Generate BACI zone geometries data  ---------------------------------------------------------------

z <- 3000
for (z in expZoneRadii){
  
  
  # Craete clearcut doghnut zones to act as controls 
  clearcutControlZones <- NULL
  clearcutExpZones <- NULL
  
  experimentRadius <- z
  controlZoneWidth <- 1000
  
  c <- '+8869-10234'
  for (c in unique(clearcuts$ID)){
    
    cSel <- clearcuts %>% filter(ID == c)
    
    # Create a 1km thick doghnut for a 3km buffer zone around clearcuts 
    # for intersecting Strava trails
    clearcutBufferOuter <- cSel %>%
      st_buffer(experimentRadius)
    
    clearcutBufferInner <- cSel %>%
      st_buffer(experimentRadius - controlZoneWidth)
    
    clearcutBuffer <- clearcutBufferOuter %>%
      st_difference(clearcutBufferInner) %>%
      dplyr::select(ID)
    
    # Plot to see
    ggplot() +
      geom_sf(data = cSel) +
      geom_sf(data = clearcutBuffer)
    
    if (is.null(clearcutControlZones)){
      clearcutControlZones <- clearcutBuffer
    } else {
      clearcutControlZones <- clearcutControlZones %>%
        bind_rows(clearcutBuffer)
    }
    
    if (is.null(clearcutExpZones)){
      clearcutExpZones <- clearcutBufferOuter
    } else {
      clearcutExpZones <- clearcutExpZones %>%
        bind_rows(clearcutBufferOuter)
    }
    
    
  }
  clearcutControlZones %>%
    st_write(paste0('./DATA/clearcut/clearcutControlZones/clearcutControlZones_', z, '.shp'), append=F)
  length(unique(clearcutControlZones$ID))
  clearcutExpZones %>%
    st_write(paste0('./DATA/clearcut/clearcutExpZones/clearcutExpZones_', z, '.shp'), append=F)
  length(unique(clearcutExpZones$ID))
  
  ggplot() +
    geom_sf(data = clearcutExpZones %>% filter(ID == clearcutExpZones$ID[1]))+
    geom_sf(data = clearcutControlZones %>% filter(ID == clearcutExpZones$ID[1]))+
    geom_sf(data = clearcuts %>% filter(ID == clearcutExpZones$ID[1]))
  
}

b <- 250
for (b in impactZoneRadii){
  
  # Define the zone considered for clearcut exposure
  clearcutImpactZones <- clearcuts %>%
    st_buffer(b)
  
  clearcutImpactZones %>%
    st_write(paste0('./DATA/clearcut/clearcutImpactZones/clearcutImpactZones_', b, '.shp'), append=F)
  
  
}

