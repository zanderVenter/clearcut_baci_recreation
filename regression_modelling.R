RUNFROMSCRATCH <- FALSE
dev.off()
# Import SSBgrid data from GEE
gridData <- st_read('./DATA/From_GEE/covariates_ssbgrid.geojson') %>%
  st_transform(projCrs) %>%
  filter(norwayMask == 1) %>%
  mutate(forest = forest*10*10,
         clearcut = clearcut*10*10,
         clearcut = ifelse(is.na(clearcut), 0, clearcut),
         forest = ifelse(is.na(forest), 0, forest),
         forest = forest + 0.001,
         clearcutRel = clearcut/forest,
         MAT = MAT*0.1) %>%
  dplyr::select(id,SSBID, MAP,clearcutRel, everything(), -norwayMask)
names(gridData)
ggplot() + 
  geom_sf(data=gridData %>% mutate(popDens = log(popDens+0.001)),aes( color=popDens, fill=popDens))
gridData %>%
  gather(key, val, MAP:terrainRuggedness) %>%
  ggplot(aes(x=val)) +
  geom_histogram() +
  facet_wrap(~key, scales='free')
dev.off()
print(gridData %>% filter(id == '00000000000000000242') %>% mutate(SSBID = as.character(SSBID)))

# Import time series clearcuts from grid data
gridDataTemp <- read_csv('./DATA/From_GEE/clearcut_years_ssbgrid.csv') %>%
  mutate(clearcut = mean*10*10) %>%
  left_join(gridData %>% dplyr::select(-clearcut,-first)) %>%
  drop_na(id) %>%
  dplyr::select(-'.geo', -'system:index', -geometry)
colSums(is.na(gridDataTemp))
gridDataTemp %>%
  group_by(year) %>%
  summarise(clearcut=sum(clearcut))

gridDataCoords <- gridData %>%
  mutate(geometry = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  as_tibble() %>%
  dplyr::select(SSBID, lat, lon)

# Define source directory for Strava data
mastDir <- ''
list.files('')

if (RUNFROMSCRATCH){
  
  # Import the OpenStreetMap base map for Strava activities
  osm_raw <- st_read(paste0(mastDir, 'Shapefiles/norway_osm_20191217.shp')) %>%
    mutate(edge_id = id) %>%
    dplyr::select(edge_id, osm_id, km) %>%
    st_transform(projCrs)
  osm_raw_lookup <- as.data.table(osm_raw %>% as_tibble() %>% dplyr::select(edge_id, osm_id, km))
  
  osm_grid_lookup <- osm_raw %>% 
    dplyr::select(edge_id, osm_id, km)%>%
    st_intersection(gridData) %>%
    as_tibble() %>%
    group_by(SSBID, edge_id, osm_id, km) %>%
    summarise()
  osm_grid_lookup <- as.data.table(osm_grid_lookup)
  
  osm_grid_lookup %>%
    write_csv('./DATA/osm_SSBgrid_lookup.csv')
} else {
  
  osm_grid_lookup <- read_csv('./DATA/osm_SSBgrid_lookup.csv')
  osm_grid_lookup <- as.data.table(osm_grid_lookup)
  
}


grid_trail_dens <- osm_grid_lookup %>%
  group_by(SSBID) %>%
  summarise(trailLength = sum(km, na.rm=T))

# Strava data
ped2017 <- readStrava(paste0(mastDir, "/2017/norway_20170101_20171231_ped_edges/Edges/norway_20170101_20171231_ped_rollup_total.csv"),'ped', 2017 )
ride2017 <- readStrava(paste0(mastDir, "/2017/norway_20170101_20171231_ride_edges/Edges/norway_20170101_20171231_ride_rollup_total.csv"),'ride', 2017 )
ski2017 <- readStrava(paste0(mastDir, "/2017/norway_20170101_20171231_winter_edges/Edges/norway_20170101_20171231_winter_rollup_total.csv"), 'ski', 2017)

ped2019 <- readStrava(paste0(mastDir, "/2019/norway_20190101_20191231_ped_edges/Edges/norway_20190101_20191231_ped_rollup_total.csv"),'ped', 2019 )
ride2019 <- readStrava(paste0(mastDir, "/2019/norway_south_20190101_20191231_ride_edges/Edges/norway_south_20190101_20191231_ride_rollup_total.csv"),'ride', 2019 ) %>%
  bind_rows(readStrava(paste0(mastDir, "/2019/norway_north_20190101_20191231_ride_edges/Edges/norway_north_20190101_20191231_ride_rollup_total.csv"),'ride', 2019 ))
ski2019 <- readStrava(paste0(mastDir, "/2019/norway_20190101_20191231_winter_edges/Edges/norway_20190101_20191231_winter_rollup_total.csv"), 'ski', 2019)

stravaJoined <- ped2017 %>%
  bind_rows(ride2017) %>%
  bind_rows(ski2017)%>%
  bind_rows(ped2019)%>%
  bind_rows(ride2019)%>%
  bind_rows(ski2019)

stravaJoined <- stravaJoined %>%
  bind_rows(stravaJoined %>%
              group_by(edge_id, year) %>%
              summarise(activities = sum(activities, na.rm=T)) %>%
              mutate(actType = 'all'))
stravaJoined %>%
  group_by(actType) %>%
  summarise(n=n())
stravaJoined <- as.data.table(stravaJoined)

stravaSub <- osm_grid_lookup[stravaJoined, on = .(edge_id), nomatch = 0]
nrow(stravaSub)

grid_activities <- as_tibble(stravaSub) %>%
  mutate(actKms = activities*km) %>%
  group_by(SSBID, actType) %>%
  summarise(actKms = sum(actKms, na.rm=T)) #sum(km, na.rm=T))
hist(grid_activities$actKms)

unique(grid_activities$actType)

regressionResults <- tibble()
i <- 'all'
for (i in c('ped','ride','ski','all')){
  grid_mod_data <- gridData %>%
    as_tibble() %>%
    left_join(gridDataCoords, by = 'SSBID') %>%
    left_join(grid_activities, by ='SSBID') %>%
    left_join(grid_trail_dens, by ='SSBID')  %>%
    filter(actType == i)
  
  # grid_mod_data %>%
  #   gather(key, val, MAP,MAT,clearcut:terrainRuggedness,actKms, trailLength) %>%
  #   ggplot(aes(x=val)) +
  #   geom_histogram() +
  #   facet_wrap(~key, scales='free')
  
  sds <- grid_mod_data %>% 
    summarise_at(vars(MAP, MAT,clearcutRel, clearcut:terrainRuggedness, trailLength), sd, na.rm=T) %>%
    gather(var, stdev)
  
  grid_mod_data_norm <- grid_mod_data%>%
    mutate(SSBID = factor(SSBID)) %>%
    mutate_at(vars(MAP,MAT,clearcutRel,clearcut:terrainRuggedness, trailLength, lat, lon), BBmisc::normalize, method="scale")
  
  model <- MASS::glm.nb(actKms ~ 
                          clearcut +
                          clearcutRel +
                          MAP +
                          MAT +
                          terrainRuggedness +
                          trailLength +
                          popDens +
                          forest +
                          distWater ,
                        data = grid_mod_data_norm)
  summary(model)
  r2 <- MuMIn::r.squaredGLMM(model)
  r2[1]
  simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
  #plot(simulationOutput)
  
  pred_x1 <- ggpredict(model, terms = "clearcut")
  #plot(pred_x1) 
  
  conf_intervals_raw <- confint(model, method = "Wald")
  
  conf_intervals <- conf_intervals_raw %>% as_tibble()%>%
    mutate(var = rownames(as.data.frame(conf_intervals_raw)))  %>%
    drop_na() %>%
    mutate(Estimate = as.data.frame(summary(model)$coefficients)$Estimate) %>%
    mutate_at(vars(`2.5 %`, `97.5 %`, Estimate), function(x)(exp(x) - 1) * 100)  %>%
    filter(var != '(Intercept)')
  conf_intervals
  
  # conf_intervals %>%
  #   ggplot(aes(x=var, y = Estimate)) +
  #   geom_hline(yintercept = 0, linetype=2) +
  #   geom_point() +
  #   geom_errorbar(width=0, aes(ymin=`2.5 %`, ymax=`97.5 %`)) +
  #   coord_flip()
  
  conf_intervals$Estimate[conf_intervals$var == 'clearcut']/sds$clearcut
  
  conf_intervals_out <- conf_intervals  %>%
    left_join(sds, by='var') %>%
    mutate(actual = round(Estimate*stdev, 2),
           actualLow = round(`2.5 %`*stdev, 2),
           actualHigh = round(`97.5 %`*stdev, 2))%>%
    mutate(label = paste0(actual, ' (', actualLow, ' to ', actualHigh, ')'))%>%
    mutate(actType = i,
           r2 = r2[1]) 
  
  regressionResults <- regressionResults %>%
    bind_rows(conf_intervals_out)
}

mean(regressionResults$r2)
regressionResults %>% filter(var == 'clearcut')

regressionResults %>%
  ggplot(aes(x=actType, y = r2)) +
  geom_point()

regressionResults %>%
  ggplot(aes(x=var, y = Estimate)) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_point() +
  geom_errorbar(width=0, aes(ymin=`2.5 %`, ymax=`97.5 %`)) +
  coord_flip() +
  facet_wrap(~actType)

regressionResults %>%
  ggplot(aes(x=var, y = actual)) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_point() +
  geom_errorbar(width=0, aes(ymin=actualLow, ymax=actualHigh)) +
  coord_flip() +
  facet_wrap(~actType)

regressionResults %>%
  filter(var %in% c('clearcut')) %>%
  ggplot(aes(x=actType, y = Estimate)) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_point() +
  geom_errorbar(width=0, aes(ymin=`2.5 %`, ymax=`97.5 %`)) +
  coord_flip() +
  facet_wrap(~var)

regressionResults %>%
  filter(var %in% c('clearcut')) %>%
  ggplot(aes(x=actType, y = actual)) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_point() +
  geom_errorbar(width=0, aes(ymin=actualLow, ymax=actualHigh)) +
  coord_flip() +
  facet_wrap(~var)


regressionResultsToPlot <- regressionResults  %>%
  filter(!var %in% c('clearcutRel')) %>%
  mutate(actType = factor(actType),
         var = factor(var, levels = c('clearcut', 'forest', 'trailLength', 'popDens', 'terrainRuggedness', 'distWater', 'MAT', 'MAP')))
levels(regressionResultsToPlot$var) <- c('Clearcut area', 'Forest area','Trail length','Population density','Terrain diversity',
                                         'Distance to water', 'Temperature', 'Precipitation')
levels(regressionResultsToPlot$actType) <- c('Total', 'Walk/run/hike', 'Ride', 'Ski')
regressionResultsToPlot

makeSuppFigCoeffs <- function(){
  regressionResultsToPlot %>%
    ggplot(aes(x=var, y = Estimate)) +
    geom_hline(yintercept = 0, linetype=2, alpha=0.5) +
    geom_point(size=0.5,shape=23) +
    geom_errorbar(width=0, aes(ymin=`2.5 %`, ymax=`97.5 %`)) +
    labs(y = expression('%'~Delta~' in recreational activity per'~sigma~' increase in predictor'),
         x = '') +
    coord_flip() +
    facet_wrap(~actType) +
    theme_bw()
}

makeSuppFigCoeffs()

ggsave("figS5.png", makeSuppFigCoeffs(), width = 17, height=13, units='cm')

makeFig6 <- function(){
  regressionResults %>%
    filter(var == 'clearcut') %>%
    ggplot(aes(x=factor(actType), y=actual)) +
    geom_point(position = position_dodge(width=0.25)) +
    geom_errorbar(aes(ymin=actualLow, ymax =actualHigh ),
                  position = position_dodge(width=0.25),
                  width = 0)+
    geom_hline(yintercept = 0, linetype=2) + 
    labs(y =  'Change (%) in recreational activity per\n square kilometer increase in clearcut area',
         x = '')+
    theme(axis.text = element_text(size=7),
          axis.title = element_text(size=8))+
    scale_x_discrete(labels = c('All', 'Walk/run/hike', 'Ride', 'Ski')) +
    ylim(-15, 25) +
    cowplot::draw_image(magick::image_read('./DATA/all_img.png'),hjust=-0.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ped_img.png'),hjust=-1.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ride_img.png'),hjust=-2.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ski_img.png'),hjust=-3.5, valign=4,scale = 5)
}



ggsave("fig6.png", makeFig6(), width = 10, height=8, units='cm')


#

# Clearcut effect - % change activity per clearcut assuming average size 7 ha
regressionResults %>%
  filter(actType == 'all', var == 'clearcut') %>%
  mutate_at(vars(actual:actualHigh), function(x){x/0.01/7})

# Clearcut effect - based on total activity numbers per type over Norway
sumAct <- grid_activities %>%
  group_by(actType) %>%
  summarise(actKms = sum(actKms)) 
sumAct

sum(gridData$clearcut)

getClearcutRegressionEffects <- function(){
  regressionResults %>%
    #filter(actType == 'all') %>%
    filter(var == 'clearcut') %>%
    mutate(ci = actual - actualLow)
}
getClearcutRegressionEffects()

regressionResults %>%
  #filter(actType == 'all') %>%
  filter(var == 'clearcut') %>%
  left_join(sumAct, by = 'actType') %>%
  # Get % change relative to sum activity counts
  mutate(actual = actual/100*actKms,
            actualLow = actualLow/100*actKms,
            actualHigh = actualHigh/100*actKms) %>%
  # extrapolate to population level base on https://www.sciencedirect.com/science/article/pii/S0169204623000051#f0020
  mutate_at(vars(actual:actualHigh), function(x){ x/(5.5/100)})  %>%
  # multiply up to km2 forest harvest 2018 #https://www.globalforestwatch.org/dashboards/country/NOR/
  #mutate_at(vars(actual:actualHigh), function(x){x*590}) %>%
  # multiply up to km2 forest harvest from our data
  mutate_at(vars(actual:actualHigh), function(x){x*sum(gridData$clearcut)}) %>%
  # divide by 5km as average trip distance to get # trips instead of actkms
  mutate_at(vars(actual:actualHigh), function(x){x/5})



#### Legacy effect clearcuts -----------------------------------------------------

colSums(is.na(gridDataTemp))

regressionResultsTemp <- tibble()
i <- 'all'
y <- 2000
for (y in c(2008,2009, 2010, 2011, 2012,2013,2014,2015,2016,2017,2018)){
  
  for (i in c('ped','ride','ski','all')){
    
    grid_mod_data <- gridDataTemp %>%
      filter(year == y) %>%
      as_tibble() %>%
      left_join(gridDataCoords, by = 'SSBID') %>%
      left_join(grid_activities, by ='SSBID') %>%
      left_join(grid_trail_dens, by ='SSBID')  %>%
      filter(actType == i) %>%
      drop_na()
    
    sds <- grid_mod_data %>% 
      summarise_at(vars(clearcut, MAP:terrainRuggedness, trailLength), sd, na.rm=T) %>%
      gather(var, stdev)
    
    grid_mod_data_norm <- grid_mod_data%>%
      mutate(SSBID = factor(SSBID)) %>%
      mutate_at(vars(clearcut, MAP:terrainRuggedness), BBmisc::normalize, method="scale")
    
    model <- MASS::glm.nb(actKms ~ 
                            clearcut +
                            clearcutRel +
                            MAP +
                            MAT +
                            terrainRuggedness +
                            trailLength +
                            popDens +
                            forest +
                            distWater ,
                          data = grid_mod_data_norm)
    summary(model)
    r2 <- MuMIn::r.squaredGLMM(model)
    r2[1]
    simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
    #plot(simulationOutput)
    
    pred_x1 <- ggpredict(model, terms = "clearcut")
    #plot(pred_x1) 
    
    conf_intervals_raw <- confint(model, method = "Wald")
    
    conf_intervals <- conf_intervals_raw %>% as_tibble()%>%
      mutate(var = rownames(as.data.frame(conf_intervals_raw)))  %>%
      drop_na() %>%
      mutate(Estimate = as.data.frame(summary(model)$coefficients)$Estimate) %>%
      mutate_at(vars(`2.5 %`, `97.5 %`, Estimate), function(x)(exp(x) - 1) * 100)  %>%
      filter(var != '(Intercept)')
    conf_intervals
    
    # conf_intervals %>%
    #   ggplot(aes(x=var, y = Estimate)) +
    #   geom_hline(yintercept = 0, linetype=2) +
    #   geom_point() +
    #   geom_errorbar(width=0, aes(ymin=`2.5 %`, ymax=`97.5 %`)) +
    #   coord_flip()
    
    conf_intervals$Estimate[conf_intervals$var == 'clearcut']/sds$clearcut
    
    conf_intervals_out <- conf_intervals  %>%
      left_join(sds, by='var') %>%
      mutate(actual = round(Estimate*stdev, 2),
             actualLow = round(`2.5 %`*stdev, 2),
             actualHigh = round(`97.5 %`*stdev, 2))%>%
      mutate(label = paste0(actual, ' (', actualLow, ' to ', actualHigh, ')'))%>%
      mutate(actType = i,
             year = y,
             r2 = r2[1]) 
    
    regressionResultsTemp <- regressionResultsTemp %>%
      bind_rows(conf_intervals_out)
  }
  
}
dev.off()

regressionResultsTemp %>%
  filter(var == 'clearcut') %>%
  ggplot(aes(x=factor(year), y = Estimate)) +
  geom_hline(yintercept = 0, linetype=2, alpha=0.5) +
  geom_point(size=0.5,shape=23) +
  geom_errorbar(width=0, aes(ymin=`2.5 %`, ymax=`97.5 %`)) +
  #labs(y = expression('%'~Delta~' in recreational activity per'~sigma~' increase in predictor'),
  #     x = '') +
  facet_wrap(~actType) +
  theme_bw()
