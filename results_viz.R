
#expZoneRadii <- c(20000)
#impactZoneRadii <- c(0, 250, 500, 1000, 2500, 5000) 

#### Import covariates --------------------------------------------------------------
clearcutsMeta <- read_csv('./DATA/clearcuts_cleaned_metadata.csv')

covariatesCont <- read_csv('./DATA/From_GEE/covariates.csv') %>%
  mutate(clearcutPerc = clearcuts*100,
         # SSB data are pop number in 250x250 grid
         # therefore mulitply by 16 to get to people/km2
         popDens = pop*16) %>%
  dplyr::select(ID, clearcutPerc, pop, popDens) %>%
  left_join(clearcutsMeta, by = 'ID')

covQuants <- covariatesCont %>%
  summarise_at(vars(clearcutPerc:area), quantile, c(0, 0.33, 0.66,  1), na.rm=T)
covQuants

covariatesCat <- covariatesCont %>%
  mutate(clearcutPerc = ifelse(clearcutPerc < covQuants$clearcutPerc[2], 'Low',
                               ifelse(clearcutPerc >= covQuants$clearcutPerc[2] & clearcutPerc < covQuants$clearcutPerc[3], 'Medium', 'High'))) %>%
  mutate(pop = ifelse(pop < covQuants$pop[2], 'Low',
                      ifelse(pop >= covQuants$pop[2] & pop < covQuants$pop[3], 'Medium', 'High'))) %>%
  mutate(area = ifelse(area < covQuants$area[2], 'Low',
                       ifelse(area >= covQuants$area[2] & area < covQuants$area[3], 'Medium', 'High')))

municipalities <- st_read("") %>%
  st_transform(st_crs(4326)) %>%
  mutate(municipality = navn) %>%
  dplyr::select(municipality)

# Bring in ID-level baci scores
baciManualIDlevel <- tibble()

for (z in expZoneRadii){
  
  for (b in impactZoneRadii){
    
    baciManualIDlevel <- baciManualIDlevel %>%
      bind_rows(read_csv(paste0('./OUTPUT/baciManual/baciIDlevel_',z,'_',b,'.csv' )) %>%
                  mutate(buffer = b, 
                         expZone = z))
    
  }
  
}

baciExplanID <- baciManualIDlevel %>%
  filter(type == 'diff')  %>%
  group_by(actType, ID) %>%
  summarise_at(vars(baci), mean, na.rm=T)  %>%
  left_join(covariatesCat, by = 'ID')%>%
  mutate_at(vars(clearcutPerc, pop, area), factor, levels=c('Low', 'Medium', 'High')) %>%
  left_join(covariatesCont %>%
              mutate(popC = pop+1,
                     areaC = area,
                     clearcutPercC = clearcutPerc) %>%
              dplyr::select(ID, popC:clearcutPercC), by = 'ID') %>%
  # extrapolate to population level base on https://www.sciencedirect.com/science/article/pii/S0169204623000051#f0020
  mutate(baciPop = baci/(5.5/100))


# Bring in ID-level strava  counts
stravaBaci <- tibble()
for (z in expZoneRadii){
  
  for (b in impactZoneRadii){
    stravaBaci <- stravaBaci %>%
      bind_rows(read_csv(paste0('./DATA/strava/stravaAgg_',z,'_',b,'.csv' )) %>%
                  mutate(buffer = b,
                         expZone = z))
    
  }
}

getTotActivities <- function(){
  act2017 <- stravaBaci %>%
    filter(buffer == 1000 & treatment == 'clearcut' & year == 2017 & actType == 'all' & expZone == 20000) %>%
    summarise(activities_agg = sum(activities_agg)) %>% pull(activities_agg)
  
  
  act2019 <- stravaBaci %>%
    filter(buffer == 1000 & treatment == 'clearcut' & year == 2019 & actType == 'all' & expZone == 20000) %>%
    summarise(activities_agg = sum(activities_agg)) %>% pull(activities_agg)
  
  print(act2017 + act2019)
}
getTotActivities()

getTotActivitiesType <- function(){
  stravaBaci %>%
    filter(buffer == 1000 & treatment == 'clearcut' & year == 2017  & expZone == 20000) %>%
    group_by(actType) %>%
    summarise(activities_agg2017 = sum(activities_agg)) %>%
    left_join(stravaBaci %>%
                filter(buffer == 1000 & treatment == 'clearcut' & year == 2019  & expZone == 20000) %>%
                group_by(actType) %>%
                summarise(activities_agg2019 = sum(activities_agg))) %>%
    mutate(sum=activities_agg2019 + activities_agg2017)
  
}
getTotActivitiesType()


# Import local case - Oslo marka
marka <- st_read('./DATA/oslo_marka.shp')%>%
  st_transform(st_crs(4326)) %>%
  filter(OBJECTID %in% c(6,5)) %>%
  mutate(name = ifelse(OBJECTID == 5, 'Østmarka', 'Nordmarka'))
ggplot() + geom_sf(data = marka, aes(fill=factor(OBJECTID)))

clearcutsMarka <- st_as_sf(clearcutsMeta, coords = c("lon", "lat"), crs = 4326) %>%
  st_intersection(marka) %>%
  drop_na(name) %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])%>%
  as_tibble() %>%
  dplyr::select(ID, area, lat, lon, name) 
clearcutsMarka %>% ggplot(aes(x=name)) + geom_bar()


# Write out osm trails for marka
mastDir <- '/'
osm_raw <- st_read(paste0(mastDir, 'Shapefiles/norway_osm_20191217.shp')) %>%
  mutate(edge_id = id) %>%
  dplyr::select(edge_id, osm_id, km) %>%
  st_transform(projCrs)

osm_marka <- st_read('../QGIS/Shapefiles/osm_marka.shp')
osm_raw

osm1000 <- read_csv('./DATA/osm_clearcut/osmLookup_clearcutZone_1000.csv') %>%
  group_by(edge_id) %>% summarise()
getLengthOSMtrails <- function(){
  osm_raw %>% as_tibble() %>% left_join(osm1000, by = 'edge_id')  %>% summarise(km = sum(km, na.rm=T))
}
getLengthOSMtrails()


#### Import model results ------------------------------------------------------------
# Overall national level
baciGlmerOverall <- tibble()

for (z in expZoneRadii){
  
  for (b in impactZoneRadii){
    
    baciGlmerOverall <- baciGlmerOverall %>%
      bind_rows(read_csv(paste0('./OUTPUT/baciModel/baciGlmerOverall_',z,'_',b,'.csv' )) %>%
                  mutate(buffer = b, 
                         expZone = z))
    
  }
  
}

baciGlmerMarka <- tibble()

for (c in c('Østmarka', 'Nordmarka')){
  for (z in expZoneRadii){
    
    for (b in c(1000)){
      
      baciGlmerMarka <- baciGlmerMarka %>%
        bind_rows(read_csv(paste0('./OUTPUT/baciModelMarka/baciGlmer_',z,'_',b,'_',c,'.csv' )) %>%
                    mutate(buffer = b, 
                           expZone = z,
                           marka = c))
      
    }
    
  }
}


baciGlmerUrbRural <- tibble()

for (c in c('Urban', 'Rural')){
  for (z in expZoneRadii){
    
    for (b in c(1000)){
      
      baciGlmerUrbRural <- baciGlmerUrbRural %>%
        bind_rows(read_csv(paste0('./OUTPUT/baciModelUrbRural/baciGlmer_',z,'_',b,'_',c,'.csv' )) %>%
                    mutate(buffer = b, 
                           expZone = z,
                           urbRural = c))
      
    }
    
  }
}
baciGlmerUrbRural

#### Modelled baci results - overall ------------------------------------------------------------

# Check spatial autocorrelation results
baciGlmerOverall %>%
  group_by(buffer, actType, expZone) %>%
  summarise(spatAc_obs = mean(spatAc_obs)) %>%
  ggplot(aes(x=factor(buffer), y=spatAc_obs, color=factor(expZone), group = factor(expZone))) +
  geom_point() +
  geom_line()+
  facet_wrap(~actType, scales='free')

# Check model AIC
baciGlmerOverall %>%
  group_by(expZone, buffer, actType) %>%
  summarise(aic = mean(aic))%>%
  ggplot(aes(x=factor(buffer), y=aic, color=actType, group = actType)) +
  geom_point() +
  geom_line() +
  facet_wrap(~expZone)

# Check model R2
baciGlmerOverall %>%
  group_by(expZone, buffer, actType) %>%
  summarise(r2 = mean(r2m))%>%
  ggplot(aes(x=factor(buffer), y=r2, color=factor(expZone), group = factor(expZone))) +
  geom_point() +
  geom_line() +
  facet_wrap(~actType)

# Modelled baci scores overall - best performing model
baciGlmerOverall%>%
  filter((expZone == 20000 & buffer == 1000 & actType == 'ped') |
           (expZone == 20000 & buffer == 500 & actType == 'ride') |
           (expZone == 10000 & buffer == 0 & actType == 'ski') |
           (expZone == 20000 & buffer == 500 & actType == 'all')) %>%
  ggplot(aes(x=actType, y=Estimate)) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2)

# Modelled baci scores by imact zone
baciGlmerOverall%>%
  #group_by(buffer, expZone, actType) %>%
  #summarise_at(vars(Estimate, lowCI, upCI), mean) %>%
  ggplot(aes(x=factor(buffer), y=Estimate, color=factor(expZone))) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~actType, scales='free')

# modelled baci scored aggregated by impact zone
baciGlmerOverallZone <- baciGlmerOverall %>% 
  group_by(actType, buffer) %>%
  summarise_at(vars(Estimate, lowCI, upCI), mean) 
baciGlmerOverallZone %>%
  ggplot(aes(x=factor(buffer), y=Estimate)) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~actType) + 
  labs(y = 'BACI effect (% change activity relative to control)',
       x = 'Impact zone radius')

# Overall modelled baci scores
overallEffects <- baciGlmerOverall%>%
  group_by(actType) %>%
  summarise_at(vars(Estimate, lowCI, upCI), mean) %>%
  mutate(ci = Estimate -lowCI)
getOverallEffects <- function(){
  return (overallEffects)
}
getOverallEffects()

# Get sample size overall
getOverallSampleSize <- function(){
  baciGlmerOverall %>%
    group_by(actType) %>%
    summarise(nID = max(nID))
}
getOverallSampleSize()

overallEffects %>%
  ggplot(aes(x=factor(actType), y=Estimate)) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2) + 
  labs(y = 'BACI effect (% change activity relative to control)',
       x = '')

# (by actType) Get population level estimates of change based on ID-level baci scores and national-level modelled estimates
overallEffects_abs <- stravaBaci %>%
  filter(treatment == 'clearcut' & year == 2017) %>%
  # average counts over buffer and control zone combinations
  group_by(ID, actType) %>%
  summarise(activities_agg = mean(activities_agg)) %>%
  # sum counts over clearcuts
  group_by(actType) %>%
  summarise(activities_agg = sum(activities_agg),
            ncuts = n()) %>%
  # extrapolate to population level base on https://www.sciencedirect.com/science/article/pii/S0169204623000051#f0020
  mutate(activities_pop = activities_agg/(5.5/100)) %>%
  # join with national-level model estimates
  left_join(overallEffects, by='actType') %>%
  mutate(Estimate_abs = Estimate/100*activities_pop,
         upCI_abs = upCI/100*activities_pop,
         lowCI_abs = lowCI/100*activities_pop)

overallEffects_abs %>%
  ggplot(aes(x=factor(actType), y=Estimate_abs)) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI_abs, ymax =upCI_abs ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2) + 
  labs(y = 'Absolute BACI effect (change activity relative to control)',
       x = '')
dev.off()

makeFig2 <- function(){
  
  p1 <- overallEffects %>%
    ggplot(aes(x=factor(actType), y=Estimate)) +
    geom_point(position = position_dodge(width=0.25)) +
    geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                  position = position_dodge(width=0.25),
                  width = 0)+
    geom_hline(yintercept = 0, linetype=2) + 
    labs(y = 'Relative BACI effect \n (% change activity relative to control)',
         x = '',
         title = 'A)')+
    theme(axis.text = element_text(size=7),
          axis.title = element_text(size=8))+
    scale_x_discrete(labels = c('All', 'Walk/run/hike', 'Ride', 'Ski')) +
    ylim(-15, 35) +
    cowplot::draw_image(magick::image_read('./DATA/all_img.png'),hjust=-0.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ped_img.png'),hjust=-1.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ride_img.png'),hjust=-2.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ski_img.png'),hjust=-3.5, valign=4,scale = 5)
  
  p2 <-   overallEffects_abs %>%
    ggplot(aes(x=factor(actType), y=Estimate_abs)) +
    geom_point(position = position_dodge(width=0.25)) +
    geom_errorbar(aes(ymin=lowCI_abs, ymax =upCI_abs ),
                  position = position_dodge(width=0.25),
                  width = 0)+
    geom_hline(yintercept = 0, linetype=2) + 
    labs(y = 'Absolute BACI effect \n (change activity relative to control)',
         x = '',
         title = 'B)')+
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6),
                       limits = c(-2500000, 1000000))+
    scale_x_discrete(labels = c('All', 'Walk/run/hike', 'Ride', 'Ski')) +
    theme(axis.text = element_text(size=7),
          axis.title = element_text(size=8))+
    cowplot::draw_image(magick::image_read('./DATA/all_img.png'),hjust=-0.5, valign=6.2,scale = 400000)+
    cowplot::draw_image(magick::image_read('./DATA/ped_img.png'),hjust=-1.5, valign=6.2,scale = 400000)+
    cowplot::draw_image(magick::image_read('./DATA/ride_img.png'),hjust=-2.5, valign=6.2,scale = 400000)+
    cowplot::draw_image(magick::image_read('./DATA/ski_img.png'),hjust=-3.5, valign=6.2,scale = 400000)
  p2
  
  fig2 <- grid.arrange(p1, p2,   nrow=1,
                       padding = unit(0, "line"),widths=c(1,1), newpage = T)
  
  fig2
  
  
}
makeFig2()
ggsave("fig2.png", makeFig2(), width = 18, height=7, units='cm')

getOverallEffects_abs <- function(){
  print(overallEffects_abs %>%
          mutate(ci = Estimate_abs - lowCI_abs,
                 Estimate_abs_percut = Estimate_abs/ncuts,
                 upCI_abs_percut =upCI_abs/ncuts,
                 lowCI_percut = lowCI_abs/ncuts,) %>%
          dplyr::select(actType, ci, Estimate_abs:lowCI_percut))
}
getOverallEffects_abs()


# (by actType & impact zone) Get population level estimates of change based on ID-level baci scores and national-level modelled estimates
overallEffects_zone_abs <- stravaBaci %>%
  filter(treatment == 'clearcut' & year == 2017) %>%
  # average counts over buffer and control zone combinations
  group_by(ID, actType, buffer) %>%
  summarise(activities_agg = mean(activities_agg)) %>%
  # sum counts over clearcuts
  group_by(actType, buffer) %>%
  summarise(activities_agg = sum(activities_agg)) %>%
  # extrapolate to population level base on https://www.sciencedirect.com/science/article/pii/S0169204623000051#f0020
  mutate(activities_pop = activities_agg/(5.5/100))%>%
  # join with national-level model estimates
  left_join(baciGlmerOverallZone, by=c('actType', 'buffer')) %>%
  mutate(Estimate_abs = Estimate/100*activities_pop,
         upCI_abs = upCI/100*activities_pop,
         lowCI_abs = lowCI/100*activities_pop)

  
# modelled baci scored absolute aggregated by impact zone
overallEffects_zone_abs %>%
  ggplot(aes(x=factor(buffer), y=Estimate_abs)) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI_abs, ymax =upCI_abs ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~actType) + 
  labs(y = 'BACI effect (absolute change activity relative to control)',
       x = 'Exposure zone radius')

getOverallEffectsByZone <- function(){
  baciGlmerOverallZone
}
getOverallEffectsByZone()

type <- 'ski'
makeSubFig3_top <- function(type, ylab, xlab, title){
 p <-  baciGlmerOverallZone %>%
   filter(actType == type) %>%
   ggplot(aes(x=factor(buffer), y=Estimate)) +
   geom_point(position = position_dodge(width=0.25)) +
   geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                 position = position_dodge(width=0.25),
                 width = 0)+
   geom_hline(yintercept = 0, linetype=2)+
   labs(y = 'Relative BACI effect (%)',
        x = 'Clearcut exposure zone radius (m)',
        title=title) + 
   ylim(-15, 45)  +
   #theme_bw() +
   theme(axis.text = element_text(size=7),
         axis.title = element_text(size=8))
 
 if (!ylab){
   p <- p + theme(axis.title.y=element_blank())
 }
 if (!xlab){
   p <- p + theme(axis.title.x=element_blank())
 }
 if (type == 'all'){
   p <- p +
     annotation_custom(rasterGrob(readPNG(paste0('./DATA/', type, '_img.png')), interpolate=T),
                       xmin='0', xmax='1000', ymin=30, ymax=40)
 } else {
   p <- p +
     annotation_custom(rasterGrob(readPNG(paste0('./DATA/', type, '_img.png')), interpolate=T),
                       xmin='250', xmax='500', ymin=30, ymax=40)
 }
 
 return (p)
}

makeFig3 <- function(){
  f3a <- makeSubFig3_top('all', TRUE, TRUE, 'A)')
  f3a
  f3b <- makeSubFig3_top('ped', FALSE, TRUE, 'B)')
  f3c <- makeSubFig3_top('ride', FALSE, TRUE, 'C)')
  f3d <- makeSubFig3_top('ski', FALSE, TRUE, 'D)')
  fig3 <- grid.arrange(f3a, f3b, f3c, f3d,   nrow=1,
                       padding = unit(0, "line"),widths=c(1.1,1,1,1), newpage = T)
  return (fig3)
}
fig3 <- makeFig3()

ggsave("fig3.png", fig3, width = 22, height=7, units='cm')

#### Urban versus rural BACI effects ---------------------------------------------------------------------------------

baciGlmerUrbRural

overallEffectsUrbRural <- baciGlmerUrbRural %>%
  dplyr::select(actType, Estimate, lowCI, upCI,  urbRural) %>%
  mutate(urbRural = ifelse(urbRural == 'Urban', 'Peri-urban', 'Rural'))

urbRuralEffects_abs <- stravaBaci %>%
  filter(treatment == 'clearcut' & year == 2017) %>%
  # average counts over buffer and control zone combinations
  group_by(ID, actType) %>%
  summarise(activities_agg = mean(activities_agg)) %>%
  # sum counts over clearcuts
  group_by(actType) %>%
  summarise(activities_agg = sum(activities_agg),
            ncuts = n()) %>%
  # extrapolate to population level base on https://www.sciencedirect.com/science/article/pii/S0169204623000051#f0020
  mutate(activities_pop = activities_agg/(5.5/100)) %>%
  # join with national-level model estimates
  left_join(overallEffectsUrbRural, by='actType') %>%
  mutate(Estimate_abs = Estimate/100*activities_pop,
         upCI_abs = upCI/100*activities_pop,
         lowCI_abs = lowCI/100*activities_pop)

makeSuppFigUrbRural <- function(){
  
  p1 <- overallEffectsUrbRural %>%
    ggplot(aes(x=factor(actType), y=Estimate, color=urbRural)) +
    geom_point(position = position_dodge(width=0.25)) +
    geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                  position = position_dodge(width=0.25),
                  width = 0)+
    geom_hline(yintercept = 0, linetype=2) + 
    labs(y = 'Relative BACI effect \n (% change activity relative to control)',
         #title = 'A)',
         x = '' )+
    theme(axis.text = element_text(size=7),
          axis.title = element_text(size=8))+
    scale_x_discrete(labels = c('All', 'Walk/run/hike', 'Ride', 'Ski')) +
    ylim(-15, 35) +
    cowplot::draw_image(magick::image_read('./DATA/all_img.png'),hjust=-0.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ped_img.png'),hjust=-1.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ride_img.png'),hjust=-2.5, valign=4,scale = 5)+
    cowplot::draw_image(magick::image_read('./DATA/ski_img.png'),hjust=-3.5, valign=4,scale = 5) +
    theme(legend.position = c(0.2, 0.8),
          legend.title = element_blank())
  
  p2 <-   urbRuralEffects_abs %>%
    ggplot(aes(x=factor(actType), y=Estimate_abs, color=urbRural)) +
    geom_point(position = position_dodge(width=0.25)) +
    geom_errorbar(aes(ymin=lowCI_abs, ymax =upCI_abs ),
                  position = position_dodge(width=0.25),
                  width = 0)+
    geom_hline(yintercept = 0, linetype=2) + 
    labs(y = 'Absolute BACI effect \n (change activity relative to control)',
         x = '',
         title = 'B)')+
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6),
                       limits = c(-2800000, 1200000))+
    scale_x_discrete(labels = c('All', 'Walk/run/hike', 'Ride', 'Ski')) +
    theme(axis.text = element_text(size=7),
          axis.title = element_text(size=8))+
    cowplot::draw_image(magick::image_read('./DATA/all_img.png'),hjust=-0.5, valign=7.2,scale = 400000)+
    cowplot::draw_image(magick::image_read('./DATA/ped_img.png'),hjust=-1.5, valign=7.2,scale = 400000)+
    cowplot::draw_image(magick::image_read('./DATA/ride_img.png'),hjust=-2.5, valign=7.2,scale = 400000)+
    cowplot::draw_image(magick::image_read('./DATA/ski_img.png'),hjust=-3.5, valign=7.2,scale = 400000)+
    theme(legend.position = c(0.2, 0.8),
          legend.title = element_blank())
  p2
  
  figUrbRur <- grid.arrange(p1, p2,   nrow=1,
                       padding = unit(0, "line"),widths=c(1,1), newpage = T)
  
  return(p1)
  
  
}
figUrbRur <- makeSuppFigUrbRural()
figUrbRur
ggsave("figS4.png",figUrbRur , width = 9, height=8, units='cm')
dev.off()

#### Explaining spatial variation in ID-level BACI effects - gam/loess model ----------------------------------------------------

# write out ID-level baci for mapping in QGIS
baciManualAgg <- baciManualIDlevel %>%
  filter(type == 'diff')   %>%
  group_by(actType, ID) %>%
  summarise_at(vars(baci), mean) %>%
  dplyr::select(ID, actType, baci) %>%
  pivot_wider(values_from = baci, names_from = actType)

baciManualAgg %>%
  gather(actType, val, all:ski) %>%
  drop_na() %>%
  group_by(actType) %>%
  summarise(n=n())

getVizParamsQGIS <- function(){
  print(quantile(baciManualAgg$all, probs=c(0.05, 0.95), na.rm=T))
  print(quantile(baciManualAgg$ped, probs=c(0.05, 0.95), na.rm=T))
  print(quantile(baciManualAgg$ride, probs=c(0.05, 0.95), na.rm=T))
  print(quantile(baciManualAgg$ski, probs=c(0.05, 0.95), na.rm=T))
  
}
getVizParamsQGIS()

myClip <- function(x, a, b) {
  ifelse(x <= a,  a, ifelse(x >= b, b, x))
}


baciManualAggClipped <- baciManualAgg  %>%
  gather(actType, baci, all:ski) %>%
  drop_na() %>%
  mutate(baci = myClip(baci, -100, 100))


baciExplanID %>%
  ungroup() %>%
  group_by(actType) %>%
  summarise(baci = sum(baci))

hist(baciExplanID$baciPop)
hist(baciExplanID$popDens)

# Make GAM plot of popDens and baci
baciExplanID %>%
  ggplot(aes(x=popDens, y=baciPop)) +
  #geom_point() +
  geom_smooth(method='gam')+
  scale_x_log10() +
  facet_wrap(~actType, scales='free') 

makeGAMsubPlot <- function(type, xlab, ylab, title){
  
  gam_model <- gam(baciPop ~ s(popDens, bs="tp", k=5), 
                   #family = nb(theta = NULL, link = "log"),
                   family=gaussian(), 
                   data=baciExplanID %>%
                     filter(popDens < 1000) %>%
                     filter(actType == type))
  #summary(gam_model)
  #gam.check(gam_model)
  #AIC(gam_model)
  
  model_p <- predict_gam(gam_model)
  p <-  model_p %>%
    ggplot(aes(popDens, fit)) +
    #scale_x_log10() +
    geom_smooth_ci() +
    geom_hline(yintercept = 0, linetype=2, linewidth=0.2)+
    labs(y = ylab,
         x = bquote('Population density (people/'~km^2~')')) + 
    ylim(-2100, 2100)  +
    annotate(geom="text", x=30, y=1900, label=title) +
    #theme_bw() +
    theme(axis.text = element_text(size=7),
          axis.title = element_text(size=8))+ 
    theme(axis.title.y=element_text(hjust = 0.8))
  
  if (!xlab){
    p <- p + theme(axis.title.x=element_blank(),
                   axis.text.x = element_blank())
  }
  return (p)
}

makeFigGam <- function(){
  g1 <- makeGAMsubPlot('all', FALSE, '', 'E)')
  g1
  g2 <- makeGAMsubPlot('ped', FALSE,'',  'F)')
  g3 <- makeGAMsubPlot('ride', FALSE, 'Absolute BACI effect (activities)', 'G)')
  g4 <- makeGAMsubPlot('ski', TRUE,'',  'H)')
  g4
  
  figGam <- grid.arrange(g1,g2,g3,g4,   nrow=4,
                         padding = unit(0, "line"),heights=c(1,1,1,1.15), newpage = T)
  return (figGam)
  
}

figGam <- makeFigGam()
ggsave("figGam.png", figGam, width = 6, height=14, units='cm')

#### Marka plots ----------------------------------------------------------


# Calculate baci absolute effects
baciGlmerMarka%>%
  ggplot(aes(x=factor(marka), y=Estimate)) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~actType, scales='free')

# Overall modelled baci scores
markaEffects <- baciGlmerMarka%>%
  group_by(actType, marka) %>%
  summarise_at(vars(Estimate, lowCI, upCI), mean) 

clearcutsLookup <- clearcutsMarka %>%
  mutate(marka = name) %>%
  dplyr::select(ID, marka)

# (by actType) Get population level estimates of change based on ID-level baci scores and marka-level modelled estimates
markaEffects_abs <- stravaBaci %>%
  left_join(clearcutsLookup, by ='ID') %>%
  drop_na(marka) %>%
  filter(treatment == 'clearcut' & year == 2017) %>%
  # average counts over buffer and control zone combinations
  group_by(ID, actType, marka) %>%
  summarise(activities_agg = mean(activities_agg)) %>%
  # sum counts over clearcuts
  group_by(actType, marka) %>%
  summarise(activities_agg = sum(activities_agg),
            ncuts = n()) %>%
  # extrapolate to population level base on https://www.sciencedirect.com/science/article/pii/S0169204623000051#f0020
  mutate(activities_pop = activities_agg/(5.5/100)) %>%
  # join with national-level model estimates
  left_join(markaEffects, by=c('actType', 'marka')) %>%
  mutate(Estimate_abs = Estimate/100*activities_pop,
         upCI_abs = upCI/100*activities_pop,
         lowCI_abs = lowCI/100*activities_pop)

markaEffects_abs%>%
  ggplot(aes(x=factor(marka), y=Estimate_abs)) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI_abs, ymax =upCI_abs ),
                position = position_dodge(width=0.25),
                width = 0)+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~actType, scales='free')


# Calculate baci effects per year based on clearcut area
clearcutAreas <- read_csv('./DATA/From_GEE/marka_clearcut_areas.csv') %>%
  dplyr::select(year, name, area, Percentage)

makeMarkaHarvestPlot <- function(){
  clearcutAreas %>%
    ggplot(aes(x=year, y=area, shape=name)) +
    geom_point(size=3, alpha=0.5) +
    geom_line() +
    labs(y = 'Clearcut area (ha)',
         x = 'Year')+
    theme(legend.position = c(0.2, 0.8),
          legend.title = element_blank())
}
makeMarkaHarvestPlot()
ggsave("markaHarvest.png", makeMarkaHarvestPlot(), width = 11, height=6, units='cm')

refYear <- clearcutAreas %>% filter(year == 2018) %>%
  mutate(ref = area) %>%
  dplyr::select(-area,-year, -Percentage) %>%
  left_join(markaEffects_abs %>%
              filter(actType == 'all') %>%
              mutate(name = marka) %>%
              dplyr::select(name, Estimate_abs:lowCI_abs), by='name')


baciExtrapolated <- clearcutAreas %>%
  left_join(refYear) %>%
  mutate(scaleFac = area/ref)  %>%
  mutate(Estimate_abs = Estimate_abs*scaleFac,
            lowCI_abs = lowCI_abs*scaleFac,
            upCI_abs = upCI_abs*scaleFac) 

baciExtrapolated %>%
  ggplot(aes(x=year, y=Estimate_abs, color=name)) +
  geom_point() +
  geom_line()

getMarkaEstimates <- function(){
  baciExtrapolated %>%
    group_by(name) %>%
    summarise_at(vars(Estimate_abs:lowCI_abs), sum) %>%
    mutate(ci = upCI_abs - Estimate_abs)
}
getMarkaEstimates()




#### Sensitivity analysis to spatial thinning -------------------------------------------

thinningDF <- read_csv('./OUTPUT/spatial_thinning_results.csv')
thinningDF %>%
  ggplot(aes(x=thinDist, y=spatAc_obs, color=actType)) +
  geom_point() +
  geom_line()

thinningDF %>%
  ggplot(aes(x=thinDist, y=spatAC_pval, color=actType)) +
  geom_point() +
  geom_line()

thinningDF %>%
  ggplot(aes(x=thinDist, y=numCuts, color=actType)) +
  geom_point() +
  geom_line()

thinningDF %>%
  ggplot(aes(x=thinDist, y=r2c, color=actType)) +
  geom_point() +
  geom_line()

makeSuppThinFig <- function(){
  thinningDF %>%
    mutate(actType = ifelse(actType == 'all', 'All',
                            ifelse(actType == 'ped', 'Walk/run/hike',
                                   ifelse(actType == 'ride', 'Ride', 'Ski')))) %>%
    ggplot(aes(x=thinDist, y=Estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=lowCI, ymax =upCI ), width = 0)+
    facet_wrap(~actType) +
    geom_hline(yintercept = 0, linetype=2, linewidth=0.2) +
    labs(y = 'Relative BACI effect (%)',
         x = 'Spatial thinning distance (m)')
}

ggsave("figS2.png", makeSuppThinFig(), width = 15, height=12, units='cm')


#### Sensitivity analysis to spatial predictors ---------------------------
spatExplanDF <- read_csv('./OUTPUT/spatial_predictors_results.csv')

spatExplanDF %>%
  ggplot(aes(x=spatialPreds, y=spatAc_obs, color=actType)) +
  geom_point() 

spatExplanDF %>%
  ggplot(aes(x=spatialPreds, y=spatAC_pval, color=actType)) +
  geom_point() 

spatExplanDF %>%
  ggplot(aes(x=spatialPreds, y=r2c, color=actType)) +
  geom_point() 

spatExplanDF %>%
  ggplot(aes(x=spatialPreds, y=Estimate, color=actType)) +
  geom_point()+
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ), width = 0)+
  facet_wrap(~actType) +
  geom_hline(yintercept = 0, linetype=2)


makeSuppSpatFig <- function(){
  spatExplanDF %>%
    mutate(actType = ifelse(actType == 'all', 'All',
                            ifelse(actType == 'ped', 'Walk/run/hike',
                                   ifelse(actType == 'ride', 'Ride', 'Ski')))) %>%
    mutate(spatialPreds = ifelse(spatialPreds == 'with', 'With spatial predictors', 'Without')) %>%
    ggplot(aes(x=spatialPreds, y=Estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=lowCI, ymax =upCI ), width = 0)+
    facet_wrap(~actType) +
    geom_hline(yintercept = 0, linetype=2, linewidth=0.2) +
    labs(y = 'Relative BACI effect (%)',
         x = 'Model structure')
}
ggsave("figS3.png", makeSuppSpatFig(), width = 15, height=12, units='cm')

#### Manual baci results - overall ------------------------------------------------------------

# Overall effect with simple baci formula
baciManualOverall %>%
  filter(type == 'diff') %>%
  ggplot(aes(x=factor(buffer), y=baci, color=factor(expZone))) +
  geom_point(position = position_dodge(width=0.25), size=2) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0) +
  geom_hline(yintercept = 0, linetype=2) +
  coord_cartesian(ylim=c(-100,100)) +
  facet_wrap(~actType)

baciManualOverall %>%
  filter(type == 'diff') %>%
  group_by(actType) %>%
  summarise_at(vars(baci, lowCI, upCI), mean, na.rm=T) %>%
  ggplot(aes(x=actType, y=baci)) +
  geom_point(position = position_dodge(width=0.25), size=2) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0) +
  geom_hline(yintercept = 0, linetype=2) 

baciManualOverall %>%
  filter(type == 'ratio') %>%
  ggplot(aes(x=factor(buffer), y=baci, color=factor(expZone))) +
  geom_point(position = position_dodge(width=0.25)) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_y_log10(limits=c(0.1,20))+
  facet_wrap(~actType)

# Overall effect with simple baci formula, but calculated as mean from individual-level baci estimates
baciManualIDlevel %>%
  filter(type == 'diff') %>%
  group_by(expZone, buffer, actType) %>%
  summarise_at(vars(baci, lowCI, upCI), mean, na.rm=T) %>%
  ggplot(aes(x=factor(buffer), y=baci, color=factor(expZone))) +
  geom_point(position = position_dodge(width=0.25), size=2) +
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0) +
  geom_hline(yintercept = 0, linetype=2) +
  coord_cartesian(ylim=c(-100,100)) +
  facet_wrap(~actType)

# Variation of baci scores across covariates
baciManualIDlevel %>%
  filter(type == 'diff', expZone == 10000)  %>%
  left_join(covariatesCat, by = 'ID')  %>%
  drop_na(baci, pop) %>%
  group_by(actType, pop, buffer) %>%
  summarise_at(vars(baci, lowCI, upCI), mean, na.rm=T) %>%
  ggplot(aes(x=pop, y = baci, color=factor(buffer)) )+
  geom_point(position = position_dodge(width=0.25))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_errorbar(aes(ymin=lowCI, ymax =upCI ),
                position = position_dodge(width=0.25),
                width = 0) +
  facet_wrap(~actType, scales='free')

# See spatial distribution
baciSpat <- baciManualIDlevel %>%
  filter(type == 'diff' & buffer == 250 & expZone == 10000)  %>%
  filter(actType == 'ped') %>%
  left_join(covariatesCat, by = 'ID')%>%
  left_join(covariatesCont %>%
              mutate(popC = log(pop+1),
                     areaC = area,
                     clearcutPercC = clearcutPerc) %>%
              dplyr::select(ID, popC:clearcutPercC), by = 'ID')  %>%
  st_as_sf(coords = c('lon', 'lat'), crs=st_crs(4326))
hist(baciSpat$baci)
hist(baciSpat$popC)
mapview(baciSpat, zcol = "baci", col.regions = brewer.pal(n = 10, name = "BrBG"))
mapview(baciSpat, zcol = "pop", col.regions = brewer.pal(n = 4, name = "Paired"))
mapview(baciSpat, zcol = "popC", col.regions = brewer.pal(n = 10, name = "Spectral"))



list.files('../QGIS/Shapefiles/')

# Correlation between individual BACI scores and covariates
baciExplan <- baciManualIDlevel  %>%
  filter(type == 'diff' & buffer == 250 & expZone == 10000)  %>%
  left_join(covariatesCat, by = 'ID')%>%
  mutate_at(vars(clearcutPerc, pop, area), factor, levels=c('Low', 'Medium', 'High')) %>%
  left_join(covariatesCont %>%
              mutate(popC = log(pop+1),
                     areaC = log(area),
                     clearcutPercC = log(clearcutPerc)) %>%
              dplyr::select(ID, popC:clearcutPercC), by = 'ID') 
names(baciExplan)

baciExplan %>%
  ggplot(aes(x=clearcutPerc, y = baci)) +
  geom_boxplot() +
  facet_wrap(~actType, scales='free')

baciExplan %>%
  gather(explan, val, popC:clearcutPercC) %>%
  ggplot(aes(x=val, y=baci, color=actType)) +
  #geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~explan, scales='free')






#### Time series graph ---------------------------------------------------------

# Define source directory for Strava data
mastDir <- '/data/R/GeoSpatialData/LandUse/Norway_StravaData/Original/'

ped2017 <- readStrava(paste0(mastDir, "/2017/norway_20170101_20171231_ped_edges/Edges/norway_20170101_20171231_ped_rollup_total.csv"),'ped', 2017 )
ride2017 <- readStrava(paste0(mastDir, "/2017/norway_20170101_20171231_ride_edges/Edges/norway_20170101_20171231_ride_rollup_total.csv"),'ride', 2017 )
ski2017 <- readStrava(paste0(mastDir, "/2017/norway_20170101_20171231_winter_edges/Edges/norway_20170101_20171231_winter_rollup_total.csv"), 'ski', 2017)

ped2018 <- readStrava(paste0(mastDir, "/2018/norway_20180101_20181231_ped_edges/Edges/norway_20180101_20181231_ped_rollup_total.csv"),'ped', 2018 )
ride2018 <- readStrava(paste0(mastDir, "/2018/norway_20180101_20181231_ride_edges/Edges/norway_20180101_20181231_ride_rollup_total.csv"),'ride', 2018 )
ski2018 <- readStrava(paste0(mastDir, "/2018/norway_20180101_20181231_winter_edges/Edges/norway_20180101_20181231_winter_rollup_total.csv"), 'ski', 2018)

ped2019 <- readStrava(paste0(mastDir, "/2019/norway_20190101_20191231_ped_edges/Edges/norway_20190101_20191231_ped_rollup_total.csv"),'ped', 2019 )
ride2019 <- readStrava(paste0(mastDir, "/2019/norway_south_20190101_20191231_ride_edges/Edges/norway_south_20190101_20191231_ride_rollup_total.csv"),'ride', 2019 ) %>%
  bind_rows(readStrava(paste0(mastDir, "/2019/norway_north_20190101_20191231_ride_edges/Edges/norway_north_20190101_20191231_ride_rollup_total.csv"),'ride', 2019 ))
ski2019 <- readStrava(paste0(mastDir, "/2019/norway_20190101_20191231_winter_edges/Edges/norway_20190101_20191231_winter_rollup_total.csv"), 'ski', 2019)

stravaJoined <- ped2017 %>%
  bind_rows(ride2017) %>%
  bind_rows(ski2017)%>%
  bind_rows(ped2018)%>%
  bind_rows(ride2018)%>%
  bind_rows(ski2018)%>%
  bind_rows(ped2019)%>%
  bind_rows(ride2019)%>%
  bind_rows(ski2019)

stravaJoined <- stravaJoined %>%
  bind_rows(stravaJoined %>%
              group_by(edge_id, year) %>%
              summarise(activities = sum(activities, na.rm=T)) %>%
              mutate(actType = 'all'))
stravaJoined <- as.data.table(stravaJoined)

z <- 20000
b <- 1000
osmFinal_df <- read_csv(paste0('./DATA/osm_clearcut/osmLookup_controlZone_',z,'.csv' )) %>%
  bind_rows(read_csv(paste0('./DATA/osm_clearcut/osmLookup_clearcutZone_',b,'.csv' ))) %>%
  ungroup()
length(unique(osmFinal_df$ID))
names(osmFinal_df)

osmLookup <- as.data.table(tibble(edge_id = unique(osmFinal_df$edge_id )))

stravaSub <- osmLookup[stravaJoined, on = .(edge_id), nomatch = 0]

strava <- as_tibble(stravaSub) %>%
  left_join(osmFinal_df %>%
              dplyr::select(edge_id, osm_id, treatment, ID), 
            by = 'edge_id',
            multiple = "all") %>%
  group_by(ID, actType, year, treatment) %>%
  summarise(activities = median(activities, na.rm=T))
hist(strava$activities)

stravaAgg <- strava %>%
  group_by(actType, year, treatment) %>%
  summarise(median = mean(activities, na.rm=T),
            sd = sd(activities),
            se = sd/n()) 

baseline <- stravaAgg %>%
  filter(year == 2017) %>%
  mutate(baseline = median) %>%
  ungroup() %>%
  dplyr::select(-median, -sd, -se, -year)

stravaAggRel <- stravaAgg %>%
  left_join(baseline) %>%
  mutate(median_rel = (median-baseline)/baseline*100)
str(stravaAggRel)

strataTimseriesPlot <- stravaAggRel %>%
  mutate(actType = ifelse(actType == 'all', 'All',
                          ifelse(actType == 'ped', 'Walk/run/hike',
                                 ifelse(actType == 'ride', 'Ride', 'Ski'))),
         treatment = ifelse(treatment == 'clearcut', 'clearcut', 'control'))%>%
  ggplot(aes(x=year, y=median_rel, color=treatment)) +
  geom_point() +
  geom_line() +
  #geom_errorbar(aes(ymin=median-se, ymax=median+se), 
  #              width=0.1) +
  facet_wrap(~actType) +
  scale_x_continuous(breaks  = c( 2017, 2018, 2019)) +
  labs(y = 'Activity change relative to 2017 (%)',
       x = 'Year') +
  theme(legend.position = 'top')
strataTimseriesPlot

ggsave("figS8.png", strataTimseriesPlot, width = 15, height=12, units='cm')

#### Legacy code -------------------------------------------------------

makeSubFig2_bottom <- function(type, ylab, xlab, title){
  
  
  p <-  overallEffects_zone_abs %>%
    filter(actType == type) %>%
    ggplot(aes(x=factor(buffer), y=Estimate_abs)) +
    geom_point(position = position_dodge(width=0.25)) +
    geom_errorbar(aes(ymin=lowCI_abs, ymax =upCI_abs ),
                  position = position_dodge(width=0.25),
                  width = 0)+
    geom_hline(yintercept = 0, linetype=2)+
    labs(y = 'Absolute BACI effect \n (absolute change activity relative to control)',
         x = 'Clearcut impact zone radius (m)',
         title=title)   +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6), 
                       limits=c(-1900000, 1000000)) +
    theme_bw()+
    theme(axis.text = element_text(size=7),
          axis.title = element_text(size=8))
  
  if (!ylab){
    p <- p + theme(axis.title.y=element_blank())
  }
  if (!xlab){
    p <- p + theme(axis.title.x=element_blank())
  }
  if (type == 'all'){
    p <- p +
      annotation_custom(rasterGrob(readPNG(paste0('./DATA/', type, '_img.png')), interpolate=T),
                        xmin='0', xmax='1000', ymin=500000, ymax=1000000)
  } else {
    p <- p +
      annotation_custom(rasterGrob(readPNG(paste0('./DATA/', type, '_img.png')), interpolate=T),
                        xmin='250', xmax='500', ymin=500000, ymax=1000000)
  }
  
  return (p)
}
f2e <- makeSubFig2_bottom('ped', TRUE, TRUE, 'E)')
f2e
f2f <- makeSubFig2_bottom('ride', FALSE, TRUE, 'F)')
f2g <- makeSubFig2_bottom('ski', FALSE, TRUE, 'G)')
f2h <- makeSubFig2_bottom('all', FALSE, TRUE, 'H)')
subPlots_bottom <- grid.arrange(f2e, f2f, f2g, f2h,   nrow=1,
                                padding = unit(0, "line"),widths=c(1.1,1,1,1), newpage = T)
#subPlots_bottom

dev.off()
fig2 <- grid.arrange(subPlots_top, subPlots_bottom,   nrow=2,
                     padding = unit(0, "line"),heights=c(1,1), newpage = T)