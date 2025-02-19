
#### Import clearcut and covariate datasets --------------------------------------------------------------------
# Get metadata for clearcut locations
clearcutsMeta <- read_csv('./DATA/clearcuts_cleaned_metadata.csv')

# Get continuous spatial predictors
covariatesCont <- read_csv('./DATA/From_GEE/covariates.csv') %>%
  mutate(clearcutPerc = clearcuts*100,
         popDens = pop/(62500/100000)) %>%
  dplyr::select(ID, clearcutPerc, pop, popDens) %>%
  left_join(clearcutsMeta, by = 'ID')
hist(covariatesCont$popDens)

quantile(covariatesCont$popDens, na.rm=T)[[4]]


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


#### Strava data for clearcut baci modelling --------------------------------------------------------
# Define source directory for Strava data
mastDir <- ''

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


#### Overall national BACI estimation -----------------------------------------------------------------
# Get some clearcut IDs to test with
clearcutExpZones_forFilter <- st_read('./DATA/clearcut/clearcutExpZones/clearcutExpZones_20000.shp')

# set.seed(123)
# testIDs <- sample(clearcutExpZones_forFilter$ID, 1000)

runningFromScratch <- FALSE

z <- 20000
for (z in expZoneRadii){
  
  b <- 1000
  for (b in impactZoneRadii){
    
    
    print(paste0('-x-----x------x------x-------x------x-------x doing expZone ', z, ' and buffer ',b))
    
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
      # Aggregate to osm ID which is are more contiguous segments
      group_by(ID, actType, year, treatment, osm_id) %>%
      summarise(activities = median(activities, na.rm=T))
    nrow(strava)
    unique(strava$actType)
    
    if (runningFromScratch){
      
      # Aggregate strava activity counts to cleatcut IDs
      # exlcude those where we do not have before-after pairs for inside & outside clearcuts
      
      stravClearcut <- strava %>%
        filter(treatment == 'clearcut')%>%
        group_by(ID) %>%
        complete(actType, year, treatment, osm_id, fill = list(activities = 0))  
      length(unique(stravClearcut$osm_id))
      
      stravControl <- strava %>%
        filter(treatment == 'no clearcut')%>%
        group_by(ID) %>%
        complete(actType, year, treatment, osm_id, fill = list(activities = 0)) %>%
        filter(ID %in% unique(stravClearcut$ID)) %>%
        filter(!osm_id %in% unique(stravClearcut$osm_id))
      length(unique(stravControl$osm_id))
      
      stravaAgg <- stravClearcut %>%
        bind_rows(stravControl) %>%
        group_by(ID, year, actType, treatment) %>%
        summarise(activities = median(activities, na.rm=T)) %>%
        group_by(ID) %>%
        complete(actType, year, treatment, fill = list(activities = NA)) %>%
        pivot_wider(names_from = year, values_from = activities) %>%
        filter(!is.na(`2017`) & !is.na(`2019`)) %>%
        filter(`2017` != 0 & `2019` != 0) %>%
        gather(year, activities, `2017`:`2019`) %>%
        pivot_wider(names_from = treatment, values_from = activities) %>%
        filter(!is.na(`clearcut`) & !is.na(`no clearcut`)) %>%
        gather(treatment, activities_agg, `clearcut`:`no clearcut`) 
      nrow(stravaAgg)
      length(unique(stravaAgg$ID))
      unique(stravaAgg$actType)
      
      stravaAgg %>%
        write_csv(paste0('./DATA/strava/stravaAgg_',z,'_',b,'.csv' ))
      
    } else {
      
      stravaAgg <-read_csv(paste0('./DATA/strava/stravaAgg_',z,'_',b,'.csv' )) %>%
        mutate(year = as.character(year)) 
      
    }
    
    
    # Filter strava data for these ID-actType combinations
    stravaFilt <- strava %>%
      mutate(year = as.character(year)) %>%
      left_join(stravaAgg, by=c('ID', 'actType', 'year', 'treatment')) %>%
      drop_na(activities_agg)
    
    
    ## __Manual BACI method ========
    
    print('Starting manual method >>>>')
    
    baciIDlevel <- tibble()
    id <- '+10039-19639'
    for (id in unique(stravaFilt$ID)){
      
      stravaFiltID <- stravaFilt %>% 
        filter(ID == id)
      
      #for (x in unique(stravaFiltID$actType)){
      for (x in c('all')){
        
        stravaFiltFilt <- stravaFiltID %>% 
          filter(actType == x) 
        
        taOverall <- stravaFiltFilt %>% filter(treatment == 'clearcut' & year == '2019') %>% pull(activities)
        caOverall <- stravaFiltFilt %>% filter(treatment == 'no clearcut' & year == '2019') %>% pull(activities)
        tbOverall <- stravaFiltFilt %>% filter(treatment == 'clearcut' & year == '2017') %>% pull(activities)
        cbOverall <- stravaFiltFilt %>% filter(treatment == 'no clearcut' & year == '2017') %>% pull(activities)
        
        baciDiff <- calculateBACIdiff(taOverall, caOverall, tbOverall, cbOverall) %>% mutate(type = 'diff')
        baciRatio <- calculateBACIratio(taOverall, caOverall, tbOverall, cbOverall)%>% mutate(type = 'ratio')
        #calculateBACIdiff(mean(taOverall), mean(caOverall), mean(tbOverall), mean(cbOverall)) %>% mutate(type = 'diff')
        baciIDlevel <- baciIDlevel %>%
          bind_rows(baciDiff %>% 
                      bind_rows(baciRatio) %>% 
                      mutate(actType = x,
                             ID = id))
      }
    }
    
    baciIDlevel %>%
      drop_na(baci) %>%
      ggplot(aes(x=baci)) +
      geom_histogram() +
      facet_grid(actType~type,scales='free')
    
    # baciOverall %>%
    #   write_csv(paste0('./OUTPUT/baciManual/baciOverall_',z,'_',b,'.csv' ))
    baciIDlevel %>%
      write_csv(paste0('./OUTPUT/baciManual/baciIDlevel_',z,'_',b,'.csv' ))
    
    
    
    ## __Modelling BACI method ========
    
    print('Starting glmer modelling >>>>')
    
    baciOverall_model <- tibble()
    x <- 'ped'
    for (x in c('ped', 'ride', 'ski', 'all')){
      
      stravaFiltFilt <- stravaAgg %>% filter(actType == x)
      
      modelDFsel <- stravaFiltFilt %>%
        mutate(time = ifelse(year == 2017, 'before', 'after') ) %>%
        mutate(treatment = factor(treatment, levels = c('no clearcut', 'clearcut')),
               time = factor(time, levels = c('before', 'after')))%>%
        left_join(clearcutsMeta, by = 'ID') %>%
        ungroup()
      colSums(is.na(modelDFsel))
      nrow(modelDFsel)
      hist(modelDFsel$activities_agg)
      
      lengthModelDF <- length(unique(modelDFsel$ID))
      
      
      t1 <-  Sys.time()
      model <- glmer.nb(activities_agg ~ 
                          treatment * time +
                          (1 | municipality/ID),
                        control=glmerControl(optimizer="bobyqa"),
                        data = modelDFsel)
      t2 <- Sys.time()
      timeTaken <- interval(ymd_hms(t1), ymd_hms(t2)) %>% as.numeric('minutes')
      
      simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
      #plot(simulationOutput)
      
      summary(model)
      
      # Test spatial aitocorrelation
      # https://crd230.github.io/lab8.html#Exploratory_Spatial_Data_Analysis
      # https://datascienceplus.com/spatial-regression-in-r-part-1-spamm-vs-glmmtmb/
      simulationOutput2 = recalculateResiduals(simulationOutput, group = modelDFsel$ID)
      
      # https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html
      #simulationOutput2 = recalculateResiduals( simulateResiduals(fittedModel = model, plot = F, re.form = NULL), group = modelDFsel$ID)
      
      #plot(simulationOutput2, quantreg = FALSE)
      # 
      latLonUnique <- modelDFsel %>%
        group_by(ID, lat, lon) %>%
        summarise(.groups = 'keep')
      # 
      spatAC <- testSpatialAutocorrelation(simulationOutput2, latLonUnique$lon, latLonUnique$lat, plot = F)
      spatAC$p.value
      spatAC$statistic[[1]]
      
      conf_intervals_raw <- confint(model, method = "Wald")
      
      conf_intervals <- conf_intervals_raw %>% as_tibble()%>%
        mutate(var = rownames(as.data.frame(conf_intervals_raw)))  %>%
        drop_na() %>%
        mutate(Estimate = as.data.frame(summary(model)$coefficients)$Estimate) %>%
        mutate_at(vars(`2.5 %`, `97.5 %`, Estimate), function(x)(exp(x) - 1) * 100) 
      names(conf_intervals) <- c('lowCI', 'upCI',  'var', 'Estimate')
      
      r2 <- MuMIn::r.squaredGLMM(model)
      
      baciOverall_model <- baciOverall_model %>% 
        bind_rows(conf_intervals %>%
                    filter(var %in% c('treatmentclearcut:timeafter')) %>%
                    mutate(actType = x,
                           spatAC_pval = spatAC$p.value,
                           spatAc_obs = spatAC$statistic[[1]],
                           spacAc_exp = spatAC$statistic[[2]],
                           aic = AIC(model),
                           r2m = r2[[1]],
                           r2c = r2[[2]],
                           modelTime = timeTaken,
                           n = nrow(modelDFsel),
                           nID = length(unique(modelDFsel$ID))))
      
      
    }
    
    baciOverall_model
    
    baciOverall_model %>%
      write_csv(paste0('./OUTPUT/baciModel/baciGlmerOverall_',z,'_',b,'.csv' ))
    
    
  }
  
  
}

#### Oslo marka case study model of BACI effects --------------------------------------


clearcutsMarka
unique(clearcutsMarka$name)

z <- 20000
for (z in expZoneRadii){
  
  b <- 2500
  for (b in c(1000)){
    
    c <- 'Østmarka'
    for (c in c('Østmarka', 'Nordmarka')){
      
      
      
      print(paste0('-x-----x------x------x-------x------x-------x doing expZone ', z, ' and buffer ',b, ' and marka: ', c))
      
      
      stravaAgg <-read_csv(paste0('./DATA/strava/stravaAgg_',z,'_',b,'.csv' )) %>%
        mutate(year = as.character(year))  %>%
        left_join(clearcutsMarka %>%
                    filter(name == c) %>%
                    dplyr::select(ID, name), by = 'ID') %>%
        drop_na(name)
      
      
      ## __Modelling BACI method ========
      
      print('Starting glmer modelling >>>>')
      
      baci_model <- tibble()
      x <- 'ski'
      for (x in c('ped', 'ride', 'ski', 'all')){
        
        stravaFiltFilt <- stravaAgg %>% filter(actType == x)
        
        modelDFsel <- stravaFiltFilt %>%
          mutate(time = ifelse(year == 2017, 'before', 'after') ) %>%
          mutate(treatment = factor(treatment, levels = c('no clearcut', 'clearcut')),
                 time = factor(time, levels = c('before', 'after')))%>%
          left_join(clearcutsMeta, by = 'ID') %>%
          ungroup()
        colSums(is.na(modelDFsel))
        nrow(modelDFsel)
        hist(modelDFsel$activities_agg)
        
        lengthModelDF <- length(unique(modelDFsel$ID))
        t1 <-  Sys.time()
        model <- glmer.nb(activities_agg ~ 
                            treatment * time +
                            (1 | ID),
                          control=glmerControl(optimizer="bobyqa"),
                          data = modelDFsel)
        t2 <- Sys.time()
        timeTaken <- interval(ymd_hms(t1), ymd_hms(t2)) %>% as.numeric('minutes')
        
        simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
        #plot(simulationOutput)
        
        summary(model)
        
        # Test spatial aitocorrelation
        # https://crd230.github.io/lab8.html#Exploratory_Spatial_Data_Analysis
        # https://datascienceplus.com/spatial-regression-in-r-part-1-spamm-vs-glmmtmb/
        simulationOutput2 = recalculateResiduals(simulationOutput, group = modelDFsel$ID)
        
        # https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html
        #simulationOutput2 = recalculateResiduals( simulateResiduals(fittedModel = model, plot = F, re.form = NULL), group = modelDFsel$ID)
        
        #plot(simulationOutput2, quantreg = FALSE)
        # 
        latLonUnique <- modelDFsel %>%
          group_by(ID, lat, lon) %>%
          summarise(.groups = 'keep')
        # 
        spatAC <- testSpatialAutocorrelation(simulationOutput2, latLonUnique$lon, latLonUnique$lat, plot = F)
        spatAC$p.value
        spatAC$statistic[[1]]
        
        conf_intervals_raw <- confint(model, method = "Wald")
        
        conf_intervals <- conf_intervals_raw %>% as_tibble()%>%
          mutate(var = rownames(as.data.frame(conf_intervals_raw)))  %>%
          drop_na() %>%
          mutate(Estimate = as.data.frame(summary(model)$coefficients)$Estimate) %>%
          mutate_at(vars(`2.5 %`, `97.5 %`, Estimate), function(x)(exp(x) - 1) * 100) 
        names(conf_intervals) <- c('lowCI', 'upCI',  'var', 'Estimate')
        
        r2 <- MuMIn::r.squaredGLMM(model)
        
        baci_model <- baci_model %>% 
          bind_rows(conf_intervals %>%
                      filter(var %in% c('treatmentclearcut:timeafter')) %>%
                      mutate(actType = x,
                             spatAC_pval = spatAC$p.value,
                             spatAc_obs = spatAC$statistic[[1]],
                             spacAc_exp = spatAC$statistic[[2]],
                             aic = AIC(model),
                             r2m = r2[[1]],
                             r2c = r2[[2]],
                             modelTime = timeTaken,
                             n = nrow(modelDFsel),
                             nID = length(unique(modelDFsel$ID))))
        
        
      }
      
      baci_model
      
      baci_model %>%
        write_csv(paste0('./OUTPUT/baciModelMarka/baciGlmer_',z,'_',b,'_',c,'.csv' ))
      
      
      
    }
    
    
  }
  
  
}

#### Urban vs rural modelling stratification BACI ----------------------------------------------------
urbRuralThresh <- quantile(covariatesCont$popDens, na.rm=T)[[4]]

clearcutsUrbRural <- covariatesCont %>%
  mutate(urbRural = ifelse(popDens >= urbRuralThresh, 'Urban', 'Rural')) %>%
  dplyr::select(ID, urbRural) %>%
  drop_na(urbRural)
ggplot(clearcutsUrbRural, aes(x=urbRural)) + geom_bar()

z <- 20000
for (z in expZoneRadii){
  
  b <- 1000
  for (b in c(1000)){
    
    c <- 'Rural'
    for (c in c('Urban', 'Rural')){
      
      
      
      print(paste0('-x-----x------x------x-------x------x-------x doing expZone ', z, ' and buffer ',b, ' and urbRural context: ', c))
      
      
      stravaAgg <-read_csv(paste0('./DATA/strava/stravaAgg_',z,'_',b,'.csv' )) %>%
        mutate(year = as.character(year))  %>%
        left_join(clearcutsUrbRural %>%
                    filter(urbRural == c) %>%
                    dplyr::select(ID, urbRural), by = 'ID') %>%
        drop_na(urbRural)
      
      
      ## __Modelling BACI method ========
      
      print('Starting glmer modelling >>>>')
      
      baci_model <- tibble()
      x <- 'ride'
      for (x in c('ped', 'ride', 'ski', 'all')){
        
        stravaFiltFilt <- stravaAgg %>% filter(actType == x)
        
        modelDFsel <- stravaFiltFilt %>%
          mutate(time = ifelse(year == 2017, 'before', 'after') ) %>%
          mutate(treatment = factor(treatment, levels = c('no clearcut', 'clearcut')),
                 time = factor(time, levels = c('before', 'after')))%>%
          left_join(clearcutsMeta, by = 'ID') %>%
          ungroup()
        colSums(is.na(modelDFsel))
        nrow(modelDFsel)
        hist(modelDFsel$activities_agg)
        
        lengthModelDF <- length(unique(modelDFsel$ID))
        
        
        t1 <-  Sys.time()
        model <- glmer.nb(activities_agg ~ 
                            treatment * time +
                            (1 | ID),
                          control=glmerControl(optimizer="bobyqa"),
                          data = modelDFsel)
        t2 <- Sys.time()
        timeTaken <- interval(ymd_hms(t1), ymd_hms(t2)) %>% as.numeric('minutes')
        
        simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
        #plot(simulationOutput)
        
        summary(model)
        
        # Test spatial aitocorrelation
        # https://crd230.github.io/lab8.html#Exploratory_Spatial_Data_Analysis
        # https://datascienceplus.com/spatial-regression-in-r-part-1-spamm-vs-glmmtmb/
        simulationOutput2 = recalculateResiduals(simulationOutput, group = modelDFsel$ID)
        
        # https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html
        simulationOutput2 = recalculateResiduals( simulateResiduals(fittedModel = model, plot = F, re.form = NULL), group = modelDFsel$ID)
        
        #plot(simulationOutput2, quantreg = FALSE)
        # 
        latLonUnique <- modelDFsel %>%
          group_by(ID, lat, lon) %>%
          summarise(.groups = 'keep')
        #
        spatAC <- testSpatialAutocorrelation(simulationOutput2, latLonUnique$lon, latLonUnique$lat, plot = F)
        spatAC$p.value
        spatAC$statistic[[1]]
        
        conf_intervals_raw <- confint(model, method = "Wald")
        
        conf_intervals <- conf_intervals_raw %>% as_tibble()%>%
          mutate(var = rownames(as.data.frame(conf_intervals_raw)))  %>%
          drop_na() %>%
          mutate(Estimate = as.data.frame(summary(model)$coefficients)$Estimate) %>%
          mutate_at(vars(`2.5 %`, `97.5 %`, Estimate), function(x)(exp(x) - 1) * 100) 
        names(conf_intervals) <- c('lowCI', 'upCI',  'var', 'Estimate')
        
        r2 <- MuMIn::r.squaredGLMM(model)
        
        baci_model <- baci_model %>% 
          bind_rows(conf_intervals %>%
                      filter(var %in% c('treatmentclearcut:timeafter')) %>%
                      mutate(actType = x,
                             spatAC_pval = spatAC$p.value,
                             spatAc_obs = spatAC$statistic[[1]],
                             spacAc_exp = spatAC$statistic[[2]],
                             aic = AIC(model),
                             r2m = r2[[1]],
                             r2c = r2[[2]],
                             modelTime = timeTaken,
                             n = nrow(modelDFsel),
                             nID = length(unique(modelDFsel$ID))))
        
        
      }
      
      baci_model
      
      baci_model %>%
        write_csv(paste0('./OUTPUT/baciModelUrbRural/baciGlmer_',z,'_',b,'_',c,'.csv' ))
      
      
      
    }
    
    
  }
  
  
}


#### Sensitivity test for the thinning of clearcut points to prevent spatial autocorrelation ------------------

# First get an averaged Strava counts for each ID 
# averaged across buffer zones and control areas
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
stravaBaci_agg <- stravaBaci %>%
  group_by(ID, actType, treatment, year) %>%
  summarise(activities_agg = median(activities_agg)) %>%
  mutate(time = ifelse(year == 2017, 'before', 'after') ) %>%
  mutate(treatment = factor(treatment, levels = c('no clearcut', 'clearcut')),
         time = factor(time, levels = c('before', 'after'))) %>%
  group_by(ID, time, actType, treatment) %>%
  summarise(activities_agg = median(activities_agg)) 


points_sf <- st_as_sf(clearcutsMeta, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(25832))

distances <- st_distance(points_sf)
points_sf[c(1,200),]
distances[1,2]

too_close <- distances < units::set_units(10000, "m")

remove_indices <- c()

# Iterate over the matrix
for (i in 1:nrow(too_close)) {
  if (i %in% remove_indices) {
    next
  }
  for (j in (i+1):ncol(too_close)) {
    if (too_close[i, j]) {
      remove_indices <- c(remove_indices, j)
    }
  }
}

points_sf_filtered <- points_sf[-remove_indices, ]

nrow(points_sf)
nrow(points_sf_filtered)
ggplot() +
  geom_sf(data = points_sf, size=0.01) +
  geom_sf(data = points_sf_filtered, color='red', size=0.01)

# Now iterate over a series of thinning distances
thinningDF <- tibble()

t <- 'ped'
d <- 5000

for (t in c('ped', 'ride', 'ski', 'all')){
  
  for (d in c(1000, 2500, 5000, 10000, 15000, 20000)){
    
    too_close <- distances < units::set_units(d, "m")
    
    remove_indices <- c()
    
    # Iterate over the matrix
    for (i in 1:nrow(too_close)) {
      if (i %in% remove_indices) {
        next
      }
      for (j in (i+1):ncol(too_close)) {
        if (too_close[i, j]) {
          remove_indices <- c(remove_indices, j)
        }
      }
    }
    
    points_sf_filtered <- points_sf[-remove_indices, ]
    nrow(points_sf)
    nrow(points_sf_filtered)
    
    modelDFsel <- stravaBaci_agg %>%
      
      filter(ID %in% points_sf_filtered$ID) %>%
      left_join(clearcutsMeta, by = 'ID') %>%
      drop_na() %>%
      filter(actType == t) %>%
      ungroup()
    colSums(is.na(modelDFsel))
    hist(modelDFsel$activities_agg)
    length(unique(modelDFsel$ID))
    
    t1 <-  Sys.time()
    model <- glmer.nb(activities_agg ~ 
                        treatment * time +
                        (1 | municipality/ID),
                      control=glmerControl(optimizer="bobyqa"),
                      data = modelDFsel)
    t2 <- Sys.time()
    timeTaken <- interval(ymd_hms(t1), ymd_hms(t2)) %>% as.numeric('minutes')
    print(paste0('time taken: ', timeTaken))
    
    simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
    #plot(simulationOutput)
    
    summary(model)
    
    # Test spatial aitocorrelation
    # https://crd230.github.io/lab8.html#Exploratory_Spatial_Data_Analysis
    # https://datascienceplus.com/spatial-regression-in-r-part-1-spamm-vs-glmmtmb/
    simulationOutput2 = recalculateResiduals(simulationOutput, group = modelDFsel$ID)
    
    # https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html
    #simulationOutput2 = recalculateResiduals( simulateResiduals(fittedModel = model, plot = F, re.form = NULL), group = modelDFsel$ID)
    
    #plot(simulationOutput2, quantreg = FALSE)
    # 
    latLonUnique <- modelDFsel %>%
      group_by(ID, lat, lon) %>%
      summarise(.groups = 'keep')
    # 
    spatAC <- testSpatialAutocorrelation(simulationOutput2, latLonUnique$lon, latLonUnique$lat, plot = F)
    spatAC$p.value
    spatAC$statistic[[1]]
    conf_intervals_raw <- confint(model, method = "Wald")
    
    
    conf_intervals <- conf_intervals_raw %>% as_tibble()%>%
      mutate(var = rownames(as.data.frame(conf_intervals_raw)))  %>%
      drop_na() %>%
      mutate(Estimate = as.data.frame(summary(model)$coefficients)$Estimate) %>%
      mutate_at(vars(`2.5 %`, `97.5 %`, Estimate), function(x)(exp(x) - 1) * 100) 
    names(conf_intervals) <- c('lowCI', 'upCI',  'var', 'Estimate')
    
    r2 <- MuMIn::r.squaredGLMM(model)
    
    thinningDF <- thinningDF %>% 
      bind_rows(conf_intervals %>%
                  filter(var %in% c('treatmentclearcut:timeafter')) %>%
                  mutate(actType = t,
                         spatAC_pval = spatAC$p.value,
                         spatAc_obs = spatAC$statistic[[1]],
                         spacAc_exp = spatAC$statistic[[2]],
                         aic = AIC(model),
                         r2m = r2[[1]],
                         r2c = r2[[2]],
                         modelTime = timeTaken,
                         thinDist = d,
                         numCuts = nrow(points_sf_filtered)))
    
  }
  
}


thinningDF %>%
  write_csv('./OUTPUT/spatial_thinning_results.csv')




#### Sensitivity test for including spatial predictors on spatial autocorrelation -----
spatExplanDF <- tibble()

t <- 'ped'

for (t in c('ped', 'ride', 'ski', 'all')){
  
  modelDFsel <- stravaBaci_agg %>%
    left_join(covariatesCont, by = 'ID') %>%
    drop_na() %>%
    filter(actType == t) %>%
    ungroup()
  colSums(is.na(modelDFsel))
  hist(modelDFsel$activities_agg)
  length(unique(modelDFsel$ID))
  lengthModelDF <- length(unique(modelDFsel$ID))
  
  
  for (x in c('with', 'without')){
    
    if (x == 'with'){
      t1 <-  Sys.time()
      model <- glmer.nb(activities_agg ~ 
                          treatment * time +
                          popDens +
                          clearcutPerc +
                          lat +
                          lon +
                          (1 | municipality/ID),
                        control=glmerControl(optimizer="bobyqa"),
                        data = modelDFsel)
      t2 <- Sys.time()
      timeTaken <- interval(ymd_hms(t1), ymd_hms(t2)) %>% as.numeric('minutes')
      print(paste0('time taken: ', timeTaken))
    } else {
      t1 <-  Sys.time()
      model <- glmer.nb(activities_agg ~ 
                          treatment * time +
                          (1 | municipality/ID),
                        control=glmerControl(optimizer="bobyqa"),
                        data = modelDFsel)
      t2 <- Sys.time()
      timeTaken <- interval(ymd_hms(t1), ymd_hms(t2)) %>% as.numeric('minutes')
      print(paste0('time taken: ', timeTaken))
    }
    
    
    simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
    #plot(simulationOutput)
    
    summary(model)
    
    # Test spatial aitocorrelation
    # https://crd230.github.io/lab8.html#Exploratory_Spatial_Data_Analysis
    # https://datascienceplus.com/spatial-regression-in-r-part-1-spamm-vs-glmmtmb/
    simulationOutput2 = recalculateResiduals(simulationOutput, group = modelDFsel$ID)
    
    # https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html
    #simulationOutput2 = recalculateResiduals( simulateResiduals(fittedModel = model, plot = F, re.form = NULL), group = modelDFsel$ID)
    
    #plot(simulationOutput2, quantreg = FALSE)
    # 
    latLonUnique <- modelDFsel %>%
      group_by(ID, lat, lon) %>%
      summarise(.groups = 'keep')
    # 
    spatAC <- testSpatialAutocorrelation(simulationOutput2, latLonUnique$lon, latLonUnique$lat, plot = F)
    spatAC$p.value
    spatAC$statistic[[1]]
    conf_intervals_raw <- confint(model, method = "Wald")
    
    
    conf_intervals <- conf_intervals_raw %>% as_tibble()%>%
      mutate(var = rownames(as.data.frame(conf_intervals_raw)))  %>%
      drop_na() %>%
      mutate(Estimate = as.data.frame(summary(model)$coefficients)$Estimate) %>%
      mutate_at(vars(`2.5 %`, `97.5 %`, Estimate), function(x)(exp(x) - 1) * 100) 
    names(conf_intervals) <- c('lowCI', 'upCI',  'var', 'Estimate')
    
    r2 <- MuMIn::r.squaredGLMM(model)
    
    spatExplanDF <- spatExplanDF %>% 
      bind_rows(conf_intervals %>%
                  filter(var %in% c('treatmentclearcut:timeafter')) %>%
                  mutate(actType = t,
                         spatialPreds = x,
                         spatAC_pval = spatAC$p.value,
                         spatAc_obs = spatAC$statistic[[1]],
                         spacAc_exp = spatAC$statistic[[2]],
                         aic = AIC(model),
                         r2m = r2[[1]],
                         r2c = r2[[2]],
                         modelTime = timeTaken))
    
  }
  
  
}
spatExplanDF %>%
  write_csv('./OUTPUT/spatial_predictors_results.csv')

