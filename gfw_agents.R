#### Agents of Landsat-based forest loss data ---------------------------
# data from https://www.nature.com/articles/s41467-024-49116-0
# specifically https://zenodo.org/records/11070255

agent <- read_csv('./DATA/disturbance_nuts2.csv') %>%
  filter(country == 'NO') %>%
  group_by(year) %>%
  summarise_at(vars(unplanned, planned, total), sum) %>%
  filter(year >= 2000) 
agent

read_csv('./DATA/disturbance_nuts2.csv') %>%
  summarise_at(vars(unplanned, planned, total), sum) 

b1 <- agent %>%
  mutate(planned = planned/1000000,
         unplanned = unplanned/1000000) %>%
  gather(key, val, planned, unplanned) %>%
  ggplot(aes(x=factor(year), y=val, fill=key)) +
  geom_bar(stat='identity') +
  labs(y = 'Area of Landsat detections (km2)',
       title = 'A)',
       fill = 'Forest loss type') +
  scale_fill_manual(values =c( '#368ab4','#cae9c3')) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle = 45, vjust=-0),
        legend.position = c(0.2, 0.8))
b1

b2 <- agent %>%
  mutate(planned = planned/total*100,
         unplanned = unplanned/total*100) %>%
  gather(key, val, planned, unplanned) %>%
  ggplot(aes(x=factor(year), y=val, fill=key)) +
  geom_bar(stat='identity') +
  labs(y = 'Percentage of Landsat detections (%)',
       title = 'B)') +
  scale_fill_manual(values =c( '#368ab4','#cae9c3')) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle = 45, vjust=-0),
        legend.position = 'none')
b2

figLandsatHarvest <- grid.arrange(b1, b2,   nrow=1,
             padding = unit(0, "line"),widths=c(1,1), newpage = T)

ggsave("figS1.png", figLandsatHarvest, width = 20, height=9, units='cm')

agent %>%
  mutate(planned = planned/total*100,
         unplanned = unplanned/total*100) %>%
  filter(year == 2018)

agent %>%
  mutate(planned = planned/total*100,
         unplanned = unplanned/total*100) %>%
  gather(key, val, planned, unplanned) %>%
  group_by(key) %>%
  summarise(val = mean(val))

#### GFW forest loss  accuracy assessment --------------------------------
# Reference data from https://zenodo.org/records/4138867

refdat <- read_csv('./DATA/disturbances_references_europe.csv')

names(refdat)

refdat %>%
  st_as_sf(coords = c('x_coord', 'y_coord'), crs=st_crs(3035)) %>%
  ggplot() +
  geom_sf()


#### Upload to GEE and extract GFW ------------------------------------

refdat %>%
  st_as_sf(coords = c('x_coord', 'y_coord'), crs=st_crs(3035)) %>%
  filter(country == 'norway') %>%
  st_transform(st_crs(4326)) %>%
  dplyr::select(plotid) %>%
  st_write('./DATA/For_GEE/senf_disturbances_references.shp')

#### Import from GEE and do acc assessment ---------------------------

refdatToJoin <- refdat %>%
  filter(country == 'norway') %>%
  mutate(year = ifelse(is.na(year_disturbance_2), year_disturbance_1, year_disturbance_2),
         disturbance = ifelse(disturbance_n > 0, 1, 0)) %>%
  dplyr::select(plotid, disturbance, year)%>%
  filter(year >= 2000 | is.na(year)) 

gfw <- read_csv('./DATA/From_GEE/senf_disturbances_references_gfw.csv') %>%
  mutate(gfwDisturbance = ifelse(first == 99, 0, 1),
         gfwYear = ifelse(first == 99, NA, first+2000)) %>%
  filter(gfwYear < 2018 | is.na(gfwYear)) %>%
  dplyr::select(plotid, gfwDisturbance, gfwYear) %>%
  left_join(refdatToJoin) %>%
  mutate(disturbance = factor(disturbance),
         gfwDisturbance = factor(gfwDisturbance))

cm <- confusionMatrix(gfw$gfwDisturbance, gfw$disturbance, positive='1')
cm
(1-cm$overall[[1]])/18*100

# Extract confusion table
conf_table <- as.table(cm$table)

# Calculate Producer's and User's Accuracy
producers_accuracy <- diag(conf_table) / rowSums(conf_table)
users_accuracy <- diag(conf_table) / colSums(conf_table)

# Calculate Omission and Commission Errors
omission_error <- 1 - producers_accuracy
commission_error <- 1 - users_accuracy

# Display results
print("Producer's Accuracy (Sensitivity):")
print(producers_accuracy)
print("User's Accuracy (Precision):")
print(users_accuracy)
print("Omission Error:")
print(omission_error)
print(omission_error/18*100)
print("Commission Error:")
print(commission_error)
print(commission_error/18*100)


refdat %>%
  st_as_sf(coords = c('x_coord', 'y_coord'), crs=st_crs(3035)) %>%
  filter(country == 'norway') %>%
  st_transform(st_crs(4326)) %>%
  mutate(year = ifelse(is.na(year_disturbance_2), year_disturbance_1, year_disturbance_2),
         disturbance = ifelse(disturbance_n > 0, 1, 0)) %>%
  dplyr::select(plotid, disturbance, year)%>%
  filter(year >= 2000 | is.na(year)) %>%
  ggplot() +
  geom_sf(aes(color=factor(disturbance)))

