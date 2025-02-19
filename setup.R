#### Import libraries -----------------------------------------------------------

library(sf)
library(tidyverse)
library(lubridate)
library(anytime)
library(grid)
library(gridExtra)
library(lme4)
library(emmeans)
library(DHARMa)
library(mapview)
library(glmmTMB)
library(RColorBrewer)
library(spaMM)
library(geojsonsf)
library(data.table)
library(spdep)
library(khroma)
library(mgcv)
library(tidymv)
library(png)
library(cowplot)
library(magick)
library(ggeffects)
library(caret)

#### Global objects  -----------------------------------------------------------

theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())+
            theme(strip.background =element_rect(fill="white")))

expZoneRadii <- c(5000, 10000, 20000)
expZoneRadii <- c(20000)
impactZoneRadii <- c(2500, 5000)
impactZoneRadii <- c(0, 250, 500, 1000) 

projCrs <- st_crs(st_read('./DATA/regions.shp'))

#### Global functions -----------------------------------------------------------

readStravaFull <- function(dir, type){
  dat <- read_csv(dir,guess_max =100000) %>%
    mutate(activities = tactcnt - tcmtcnt,
           actType = type) %>%
    dplyr::select(edge_id, actType, activities)
  return (dat)
}

readStrava <- function(dir, type, year){
  dat <- read_csv(dir,guess_max =100000) %>%
    mutate(activities = tactcnt - tcmtcnt,
           actType = type,
           year = year) %>%
    dplyr::select(edge_id, year, actType, activities)
  return (dat)
}

# Calculate BACI function
calculateBACIdiff <- function(ta, ca, tb, cb){
  
  baci_diff <- (mean(ta) -  mean(ca)) - (mean(tb)-mean(cb))
  
  error <- qnorm(0.975)*sqrt(((((length(c(ta))-1)*var(c(ta)) + (length(c(ca))-1)*var(c(ca)) + (length(c(tb))-1)*var(c(tb))+ (length(c(cb))-1)*var(c(cb)))/(length(c(ca))+length(c(ta))+length(c(cb))+length(c(tb))-4))/length(c(ca)))
                             + ((((length(c(ta))-1)*var(c(ta)) + (length(c(ca))-1)*var(c(ca)) + (length(c(tb))-1)*var(c(tb))+ (length(c(cb))-1)*var(c(cb)))/(length(c(ca))+length(c(ta))+length(c(cb))+length(c(tb))-4))/length(c(ta)))
                             + ((((length(c(ta))-1)*var(c(ta)) + (length(c(ca))-1)*var(c(ca)) + (length(c(tb))-1)*var(c(tb))+ (length(c(cb))-1)*var(c(cb)))/(length(c(ca))+length(c(ta))+length(c(cb))+length(c(tb))-4))/length(c(cb)))
                             +((((length(c(ta))-1)*var(c(ta)) + (length(c(ca))-1)*var(c(ca)) + (length(c(tb))-1)*var(c(tb))+ (length(c(cb))-1)*var(c(cb)))/(length(c(ca))+length(c(ta))+length(c(cb))+length(c(tb))-4))/length(c(tb))))
  
  return (tibble(baci=(baci_diff), lowCI=(baci_diff-error), upCI = (baci_diff+error)))
  
}

calculateBACIratio <- function(ta, ca, tb, cb){
  
  treatment_ratio_after <- mean(ta)/mean(ca)
  control_ratio_after <- 1 
  treatment_ratio_before <- mean(tb)/mean(cb)
  control_ratio_before <- 1 
  baci_ratio <- treatment_ratio_after / treatment_ratio_before
  
  # Calculate the variances of the treatment and control ratios
  var_treatment_ratio_after <- var(ta) / length(ta)^2
  var_treatment_ratio_before <- var(tb) / length(tb)^2
  var_control_ratio_after <- var(ca) / length(ca)^2
  var_control_ratio_before <- var(cb) / length(cb)^2
  
  # Calculate the variance of the BACI ratio using the delta method
  baci_ratio_var <- baci_ratio^2 * (var_treatment_ratio_after / treatment_ratio_after^2 + var_treatment_ratio_before / treatment_ratio_before^2 +
                                      var_control_ratio_after / control_ratio_after^2 + var_control_ratio_before / control_ratio_before^2)
  
  # Calculate the standard error of the BACI ratio
  baci_ratio_se <- sqrt(baci_ratio_var)
  
  # Calculate the 95% confidence interval
  confidence_interval <- log(baci_ratio) + qnorm(c(0.025, 0.975)) * baci_ratio_se
  
  return (tibble(baci=baci_ratio, lowCI=exp(confidence_interval)[1], upCI=exp(confidence_interval)[2]))
  
}