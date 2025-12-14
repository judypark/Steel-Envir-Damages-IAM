library(dplyr)
library(tidyr)
library(readxl)

setwd("C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/2_NEI2017_Processing")

## AP4 structure
ap4_counties <- read_xlsx("C:/Users/judec/Documents/CMU/APEEP/0_AP4_from_website/AP4_Inputs/AP4_County_List.xlsx")
ap4_egu <- read_xlsx("C:/Users/judec/Documents/CMU/APEEP/0_AP4_from_website/AP4_Inputs/AP4_EGU_List.xlsx")

## NEI facilities
fac_us <- read.csv("C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/2_NEI2017_Processing/emissions_by_facility_2017_JP.csv")

## Modeled count
ap4_egu <- ap4_egu %>% mutate(counter = 1) %>% group_by(eis) %>% mutate(tag = cumsum(counter)) %>% ungroup()
ap4_egu <- subset(ap4_egu, select = -counter)

## EGU
ap4_egu <- left_join(ap4_egu, fac_us, by = "eis")
info_egu <- subset(ap4_egu, select = -c(nh3,nox,pmp,so2,voc,co2,ch4,n2o))
ap4_egu <- ap4_egu %>% mutate(nh3 = ifelse(tag > 1, 0, nh3),
                              nox = ifelse(tag > 1, 0, nox),
                              pmp = ifelse(tag > 1, 0, pmp),
                              so2 = ifelse(tag > 1, 0, so2),
                              voc = ifelse(tag > 1, 0, voc))
ap4_egu <- subset(ap4_egu, select = c(nh3,nox,pmp,so2,voc))
ap4_egu <- ap4_egu %>% mutate_at(vars("nh3","nox","pmp","so2","voc"), ~replace_na(.,0))
fac_us <- anti_join(fac_us, info_egu, by = "eis")

## Non-EGU
info_nonegu <- subset(fac_us, select = -c(nh3,nox,pmp,so2,voc,co2,ch4,n2o))
mobile <- fac_us[fac_us$mobile == "Yes",] # mobile facilities
ap4_nonegu <- fac_us %>%
  group_by(fips) %>%
  summarize(nh3 = sum(nh3),
            nox = sum(nox),
            pmp = sum(pmp),
            so2 = sum(so2),
            voc = sum(voc))
ap4_nonegu <- left_join(ap4_counties, ap4_nonegu, by = "fips")
ap4_nonegu <- ap4_nonegu %>% mutate_at(vars("nh3","nox","pmp","so2","voc"), ~replace_na(.,0))
mobile_assignment <- ap4_nonegu %>% mutate(fipsst = floor(fips/1000)) %>%
  group_by(fipsst) %>% mutate(state_nox = sum(nox)) %>% ungroup() %>%
  mutate(perc_nox = nox/state_nox) # assign mobile emissions based on county nox percentage of state
mobile_assignment <- subset(mobile_assignment, select = c(fips, fipsst, perc_nox))
mobile <- mobile %>%
  group_by(fipsst) %>%
  summarize(nh3 = sum(nh3),
            nox = sum(nox),
            pmp = sum(pmp),
            so2 = sum(so2),
            voc = sum(voc))
mobile_assignment <- left_join(mobile_assignment, mobile, by = "fipsst")
mobile_assignment <- mobile_assignment %>% mutate_at(vars("nh3","nox","pmp","so2","voc"), ~replace_na(.,0))
mobile_assignment <- mobile_assignment %>%
  mutate(ap4 = NA,
         nh3 = nh3*perc_nox,
         nox = nox*perc_nox,
         pmp = pmp*perc_nox,
         so2 = so2*perc_nox,
         voc = voc*perc_nox)
mobile_assignment <- subset(mobile_assignment, select = -c(fipsst, perc_nox))
ap4_nonegu <- rbind(ap4_nonegu, mobile_assignment)
ap4_nonegu <- ap4_nonegu %>%
  group_by(fips) %>%
  summarize(ap4 = first(ap4),
            nh3 = sum(nh3),
            nox = sum(nox),
            pmp = sum(pmp),
            so2 = sum(so2),
            voc = sum(voc))
ap4_nonegu <- subset(ap4_nonegu, select = c(nh3,nox,pmp,so2,voc))

## Metric tons
ap4_egu_mt <- ap4_egu*0.9071847
ap4_nonegu_mt <- ap4_nonegu*0.9071847

## Output
write.csv(info_egu, "NEI_2017/info_egu_2017.csv", row.names = F)
write.csv(info_nonegu, "NEI_2017/info_nonegu_2017.csv", row.names = F)

write.table(ap4_egu, "NEI_2017/emi_egu_st_2017.csv", row.names = F, col.names = F, sep = ",")
write.table(ap4_nonegu, "NEI_2017/emi_nonegu_st_2017.csv", row.names = F, col.names = F, sep = ",")
write.table(ap4_egu_mt, "NEI_2017/emi_egu_mt_2017.csv", row.names = F, col.names = F, sep = ",")
write.table(ap4_nonegu_mt, "NEI_2017/emi_nonegu_mt_2017.csv", row.names = F, col.names = F, sep = ",")

## end of script.