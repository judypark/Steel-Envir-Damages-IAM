library(dplyr)
library(tidyr)
library(sf)

setwd("C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/2_NEI2017_Processing")

## NEI Point Files
## Source: https://www.epa.gov/air-emissions-inventories/2017-national-emissions-inventory-nei-data#datas (04/27/2022)
# US facilities
fac_1 <- read.csv("C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/0_2017NEI_Raw_Data/2017neiJan_facility_process_byregions/point_12345.csv")
fac_2 <- read.csv("C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/0_2017NEI_Raw_Data/2017neiJan_facility_process_byregions/point_678910.csv")

oldnames <- c("eis.facility.id","pollutant.desc","state","fips.state.code","county",
              "fips.code","site.name","naics.code","naics.description",
              "scc","facility.source.type","address","city",
              "zip.code","site.latitude","site.longitude","total.emissions")
newnames <- c("eis","description","state","fipsst","county","fips","name","naics","naics_description",
              "scc","source_type","address","city","zip","lat","lon","emissions")
for (i in 1:length(oldnames)) {
  colnames(fac_1)[colnames(fac_1) == oldnames[i]] <- newnames[i]
}

fac_1 <- subset(fac_1, select = newnames)

oldnames <- c("eis_facility_id","pollutant_desc","state","stfips","county",
              "fips","site_name","naics_code","naics_description",
              "scc","facility_source_type","address","city",
              "zip_code","site_latitude","site_longitude","total_emissions")
newnames <- c("eis","description","state","fipsst","county","fips","name","naics","naics_description",
              "scc","source_type","address","city","zip","lat","lon","emissions")
for (i in 1:length(oldnames)) {
  colnames(fac_2)[colnames(fac_2) == oldnames[i]] <- newnames[i]
}

fac_2 <- subset(fac_2, select = newnames)

# Filter for relevant pollutants
pollutants <- c("Sulfur Dioxide","Ammonia","Nitrogen Oxides",
                "PM2.5 Primary (Filt + Cond)","Volatile Organic Compounds",
                "Carbon Dioxide","Methane","Nitrous Oxide")
fac_1 <- fac_1[fac_1$description %in% pollutants,]
fac_2 <- fac_2[fac_2$description %in% pollutants,]

# All facilities
fac_us <- rbind(fac_1,fac_2)
fac_us <- fac_us %>% mutate(mobile = ifelse(county == "Multiple (portable facilities)","Yes","No"))

# Tribal facilities
fac_us <- fac_us %>% mutate(fipsst = ifelse(floor(fips/1000) == 88,88,fipsst))
fac_t <- fac_us[fac_us$fipsst == 88,]
fac_us <- fac_us[fac_us$fipsst != 88,]

fac_t <- fac_t %>% mutate(tribal = "Yes")
fac_us <- fac_us %>% mutate(tribal = "No")

us_counties <- st_read('C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/References/Model Data - 2017/National Emissions Inventory/Data_2017/tl_2017_us_county/tl_2017_us_county.shp', quiet = T)
fac_t <- fac_t %>%
  mutate(lat_work = lat, lon_work = lon)
fac_t <- fac_t %>% 
  st_as_sf(coords = c("lon_work","lat_work"), crs = st_crs(us_counties))
intersected <- st_intersects(fac_t, us_counties)
fac_t <- fac_t %>%
  mutate(intersection = as.integer(intersected),
         fips = as.numeric(us_counties$GEOID[intersection]))
fac_t <- st_drop_geometry(fac_t)
fac_t <- subset(fac_t, select = -intersection)

fac_t <- subset(fac_t, select = -c(state,fipsst,county))
county_info <- read.csv("C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/2_NEI2017_Processing/County_Info.csv")
st_fipsst <- read.csv("C:/Users/judec/Documents/CMU/APEEP/0_Data_Judy/References/Model Data - 2017/Populations & Mortality Rates/Crosswalks/state_to_fipsst.csv")
county_info <- left_join(county_info, st_fipsst, by = "fipsst")
county_info <- subset(county_info, select = c("fips","county","state","fipsst"))

fac_t <- left_join(fac_t, county_info, by = "fips")
fac_t <- fac_t %>% relocate(c(state,fipsst,county), .before = fips)

# All facilities
fac_us <- rbind(fac_us,fac_t)
rm(fac_1,fac_2,fac_t,intersected,st_fipsst,us_counties)

## Collapse to scc level
fac_scc <- fac_us %>%
  group_by(eis, scc, description) %>%
  summarize(state = first(state), fipsst = first(fipsst),
            county = first(county), fips = first(fips), 
            name = first(name), 
            naics = first(naics), naics_description = first(naics_description), 
            source_type = first(source_type),
            address = first(address), city = first(city), zip = first(zip), 
            lat = first(lat), lon = first(lon), 
            mobile = first(mobile), tribal = first(tribal),
            emissions = sum(as.numeric(emissions), na.rm = T)) %>%
  mutate(emissions = ifelse(is.na(emissions),0,emissions))

## Collapse to facility level
fac_us <- fac_us %>%
  group_by(eis, description) %>%
  summarize(state = first(state), fipsst = first(fipsst),
            county = first(county), fips = first(fips), 
            name = first(name), 
            naics = first(naics), naics_description = first(naics_description), 
            source_type = first(source_type),
            address = first(address), city = first(city), zip = first(zip), 
            lat = first(lat), lon = first(lon), 
            mobile = first(mobile), tribal = first(tribal),
            emissions = sum(as.numeric(emissions), na.rm = T)) %>%
  mutate(emissions = ifelse(is.na(emissions),0,emissions))

## Spread by pollutant
fac_scc <- spread(fac_scc, description, emissions)
fac_us <- spread(fac_us, description, emissions)

oldnames <- c("Nitrogen Oxides","PM2.5 Primary (Filt + Cond)",
              "Volatile Organic Compounds","Ammonia", "Sulfur Dioxide",
              "Methane","Carbon Dioxide","Nitrous Oxide")
newnames <- c("nox","pmp","voc","nh3","so2","ch4","co2","n2o")
for (i in 1:length(oldnames)) {
  colnames(fac_scc)[colnames(fac_scc) == oldnames[i]] <- newnames[i]
  colnames(fac_us)[colnames(fac_us) == oldnames[i]] <- newnames[i]
}

fac_scc <- fac_scc %>% relocate(c(co2,ch4,n2o), .after = last_col())
fac_scc <- fac_scc %>% mutate_at(vars("nh3","nox","pmp","so2","voc","co2","ch4","n2o"), ~replace_na(.,0))
fac_us <- fac_us %>% relocate(c(co2,ch4,n2o), .after = last_col())
fac_us <- fac_us %>% mutate_at(vars("nh3","nox","pmp","so2","voc","co2","ch4","n2o"), ~replace_na(.,0))

## Output
write.csv(fac_scc, "emissions_by_scc_2017.csv", row.names = FALSE)
write.csv(fac_us, "emissions_by_facility_2017.csv", row.names = FALSE)

## end of script.