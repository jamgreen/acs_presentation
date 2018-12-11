#making geogif for changing mfg employment in multnomah county, or maybe pdx proper
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(devtools, dplyr, tigris, sf, stringr)
options(tigris_class = "sf")

install_github("jamgreen/lehdr")
library(lehdr)

#bring in lehd data for state and filter total emp and mfg
years <- 2004:2015

or_lehd <- grab_lodes(state = "or", year = years, lodes_type = "wac",
                      job_type = "JT01", segment = "S000", agg_geo = "bg")

or_lehd <- or_lehd %>% 
  select(1:4, CNS07)

names(or_lehd) <- tolower(names(or_lehd))

#download state block group file
#create new county column

or_bg <- block_groups(state = "or") %>% 
  select(1:2,5, geom = geometry)

names(or_bg) <- tolower(names(or_bg))


#filter down to multco and join to the shapefile

or_lehd <- or_lehd %>% 
  mutate(county_fips = str_sub(w_bg, 1, 5)) %>% 
  filter(county_fips == "41051")

or_lehd <- or_lehd %>% 
  left_join(or_bg, by = c("w_bg" = "geoid"))

#prep year into proper date and export

or_lehd <- or_lehd %>%
  mutate(lehd_date = paste0(year,"-01-01"))

or_lehd$lehd_date <- as.Date(or_lehd$lehd_date, format = "%Y-%m-%d")

or_lehd <- or_lehd %>% 
  st_as_sf(sf_column_name = "geom")

or_lehd <- or_lehd %>% 
  st_transform(crs = 2992)

st_write(or_lehd, "multco_mfg_lehd.shp")
