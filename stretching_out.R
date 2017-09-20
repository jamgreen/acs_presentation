#further examples in the presentation


if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(sf, tigris, viridis, ggthemes, ggplot2, tidycensus, stringr, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

#stretch example is MHI at census tract level for METRO counties
#gonna collect census tracts with geography, sum up to counties for graph

#key <- census_api_key("c67f1b3134300374d51a55c543649f843fb7d2b3",install = TRUE)
acs_key <- Sys.getenv("CENSUS_API_KEY")

vartabs <- load_variables(year = 2015, dataset = "acs5")

mhi_tables <- c("B19013_001")

#download tracts and county, get the tracts for PDX Metro counties and counties for the state

mhi_tract <- get_acs(geography = "tract", variables = mhi_tables, state = "OR", geometry = TRUE)
mhi_tract <- mhi_tract %>% mutate(CountyFIPS = str_sub(GEOID, 1, 5))

metro_counties <- c("41051", "41005", "41009", "41067", "41071") 
metro_tract <- mhi_tract %>% filter(CountyFIPS %in% metro_counties)

or_mhi_county <- get_acs(geography ="county", variables = mhi_tables, state = "OR", geometry = TRUE)

#Metro tract map

p1 <- ggplot() + geom_sf(data = metro_tract, aes(fill = estimate)) +
  coord_sf(datum = NA) + 
  theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14, margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9, margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar, name = "MHI Estimate") +
  labs( caption = "Source: US Census Bureau ACS (2011-2015)",
        title = "Median Household Income for PDX Metro\n at the census tract level",
        subtitle = "An R 'sf' Example") + theme_minimal()


#state county map

p2 <- ggplot() + geom_sf(data = or_mhi_county, aes(fill = estimate)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 14, margin = margin(b = -20))) +
  theme(plot.subtitle = element_text(size = 9, margin = margin(t = -15), hjust = 0)) +
  theme(plot.caption = element_text(size = 9, margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar, name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for Oregon\n at the county level",
       subtitle = "An R 'sf' Example") + theme_minimal()



#vulnerability index, get poverty, White/NonWhite, <5, 64<

vul_vars <- c("B17001_001", "B17001_002",  "B02001_001", "B02001_002", "B01001_003", 
              "B01001_020","B01001_021", "B01001_022", "B01001_023", "B01001_024", 
              "B01001_025","B01001_027", "B01001_044", "B01001_045", "B01001_046", 
              "B01001_047", "B01001_048", "B01001_049")

vul_acs <- get_acs(geography = "tract", variables = vul_vars, state = "OR", output = "wide")
vul_acs <- vul_acs %>% mutate(CountyFIPS = str_sub(GEOID, 1, 5))

vul2 <- vul_acs %>% mutate(PovShare = B17001_002E/B17001_001E,
                          NonWhite = (B02001_001E - B02001_002E)/B02001_001E,
                           Under5 = (B01001_003E + B01001_027E)/B02001_001E,
                           Older64Male = B01001_020E + B01001_021E + B01001_022E +
                             B01001_023E + B01001_024E +B01001_025E, 
                           Older64Female = B01001_044E + B01001_045E + B01001_046E +
                             B01001_047E + B01001_048E + B01001_049E, 
                           Older64 = (Older64Male + Older64Female)/B02001_001E) %>% 
  select(NAME, GEOID, CountyFIPS, PovShare, NonWhite, Under5, Older64)

vul2 <- vul2 %>% 
  mutate(z_Pov = (PovShare - mean(PovShare, na.rm = TRUE))/sd(PovShare, na.rm = TRUE),
         z_NonWhite = (NonWhite - mean(NonWhite, na.rm = TRUE))/sd(NonWhite, na.rm = TRUE),
         z_Under5 = (Under5 - mean(Under5, na.rm = TRUE))/sd(Under5, na.rm = TRUE),
         z_Older64 = (Older64 - mean(Older64, na.rm = TRUE))/sd(Older64, na.rm = TRUE))

vul2 <- vul2 %>% 
  mutate(VulIndex = (z_Pov + z_NonWhite + z_Under5 + z_Older64)/4) %>% 
  select(GEOID, CountyFIPS, z_Pov, z_NonWhite, z_Under5, z_Older64, VulIndex)

metro_counties <- c("41051", "41005", "41009", "41067", "41071") 

vul2 <- vul2 %>% filter(CountyFIPS %in% metro_counties)

or_tracts <- tracts(state = "OR")

metro_vul <- inner_join(vul2, or_tracts, by = c("GEOID" = "GEOID")) %>% 
  select(1:7, geometry) %>% st_as_sf()

p4 <- ggplot() + geom_sf(data = metro_vul, aes(fill = VulIndex)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 9)) +
  scale_fill_viridis(name = "Social Vulnerability Measure") +
  labs(title = "Social Vulnerability for Metro oregon",
       subtitle = "An R 'sf' example",
       caption = "Source: US Census Bureau ACS (2011-2015)") +
  theme_minimal()
