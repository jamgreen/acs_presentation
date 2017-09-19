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



  pov_share_wide <- pov_wide %>% 
                        mutate(WhitePov = (B17020H_002E/B17020H_001E)*100, 
                               BlackPov = (B17020B_002E/B17020B_001E)*100) %>% 
                        select(GEOID, NAME, WhitePov, BlackPov)

pov_share_wide <- pov_share_wide %>% mutate(PovDiff = BlackPov - WhitePov)
pov_share_wide <- arrange(pov_share_wide, desc(PovDiff))

pov_share_wide$NAME <- factor(pov_share_wide$NAME, levels = rev(pov_share_wide$NAME))
#Follow Bob Rudis guide here: https://rud.is/b/2016/04/17/ggplot2-exercising-with-ggalt-dumbbells/
#building out the plot

gg <- ggplot()
gg <- gg + geom_segment(data = pov_share_wide, aes(y = NAME, yend = NAME, x = 0, xend = 1,
                                                   color = "#b2b2b2", size = .15))

gg <- gg + geom_dumbbell(data = pov_share_wide, aes(y = NAME, x = WhitePov, xend = BlackPov),
                         size = 1.5, color = "#b2b2b2", point.size.l = 3, point.size.r = 3,
                         point.colour.l = "#9fb059", point.colour.r = "#edae52")
