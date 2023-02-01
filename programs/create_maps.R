################################################################################
# This program creates 4 maps: a global temperature map, a global gdppc difference map
# for regression 1 table 2, a global gdppc difference map for regression 3 
# table 2, and a global gdppc difference map for regression 3 table 1.
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 8/12/2022
################################################################################

############
# Process Data for countries with Economic Data + Climate Projections
############
#Create temperature difference vector (temp 2100 - temp 2010)
globalTemp <- results_model_reg1 %>%
  mutate(diff = as.numeric(T2100) - as.numeric(T2010)) %>%
  rename(countryname = country) %>%
  select(countryname, diff, T2010)

############
# Process Data for countries without Economic Data + Climate Projections
############

#Countries that are missing economic data, but have temperature paths for
missing_countries <- c('Turkmenistan', 'Afghanistan', 'Albania', 'Cuba', 'Greenland', 'Somalia', 'Papua_New_Guinea', 
                       'France_Guiana', 'Libya', 'Eritrea', 'Swaziland', 'Kosovo', 'S._Sudan', 'Puerto_Rico',
                       'N._Korea', 'Myanmar', 'Montenegro', 'Guyana', 'West_Bank')

for (country in missing_countries) {
  files <- list.files(path = here('data','climProj',country,'atlas', 'series','CMIP5',
                                  'rcp85','monthly_dump1',country), pattern = "*.txt")
  
  df_list <- list()
  
  
  for (i in 1:length(files)) {
    iter <- i
    res <- try({
      df_list[[i]] <- read.table(here('data','climProj',country,'atlas', 'series','CMIP5',
                                      'rcp85','monthly_dump1',country,files[i]))    }
    )
    if(inherits(res, "try-error" )& (!(iter < 40))) {
      next
    }
  }
  
  CMIP5 <- bind_rows(df_list, .id = "projection_type")
  
  CMIP5avg <- CMIP5 %>%
    mutate(year = V3) %>%
    group_by(year) %>%
    mutate(avg = mean(V2),
           kmni_countries = country
    ) %>%
    filter(projection_type == 1) %>%
    ungroup() %>%
    filter(year >= 2010) %>%
    select(year, avg)
  
  temp_2010 <- CMIP5avg$avg[(CMIP5avg$year == 2010)]
  temp_2100 <- CMIP5avg$avg[(CMIP5avg$year == 2100)]
  tempdiff <- temp_2100 - temp_2010
  globalTemp <- rbind(globalTemp, c(country, tempdiff,temp_2010))
}

globalTemp$countryname <- recode(globalTemp$countryname,
                                      'United States' = 'USA',
                                      'United Kingdom' = 'UK',
                                      'Korea, Rep.' = 'South Korea',
                                      'Dominican Rep'='Dominican Republic',
                                      'Czech Rep' = 'Czech Republic',
                                      'Eq._Guinea' = 'Equatorial Guinea',
                                      'Macedonia' = 'North Macedonia',
                                      'France Guiana'='French Guiana',
                                      'N._Korea' = 'North Korea',
                                      'S._Sudan' = 'South Sudan',
                                 'Venezuela, RB' = 'Venezuela',
                                 'Russian Federation' = 'Russia',
                                'Slovak Republic' = 'Slovakia',
                                'Syrian Arab Republic' = 'Syria',
                                'Egypt, Arab Rep.' = 'Egypt',
                                'Yemen, Rep.' = 'Yemen',
                                'Congo, Dem. Rep.' = 'Democratic Republic of the Congo',
                                'Congo, Rep.' = 'Republic of Congo',
                                'Kyrgyz Republic' = 'Kyrgyzstan',
                                'Papua_New_Guinea' = 'Papua New Guinea',
                                'Puerto_Rico' = 'Puerto Rico',
                                'France_Guiana' = 'French Guiana',
                                'Iran, Islamic Rep.' = 'Iran',
                                'Lao PDR' = 'Laos',
                                "Cote d'Ivoire" = 'Ivory Coast',
                                'Macedonia, FYR' = 'North Macedonia',
                                'Gambia, The' = 'Gambia',
                                'West_Bank' = 'Palestine')

#Merge with geographic data
map.world <- map_data("world")
map.world <- map.world[!(map.world$region == 'Antarctica'),]
map.world_joined <- left_join(map.world, globalTemp, by = c('region' = 'countryname')) %>%
  mutate(diff = as.numeric(diff),
         T2010 = as.numeric(T2010))

################################################################################
# Figure 3- Change in Temperature under RCP 8.5 between 2010-2100
################################################################################

#Make temp map
tempMap <- ggplot() +
  geom_polygon(data=map.world_joined, aes(x=long, y = lat, group = group, fill = diff), color='black', size = 0.1) +
  scale_fill_viridis_c(name = 'Temp Change (C°)',option="magma", breaks=c(2,3,4,5,6,7), limits=c(1.8,7.2)) +
  theme_mapsEKG()
  
ggsave(filename = here('results','figures', 'tempMap_diff.eps'), 
       plot = tempMap, 
       units = "in", 
       height = 8, width = 14, dpi = 300, 
       device = "eps")

################################################################################
# Figure 2 - Average Annual temperature in 2010
################################################################################

#Make temp map
tempMap2 <- ggplot() +
  geom_polygon(data=map.world_joined, aes(x=long, y = lat, group = group, fill = T2010), color='black', size = 0.1) +
  scale_fill_gradientn(colors=c('blue','white','red'),
                       values=rescale(c(-20,optTemp_model_tfp2,30)),
                       limits=c(-22,32),
                       name = 'Temperature (C°)') +
  theme_mapsEKG()
tempMap2

ggsave(filename = here('results','figures', 'tempMap_2010.eps'), 
       plot = tempMap2, 
       units = "in", 
       height = 8, width = 14, dpi = 300, 
       device = "eps")

################################################################################
# Figure 9 - Results with Reduced-Form Growth Effects
################################################################################

#Recode countryname variable
results_rf_ge <- results_rf_reg1
results_rf_ge$country <- recode(results_rf_ge$country,
                                   'United States' = 'USA',
                                   'United Kingdom' = 'UK',
                                   'Korea, Rep.' = 'South Korea',
                                   'Dominican Rep'='Dominican Republic',
                                   'Czech Rep' = 'Czech Republic',
                                   'Eq._Guinea' = 'Equatorial Guinea',
                                   'Macedonia' = 'North Macedonia',
                                   'France Guiana'='French Guiana',
                                   'N._Korea' = 'North Korea',
                                   'S._Sudan' = 'South Sudan',
                                   'Venezuela, RB' = 'Venezuela',
                                   'Russian Federation' = 'Russia',
                                   'Slovak Republic' = 'Slovakia',
                                   'Syrian Arab Republic' = 'Syria',
                                   'Egypt, Arab Rep.' = 'Egypt',
                                   'Yemen, Rep.' = 'Yemen',
                                   'Congo, Dem. Rep.' = 'Democratic Republic of the Congo',
                                   'Congo, Rep.' = 'Republic of Congo',
                                   'Kyrgyz Republic' = 'Kyrgyzstan',
                                   'Papua_New_Guinea' = 'Papua New Guinea',
                                   'Puerto_Rico' = 'Puerto Rico',
                                   'France_Guiana' = 'French Guiana',
                                   'Iran, Islamic Rep.' = 'Iran',
                                   'Lao PDR' = 'Laos',
                                   "Cote d'Ivoire" = 'Ivory Coast',
                                   'Macedonia, FYR' = 'North Macedonia',
                                   'Gambia, The' = 'Gambia',
                                   'West_Bank' = 'Palestine')

map.world <- map_data("world")
map.world <- map.world[!(map.world$region == 'Antarctica'),]
map.world_joined <- left_join(map.world, results_rf_ge, by = c('region' = 'country'))
map.world_joined <- map.world_joined %>%
  mutate(Ydiff = as.numeric(Ydiff))
map.world_joined$Ydiff[map.world_joined$Ydiff < -110] <- -110
map.world_joined$Ydiff[map.world_joined$Ydiff > 120] <- 120
GDPPCMap1 <- ggplot() +
  geom_polygon(data=map.world_joined, aes(x=long, y = lat, group = group, fill = Ydiff), color='black', size = 0.1) +
  scale_fill_distiller(palette = "BrBG", breaks=c(-100,-50,0,50, 100), limits=c(-110,120),name = 'GDPPC % Change') +
  theme_mapsEKG()

ggsave(filename = here('results','figures', 'growth_effects_rf.eps'), plot = GDPPCMap1, units = "in", height = 8, width = 14, dpi = 300, device = "eps")
ggsave(filename = here('results','figures', 'growth_effects_rf.png'), plot = GDPPCMap1, units = "in", height = 8, width = 14, dpi = 300, device = "png")

################################################################################
# Figure 4 - Impact of Climate Change on GDP per Capita in 2100
################################################################################
level_effects_model_tbl <- results_model_reg2 %>%
  select(country,Ydiff, Adiff)

#Recode countryname variable
level_effects_model_tbl$country <- recode(level_effects_model_tbl$country,
                                          'United States' = 'USA',
                                          'United Kingdom' = 'UK',
                                          'Korea, Rep.' = 'South Korea',
                                          'Dominican Rep'='Dominican Republic',
                                          'Czech Rep' = 'Czech Republic',
                                          'Eq._Guinea' = 'Equatorial Guinea',
                                          'Macedonia' = 'North Macedonia',
                                          'France Guiana'='French Guiana',
                                          'N._Korea' = 'North Korea',
                                          'S._Sudan' = 'South Sudan',
                                          'Venezuela, RB' = 'Venezuela',
                                          'Russian Federation' = 'Russia',
                                          'Slovak Republic' = 'Slovakia',
                                          'Syrian Arab Republic' = 'Syria',
                                          'Egypt, Arab Rep.' = 'Egypt',
                                          'Yemen, Rep.' = 'Yemen',
                                          'Congo, Dem. Rep.' = 'Democratic Republic of the Congo',
                                          'Congo, Rep.' = 'Republic of Congo',
                                          'Kyrgyz Republic' = 'Kyrgyzstan',
                                          'Papua_New_Guinea' = 'Papua New Guinea',
                                          'Puerto_Rico' = 'Puerto Rico',
                                          'France_Guiana' = 'French Guiana',
                                          'Iran, Islamic Rep.' = 'Iran',
                                          'Lao PDR' = 'Laos',
                                          "Cote d'Ivoire" = 'Ivory Coast',
                                          'Macedonia, FYR' = 'North Macedonia',
                                          'Gambia, The' = 'Gambia',
                                          'West_Bank' = 'Palestine')

map.world <- map_data("world")
map.world <- map.world[!(map.world$region == 'Antarctica'),]
map.world_joined <- left_join(map.world, level_effects_model_tbl, by = c('region' = 'country'))
map.world_joined <- map.world_joined %>%
  mutate(Ydiff = as.numeric(Ydiff),
         Adiff = as.numeric(Adiff))

GDPPCMap3 <- ggplot() +
  geom_polygon(data=map.world_joined, aes(x=long, y = lat, group = group, fill = Ydiff), color='black', size = 0.1) +
  scale_fill_distiller(palette = "PRGn", breaks=c(-10,-5,0,5,10), limits=c(-12.6,10.5),name = 'GDPPC % Change') +
  theme_mapsEKG()
GDPPCMap3
ggsave(filename = here('results','figures', 'level_effects_model.eps'), plot = GDPPCMap3, units = "in", height = 8, width = 14, dpi = 300, device = "eps")
ggsave(filename = here('results','figures', 'level_effects_model.png'), plot = GDPPCMap3, units = "in", height = 8, width = 14, dpi = 300, device = "png")


