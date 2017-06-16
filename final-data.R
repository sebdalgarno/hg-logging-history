source('header.R')

### polygons
# read cleaned shape, fix fields and wirte as two .shps
log17cl <- st_read('input/data/new/output/shps/log17-YearHarvest-Cleaned-UTM8.shp')

log17cl %<>% mutate(SecondGrow = ifelse(is.na(SecondGrow), 0, SecondGrow))

log17all <- log17cl %>% mutate(SG = SecondGrow) %>%
  select(-SecondGrow, -ID) %>%
  mutate(ID = 1:n()) %>%
  mutate(DigiSource = Source,
         DateSource = ifelse(is.na(YearSource), "DATA", 
                             ifelse(YearSource == "man", "GTL", 
                                    ifelse(YearSource == "ccb", "CCB", YearSource)))) %>%
  select(-YearSource, -Source) %>%
  select(ID, YearHarv, SG, DateSource, DigiSource)

log17og <- filter(log17all, SG != 1) %>% select(-SG)
log17sg <- filter(log17all, SG == 1) %>% select(-SG)

write_shp(log17og, layer = 'log17-YearHarvest-OG-UTM8', path = path.expand("input/data/new/output/shps"))
write_shp(log17sg, layer = 'log17-YearHarvest-SG-UTM8', path = path.expand("input/data/new/output/shps"))
write_shp(log17all, layer = 'log17-YearHarvest-ALL-UTM8', path = path.expand("input/data/new/output/shps"))

### roads

