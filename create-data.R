source('header.R')

set_sub('clean')

log04 <- load_data("log04")
vri <- load_data("vriage")
open14 <- load_data("rsltopen14")

# VRI harvested >2004 logged subset
# 3 cases
# 1. has opening and yearharv info - filter > 2004
# 2. has openingID but no yearharv info - include
# 3. has yearHarv info but no openingID - include and filter > 2004
vriharv04 <- filter(vri, (!is.na(DateHarv) & YearHarv > 2003) | 
                      (OpeningID != 0 & is.na(YearHarv)) | 
                      (OpeningID == 0 & YearHarv > 2003))

vriharv <- filter(vri, (!is.na(DateHarv) | (OpeningID != 0 & is.na(YearHarv))))

vrisub <- filter(vri, !is.na(DateHarv) & YearHarv > 2003)
logsub <- filter(log04, YearHarv != 0)

# check
# View(filter(vriharv, OpeningID == 0 & is.na(YearHarv)))
# View(filter(vriharv, OpeningID == 0 & !is.na(YearHarv)))
# View(filter(vriharv, OpeningID != 0 & is.na(YearHarv)))

# add Source column
log04 %<>% mutate(Source = "log04")
vriharv04 %<>% mutate(Source = "vri")
open14 %<>% mutate(Source = "rslts")
vrisub %<>% mutate(Source = "vri")
logsub %<>% mutate(Source = 'log04')

# select only year and ID
log04 %<>% select(YearHarv, Source)
vriharv04 %<>% select(YearHarv, Source)
open14 %<>% select(YearHarv, Source)
vrisub %<>% select(YearHarv, Source)
logsub %<>% select(YearHarv, Source)

# combine datasets - with source info and common column names
st_crs(log04) <- 26908
st_crs(vriharv04) <- 26908
st_crs(vrisub) <- 26908
st_crs(logsub) <- 26908

harv <- rbind(log04, vriharv04, open14)
harvsub <- rbind(logsub, vrisub, open14)
harvog <- filter(harvsub, YearHarv < max(YearHarv) - 40)

harv %<>% mutate(YearHarv = replace(YearHarv, is.na(YearHarv), 999))

write_shp(harv, layer = 'YearHarvest-SourcesCombined-UTM8')
write_shp(vriharv, layer = 'VRI-OnyHarvested-UTM8')
write_shp(harvsub, layer = 'YearHarvest-NoHoles-UTM8')
write_shp(harvog, layer = 'YearHarvest-NoHoles-OG-UTM8')


set_sub('create')

save_data(harv)

### post-digitising
log17 <- st_read('input/data/new/output/shps/Digitised-Block-Polygon-UTM8.shp')
log17 %<>% select(-id)
st_crs(log17) <- 26908
log17 %<>% mutate(Source = "log17")

harvsub %<>% mutate(SecondGrow = 0)

# combine
log17com <- rbind(harvsub, log17)
log17com %<>% mutate(ID = 1:n())

# create a shape > 1976 or digitised
harvsg <- filter(log17com, YearHarv >= 1977 | Source == "log17")

write_shp(log17com, layer = 'log17-YearHarvest-UTM8', path = path.expand("input/data/new/output/shps"))
write_shp(harvsg, layer = 'YearHarvest-SecondGrowth-UTM8')

## filter polys that intersect
# check and fix (qgis) invalid geometries
log17false <- log17com[!st_is_valid(log17com),]

# get intersected polys only
log17inter <- st_read('input/data/new/output/shps/log17-intersect.shp')
# isolate those with an intersection
log17inter %<>% group_by(ID) %>% 
  mutate(n = n()) %>%
  filter(n > 1) %>% 
  st_cast("MULTIPOLYGON")

log17inter %<>% st_intersection(.,.)

set_sub('intersect')
save_data(log17inter)

write_shp(log17inter, layer = 'log17-onlyOverlap-intersection', path = path.expand("input/data/new/output/shps"))

# read cleaned shape, fix fields and wirte as two .shps
log17cl <- st_read('input/data/new/output/shps/log17-YearHarvest-Cleaned-UTM8.shp')

log17cl %<>% mutate(SecondGrow = ifelse(is.na(SecondGrow), 0, SecondGrow))


log17og <- filter(log17cl, SecondGrow != 1) %>% select(-SecondGrow)
log17sg <- filter(log17cl, SecondGrow == 1) %>% select(-SecondGrow)

write_shp(log17og, layer = 'log17-YearHarvest-OG-UTM8', path = path.expand("input/data/new/output/shps"))
write_shp(log17sg, layer = 'log17-YearHarvest-SG-UTM8', path = path.expand("input/data/new/output/shps"))
write_shp(log17cl, layer = 'log17-YearHarvest-ALL-UTM8', path = path.expand("input/data/new/output/shps"))



