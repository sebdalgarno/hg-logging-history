source('header.R')

set_sub("clean")

road <- load_data("roads")
road04 <- load_data("road04")
digiroad <- st_read('input/data/new/output/shps/Digitised-Road-0619-UTM8.shp')
st_crs(road) <- "+proj=utm +zone=8 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
st_crs(road04) <- "+proj=utm +zone=8 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

road %<>% mutate(ID = 1:n())
roadin <- road[st_buffer(road04, 25),]
roadout <- filter(road, !(road$ID %in% roadin$ID))

prep <- function(data, source){
  st_crs(data) <- "+proj=utm +zone=8 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  data %<>% mutate(ID = 1:n()) %>%
    select(ID, Year) %>% 
    mutate(Source = source)
}

roads <- rbind(
               prep(road04, source = "log04"),
               prep(digiroad, source = "log17"))

write_shp(roads, "log17-Roads-All-UTM8", path = path.expand("input/data/new/output/shps/"))

### polygons
# read cleaned shape, fix fields and wirte as two .shps
log17cl <- st_read('input/data/new/output/shps/log17-YearHarvest-Cleaned0619-UTM8.shp')
st_crs(log17cl) <- "+proj=utm +zone=8 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

log17cl %<>% mutate(SecondGrow = ifelse(is.na(SecondGrow), 0, SecondGrow))

# for checking
log17all <- log17cl %>% mutate(SG = SecondGrow) %>%
  select(-SecondGrow, -ID) %>%
  mutate(ID = 1:n()) %>%
  mutate(PolySource = Source,
         DateSource = ifelse(is.na(YearSource), "data", 
                             ifelse(YearSource == "man", "gtl", 
                                    ifelse(YearSource == "ccb", "ccb", YearSource)))) %>%
  select(-YearSource, -Source) %>%
  select(ID, YearHarv, SG, DateSource, PolySource)

log17og <- filter(log17all, SG != 1) %>% select(-SG)
log17sg <- filter(log17all, SG == 1) %>% select(-SG)

write_shp(log17og, layer = 'log17-YearHarvest-OG-UTM8', path = path.expand("input/data/new/output/shps/"))
write_shp(log17sg, layer = 'log17-YearHarvest-SG-UTM8', path = path.expand("input/data/new/output/shps/"))
write_shp(log17all, layer = 'log17-YearHarvest-ALL-UTM8', path = path.expand("input/data/new/output/shps/"))

# filter by year and intersect with roads
year <- 2016
data <- log17sg

dat <- filter(data, YearHarv %in% year) 
  # filter(is.na(Roadless))
din <- dat[roads,]
dout <- filter(dat, !(dat$ID %in% din$ID))
dpre <- filter(data, YearHarv < year)
dtou <-dout[st_buffer(dpre, 1),]
dtouch <- filter(dout, !(dout$ID %in% dtou$ID))
dpreand <- filter(log17cl, YearHarv > 2003 & YearHarv <= year)
write_shp(dtouch, layer = paste0("sg", year, "-roadless-polys"), path = path.expand("input/data/new/output/shps/"))
write_shp(dpreand, layer = paste0("sg", year, "pre-log17"), path = path.expand("input/data/new/output/shps/"))

### roads

