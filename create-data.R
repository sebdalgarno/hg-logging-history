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
vriharv <- filter(vri, (!is.na(DateHarv) & YearHarv > 2004) | 
                      (OpeningID != 0 & is.na(YearHarv)) | 
                      (OpeningID == 0 & YearHarv > 2004))

# check
# View(filter(vriharv, OpeningID == 0 & is.na(YearHarv)))
# View(filter(vriharv, OpeningID == 0 & !is.na(YearHarv)))
# View(filter(vriharv, OpeningID != 0 & is.na(YearHarv)))

# add Source column
log04 %<>% mutate(Source = "log04")
vriharv %<>% mutate(Source = "vri")
open14 %<>% mutate(Source = "rslts")

# select only year and ID
log04 %<>% select(YearHarv, Source)
vriharv %<>% select(YearHarv, Source)
open14 %<>% select(YearHarv, Source)

# combine datasets - with source info and common column names
st_crs(log04) <- 26908
st_crs(vriharv) <- 26908

harv <- rbind(log04, vriharv, open14)

write_shp(harv, layer = 'YearHarvest-SourcesCombined-UTM8')

set_sub('create')

save_data(harv)





