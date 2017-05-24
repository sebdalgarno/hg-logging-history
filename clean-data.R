source('header.R')

set_sub("get")

load_datas()

# transform all to utm zone 8
list <- gsub(".rds", "", list.files(path = 'output/data/get/')) 

for(i in 1:length(list)) {
  assign(list[i], trans(load_data(list[i])))
}

# change variable names (10 characters max for sriting shps)
concut %<>% select(ObjectID = OBJECTID, DataSource = DATA_SOURCE, 
                   AreaSqm = FEATURE_AREA_SQM, AreaHa = AREA_HA,
                   BlockID = VEG_CONSOLIDATED_CUT_BLOCK_ID,
                   OpeningID = OPENING_ID, YearHarv = HARVEST_YEAR,
                   DistStart = DISTURBANCE_START_DATE, DistEnd = DISTURBANCE_END_DATE,
                   LengthM = FEATURE_LENGTH_M, geometry)

roads %<>% select(Location = LOCATION, ObjectID = OBJECTID, DateRetire = RETIREMENT_DATE,
                  RoadID = ROAD_SECTION_ID, RoadName = ROAD_SECTION_NAME, Width = SECTION_WIDTH,
                  Type = FILE_TYPE_DESCRIPTION, LengthKm = ROAD_SECTION_LENGTH,
                  Status = LIFE_CYCLE_STATUS_CODE, DateAward = AWARD_DATE, Client = CLIENT_NAME,
                  LengthM = FEATURE_LENGTH)

vriage %<>% select(PolygonID = POLYGON_ID, OpeningID = OPENING_ID, DateHarv = HARVEST_DA,
                   ProjAge1 = PROJ_AGE_1, ProjAge2 = PROJ_AGE_2)

rsltinv %<>% select(CoverID = FOREST_COVER_ID, OpeningID = OPENING_ID, AreaSqm = FEATURE_AREA,
                    PerimeterM = FEATURE_PERIMETER, DataSource = DATA_SOURCE_CODE)

rsltres %<>% select(FileID = FOREST_FILE_ID, PermitID = CUTTING_PERMIT_ID, BlockID = CUT_BLOCK_ID, 
                    OpeningID = OPENING_ID, AreaHa = SILV_POLYGON_AREA)

rsltopen14 %<>% select(YearHarv = END_YR)
  
log04 %<>% select(AreaHa = AREA, PerimeterM = PERIMETER, YearHarv = YEAR)

road04 %<>% select(Year = YEAR, Length = LENGTH)

# new areas with units
concut %<>% mutate(AreaU = st_area(.),
                   LengthU = st_length(.))

roads %<>% mutate(LengthU = st_length(.))

rsltinv %<>% mutate(AreaU = st_area(.))

rsltres %<>% mutate(AreaU = st_area(.))

log04 %<>% mutate(AreaU = st_area(.),
                  LengthU = st_length(.))

road04 %<>% mutate(LengthU = st_length(.))

rsltopen14 %<>% mutate(AreaU = st_area(.))

# DateTime
vriage %<>% mutate(DateHarv = lubridate::ymd(DateHarv, tz = tz_analysis)) %>%
  mutate(YearHarv = as.integer(lubridate::year(DateHarv)))

roads %<>% mutate(DateAward = lubridate::ymd_hms(DateAward, tz = tz_analysis),
                  DateRetire = lubridate::ymd_hms(DateRetire, tz = tz_analysis))

concut %<>% mutate(DistStart = lubridate::ymd_hms(DistStart, tz = tz_analysis),
                   DistEnd = lubridate::ymd_hms(DistEnd, tz = tz_analysis))

# unique IDs
log04 %<>% mutate(Log04ID = 1:n())
rsltopen14 %<>% mutate(newopenID = 1:n())

# write transformed .shps
write_shp(concut, layer = "Consolidated-Cut-Blocks-UTM8-2017")
write_shp(vriage, layer = "VRI-AgeFields-UTM8-2017")
write_shp(rsltinv, layer = "RESULTS-Inventory-UTM8-2017")
write_shp(rsltres, layer = "RESULTS-Reserve-UTM8-2017")
write_shp(rsltopen14, layer = "RESULTS-Openings-UTM8-2014on")
write_shp(roads, layer = "Roads-UTM8-2017")
write_shp(log04, layer = "Log04-UTM8-2017")
write_shp(road04, layer = "Roads04-UTM8-2017")

set_sub("clean")

rm(vri)
save_datas()


