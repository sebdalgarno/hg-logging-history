source('header.R')

set_sub("get")

# read in old 2004 data
log04 <- st_read('input/data/2004_Shapefiles_Blocks_Roads/log_year_2004_utm8.shp')
road04 <- st_read('input/data/2004_Shapefiles_Blocks_Roads/roads2004_for_movie.shp')

# read in new data
vri <- st_read('input/data/VRI_HG_all_fields/VRI2015_HG.shp')
vriage <- st_read('input/data/VRI_HG_age_fields_only/VRI2015_HG_AgeFields_Only.shp')

concut <- st_read('input/data/new/raw/VEG_CONSOLIDATED_CUT_BLOCKS_SP.geojson')
rsltinv <- st_read('input/data/new/raw/RSLT_FOREST_COVER_INV_SVW.geojson')
rsltres <- st_read('input/data/new/raw/RSLT_FOREST_COVER_RESERVE_SVW.geojson')
roads <- st_read('input/data/new/raw/FTEN_ROAD_SECTION_LINES_SVW.geojson')

save_datas()

