# quick ggplots
ggsf <- function(data) {
  ggplot(data) + geom_sf()
}

# quick leaflets
leaf <- function(data) {
  data %<>% as("Spatial")
  leaflet() %>% addPolygons(data)
}

# transform all to epsg
trans <- function(data, crs = epsg) {
  if(st_crs(data)$epsg == crs) {
    return(data)
  }
  if(st_crs(data)$epsg != crs) {
    data %<>% st_transform(crs)
    return(data)
  }
}

# write shapefiles, overwrite if exists
write_shp <- function(data, path = path.expand("input/data/new/clean/"), layer) {
  
  file <- paste0(path, layer)
  if (file.exists(paste0(file, ".shp"))) {
    file.remove(paste0(file, ".shp"))
  }
  if (file.exists(paste0(file, ".shx"))) {
    file.remove(paste0(file, ".shx"))
  }
  if (file.exists((paste0(file, ".prj")))) {
    file.remove(paste0(file, ".prj"))
  }
  if (file.exists((paste0(file, ".dbf")))) {
    file.remove(paste0(file, ".dbf"))
  }
  st_write(data, dsn = path, layer = layer, driver = "ESRI Shapefile", update = T)
}

# plot_save
plot_save <- function(plot, name, path = path.expand("input/data/new/output/plots/"),
                                                     width = 6, height = 6) {
  ggsave(file = name, plot = plot, path = path, width = width, height = height)
}

# convert data.frame with xy coords containing 'x_contains' and 'y_contains' to sf object
sf_convert = function(data, epsg = 26910, x_contains = 'Easting', y_contains = 'Northing') {
  coords <- select(data, contains(x_contains), contains(y_contains))
  if(length(names(coords)) > 2)
    error("More than two columns selected as coordinates.")
  data %<>% st_as_sf(coords = names(coords)) %>%
    st_set_crs(epsg)
}

# convert to sp object
sp_convert = function(data, epsg = 26910, x = 'Easting', y = 'Northing') {
  sp::coordinates(data) <- c(x, y)
  sp::proj4string(data) <- CRS(paste0("+init=epsg:", epsg))
  return(data)
}

# get ColorCode (accepts sp or sf objects)
get_color <- function(data) {
  proportion_range <- function (x, na.rm = FALSE) {
    (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
  }
  if(inherits(data, 'Spatial')) {
    red <- proportion_range(data@data$EastingCentroid)
    yellow <- proportion_range(data@data$NorthingCentroid)
    data@data$ColorCode <- grDevices::rgb(red = red, yellow = yellow)
    data
  }
  
  else{
    red <- proportion_range(data$EastingCentroid)
    blue <- proportion_range(data$NorthingCentroid)
    data$ColorCode <- grDevices::rgb(red = red, blue = blue, green = 0.5)
    data
  }
}

# convert DateTime columns to POSIXct
date_time <- function(data, fun = lubridate::ymd_hms, contains = "Date") {
  date <- select(data, contains(contains))
  data %<>% select(-contains(contains))
  date[] <- lapply(date, fun, tz = tz_analysis)
  cbind(data, date)
}