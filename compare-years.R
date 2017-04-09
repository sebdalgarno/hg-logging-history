source('header.R')
set_sub("clean")

load_datas()

# load clipped vri (qgis output)
inter <- st_read(layer = "log04-vri-intersect", dsn = "input/data/new/output/shps")

# area of intersected polys
inter %<>% st_cast("POLYGON")
inter %<>% mutate(VriAreaU = st_area(.))

# calculate proportional area - filter if overlaps by less than 10%
inter %<>% mutate(ProportionalArea = as.numeric(VriAreaU/AreaU))
inter %<>% filter(ProportionalArea > 0.25)

# year difference
inter %<>% filter(!is.na(YearHarv) & YearHarv > 0 & Year > 0)
inter %<>% filter(YearHarv < 2005)
inter %<>% mutate(YearDiff = YearHarv - Year)

# mean by log04 ID
inter.grp <- group_by(inter, Log04ID) %>% summarise(AveYearDif = mean(YearDiff)) %>% st_cast("MULTIPOLYGON")

# inter plot
write_shp(inter.grp, layer = "log04-vri-intersection-summary", path = path.expand("~/Dropbox/Contracts/Gowgaia/logging-history/r-hglog/r-hglog/input/data/new/output/shps/"))

# find proportion of intersected polygons that are within 5 years difference
inter %<>% mutate(within5 = ifelse(abs(YearDiff) > 5, 0, 1),
                  within10 = ifelse(abs(YearDiff) > 10, 0, 1)) 
prop5 <- sum(inter$within5)/nrow(inter)
prop10 <- sum(inter$within10)/nrow(inter)

# plot difference in harvest year
diffmap <- ggplot() + geom_sf(data = inter.grp, aes(fill = AveYearDif), color = "transparent") +
  scale_fill_gradient2(name = "Mean Year\nDifference\n(VRI - 2004)") + theme_dark()

diffplot <- ggplot() + geom_point(data = inter, aes(x = YearHarv, y = Year), size = 0.3) + 
  labs(y = "2004 Movie Data", x = "VRI Data")

diffhist <- ggplot() + geom_histogram(data = inter, aes(x = YearDiff), binwidth = 1) + 
  labs(x = "Difference in Harvest Year (bin width = 1)", y = "Count")


plot_save("diffplot.png", plot = diffplot)
plot_save("diffhist.png", plot = diffhist)
plot_save("diffmap.png", plot = diffmap)
