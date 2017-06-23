source('header.R')

set_sub("final")

load_datas()

poly <- log17Poly
road <- log17Road

npal <- max(poly$YearHarv)-min(poly$YearHarv)
get_color <- colorRampPalette(c("red", "yellow"))
tmp <- get_color(100)

tifferize <- function(poly = poly, road = road, i = i) {
  poly %<>% filter(YearHarv == i)
  road %<>% filter(YearHarv == i)
  
  poly %<>% mutate(CC == )
}