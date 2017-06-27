source('header.R')

set_sub("final")

load_datas()

poly <- log17Poly %>%
  mutate(Type = "poly",
         Year = YearHarv)
road <- log17Road

road15 <- st_buffer(road, 15) %>%
  mutate(Type = "road")

comb <- rbind(select(poly, Year, ID, Type), select(road15, Year, ID, Type))

set_sub("logarea")

get_color <- colorRampPalette(c("#ff8d00", "#ffff97"))

labs <- c(1901, 2016)

comb %<>% mutate(Year = replace(Year, Year == 2017, NA),
                 Year = replace(Year, Year == 88, 1988)) %>%
  filter(!is.na(Year)) %>%
  filter(Year != 0)
comb %<>% mutate(AreaHa = as.numeric(st_area(.))/10000) %>%
  group_by(Year) %>%
  summarise(SumAreaHa = sum(AreaHa))
comb %<>% mutate(Color = get_color(116))

road15 %<>% mutate(Year = replace(Year, Year == 2017, NA),
                 Year = replace(Year, Year == 88, 1988)) %>%
  filter(!is.na(Year)) %>%
  filter(Year != 0)
road15 %<>% mutate(AreaHa = as.numeric(st_area(.))/10000) %>%
  group_by(Year) %>%
  summarise(SumAreaHa = sum(AreaHa))
road15 %<>% mutate(Color = get_color(116))

combcsv <- comb
st_geometry(combcsv) <- NULL

write_csv(combcsv, 'input/data/new/output/csv/log17-area-summary.csv')

logplot <- function(data = comb, n = i) {
  data %<>% mutate(Point = ifelse(Year == n, n, NA),
                   lovals = predict(loess(SumAreaHa ~ Year,.)))
  val <- filter(data, Year == n)
  value <- val$lovals
  
  p <- ggplot(data) + 
    geom_bar(aes(x = Year, y = SumAreaHa, color = Color, fill = Color), 
             stat = "identity", width = 1) + 
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_continuous(expand = c(0,0), limits = c(0, 7000), 
                       position = "right", breaks = seq(1000, 7000, 1000),
                       labels = c("1000", rep("", 3), "5000", rep("", 2))) +
    scale_x_continuous(labels = labs, expand = c(0,0), breaks = c(1908, 2009)) +
    labs(x = "", y = "") + 
    theme(panel.background = element_rect(fill='black', colour='black'),
          panel.grid = element_blank()) +
    geom_smooth(aes(x = Year, y = SumAreaHa), method = "loess", 
                se = F, colour = "red") +
    theme(axis.text.x = element_text(size = 25, color = c("#ff8d00",  "#ffff97")), 
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 15, color = "grey",
                                     hjust = -0.1)) +
    geom_point(data = data, aes(Point, lovals), color = "white", 
               fill = "white", size = 2, pch = 21) + 
    geom_hline(yintercept = value, color = "white", size = 0.25) + 
    geom_vline(xintercept = n, color = "white", size = 0.25) 
  
  save_plot(paste0(n, "-area-logged-roads"), plot = p, width = 6, height = 4, csv = F)
   p 
}

for (i in 2006) {
  logplot(n = i)
}


ggplot(poly) + 
  geom_bar(aes(x = YearHarv, y = SumArea, color = Color, fill = Color), 
           stat = "identity", width = 1) + 
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 7000)) +
  theme_dark() + 
  labs(x = "", y = "") + 
  theme(panel.background = element_rect(fill='black', colour='black'),
        panel.grid = element_blank()) +
  geom_smooth(aes(x = YearHarv, y = SumArea), method = "loess", se = F, colour = "red") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
 