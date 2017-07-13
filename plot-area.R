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

pals <- c(rgb(227, 94, 0, maxColorValue = 255), rgb(255, 255, 0, maxColorValue = 255))

get_color <- colorRampPalette(pals)

labs <- c(1900, 2016)

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

newha <- read_csv('input/movie_drafts/updated_movie_hectares_070817.csv')
newha %<>% select(Year = `First Harvest Year`, SumAreaHa = Total)
newha %<>% mutate(Year = replace(Year, Year == 2017, NA)) %>%
  filter(!is.na(Year))
newha %<>% mutate(Color = get_color(116))

logplot <- function(data = comb, n = i) {
  data %<>% mutate(Point = ifelse(Year == n, n, NA),
                   lovals = predict(lm(SumAreaHa ~ poly(Year, 6),.)))
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
    geom_smooth(aes(x = Year, y = SumAreaHa), method = "lm", formula = y ~ poly(x, 6),  
                se = F, colour = "red") +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    geom_point(data = data, aes(Point, lovals), color = "white", 
               fill = "white", size = 2, pch = 21) + 
    geom_hline(yintercept = value, color = "white", size = 0.25) + 
    geom_vline(xintercept = n, color = "white", size = 0.25) 
  
  ggsave(paste0(i, ".png"), bg = "transparent", width = 6, 
         height = 3.5, dpi = 300, path = "input/data/output/plots/new-Poly6")
  
  # save_plot(paste0(n, "-area-logged-6"), plot = p, width = 6, height = 4, csv = F)
  #  p 
}

for (i in 1901:2016) {
  logplot(data = newha, n = i)
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
 