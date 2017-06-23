source('header.R')

set_sub("final")

load_datas()

poly <- log17Poly
road <- log17Road

set_sub("logarea")

get_color <- colorRampPalette(c("#ff8d00", "#ffff97"))

labs <- c(1901, 2016)

poly %<>% mutate(YearHarv = replace(YearHarv, YearHarv == 2017, NA)) %>%
  filter(!is.na(YearHarv))
poly %<>% mutate(Area = as.numeric(st_area(.))/10000) %>%
  group_by(YearHarv) %>%
  summarise(SumArea = sum(Area))
poly %<>% mutate(Color = get_color(116))

road %<>% mutate(Area = st_area(st_buffer(., 50)))

logplot <- function(data = poly, n = i) {
  data %<>% mutate(Point = ifelse(YearHarv == n, n, NA),
                   lovals = predict(loess(SumArea ~ YearHarv,.)))
  val <- filter(data, YearHarv == n)
  value <- val$lovals
  
  p <- ggplot(data) + 
    geom_bar(aes(x = YearHarv, y = SumArea, color = Color, fill = Color), 
             stat = "identity", width = 1) + 
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_continuous(expand = c(0,0), limits = c(0, 7000), 
                       position = "right", breaks = seq(1000, 7000, 1000),
                       labels = c("1000", rep("", 3), "5000", rep("", 2))) +
    scale_x_continuous(labels = labs, expand = c(0,0), breaks = c(1906, 2011)) +
    labs(x = "", y = "") + 
    theme(panel.background = element_rect(fill='black', colour='black'),
          panel.grid = element_blank()) +
    geom_smooth(aes(x = YearHarv, y = SumArea), method = "loess", se = F, colour = "red") +
    theme(axis.text.x = element_text(size = 50, color = c("#ff8d00",  "#ffff97")), 
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 30, color = "grey",
                                     hjust = -0.1)) +
    geom_point(data = data, aes(Point, lovals), color = "white", 
               fill = "white", size = 5, pch = 21) + 
    geom_hline(yintercept = value, color = "white", size = 0.3) + 
    geom_vline(xintercept = n, color = "white", size = 0.3) 
  
  save_plot(paste0(n, "-area-logged"), plot = p)
   p 
}

for (i in 2014:2016) {
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
 