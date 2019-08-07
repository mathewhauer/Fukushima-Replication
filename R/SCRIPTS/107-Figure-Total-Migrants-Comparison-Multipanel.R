###------Total Migrants Multipanel-----
## @knitr Figure-Total-Migrants-Comparison-Multipanel-Map

migdat19 <- filter(regdat, X1 %in% c("Fukushima", "Iwate", "Miyagi")) %>%
  dplyr::select(origin, X1, num2010, num2011) %>%
  ungroup()%>%
  mutate(origin = str_upper(origin),
         origin = case_when(
           origin == "GIFU" ~ "GIHU",
           origin == "GUMMA" ~ "GUNMA",
           origin == "TOKYO-TO" ~ "TOKYO",
           origin == "KYOTO-FU" ~ "KYOTO",
           origin == "OSAKA-FU" ~ "OSAKA",
           TRUE ~ as.character(origin)
         )) 

fukushima19 <- filter(migdat19, X1 == "Fukushima")
iwate19 <- filter(migdat19, X1 == "Iwate")
miyagi19 <- filter(migdat19, X1 == "Miyagi")

japan <- shapefile("../R/DATA-RAW/japanprefecture/japan_prefecture_boundary")
japan_bb <- bb(japan, xlim=c(.15, .85), ylim=c(0.35, 1), relative = TRUE)

fukmap <- append_data(japan, fukushima19, key.shp= "NAM", key.data ="origin")
iwatemap <- append_data(japan, iwate19, key.shp= "NAM", key.data ="origin")
miyagimap <- append_data(japan, miyagi19, key.shp= "NAM", key.data ="origin")

fukmap <- st_as_sf(fukmap) %>%
  gather(key = Year, value = migrants, num2010:num2011) %>%
  mutate(Year = case_when(
    Year == "num2010" ~ "Fukushima 2010",
    Year == "num2011" ~ "Fukushima 2011",
    TRUE ~ as.character(Year)
  ))

iwatemap <- st_as_sf(iwatemap) %>%
  gather(key = Year, value = migrants, num2010:num2011) %>%
  mutate(Year = case_when(
    Year == "num2010" ~ "Iwate 2010",
    Year == "num2011" ~ "Iwate 2011",
    TRUE ~ as.character(Year)
  ))

miyagimap <- st_as_sf(miyagimap) %>%
  gather(key = Year, value = migrants, num2010:num2011) %>%
  mutate(Year = case_when(
    Year == "num2010" ~ "Miyagi 2010",
    Year == "num2011" ~ "Miyagi 2011",
    TRUE ~ as.character(Year)
  ))

fukmap$migrants2 <- factor(
  cut(fukmap$migrants, c(0,50,125,250,500,1500,8610)),
  labels = c("Under 50", "50 to 124", "125 to 249", "250 to 499", "500 to 1,499", "1,500 to 8,610")
)

iwatemap$migrants2 <- factor(
  cut(iwatemap$migrants, c(0,20,40,100,250,900,4631)),
  labels = c("Under 20", "20 to 39", "40 to 99", "100 to 249", "250 to 899", "900 to 4,631")
)

miyagimap$migrants2 <- factor(
  cut(miyagimap$migrants, c(0,75,150,300,850,2500,9068)),
  labels = c("Under 75", "75 to 149", "150 to 299", "300 to 849", "850 to 2,499", "2,500 to 9,068")
)

fukdattext <- data.frame(
  Year = c("Fukushima 2010", "Fukushima 2011"),
  label = c(prettyNum(sum(fukmap$migrants[which(fukmap$Year == "Fukushima 2010")], na.rm=T), big.mark = ","),
            prettyNum(sum(fukmap$migrants[which(fukmap$Year == "Fukushima 2011")], na.rm=T), big.mark = ","))
)

iwadattext <- data.frame(
  Year = c("Iwate 2010", "Iwate 2011"),
  label = c(prettyNum(sum(iwatemap$migrants[which(iwatemap$Year == "Iwate 2010")], na.rm=T), big.mark = ","),
            prettyNum(sum(iwatemap$migrants[which(iwatemap$Year == "Iwate 2011")], na.rm=T), big.mark = ","))
)

miydattext <- data.frame(
  Year = c("Miyagi 2010", "Miyagi 2011"),
  label = c(prettyNum(sum(miyagimap$migrants[which(miyagimap$Year == "Miyagi 2010")], na.rm=T), big.mark = ","),
            prettyNum(sum(miyagimap$migrants[which(miyagimap$Year == "Miyagi 2011")], na.rm=T), big.mark = ","))
)


mapfunc <- function(df, text){
  ggplot(data=df) +
    geom_sf(aes(fill = migrants2), color = "white", lwd = 1/2) +
    coord_sf(ylim = c(30,46), xlim = c(127,150), expand = FALSE, datum = NA) +
    facet_wrap(~Year, ncol=3) +
    viridis::scale_fill_viridis("Migrants",
                                # option = "cividis", 
                                direction = -1,
                                discrete = TRUE, na.value="gray") +
    geom_text(data = text, mapping = aes(130, 45, label = label)) +
    theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()
    )+
    NULL
}

a19 <- mapfunc(fukmap, fukdattext)
b19 <- mapfunc(iwatemap, iwadattext)
c19 <- mapfunc(miyagimap, miydattext)

plot_grid(a19,b19,c19,ncol=1, align = "vh")