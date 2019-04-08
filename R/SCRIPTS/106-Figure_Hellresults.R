###------Hellinger Figure-----
## @knitr HellFig

# Making the data for the Figure.
ridgedat <- KLdat %>%
  mutate(Prefecture = case_when(
    id == "Fukushima" ~ "Fukushima",
    id == "Iwate" ~ "Iwate",
    id == "Miyagi" ~ "Miyagi",
    id == "Evacuees" ~ "Evacuees",
    TRUE ~ "All Prefectures"))

ggplot(data=ridgedat, aes(x= distance)) +
  geom_density() +
  geom_rug(color = "gray80") +
  geom_rug(data = filter(ridgedat, Prefecture %in% c("Fukushima", "Iwate", "Miyagi", "Evacuees")),
           aes(color = Prefecture)) +
  scale_color_brewer(palette="Set1") +
  scale_x_continuous(expand = c(0.0, 0),
                     # breaks = c(0.13, 0.41)
                     limits = c(0,0.4)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.3))+
  labs(x = "Hellinger Distance")