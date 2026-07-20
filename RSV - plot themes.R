# plot themes

library(showtext)
library(ggplot2)


font_add_google("Roboto Condensed", family = "roboto_c")

# test theme
theme_dth_1 <- 
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family = "roboto_c"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray80"),
        strip.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_rect(color = "gray80"),
        plot.subtitle = element_text(hjust=0.5)
  )

# map
theme_dth_maps <- 
  theme_void(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family = "roboto_c"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray80"),
        strip.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_rect(color = "gray80"),
        plot.subtitle = element_text(hjust=0.5)
  )
