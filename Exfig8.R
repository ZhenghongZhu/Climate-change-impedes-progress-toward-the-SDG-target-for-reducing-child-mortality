

load("Exfig8.RData")


#######An example for NMR under SSP1-1.9 in West Africa

d1 <- subset(d,d$region=="West Africa"&d$SSP==119)

library(ggplot2)

ggplot(d1, aes(x = variable, y = value, fill = group)) +
  geom_area(position = "stack", color = "white", linewidth = 0.5) +  
  labs(
    title = " ",
    x = " ",
    y = " ",
    color = " "
  ) +
  scale_x_continuous(breaks=c(2030,2060,2090),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 55, color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 55),
        axis.ticks.y = element_line(size = 3, color = "black", lineend = "round"),
        axis.ticks.length.y = unit(0.5, "cm"),
        axis.line.y = element_line(color = "black", size = 3),
        axis.ticks.x = element_line(size = 3, color = "black", lineend = "round"),
        axis.ticks.length.x = unit(0.5, "cm"),
        axis.line.x = element_line(color = "black", size = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(family = "serif",size = 55),
        legend.text = element_text(family = "serif",size=55),
        legend.position = "none", 
        plot.margin = margin(0, 2, 0, 0, "cm"),
        axis.title.x = element_text(
          size = 55, 
          color = "black",
          margin = margin(t = 30, r = 0, b = 0, l = 0)  
        ),
        axis.title.y = element_text(
          size = 55, 
          color = "black",
          margin = margin(t = 0, r = 30, b = 0, l = 0)  
        ),
        axis.text.x = element_text(margin = margin(t = 20)),
        axis.text.y = element_text(margin = margin(r = 20)))

















