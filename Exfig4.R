
load("Exfig4.RData")

#######An example for NMR under SSP1-1.9 in West Africa

d1 <- subset(d_NMR,region=="West Africa"&SSP==119)

library(ggplot2)

ggplot(d1) +
  geom_ribbon(aes(x = variable, ymin = l*100,ymax=u*100, group = effect, fill = effect),
              alpha = 0.5) +  
  geom_line(aes(x = variable, y = value*100, group = effect, color = effect,shape=effect),size =3) +
  geom_point(aes(x = variable, y = value*100, group = effect, color = effect,shape=effect),size = 6) +
  scale_fill_manual(
    name = " ",
    values = c("cold" = "steelblue", "heat" = "indianred")
  ) +
  scale_color_manual(
    name = " ",
    values = c("cold" = "steelblue", "heat" = "indianred")
  ) +
  scale_shape_manual(
    name = " ",
    values = c("cold" = 18, "heat" = 16)
  ) +
  labs(
    title = " ",
    x = " ",
    y = "West Africa",
    color = " "
  ) +
  scale_x_continuous(breaks=c(2030,2060,2090),expand = c(0,0))+
  scale_y_continuous(breaks=c(0,10,20,30),expand = c(0,0),limits = c(0,30))+
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


