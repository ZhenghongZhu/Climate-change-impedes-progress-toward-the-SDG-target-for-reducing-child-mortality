

load("Exfig7.RData")

#######An example for U5MR under SSP1-1.9 in West Africa

library(ggplot2)

aa <- subset(d_U5MR,region=="West Africa"&SSP==119)


ggplot() +
  geom_line(data = aa, aes(x = variable, y = value, colour = adapt),size=4)+
  theme_classic()+
  geom_hline(yintercept = 25, linetype = "dashed", color = "black",size=4) +
  scale_colour_manual(values=c("#DC0000FF","#F39B7FFF","#E9AA52FF","#00A087FF"),
                      name = " ",  
                      labels = c("0%", "10%", "50%", "90%"))+
  labs(title = " ",fill=" ") +
  ylab("")+
  xlab(" ")+
  scale_x_continuous(breaks=c(2030,2060,2090),expand = c(0,0),labels = c(2030,2060,2090))+
  scale_y_log10(breaks=c(25))+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=18)))+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 40), 
        axis.title = element_text(size = 40, color = "black"),
        axis.text = element_text(size = 40, color = "black"),
        plot.subtitle = element_text(size = 40),
        axis.ticks.y = element_line(size = 4, color = "black", lineend = "round"),
        axis.ticks.length.y = unit(0.8, "cm"),
        axis.line.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_line(size = 4, color = "black", lineend = "round"),
        axis.ticks.length.x = unit(0.8, "cm"),
        axis.line.x = element_line(color = "black", size = 4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(family = "serif",size = 40),
        legend.text = element_text(family = "serif",size=55),
        legend.position = "none",
        axis.title.x = element_text(
          size = 40, 
          color = "black",
          margin = margin(t = 0, r = 0, b = 0, l = 0) 
        ),
        axis.title.y = element_text(
          size = 40, 
          color = "black",
          margin = margin(t = 0, r = 0, b = 0, l = 0)  
        ))

