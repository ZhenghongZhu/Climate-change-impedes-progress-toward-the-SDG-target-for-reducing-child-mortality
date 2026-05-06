###################################
load("Fig3.RData")

library(ggplot2)


####An example for U5MR in Southeast Africa

aa <- subset(a_U5MR,region=="Eastern&Southern Africa")
bb <- subset(b_U5MR,region=="Eastern&Southern Africa")



ggplot() +
  geom_line(data = aa, aes(x = variable, y = value, colour = data_name),size=2)+
  geom_line(data = bb, aes(x = variable, y = value,colour = data_name),size=2)+
  theme_classic()+
  geom_hline(yintercept = 15, linetype = "dashed", color = "black",size=2) +
  scale_colour_manual(values=c("#4DBBD5FF", "#F7D380FF","#F39B7FFF","#DC0000FF","black","grey"),
                      name = " ",  # 设置图例的名称
                      labels = c("SSP1-1.9", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5","Counterfactual","Factual"))+
  scale_fill_manual(values=c("#4DBBD5FF", "#F7D380FF","#F39B7FFF","#DC0000FF","black","grey"))+
  labs(title = "Southeast Africa",fill=" ") +
  ylab(" ")+
  xlab("Year")+
  scale_x_continuous(breaks=c(2000,2030,2060,2090),expand = c(0,0))+
  scale_y_continuous(breaks=c(25))+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=18)))+
  theme(text = element_text(family = "serif"),
        plot.title = element_text( size = 15), 
        axis.title = element_text(size = 15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        plot.subtitle = element_text(size = 15),
        axis.ticks.y = element_line(size = 2, color = "black", lineend = "round"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.x = element_line(size =2, color = "black", lineend = "round"),
        axis.ticks.length.x = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(family = "serif",size = 15),
        legend.text = element_text(family = "serif",size=15),
        legend.position = "none", plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        axis.title.x = element_text(
          size = 15, 
          color = "black",
          margin = margin(t = 10, r = 0, b = 0, l = 0) 
        ),
        axis.title.y = element_text(
          size = 15, 
          color = "black",
          margin = margin(t = 0, r = 10, b = 0, l = 0) 
        ),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(margin = margin(r = 10)),
        panel.border = element_rect(
          color = "black",  
          fill = NA,        
          size = 2.5         
        ))



##########
####An example for NMR in Southeast Africa

aa <- subset(a_NMR,region=="Eastern&Southern Africa")
bb <- subset(b_NMR,region=="Eastern&Southern Africa")


ggplot() +
  geom_line(data = aa, aes(x = variable, y = value, colour = data_name),size=4)+
  geom_line(data = bb, aes(x = variable, y = value,colour = data_name),size=4)+
  theme_classic()+
  geom_hline(yintercept = 12, linetype = "dashed", color = "black",size=4) +
  scale_colour_manual(values=c("#4DBBD5FF", "#F7D380FF","#F39B7FFF","#DC0000FF","black","grey"),
                      name = " ", 
                      labels = c("SSP1-1.9", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5","Counterfactual","Factual"))+
  scale_fill_manual(values=c("#4DBBD5FF", "#F7D380FF","#F39B7FFF","#DC0000FF","black","grey"))+
  labs(title = "Southeast Africa",fill=" ") +
  ylab(" ")+
  xlab("Year")+
  scale_x_continuous(breaks=c(2000,2030,2060,2090),expand = c(0,0))+
  scale_y_continuous(breaks=c(12))+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=18)))+
  theme(text = element_text(family = "serif"),
        plot.title = element_text( size = 15), 
        axis.title = element_text(size = 15, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        plot.subtitle = element_text(size = 15),
        axis.ticks.y = element_line(size = 2, color = "black", lineend = "round"),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.ticks.x = element_line(size =2, color = "black", lineend = "round"),
        axis.ticks.length.x = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(family = "serif",size = 15),
        legend.text = element_text(family = "serif",size=15),
        legend.position = "none", plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        axis.title.x = element_text(
          size = 15, 
          color = "black",
          margin = margin(t = 10, r = 0, b = 0, l = 0)  
        ),
        axis.title.y = element_text(
          size = 15, 
          color = "black",
          margin = margin(t = 0, r = 10, b = 0, l = 0)  
        ),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.text.y = element_text(margin = margin(r = 10)),
        panel.border = element_rect(
          color = "black", 
          fill = NA,        
          size = 2.5        
        ))


























