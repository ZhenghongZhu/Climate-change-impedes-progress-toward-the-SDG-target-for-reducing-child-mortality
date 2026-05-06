
##########################
load("Fig2&Exfig3.RData")

library(ggplot2)

ggplot() +
  geom_line(data = df_fut, aes(x = Year, y = Mean, colour = Group),size=2)+theme_classic()+
  geom_line(data = df_his, aes(x = Year, y = Mean),size=2,color="black")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black",size=2.5) +
  scale_colour_manual(values=c("#4DBBD5FF", "#F7D380FF","#F39B7FFF","#DC0000FF"),
                      name = " ",  
                      labels = c("SSP1-1.9", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"))+
  geom_ribbon(data = df_fut,aes(x = Year,ymax = Max, ymin = Min, fill = Group), alpha = 0.3,show.legend = FALSE, color = NA )+
  scale_fill_manual(values=c("#4DBBD5FF", "#F7D380FF","#F39B7FFF","#DC0000FF"))+
  labs(title="")+
  ylab("Level of Warming(°C)")+
  scale_x_continuous(breaks=c(1990,2019,2030,2060,2090),expand = c(0,0))+
  scale_y_continuous(breaks=c(0,1,3,5,7),,expand = c(0,0),limits = c(-1,7))+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=15)))+
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 15, color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 15),
        axis.ticks.y = element_line(size = 1, color = "black", lineend = "round"),
        axis.ticks.length.y = unit(0.5, "cm"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.ticks.x = element_line(size = 1, color = "black", lineend = "round"),
        axis.ticks.length.x = unit(0.5, "cm"),
        axis.line.x = element_line(color = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(family = "serif",size = 15),
        legend.text = element_text(family = "serif",size=15),
        legend.position = "right", 
        legend.justification = c(0, 1), 
        legend.direction = "vertical", 
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_text(
          size = 15, 
          color = "black",
          margin = margin(t = 30, r = 0, b = 0, l = 0)  
        ),
        axis.title.y = element_text(
          size = 15, 
          color = "black",
          margin = margin(t = 0, r = 30, b = 0, l = 0) 
        ))




###FIG2b
color <- c( "#FEE0D2FF", "#FCBBA1FF",  "#EF3B2CFF", "#A50F15FF", "#67000DFF")

a$value <- ifelse(a$heat_his<=10000,"a",ifelse(a$heat_his<=100000,"b",
          ifelse(a$heat_his<=1000000,"c",ifelse(a$heat_his<=10000000,"d","e"))))

ggplot()+geom_tile(data=a,aes(x=lon,y=lat,fill=value))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = color,)+
  labs(title = "Historical (1990-2019)",fill="Average heat exposure (person-days)") +
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  theme(plot.title = element_text(hjust = 0.5,family = "serif"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.position = "none") +
  coord_cartesian(xlim=c(-20,105),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)


###FIG2c
###An example for SSP1-1.9 in the year 2030

a$heat_SSP119_2030 <- a$heat_SSP119_2030-a$heat_his

a$value <- ifelse(a$heat_SSP119_2030<=0,"b",
                   ifelse(a$heat_SSP119_2030<=5000,"c",ifelse(a$heat_SSP119_2030<=50000,"d",
                                                       ifelse(a$heat_SSP119_2030<=500000,"f","g"))))

ggplot()+geom_tile(data=a,aes(x=lon,y=lat,fill=value))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = color)+
  labs(title = " ",fill=" ") +
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  theme(plot.title = element_text(hjust = 0.5,family = "serif"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.position = "none", 
        panel.border = element_rect(
          color = "black", 
          fill = NA, 
          size = 2  
        )) +
  coord_cartesian(xlim=c(-20,105),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)



###Extended Data Fig. 3a
color <- c("#F7FBFFFF", "#DEEBF7FF", "#C6DBEFFF", "#4292C6FF",  "#08306BFF")



b$value <- ifelse(b$cold_his<=10000,"a",ifelse(b$cold_his<=100000,"b",
           ifelse(b$cold_his<=1000000,"c",ifelse(b$cold_his<=10000000,"d","e"))))

ggplot()+geom_tile(data=b,aes(x=lon,y=lat,fill=value))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = color)+
  labs(title = "Historical (1990-2019)",fill="Average cold exposure (person-days)") +
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  theme(plot.title = element_text(hjust = 0.5,family = "serif"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.position = "none") +
  coord_cartesian(xlim=c(-20,105),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)



###Extended Data Fig. 3b
###An example for SSP1-1.9 in the year 2030

b$cold_SSP119_2030 <- b$cold_SSP119_2030-b$cold_his

b$value <- ifelse(b$cold_SSP119_2030<= -100000,"b",
                  ifelse(b$cold_SSP119_2030<=0,"c",ifelse(b$cold_SSP119_2030<=1000000,"d",
                                                             ifelse(b$cold_SSP119_2030<=10000000,"f","g"))))

ggplot()+geom_tile(data=b,aes(x=lon,y=lat,fill=value))+
  theme_minimal()+
  geom_polygon(data=line1,aes(x=long,y=lat,group=group),col="grey",alpha=0,fill="white",size=0.5)+
  scale_fill_manual(values = color)+
  labs(title = " ",fill=" ") +
  scale_x_continuous(breaks=c(-25,0,25,50,75,100),labels = c("25°W","0°","25°E","50°E","75°E","100°E"))+
  scale_y_continuous(breaks=c(-20,0,20),labels = c("20°S","0°","20°N"))+
  theme(plot.title = element_text(hjust = 0.5,family = "serif"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        legend.text = element_text(family = "serif"),
        legend.position = "none", 
        panel.border = element_rect(
          color = "black", 
          fill = NA, 
          size = 2  
        )) +
  coord_cartesian(xlim=c(-20,105),ylim=c(-35,35))+
  xlab(NULL)+
  ylab(NULL)





