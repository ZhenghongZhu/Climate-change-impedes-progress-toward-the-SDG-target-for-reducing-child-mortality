
load("Fig4.RData")


##An example for U5MR in SSP585
w1 <- subset(U5MR_net,SSP==585)
w2 <- subset(U5MR_driver,SSP==585)


ggplot() +
  theme_minimal()+
  geom_bar(data = w2, aes(x = year, y = value, fill = name), stat = "identity", position = "stack",width = 8) +
  geom_bar(data = w1, aes(x = year, y = value), fill = "grey50", stat = "identity",width = 8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black",size=1) +
  scale_fill_manual(values = c("pp" = "#00A087FF", "he" = "#E64B35FF", "co" = "#4DBBD5FF","de"="#F39B7FFF")) +
  labs(title = " ",x = " ",y = " ",fill = " ")+
  scale_x_continuous(breaks = c(14,34,54,84,104,124),labels = c("SA", "SEA", "WA","SA", "SEA", "WA"))+
  scale_y_continuous(breaks = c(-40,-20,0,15),expand = c(0,0),limits = c(-40,15))+
  theme(
    text = element_text(family = "serif"),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 20, color = "black"),
    axis.text.x = element_text(angle = 30),
    axis.text = element_text(size = 18, color = "black"),
    axis.ticks.y = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.line.y = element_line(color = "black", size = 1),
    axis.ticks.x = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(color = "black", size = 1)
  ) + 
  geom_segment(
    aes(x = 6, xend = 62, y = 11, yend = 11), 
    color = "black", size = 1, lineend = "butt"
  ) +
  geom_segment(
    aes(x = 6, xend = 6, y = 11, yend = 9),  
    color = "black", size = 1
  ) +
  geom_segment(
    aes(x = 62, xend = 62, y = 11, yend = 9), 
    color = "black", size = 1
  ) +
  geom_text(
    aes(x = 34, y = 14, label = "2060 vs. 2030"),  
    color = "black", size = 6, fontface = "bold",family="serif"
  ) + 
  geom_segment(
    aes(x = 76, xend = 134, y = 11, yend = 11), 
    color = "black", size = 1, lineend = "butt"
  ) +
  geom_segment(
    aes(x = 76, xend = 76, y = 11, yend = 9),  
    color = "black", size = 1
  ) +
  geom_segment(
    aes(x = 134, xend = 134, y = 11, yend = 9),  
    color = "black", size = 1
  ) +
  geom_text(
    aes(x = 105, y = 14, label = "2090 vs. 2060"),  
    color = "black", size = 6, fontface = "bold",family="serif"
  ) 



##An example for NMR in SSP585
w1 <- subset(NMR_net,SSP==585)
w2 <- subset(NMR_driver,SSP==585)

ggplot() +
  theme_minimal()+
  geom_bar(data = w2, aes(x = year, y = value, fill = name), stat = "identity", position = "stack",width = 8) +
  geom_bar(data = w1, aes(x = year, y = value), fill = "grey50", stat = "identity",width = 8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black",size=1) +
  scale_fill_manual(values = c("pp" = "#00A087FF", "he" = "#E64B35FF", "co" = "#4DBBD5FF","de"="#F39B7FFF")) +
  labs(title = " ",x = " ",y = " ",fill = " ")+
  scale_x_continuous(breaks = c(14,34,54,84,104,124),labels = c("SA", "SEA", "WA","SA", "SEA", "WA"))+
  scale_y_continuous(breaks = c(-10,-5,0,5),expand = c(0,0),limits = c(-10,5))+
  theme(
    text = element_text(family = "serif"),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 20, color = "black"),
    axis.text.x = element_text(angle = 30),
    axis.text = element_text(size = 18, color = "black"),
    axis.ticks.y = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.line.y = element_line(color = "black", size = 1),
    axis.ticks.x = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(color = "black", size = 1)
  ) + 
  geom_segment(
    aes(x = 6, xend = 62, y = 4.2, yend = 4.2), 
    color = "black", size = 1, lineend = "butt"
  ) +
  geom_segment(
    aes(x = 6, xend = 6, y = 4.2, yend = 3.7), 
    color = "black", size = 1
  ) +
  geom_segment(
    aes(x = 62, xend = 62, y = 4.2, yend = 3.7),  
    color = "black", size = 1
  ) +
  geom_text(
    aes(x = 34, y = 4.7, label = "2060 vs. 2030"), 
    color = "black", size = 6, fontface = "bold",family="serif"
  ) + 
  geom_segment(
    aes(x = 76, xend = 134, y = 4.2, yend = 4.2),  
    color = "black", size = 1, lineend = "butt"
  ) +
  geom_segment(
    aes(x = 76, xend = 76, y = 4.2, yend = 3.7), 
    color = "black", size = 1
  ) +
  geom_segment(
    aes(x = 134, xend = 134, y = 4.2, yend = 3.7),  
    color = "black", size = 1
  ) +
  geom_text(
    aes(x = 105, y = 4.7, label = "2090 vs. 2060"),  
    color = "black", size = 6, fontface = "bold",family="serif"
  ) 









