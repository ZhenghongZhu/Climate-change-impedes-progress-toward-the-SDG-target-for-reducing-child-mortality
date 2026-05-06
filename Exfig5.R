
load("Exfig5.RData")

###Extended Data Fig. 5a

library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

d <- subset(a_U5MR,ssp==245)
d$region <- factor(d$region, levels = c("South Asia","Eestern&Southern Africa","West Africa"))


calculate_proportions <- function(data, year_column) {
  data %>%
    group_by(region) %>%
    summarize(
      proportion_above_25 = round(sum(get(year_column) > 25) / n() * 100, 1),
      proportion_below_25 = round(sum(get(year_column) < 25) / n() * 100, 1)
    ) %>%
    pivot_longer(cols = c(proportion_above_25, proportion_below_25), names_to = "category", values_to = "proportion")
}


proportions_2030 <- calculate_proportions(d, "2030")
proportions_2060 <- calculate_proportions(d, "2060")
proportions_2090 <- calculate_proportions(d, "2090")


all_proportions <- rbind(
  cbind(proportions_2030, year = "2030"),
  cbind(proportions_2060, year = "2060"),
  cbind(proportions_2090, year = "2090")
)


category_order <- c("proportion_above_25", "proportion_below_25")

region_labels <- c(
  "South Asia" = "SA",
  "Eestern&Southern Africa" = "SEA",
  "West Africa" = "WA"
)

all_proportions1 <- subset(all_proportions,all_proportions$year=='2030')

all_proportions1$year  <-  "Proportion (%)"


p1 <- ggplot(all_proportions1, aes(x = region, y = proportion, fill = factor(category, levels = category_order))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~year, nrow = 1) +
  scale_fill_discrete(name = "SDG3.2.1 Goal Achievement", labels = c("No (%)","Yes (%)")) +
  scale_x_discrete(labels = region_labels, name = " ") + 
  theme_minimal() +
  guides(fill = guide_legend(title.position = "top")) +
  geom_text(
    aes(label = ifelse(proportion != 0, sprintf("%.1f", proportion), "")), 
    position = position_stack(vjust = 0.5), 
    family = "serif",
    size = 18
  )+
  theme(
    # 显示 Y 轴标签和标题
    axis.text.x = element_text(color = "black",family = "serif",size=30,angle = 45), 
    axis.title.x = element_text(family = "serif"),
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(),  
    text = element_text(family = "serif"), 
    strip.text = element_text(size=45, face = "bold"),
    legend.text = element_text(size=30, family = "serif"), 
    legend.title = element_text(size=30, family = "serif"),
    legend.position = "none",
    legend.box.spacing = unit(0, "pt")
  )


d <- d[order(d$`2030`),]

blank_rows <- data.frame(matrix(ncol = ncol(d), nrow = 18))
colnames(blank_rows) <- colnames(d)
d <- rbind(d, blank_rows)
d$id <- seq(1, nrow(d))
d$angle <- 90 - 360 * (d$id-0.5) /72

empty_bar <- 0
base_data <- d %>% 
  group_by(region) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
base_data <- base_data[1:3,]


d$goal <- ifelse(d$`2030`>=25,"No","Yes")

d$length <- c(37:108)

p2 <- ggplot(d) +
  geom_bar(aes(x = as.factor(id), y = `2030`,fill=goal), stat = "identity") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  geom_segment(aes(x = 1, y = 25, xend = 54, yend = 25), 
               linetype = "dashed", color = "black", size = 1)+
  theme_minimal() +
  theme(
    # 显示 Y 轴标签和标题
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),     plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    
    legend.position = "none"
  ) +
  ylim(c(-35, 120))+
  coord_polar(start = 0) + 
  geom_text(data = d, aes(x = id, y = length, label = country, hjust = 0), color = "black", 
            size = 9, angle = d$angle, inherit.aes = FALSE, family = "serif") 

pa <- ggdraw() +
  draw_plot(p2) +  
  draw_plot(p1, x = 0.15, y = 0.47, width = 0.35, height = 0.35) 


###Extended Data Fig. 5b

d <- subset(a_NMR,ssp==245)
d$region <- factor(d$region, levels = c("South Asia","Eestern&Southern Africa","West Africa"))


calculate_proportions <- function(data, year_column) {
  data %>%
    group_by(region) %>%
    summarize(
      proportion_above_12 = round(sum(get(year_column) > 12) / n() * 100, 1),
      proportion_below_12 = round(sum(get(year_column) < 12) / n() * 100, 1)
    ) %>%
    pivot_longer(cols = c(proportion_above_12, proportion_below_12), names_to = "category", values_to = "proportion")
}

proportions_2030 <- calculate_proportions(d, "2030")
proportions_2060 <- calculate_proportions(d, "2060")
proportions_2090 <- calculate_proportions(d, "2090")

all_proportions <- rbind(
  cbind(proportions_2030, year = "2030"),
  cbind(proportions_2060, year = "2060"),
  cbind(proportions_2090, year = "2090")
)

category_order <- c("proportion_above_12", "proportion_below_12")

region_labels <- c(
  "South Asia" = "SA",
  "Eestern&Southern Africa" = "SEA",
  "West Africa" = "WA"
)

all_proportions1 <- subset(all_proportions,all_proportions$year=='2030')

all_proportions1$year  <-  "Proportion (%)"


p1 <- ggplot(all_proportions1, aes(x = region, y = proportion, fill = factor(category, levels = category_order))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~year, nrow = 1) +
  scale_fill_discrete(name = "SDG3.2.1 Goal Achievement", labels = c("No (%)","Yes (%)")) + 
  scale_x_discrete(labels = region_labels, name = " ") + 
  theme_minimal() +
  guides(fill = guide_legend(title.position = "top")) +
  geom_text(
    aes(label = ifelse(proportion != 0, sprintf("%.1f", proportion), "")), 
    position = position_stack(vjust = 0.5), 
    family = "serif",
    size = 18
  )+
  theme(
    axis.text.x = element_text(color = "black",family = "serif",size=30,angle = 45), 
    axis.title.x = element_text(family = "serif"), 
    axis.text.y = element_blank(),  
    axis.title.y = element_blank(), 
    text = element_text(family = "serif"),  
    strip.text = element_text(size=45, face = "bold"),
    legend.text = element_text(size=30, family = "serif"), 
    legend.title = element_text(size=30, family = "serif"),
    legend.position = "none",
    legend.box.spacing = unit(0, "pt")
  )



d <- d[order(d$`2030`),]
blank_rows <- data.frame(matrix(ncol = ncol(d), nrow = 18))
colnames(blank_rows) <- colnames(d)
d <- rbind(d, blank_rows)
d$id <- seq(1, nrow(d))
d$angle <- 90 - 360 * (d$id-0.5) /72

d$goal <- ifelse(d$`2030`>=12,"No","Yes")

d$length <- seq(10.5,46,0.5)


p2 <- ggplot(d) +
  geom_bar(aes(x = as.factor(id), y = `2030`,fill=goal), stat = "identity") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  geom_segment(aes(x = 1, y = 12, xend = 54, yend = 12), 
               linetype = "dotted", color = "black", size = 1)+
  theme_minimal() +
  theme(
    # 显示 Y 轴标签和标题
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),     plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    
    legend.position = "none"
  ) +
  ylim(c(-25, 75))+
  coord_polar(start = 0) + 
  geom_text(data = d, aes(x = id, y = length, label = country, hjust = 0), color = "black", 
            size = 9, angle = d$angle, inherit.aes = FALSE, family = "serif") 


pb <- ggdraw() +
  draw_plot(p2) +  
  draw_plot(p1, x = 0.15, y = 0.47, width = 0.35, height = 0.35) 








