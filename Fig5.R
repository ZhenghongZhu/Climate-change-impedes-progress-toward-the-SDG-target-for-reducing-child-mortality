load("Fig5.RData")


####An example for U5MR in 2090

ggplot(d_U5MR_2090, aes(x = y, y = x)) +
  geom_tile(aes(fill = z), color = "white", linewidth = 0.5) +
  # 添加25分界的颜色标尺
  scale_fill_gradientn(
    colors = c("#FFF5F0FF",   "#EF3B2CFF"),
    values = scales::rescale(c(8,35)),
    name = "U5MR (per 1000 LB)"  # 修改图例名称
  ) +
  # 添加文本标签显示具体数值
  geom_text(aes(label = sprintf("%.1f", z)), size = 6,family="serif") +
  # 设置X轴和Y轴标签
  scale_y_discrete(labels = c("SSP1-1.9", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")) +
  scale_x_discrete(labels = c( "90%" , "50%","10%","0%")) +
  labs(
    title = " ",
    x = "Adaptation level",  # 去除X轴名称
    y = ""  # 设置Y轴名称
  ) +
  theme_minimal() +
  theme(
    # 设置所有字体为serif
    text = element_text(family = "serif"),
    axis.title = element_text(size=20, colour = "black"),
    axis.text.x = element_text(size=18,colour = "black"),
    axis.text.y = element_text(size=18,colour = "black"),
    plot.title = element_text(hjust = 0.5,size=20,),
    # 将图例置于下方
    legend.position = "bottom",
    # 可根据需要调整图例方向
    legend.direction = "horizontal",
    legend.text = element_text(size = 16, colour = "black"),  # 图例数值标签大小
    legend.title = element_text(
      size = 18, colour = "black")
  )












