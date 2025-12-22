library(dplyr)
library(fixest)
library(splines)

### (1) Main Analysis
## load data
load("Sensitivity Analysis.RData")
data.all <- chr_data   ###chr_data is for child death, neo_data is for neonatal death, 
                       ###heat is day counts for temp > 90th, cold is day counts for temp < 10th,

FE.table$combination <- gsub("ZCTA","CentreId",FE.table$combination)
FE.table <- FE.table[!grepl("week", FE.table$combination, ignore.case = FALSE), ]
FE.table <- FE.table[-8,]
FE.table$Model_index <- 1:19
# 

## prepare models
FEs <- FE.table$combination

table <- data.frame(model = c(paste0("Model ",1:length(FEs))),
                    category = FE.table$category,
                    FE = FEs)

formulas <- c(paste0("deaths~ heat + cold+ns(rh, df=3)|",
                     FEs))

## run each model
for (i in 1:length(formulas)){
  print(i)
  data <- data.all
  
  formula.i <- formulas[i]
  
  model = feglm(as.formula(formula.i), 
                data = data,
                vcov = "iid",
                offset = log(o$birth),
                family = "quasipoisson")
  
  coef <- model$coefficients[1]
  SE <- model$se[1]
  P <- summary(model)$coeftable[1,4]
  FE.size <- sum(model$fixef_sizes)
  
  table[i,"coef"] <- coef
  table[i,"SE"] <- SE
  table[i,"P"] <- P
  table[i,"FE.size"] <- FE.size
  
}

table <- table %>%
  mutate(pct = (exp(coef) - 1) * 100,
         pct_lower = (exp(coef - 1.96*SE) - 1) * 100,
         pct_upper = (exp(coef + 1.96*SE) - 1) * 100)


### (2) Permutation tests
library(foreach)
library(doParallel)

## Temporal permutation
n <- 1000
for (i in 1:length(formulas)){
  print(i)
  formula.i <- formulas[i]
  model.name <- table[i,"model"]
  
  simulation.coefs <- foreach(
    j = 1:n, 
    .combine = 'c',
    .packages = c("fixest","dplyr","splines")
  ) %dopar% {
    data <- data.all
    
    data <- data %>%
      group_by(CentreId) %>%
      mutate(heat = sample(heat))
    
    model = feglm(as.formula(formula.i), 
                  data = data,
                  vcov = "iid",
                  family = "quasipoisson")
    
    model$coefficients[1]
  }
  
  temporal.table <- data.frame(n=1:n,
                               coef = simulation.coefs)
  
  saveRDS(temporal.table, paste0("Randomization_temporal_",model.name,".RDS"))
  
}


## Spatial permutation
n <- 1000
for (i in 1:length(formulas)){
  print(i)
  formula.i <- formulas[i]
  model.name <- table[i,"model"]
  
  simulation.coefs <- foreach(
    j = 1:n, 
    .combine = 'c',
    .packages = c("fixest","dplyr","splines")
  ) %dopar% {
    data <- data.all
    
    data <- data %>%
      group_by(month) %>%
      mutate(heat = sample(heat))
    
    model = feglm(as.formula(formula.i), 
                  data = data,
                  vcov = "iid",
                  family = "quasipoisson")
    
    model$coefficients[1]
  }
  
  spatial.table <- data.frame(n=1:n,
                              coef = simulation.coefs)
  
  saveRDS(spatial.table, paste0("Randomization_spatial_",model.name,".RDS"))
  
}


### (3) Different SE calculations
VCOVs <- c( "iid", "hetero","cluster by ZCTA", "cluster by county", "twoway")

table.SE <- data.frame(VCOV = VCOVs,
                       model = "Model 38",
                       FE = "week + COUNTY^year + ZCTA^month")

### (2) run each model
for (i in 1:length(VCOVs)){
  print(VCOVs[i])
  data <- data.all
  
  if(VCOVs[i] == "cluster by ZCTA"){
    model = feglm(deaths~ heat + cold+ns(rh, df=3)| week + COUNTY^year+ZCTA^month, 
                  data = data,
                  cluster = data$ZCTA,
                  family = "quasipoisson")
  }else if (VCOVs[i] == "cluster by county"){
    model = feglm(deaths~ heat + cold+ns(rh, df=3)| week + COUNTY^year+ZCTA^month, 
                  data = data,
                  cluster = data$COUNTY,
                  family = "quasipoisson")
  }else if (VCOVs[i] == "twoway"){
    model = feglm(deaths~ heat + cold+ns(rh, df=3)| week + COUNTY^year+ZCTA^month, 
                  data = data,
                  cluster = c("week", "ZCTA"),
                  family = "quasipoisson")
  }else{
    model = feglm(deaths~ heat + cold+ns(rh, df=3)| week + COUNTY^year+ZCTA^month, 
                  data = data,
                  vcov = VCOVs[i],
                  family = "quasipoisson")
  }
  
  coef <- model$coefficients[1]
  SE <- model$se[1]
  P <- summary(model)$coeftable.SE[1,4]
  
  table.SE[i,"coef"] <- coef
  table.SE[i,"SE"] <- SE
  table.SE[i,"P"] <- P
}


medians <- numeric(19)

# 循环从1到19
for (i in 1:19) {
  # 构建文件名，注意空格处理与原格式一致
  filename <- paste0("Randomization_temporal_Model ", i, ".RDS")
  
  # 读取文件
  d <- readRDS(filename)
  
  # 计算coef列的中位数并存储
  medians[i] <- median(d$coef)
  
  hist(d$coef, 
       main = paste0("Model ", i),  # 标题包含模型编号
       xlab = "Coef",  # X轴名称，可根据需要修改
       family = "serif") 
  abline(v = 0, lty = 1, col = "red", lwd = 2.5)
  
  # 可选：打印当前进度
  cat("Model", i, "的中位数：", medians[i], "\n")
}


medians <- numeric(19)

# 循环从1到19
for (i in 1:19) {
  # 构建文件名，注意空格处理与原格式一致
  filename <- paste0("Randomization_spatial_Model ", i, ".RDS")
  
  # 读取文件
  d <- readRDS(filename)
  
  # 计算coef列的中位数并存储
  medians[i] <- median(d$coef)
  hist(d$coef, main = paste0("Model ", i))
  # 可选：打印当前进度
  cat("Model", i, "的中位数：", medians[i], "\n")
}



###effect visualization
library(gridExtra)
library(ggplot2)
# data from table.SE
data <- data.frame(
  pct = c(-1.4, 1.7, 0.8, 4.5, -1.3, -4.4, -7.2, -7.6, -7.4, 0.0, 
          -2.6, -1.5, -4.5, -7.9, -2.4, -7.0, 1.2, -7.0, 0.8),
  pct_lower = c(-7.5, -4.6, -5.4, -2.0, -7.3, -10.5, -13.0, -13.4, -13.3, 0,
                -11.8, -7.5, -10.6, -13.7, -10.3, -12.9, -5.0, -12.9, -5.4),
  pct_upper = c(5.0, 8.5, 7.4, 11.4, 5.1, 2.1, -1.0, -1.5, -1.2, 0,
                7.6, 4.9, 2.0, -1.7, 6.1, -0.7, 7.8, -0.8, 7.4)
)


data <- data %>%
  mutate(
    group = factor(1:nrow(data), 
                   levels = 1:nrow(data),  
                   labels = paste0("Model ", 1:19))  
  )

data$x <- c(1,1,2,1,2,1,1,1,1,2,1,2,1,1,1,1,1,1,2)
data$x <- as.character(data$x)

ggplot(data, aes(x = pct, y = group,color =x)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8) +  # 无效线
  geom_vline(xintercept = 7.5, linetype = "dotted", color = "black", linewidth = 0.8) +  # +7.5线
  geom_vline(xintercept = -7.5, linetype = "dotted", color = "black", linewidth = 0.8) +  # -7.5线
  geom_errorbarh(aes(xmin = pct_lower, xmax = pct_upper), 
                 height = 0.2) +  
  geom_point( size = 3) +  
  coord_flip() +
  scale_color_manual(values = c("1" = "steelblue", "2" = "indianred")) +
  labs(
    title = " ",
    x = "Difference in estimated percent change (%)",
    y = " "
  ) +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        axis.title = element_text( color = "black"),
        axis.text.x = element_text( angle = 45,color = "black"),
        axis.text.y = element_text( color = "black"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "none"
  )




### spatial_temporal Analysis Visualization
plot_list <- list()
for (i in 1:19) {

  filename <- paste0("Randomization_spatial_Model ", i, ".RDS")###paste0("Randomization_temporal_Model ", i, ".RDS")
  

  d <- readRDS(filename)
  

  p <- ggplot(d, aes(x = coef)) +
    geom_histogram(color = "black", fill = "white", bins = 30) +  
    geom_vline(xintercept = 0, color = "red", linetype = 1, linewidth = 1.5) +  
    ggtitle(paste0("Model ", i)) +  
    xlab("Coef") + 
    ylab("Frequency") + 
    theme_bw() +
    theme(
      text = element_text(family = "serif"), 
      plot.title = element_text(hjust = 0.5, size = 10), 
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9)
    )
  

  plot_list[[i]] <- p
  

  cat("Model", i, "_median：", medians[i], "\n")
}

plot_list[[20]] <- ggplot() + 
  theme_void()  
grid_plot <- grid.arrange(grobs = plot_list, ncol = 4, nrow = 5)

















