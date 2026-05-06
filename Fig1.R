
library(mixmeta)
library(dlnm)
library(splines)
library(ggplot2)
library(dplyr)


load("Fig1_U5MR.RData")

# ---- Fig1a ----
# Subset daily data for South Asia
id_sel <- unique(data_month$CentreId)[c(1:4)]

ee <- data_day %>%
  filter(CentreId %in% id_sel)

# Define temperature range and create 1°C bins
min_temp <- floor(min(ee$value))
max_temp <- ceiling(max(ee$value))

ee <- ee %>%
  mutate(temperature_bin = cut(value, breaks = seq(min_temp, max_temp, by = 1)))

# Aggregate total deaths within each temperature bin
aggregated_data <- ee %>%
  group_by(temperature_bin) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop")

# Calculate midpoint of each temperature bin
aggregated_data$temperature_midpoint <- as.numeric(
  sapply(strsplit(as.character(aggregated_data$temperature_bin), ",|\\(|\\]"), function(x) {
    lower <- as.numeric(x[2])
    upper <- as.numeric(x[3])
    (lower + upper) / 2
  })
)

# Subset site-level data corresponding to selected centers
dd <- subset(data_site, data_site$study %in% unique(data_month$CentreId)[c(1:4)])

# Extract model_first_stage coefficients and covariance matrices
coef_list1 <- list()
vcov_list1 <- list()

k <- c(1:4)
for (i in c(1:4)) {
  coef_list1[[i]] <- model_first_stage[[k[i]]]$blup
  vcov_list1[[i]] <- model_first_stage[[k[i]]]$vcov
}

# Combine coefficients into matrix form
coef_matrix1 <- do.call(rbind, coef_list1)
vcov_list1 <- Filter(Negate(is.null), vcov_list1)

# Perform multivariate meta-analysis (REML)
meta_model_SA <- mixmeta(coef_matrix1, vcov_list1, method = "reml", data = dd)

print(summary(meta_model_SA), digits = 5)

# Define exposure (temperature) distribution
exposure_levels <- ee$value

# Specify spline function and knot placement (percentiles)
var_fun <- "ns"
var_prc <- c(0.1, 0.75, 0.9)
var_knots <- quantile(exposure_levels, var_prc, na.rm = TRUE)
boundary_knots <- range(exposure_levels, na.rm = TRUE)

# Define lag structure (up to 30 days)
max_lag <- 30
lag_knots <- logknots(max_lag, 2)

# Construct cross-basis matrix for DLNM
cross_basis <- crossbasis(
  exposure_levels,
  lag = c(0, max_lag),
  argvar = list(fun = var_fun, knots = var_knots, Boundary.knots = boundary_knots),
  arglag = list(knots = lag_knots)
)

# Define prediction grid
pred_prc <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))
at_temp <- seq(min(exposure_levels), max(exposure_levels), 0.01)

# Initial prediction using fixed centering temperature (25°C)
basis_predict <- crosspred(
  cross_basis, meta_model_SA,
  cen = 25, at = at_temp, model.link = "log"
)

# Extract prediction results
predicted_values <- basis_predict$allRRfit
ci_low <- basis_predict$allRRlow
ci_high <- basis_predict$allRRhigh
exposure <- as.numeric(rownames(basis_predict$matfit))

# Create plotting dataset
data_for_plot <- data.frame(
  Exposure = exposure,
  Predicted = predicted_values,
  CI_Low = ci_low,
  CI_High = ci_high
)

# Identify Minimum Mortality Temperature (MMT)
MMT <- data_for_plot$Exposure[which(data_for_plot$Predicted == min(data_for_plot$Predicted))]

# Recalculate predictions centered at MMT
basis_predict <- crosspred(
  cross_basis, meta_model_SA,
  cen = MMT, at = at_temp, model.link = "log"
)

# Extract updated prediction results
predicted_values <- basis_predict$allRRfit
ci_low <- basis_predict$allRRlow
ci_high <- basis_predict$allRRhigh
exposure <- as.numeric(rownames(basis_predict$matfit))

# Prepare final plotting dataset
data_for_plot1 <- data.frame(
  Exposure = exposure,
  Predicted = predicted_values,
  CI_Low = ci_low,
  CI_High = ci_high
)


data_for_plot1 <- subset(data_for_plot1, data_for_plot1$Exposure >= 14)

# Split data relative to MMT for visualization
data_for_plot1_less_than_ref <- data_for_plot1[data_for_plot1$Exposure < MMT, ]
data_for_plot1_greater_than_ref <- data_for_plot1[data_for_plot1$Exposure >= MMT, ]



ggplot() +
  geom_line(data = data_for_plot1_less_than_ref, aes(x = Exposure, y = Predicted), color = "steelblue", size = 1.2) +
  geom_line(data = data_for_plot1_greater_than_ref, aes(x = Exposure, y = Predicted), color = "#DC0000FF", size = 1.2) +
  geom_ribbon(data = data_for_plot1, aes(x = Exposure, ymin = CI_Low, ymax = CI_High),
              fill = "grey", alpha = 0.2) +
  labs(x = "Temperature (°C)", y = "RR for Under-5 Mortality",
       subtitle = "South Asia") +
  scale_x_continuous(breaks = seq(10,35,5))+
  scale_y_log10(expand = c(0, 0)) +
  geom_vline(xintercept = MMT, color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0.3, 10)) +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(size = 24),
    axis.ticks.y = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.y = element_line(color = "black", size = 1),
    axis.ticks.x = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(color = "black", size = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


aggregated_data <- subset(aggregated_data,aggregated_data$temperature_midpoint>=14)

ggplot() +
  geom_col(data = aggregated_data, aes(x = temperature_midpoint, y = total_deaths), fill = "pink", color = "black") +
  theme_minimal() +
  labs(x = " ", y = "Deaths (n)") +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(size = 24),
    legend.position = "none",
    axis.ticks.y.right = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.y.right = unit(0.2, "cm"),
    axis.line.y.right = element_line(color = "black", size = 1),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    position = "right",
    breaks = c(0, 200, 400),
    limits = c(0, 400),
    expand = c(0, 0)
  )

# ---- Fig1b ----
# Subset daily data for Southeast Africa
id_sel <- unique(data_month$CentreId)[c(9:11,13,14,19:22,27:33)]

ee <- data_day %>%
  filter(CentreId %in% id_sel)

dd <- data_site %>%
  filter(study %in% id_sel)


min_temp <- floor(min(ee$value))
max_temp <- ceiling(max(ee$value))

aggregated_data <- ee %>%
  mutate(temperature_bin = cut(value, breaks = seq(min_temp, max_temp, by = 1))) %>%
  group_by(temperature_bin) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop")

# Compute bin midpoints
aggregated_data$temperature_midpoint <- as.numeric(
  sapply(strsplit(as.character(aggregated_data$temperature_bin), ",|\\(|\\]"), function(x) {
    (as.numeric(x[2]) + as.numeric(x[3])) / 2
  })
)


coef_list1 <- list()
vcov_list1 <- list()

k <- c(9:11,13,14,19:22,27:33)

for (i in seq_along(k)) {
  coef_list1[[i]] <- model_first_stage[[k[i]]]$blup
  vcov_list1[[i]] <- model_first_stage[[k[i]]]$vcov
}

coef_matrix1 <- do.call(rbind, coef_list1)
vcov_list1 <- Filter(Negate(is.null), vcov_list1)


meta_model_ESA <- mixmeta(coef_matrix1, vcov_list1, method = "reml", data = dd)
print(summary(meta_model_ESA), digits = 5)


exposure_levels <- ee$value

var_knots <- quantile(exposure_levels, c(0.1, 0.75, 0.9), na.rm = TRUE)
boundary_knots <- range(exposure_levels, na.rm = TRUE)

max_lag <- 30
lag_knots <- logknots(max_lag, 2)

cross_basis <- crossbasis(
  exposure_levels,
  lag = c(0, max_lag),
  argvar = list(fun = "ns", knots = var_knots, Boundary.knots = boundary_knots),
  arglag = list(knots = lag_knots)
)

at_temp <- seq(min(exposure_levels), max(exposure_levels), 0.01)

quantile(exposure_levels, c(0.05, 0.95))
low  <- quantile(ee$value, 0.01)
high <- quantile(ee$value, 0.99)


basis_predict <- crosspred(cross_basis, meta_model_ESA, cen = 25,
                           at = at_temp, model.link = "log")

data_for_plot <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
)

MMT <- data_for_plot$Exposure[which.min(data_for_plot$Predicted)]


basis_predict <- crosspred(cross_basis, meta_model_ESA, cen = MMT,
                           at = at_temp, model.link = "log")

data_for_plot1 <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
)


data_for_plot1 <- subset(data_for_plot1, Exposure >= 10)


data_for_plot1_less_than_ref    <- data_for_plot1[data_for_plot1$Exposure <  MMT, ]
data_for_plot1_greater_than_ref <- data_for_plot1[data_for_plot1$Exposure >= MMT, ]



ggplot() +
  geom_line(data = data_for_plot1_less_than_ref, aes(x = Exposure, y = Predicted), color = "steelblue", size = 1.2) +
  geom_line(data = data_for_plot1_greater_than_ref, aes(x = Exposure, y = Predicted), color = "#DC0000FF", size = 1.2) +
  # 置信区间
  geom_ribbon(data = data_for_plot1, aes(x = Exposure, ymin = CI_Low, ymax = CI_High),
              fill = "grey", alpha = 0.2) +
  labs(x = "Temperature (°C)", y = " ",
       subtitle = "Eastern&Southern Africa") +
  scale_x_continuous(breaks = seq(10,35,5),limits = c(9.7,34.8))+
  scale_y_log10(expand = c(0, 0)) +
  geom_vline(xintercept = MMT, color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0.3, 10)) +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(size = 24),
    axis.ticks.y = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.y = element_line(color = "black", size = 1),
    axis.ticks.x = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(color = "black", size = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



aggregated_data <- subset(aggregated_data,aggregated_data$temperature_midpoint>=10)

ggplot() +
  geom_col(data = aggregated_data, aes(x = temperature_midpoint, y = total_deaths), fill = "pink", color = "black") +
  theme_minimal() +
  labs(x = " ", y = "Deaths (n)") +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(size = 24),
    legend.position = "none",
    axis.ticks.y.right = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.y.right = unit(0.2, "cm"),
    axis.line.y.right = element_line(color = "black", size = 1),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    position = "right",
    breaks = c(0,  1000,2000,3000),
    limits = c(0, 3000),
    expand = c(0, 0)
  )

# ---- Fig1c ----
# Subset daily data for West Africa
id_sel <- unique(data_month$CentreId)[c(5:8, 15:18, 23:26)]

ee <- data_day %>%
  filter(CentreId %in% id_sel)

dd <- data_site %>%
  filter(study %in% id_sel)


min_temp <- floor(min(ee$value))
max_temp <- ceiling(max(ee$value))

aggregated_data <- ee %>%
  mutate(temperature_bin = cut(value, breaks = seq(min_temp, max_temp, by = 1))) %>%
  group_by(temperature_bin) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop")

# Compute bin midpoints
aggregated_data$temperature_midpoint <- as.numeric(
  sapply(strsplit(as.character(aggregated_data$temperature_bin), ",|\\(|\\]"), function(x) {
    (as.numeric(x[2]) + as.numeric(x[3])) / 2
  })
)


k <- c(5:8, 15:18, 23:26)

coef_list1 <- vector("list", length(k))
vcov_list1 <- vector("list", length(k))

for (i in seq_along(k)) {
  coef_list1[[i]] <- model_first_stage[[k[i]]]$blup
  vcov_list1[[i]] <- model_first_stage[[k[i]]]$vcov
}

coef_matrix1 <- do.call(rbind, coef_list1)
vcov_list1  <- Filter(Negate(is.null), vcov_list1)


meta_model_WA <- mixmeta(coef_matrix1, vcov_list1, method = "reml", data = dd)
print(summary(meta_model_WA), digits = 5)


exposure_levels <- ee$value

var_knots <- quantile(exposure_levels, c(0.1, 0.75, 0.9), na.rm = TRUE)

cross_basis <- crossbasis(
  exposure_levels,
  lag = c(0, 30),
  argvar = list(fun = "ns", knots = var_knots,
                Boundary.knots = range(exposure_levels, na.rm = TRUE)),
  arglag = list(knots = logknots(30, 2))
)

# Prediction grid
at_temp <- seq(min(exposure_levels), max(exposure_levels), 0.01)

# Quantiles (kept for consistency)
quantile(exposure_levels, c(0.05, 0.95))
low  <- quantile(ee$value, 0.01)
high <- quantile(ee$value, 0.99)


basis_predict <- crosspred(cross_basis, meta_model_WA, cen = 25,
                           at = at_temp, model.link = "log")

data_for_plot <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
)

# Minimum mortality temperature
MMT <- data_for_plot$Exposure[which.min(data_for_plot$Predicted)]


basis_predict <- crosspred(cross_basis, meta_model_WA, cen = MMT,
                           at = at_temp, model.link = "log")

data_for_plot1 <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
) %>%
  filter(Exposure >= 19)

# Split by MMT
data_for_plot1_less_than_ref    <- data_for_plot1[data_for_plot1$Exposure <  MMT, ]
data_for_plot1_greater_than_ref <- data_for_plot1[data_for_plot1$Exposure >= MMT, ]



ggplot() +
  geom_line(data = data_for_plot1_less_than_ref, aes(x = Exposure, y = Predicted), color = "steelblue", size = 1.2) +
  geom_line(data = data_for_plot1_greater_than_ref, aes(x = Exposure, y = Predicted), color = "#DC0000FF", size = 1.2) +
  geom_ribbon(data = data_for_plot1, aes(x = Exposure, ymin = CI_Low, ymax = CI_High),
              fill = "grey", alpha = 0.2) +
  labs(x = "Temperature (°C)", y = " ",
       subtitle = "West Africa") +
  scale_x_continuous(breaks = seq(20,35,5))+
  scale_y_log10(expand = c(0, 0)) +
  geom_vline(xintercept = MMT, color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0.3, 10)) +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(size = 24),
    axis.ticks.y = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.y = element_line(color = "black", size = 1),
    axis.ticks.x = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(color = "black", size = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



aggregated_data <- subset(aggregated_data,aggregated_data$temperature_midpoint>=19)

ggplot() +
  geom_col(data = aggregated_data, aes(x = temperature_midpoint, y = total_deaths), fill = "pink", color = "black") +
  theme_minimal() +
  labs(x = " ", y = "Deaths (n)") +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(size = 24),
    legend.position = "none",
    axis.ticks.y.right = element_line(size = 1, color = "black", lineend = "round"),
    axis.ticks.length.y.right = unit(0.2, "cm"),
    axis.line.y.right = element_line(color = "black", size = 1),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    position = "right",
    breaks = c(0,  2500,5000,7500),
    limits = c(0, 7500),
    expand = c(0, 0)
  )

######
load("Fig1_NMR.RData")

# ---- Fig1d ----
# Subset daily data for South Asia
id_sel <- unique(data_month$CentreId)[c(1:4)]

ee <- data_day %>%
  filter(CentreId %in% id_sel)

# Define temperature range and create 1°C bins
min_temp <- floor(min(ee$value))
max_temp <- ceiling(max(ee$value))

ee <- ee %>%
  mutate(temperature_bin = cut(value, breaks = seq(min_temp, max_temp, by = 1)))

# Aggregate total deaths within each temperature bin
aggregated_data <- ee %>%
  group_by(temperature_bin) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop")

# Calculate midpoint of each temperature bin
aggregated_data$temperature_midpoint <- as.numeric(
  sapply(strsplit(as.character(aggregated_data$temperature_bin), ",|\\(|\\]"), function(x) {
    lower <- as.numeric(x[2])
    upper <- as.numeric(x[3])
    (lower + upper) / 2
  })
)

# Subset site-level data corresponding to selected centers
dd <- subset(data_site, data_site$study %in% unique(data_month$CentreId)[c(1:4)])

# Extract model_first_stage coefficients and covariance matrices
coef_list1 <- list()
vcov_list1 <- list()

k <- c(1:4)
for (i in c(1:4)) {
  coef_list1[[i]] <- model_first_stage[[k[i]]]$blup
  vcov_list1[[i]] <- model_first_stage[[k[i]]]$vcov
}

# Combine coefficients into matrix form
coef_matrix1 <- do.call(rbind, coef_list1)
vcov_list1 <- Filter(Negate(is.null), vcov_list1)

# Perform multivariate meta-analysis (REML)
meta_model_SA <- mixmeta(coef_matrix1, vcov_list1, method = "reml", data = dd)

print(summary(meta_model_SA), digits = 5)

# Define exposure (temperature) distribution
exposure_levels <- ee$value

# Specify spline function and knot placement (percentiles)
var_fun <- "ns"
var_prc <- c(0.1, 0.75, 0.9)
var_knots <- quantile(exposure_levels, var_prc, na.rm = TRUE)
boundary_knots <- range(exposure_levels, na.rm = TRUE)

# Define lag structure (up to 30 days)
max_lag <- 30
lag_knots <- logknots(max_lag, 2)

# Construct cross-basis matrix for DLNM
cross_basis <- crossbasis(
  exposure_levels,
  lag = c(0, max_lag),
  argvar = list(fun = var_fun, knots = var_knots, Boundary.knots = boundary_knots),
  arglag = list(knots = lag_knots)
)

# Define prediction grid
pred_prc <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))
at_temp <- seq(min(exposure_levels), max(exposure_levels), 0.01)

# Initial prediction using fixed centering temperature (25°C)
basis_predict <- crosspred(
  cross_basis, meta_model_SA,
  cen = 25, at = at_temp, model.link = "log"
)

# Extract prediction results
predicted_values <- basis_predict$allRRfit
ci_low <- basis_predict$allRRlow
ci_high <- basis_predict$allRRhigh
exposure <- as.numeric(rownames(basis_predict$matfit))

# Create plotting dataset
data_for_plot <- data.frame(
  Exposure = exposure,
  Predicted = predicted_values,
  CI_Low = ci_low,
  CI_High = ci_high
)

# Identify Minimum Mortality Temperature (MMT)
MMT <- data_for_plot$Exposure[which(data_for_plot$Predicted == min(data_for_plot$Predicted))]

# Recalculate predictions centered at MMT
basis_predict <- crosspred(
  cross_basis, meta_model_SA,
  cen = MMT, at = at_temp, model.link = "log"
)

# Extract updated prediction results
predicted_values <- basis_predict$allRRfit
ci_low <- basis_predict$allRRlow
ci_high <- basis_predict$allRRhigh
exposure <- as.numeric(rownames(basis_predict$matfit))

# Prepare final plotting dataset
data_for_plot1 <- data.frame(
  Exposure = exposure,
  Predicted = predicted_values,
  CI_Low = ci_low,
  CI_High = ci_high
)

# Restrict to temperature ≥ 14°C
data_for_plot1 <- subset(data_for_plot1, data_for_plot1$Exposure >= 14)

# Split data relative to MMT for visualization
data_for_plot1_less_than_ref <- data_for_plot1[data_for_plot1$Exposure < MMT, ]
data_for_plot1_greater_than_ref <- data_for_plot1[data_for_plot1$Exposure >= MMT, ]

# Plot exposure-response curve
ggplot() +
  geom_line(data = data_for_plot1_less_than_ref,
            aes(x = Exposure, y = Predicted),
            color = "steelblue", size = 1.2) +
  geom_line(data = data_for_plot1_greater_than_ref,
            aes(x = Exposure, y = Predicted),
            color = "#DC0000FF", size = 1.2) +
  geom_ribbon(data = data_for_plot1,
              aes(x = Exposure, ymin = CI_Low, ymax = CI_High),
              fill = "grey", alpha = 0.2) +
  labs(x = "Temperature (°C)", y = "RR for Neonatal Mortality",
       subtitle = "South Asia") +
  scale_y_log10(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(15, 35, 5)) +
  geom_vline(xintercept = MMT, color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  theme_minimal() +
  coord_cartesian(ylim = c(0.3, 10)) +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.subtitle = element_text(size = 24),
    axis.ticks.y = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.y = element_line(color = "black", size = 1),
    axis.ticks.x = element_line(size = 1, color = "black"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(color = "black", size = 1),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Prepare histogram-style death counts
aggregated_data <- subset(aggregated_data, aggregated_data$temperature_midpoint >= 14)

# Plot distribution of deaths by temperature
ggplot() +
  geom_col(data = aggregated_data,
           aes(x = temperature_midpoint, y = total_deaths),
           fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(x = " ", y = "Deaths (n)") +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    plot.subtitle = element_text(size = 24),
    legend.position = "none",
    axis.ticks.y.right = element_line(size = 1, color = "black"),
    axis.ticks.length.y.right = unit(0.2, "cm"),
    axis.line.y.right = element_line(color = "black", size = 1),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    position = "right",
    breaks = c(0, 50, 100),
    limits = c(0, 100),
    expand = c(0, 0)
  )

# ---- Fig1e ----
# Subset daily data for Southeast Africa
id_sel <- unique(data_month$CentreId)[c(9:11,13,14,19:22,27:33)]

ee <- data_day %>%
  filter(CentreId %in% id_sel)

dd <- data_site %>%
  filter(study %in% id_sel)


min_temp <- floor(min(ee$value))
max_temp <- ceiling(max(ee$value))

aggregated_data <- ee %>%
  mutate(temperature_bin = cut(value, breaks = seq(min_temp, max_temp, by = 1))) %>%
  group_by(temperature_bin) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop")

# Compute bin midpoints
aggregated_data$temperature_midpoint <- as.numeric(
  sapply(strsplit(as.character(aggregated_data$temperature_bin), ",|\\(|\\]"), function(x) {
    (as.numeric(x[2]) + as.numeric(x[3])) / 2
  })
)


coef_list1 <- list()
vcov_list1 <- list()

k <- c(9:11,13,14,19:22,27:33)

for (i in seq_along(k)) {
  coef_list1[[i]] <- model_first_stage[[k[i]]]$blup
  vcov_list1[[i]] <- model_first_stage[[k[i]]]$vcov
}

coef_matrix1 <- do.call(rbind, coef_list1)
vcov_list1 <- Filter(Negate(is.null), vcov_list1)


meta_model_ESA <- mixmeta(coef_matrix1, vcov_list1, method = "reml", data = dd)
print(summary(meta_model_ESA), digits = 5)


exposure_levels <- ee$value

var_knots <- quantile(exposure_levels, c(0.1, 0.75, 0.9), na.rm = TRUE)
boundary_knots <- range(exposure_levels, na.rm = TRUE)

max_lag <- 30
lag_knots <- logknots(max_lag, 2)

cross_basis <- crossbasis(
  exposure_levels,
  lag = c(0, max_lag),
  argvar = list(fun = "ns", knots = var_knots, Boundary.knots = boundary_knots),
  arglag = list(knots = lag_knots)
)

at_temp <- seq(min(exposure_levels), max(exposure_levels), 0.01)

quantile(exposure_levels, c(0.05, 0.95))
low  <- quantile(ee$value, 0.01)
high <- quantile(ee$value, 0.99)


basis_predict <- crosspred(cross_basis, meta_model_ESA, cen = 25,
                           at = at_temp, model.link = "log")

data_for_plot <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
)

MMT <- data_for_plot$Exposure[which.min(data_for_plot$Predicted)]


basis_predict <- crosspred(cross_basis, meta_model_ESA, cen = MMT,
                           at = at_temp, model.link = "log")

data_for_plot1 <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
)


data_for_plot1 <- subset(data_for_plot1, Exposure >= 10)


data_for_plot1_less_than_ref    <- data_for_plot1[data_for_plot1$Exposure <  MMT, ]
data_for_plot1_greater_than_ref <- data_for_plot1[data_for_plot1$Exposure >= MMT, ]



ggplot() +
  geom_line(data = data_for_plot1_less_than_ref,
            aes(Exposure, Predicted), color = "steelblue", size = 1.2) +
  geom_line(data = data_for_plot1_greater_than_ref,
            aes(Exposure, Predicted), color = "#DC0000FF", size = 1.2) +
  geom_ribbon(data = data_for_plot1,
              aes(Exposure, ymin = CI_Low, ymax = CI_High),
              fill = "grey", alpha = 0.2) +
  scale_y_log10(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(10,35,5), limits = c(9.7,34.8)) +
  geom_vline(xintercept = MMT) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_cartesian(ylim = c(0.3, 10)) +
  labs(x = "Temperature (°C)", y = " ", subtitle = "Eastern&Southern Africa") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 18),
    plot.subtitle = element_text(size = 24),
    legend.position = "none",
    axis.ticks = element_line(size = 1),
    axis.line  = element_line(size = 1),
    panel.grid = element_blank()
  )


aggregated_data <- subset(aggregated_data, temperature_midpoint >= 10)

ggplot() +
  geom_col(data = aggregated_data,
           aes(temperature_midpoint, total_deaths),
           fill = "skyblue", color = "black") +
  scale_y_continuous(
    position = "right",
    breaks = c(0, 400, 800),
    limits = c(0, 800),
    expand = c(0, 0)
  ) +
  labs(x = " ", y = "Deaths (n)") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 18),
    axis.text.x = element_blank(),
    legend.position = "none",
    axis.ticks.y.right = element_line(size = 1),
    axis.line.y.right  = element_line(size = 1),
    panel.grid = element_blank()
  )


# ---- Fig1f ----
# Subset daily data for West Africa
id_sel <- unique(data_month$CentreId)[c(5:8, 15:18, 23:26)]

ee <- data_day %>%
  filter(CentreId %in% id_sel)

dd <- data_site %>%
  filter(study %in% id_sel)


min_temp <- floor(min(ee$value))
max_temp <- ceiling(max(ee$value))

aggregated_data <- ee %>%
  mutate(temperature_bin = cut(value, breaks = seq(min_temp, max_temp, by = 1))) %>%
  group_by(temperature_bin) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop")

# Compute bin midpoints
aggregated_data$temperature_midpoint <- as.numeric(
  sapply(strsplit(as.character(aggregated_data$temperature_bin), ",|\\(|\\]"), function(x) {
    (as.numeric(x[2]) + as.numeric(x[3])) / 2
  })
)

#  Extract model_first_stage 
k <- c(5:8, 15:18, 23:26)

coef_list1 <- vector("list", length(k))
vcov_list1 <- vector("list", length(k))

for (i in seq_along(k)) {
  coef_list1[[i]] <- model_first_stage[[k[i]]]$blup
  vcov_list1[[i]] <- model_first_stage[[k[i]]]$vcov
}

coef_matrix1 <- do.call(rbind, coef_list1)
vcov_list1  <- Filter(Negate(is.null), vcov_list1)

#  Meta-analysis 
meta_model_WA <- mixmeta(coef_matrix1, vcov_list1, method = "reml", data = dd)
print(summary(meta_model_WA), digits = 5)

#  DLNM setup 
exposure_levels <- ee$value

var_knots <- quantile(exposure_levels, c(0.1, 0.75, 0.9), na.rm = TRUE)

cross_basis <- crossbasis(
  exposure_levels,
  lag = c(0, 30),
  argvar = list(fun = "ns", knots = var_knots,
                Boundary.knots = range(exposure_levels, na.rm = TRUE)),
  arglag = list(knots = logknots(30, 2))
)

# Prediction grid
at_temp <- seq(min(exposure_levels), max(exposure_levels), 0.01)

# Quantiles (kept for consistency)
quantile(exposure_levels, c(0.05, 0.95))
low  <- quantile(ee$value, 0.01)
high <- quantile(ee$value, 0.99)

#  First prediction (cen = 25) 
basis_predict <- crosspred(cross_basis, meta_model_WA, cen = 25,
                           at = at_temp, model.link = "log")

data_for_plot <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
)

# Minimum mortality temperature
MMT <- data_for_plot$Exposure[which.min(data_for_plot$Predicted)]

#  Re-centered prediction 
basis_predict <- crosspred(cross_basis, meta_model_WA, cen = MMT,
                           at = at_temp, model.link = "log")

data_for_plot1 <- data.frame(
  Exposure  = as.numeric(rownames(basis_predict$matfit)),
  Predicted = basis_predict$allRRfit,
  CI_Low    = basis_predict$allRRlow,
  CI_High   = basis_predict$allRRhigh
) %>%
  filter(Exposure >= 19)

# Split by MMT
data_for_plot1_less_than_ref    <- data_for_plot1[data_for_plot1$Exposure <  MMT, ]
data_for_plot1_greater_than_ref <- data_for_plot1[data_for_plot1$Exposure >= MMT, ]

#  Exposure-response plot 
ggplot() +
  geom_line(data = data_for_plot1_less_than_ref,
            aes(Exposure, Predicted), color = "steelblue", size = 1.2) +
  geom_line(data = data_for_plot1_greater_than_ref,
            aes(Exposure, Predicted), color = "#DC0000FF", size = 1.2) +
  geom_ribbon(data = data_for_plot1,
              aes(Exposure, ymin = CI_Low, ymax = CI_High),
              fill = "grey", alpha = 0.2) +
  geom_vline(xintercept = MMT, color = "black", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  scale_y_log10(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.3, 10)) +
  labs(x = "Temperature (°C)", y = " ", subtitle = "West Africa") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 18),
    plot.subtitle = element_text(size = 24),
    legend.position = "none",
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line = element_line(color = "black", size = 1),
    panel.grid = element_blank()
  )

#  Death distribution 
aggregated_data <- aggregated_data %>%
  filter(temperature_midpoint >= 19)

ggplot() +
  geom_col(data = aggregated_data,
           aes(temperature_midpoint, total_deaths),
           fill = "skyblue", color = "black") +
  scale_y_continuous(
    position = "right",
    breaks = c(0, 500, 1000),
    limits = c(0, 1000),
    expand = c(0, 0)
  ) +
  labs(x = " ", y = "Deaths (n)") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 18),
    axis.text.x = element_blank(),
    legend.position = "none",
    axis.ticks.y.right = element_line(size = 1),
    axis.line.y.right  = element_line(size = 1),
    panel.grid = element_blank()
  )


















