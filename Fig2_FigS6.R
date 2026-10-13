# Main Figure 2 and Supplementary Figure S6 (final numbering, 2026-10-13).
#!/usr/bin/env Rscript
# Rebuild the supplied numerical figure data with editable vector maps and text.
# Usage: Rscript Fig2_FigS6.R Fig2_FigS6.RData output_directory [both|figure2|figs6]
suppressPackageStartupMessages({
  library(ggplot2)
  library(sf)
  library(grid)
  library(ragg)
  library(jsonlite)
})
args <- commandArgs(trailingOnly = TRUE)
data_file <- if (length(args) >= 1) args[1] else "Fig2_FigS6.RData"
output_dir <- if (length(args) >= 2) args[2] else "output"
target <- if (length(args) >= 3) args[3] else "both"
stopifnot(target %in% c("both", "figure2", "figs6"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source_dir <- file.path(output_dir, "SourceData")
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
env <- new.env(parent = baseenv())
loaded <- load(data_file, envir = env)
stopifnot(all(c("a", "b", "df_his", "df_fut", "line1") %in% loaded))
heat <- env$a
cold <- env$b
historical <- env$df_his
future <- env$df_fut
stopifnot(nrow(heat) == nrow(cold), identical(heat$lon, cold$lon),
          identical(heat$lat, cold$lat), all(future$Min <= future$Mean),
          all(future$Mean <= future$Max), all(range(historical$Year) == c(1990, 2018)))

scenarios <- c("119", "245", "370", "585")
scenario_labels <- c("SSP1-1.9", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
decades <- c("2030", "2060", "2090")
future$Scenario <- factor(as.character(future$Group), levels = scenarios,
                          labels = scenario_labels)
stopifnot(!anyNA(future$Scenario))

# The saved source contains one already-subtracted demonstration column.
# Retain that column directly, and subtract the historical value for all others.
heat_changes <- heat[, c("lon", "lat")]
cold_changes <- cold[, c("lon", "lat")]
for (s in scenarios) for (yr in decades) {
  hc <- paste0("heat_SSP", s, "_", yr)
  cc <- paste0("cold_SSP", s, "_", yr)
  stopifnot(hc %in% names(heat), cc %in% names(cold))
  heat_changes[[hc]] <- heat[[hc]] - heat$heat_his
  cold_changes[[cc]] <- if (s == "119" && yr == "2030") cold[[cc]] else cold[[cc]] - cold$cold_his
}
stopifnot(any(cold$cold_SSP119_2030 < 0),
          identical(cold_changes$cold_SSP119_2030, cold$cold_SSP119_2030))

write.csv(heat[, c("lon", "lat", "heat_his")], file.path(source_dir, "historical_heat.csv"), row.names = FALSE)
write.csv(cold[, c("lon", "lat", "cold_his")], file.path(source_dir, "historical_cold.csv"), row.names = FALSE)
write.csv(heat_changes, file.path(source_dir, "future_heat_changes.csv"), row.names = FALSE)
write.csv(cold_changes, file.path(source_dir, "future_cold_changes.csv"), row.names = FALSE)
write.csv(historical, file.path(source_dir, "historical_warming.csv"), row.names = FALSE)
write.csv(future[, c("Year", "Mean", "Min", "Max", "Scenario")], file.path(source_dir, "future_warming.csv"), row.names = FALSE)

# Preserve the supplied country outlines; crop them to the shared map extent.
sf_use_s2(FALSE)
boundaries <- st_as_sf(env$line1)
if (is.na(st_crs(boundaries))) st_crs(boundaries) <- 4326
boundaries <- suppressWarnings(st_crop(boundaries, xmin = -20, ymin = -36, xmax = 112, ymax = 38))
stopifnot(nrow(boundaries) > 0)
saveRDS(boundaries, file.path(source_dir, "map_boundaries.rds"))

tile_width <- min(diff(sort(unique(heat$lon))))
tile_height <- min(diff(sort(unique(heat$lat))))
stopifnot(tile_width > 0, tile_height > 0)
font_family <- "Times New Roman"
font_scale <- 1.5
pt <- function(size) size * font_scale
warm_colors <- c("#4DBBD5", "#F7D380", "#F39B7F", "#DC0000")
heat_hist_colors <- c("#FEE0D2", "#FCBBA1", "#EF3B2C", "#A50F15", "#67000D")
heat_change_colors <- c("#FFF5F0", "#FCBBA1", "#FB6A4A", "#CB181D", "#67000D")
cold_hist_colors <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#4292C6", "#08306B")
cold_change_colors <- cold_hist_colors
cuts <- list(heat_historical = c(1e4, 1e5, 1e6, 1e7),
             cold_historical = c(5e4, 5e5, 5e6, 5e7),
             heat_change = c(0, 5e3, 5e4, 5e5),
             cold_change = c(-1e5, 0, 1e6, 1e7))
classify <- function(x, thresholds) factor(findInterval(x, thresholds, left.open = TRUE) + 1L, levels = 1:5)

map_plot <- function(data, values, thresholds, colors, title = NULL,
                     show_longitude = TRUE, show_latitude = TRUE,
                     merge_same_class_tiles = FALSE) {
  dat <- data[, c("lon", "lat")]
  dat$Class <- classify(values, thresholds)
  if (merge_same_class_tiles) {
    tile_geometries <- st_sfc(lapply(seq_len(nrow(dat)), function(i) {
      x <- dat$lon[i]; y <- dat$lat[i]
      st_polygon(list(rbind(c(x - tile_width / 2, y - tile_height / 2),
                             c(x + tile_width / 2, y - tile_height / 2),
                             c(x + tile_width / 2, y + tile_height / 2),
                             c(x - tile_width / 2, y + tile_height / 2),
                             c(x - tile_width / 2, y - tile_height / 2))))
    }), crs = 4326)
    present_classes <- levels(dat$Class)[levels(dat$Class) %in% dat$Class]
    merged_geometries <- st_sfc(lapply(present_classes, function(cl) {
      suppressMessages(suppressWarnings(st_union(tile_geometries[dat$Class == cl])))[[1]]
    }), crs = 4326)
    merged <- st_sf(Class = factor(present_classes, levels = levels(dat$Class)),
                    geometry = merged_geometries)
    # Exact planar cell areas are preserved in every category; no color-class smoothing.
    actual_areas <- as.numeric(st_area(st_set_crs(merged, NA)))
    expected_areas <- as.numeric(table(dat$Class)[present_classes]) * tile_width * tile_height
    stopifnot(all(abs(actual_areas - expected_areas) < 1e-6))
    fill_layer <- geom_sf(data = merged, aes(fill = Class), colour = NA,
                          linewidth = 0, inherit.aes = FALSE)
  } else {
    fill_layer <- geom_tile(data = dat, aes(lon, lat, fill = Class), width = tile_width,
                            height = tile_height, linewidth = 0, colour = NA)
  }
  ggplot() +
    fill_layer +
    geom_sf(data = boundaries, fill = NA, colour = "#B5B5B5", linewidth = 0.10,
            inherit.aes = FALSE) +
    scale_fill_manual(values = colors, drop = FALSE, na.value = "white", guide = "none") +
    scale_x_continuous(breaks = c(0, 50, 100), labels = c("0\u00b0", "50\u00b0E", "100\u00b0E")) +
    scale_y_continuous(breaks = c(-20, 0, 20), labels = c("20\u00b0S", "0\u00b0", "20\u00b0N")) +
    coord_sf(xlim = c(-20, 112), ylim = c(-36, 38), expand = FALSE,
             crs = st_crs(4326), default_crs = st_crs(4326), datum = st_crs(4326)) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal(base_size = pt(6.2), base_family = font_family) +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA, colour = "#444444", linewidth = 0.22),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          axis.text = element_text(size = pt(5.8), colour = "black"),
          axis.text.x = if (show_longitude) element_text(margin = margin(t = 1.5)) else element_blank(),
          axis.text.y = if (show_latitude) element_text(margin = margin(r = 1.5)) else element_blank(),
          axis.ticks = element_line(colour = "#444444", linewidth = 0.20),
          axis.ticks.x = if (show_longitude) element_line(colour = "#444444", linewidth = 0.20) else element_blank(),
          axis.ticks.y = if (show_latitude) element_line(colour = "#444444", linewidth = 0.20) else element_blank(),
          axis.ticks.length = unit(0.8, "mm"),
          plot.title = element_text(size = pt(7), hjust = 0.5, margin = margin(b = 2)),
          plot.margin = margin(1, 1.5, 1, 1.5, unit = "mm"))
}

warming_plot <- ggplot() +
  geom_ribbon(data = future, aes(Year, ymin = Min, ymax = Max, fill = Scenario),
              alpha = 0.28, colour = NA, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_line(data = future, aes(Year, Mean, colour = Scenario), linewidth = 0.45) +
  geom_line(data = historical, aes(Year, Mean), colour = "black", linewidth = 0.45) +
  scale_colour_manual(values = warm_colors, name = NULL) +
  scale_fill_manual(values = warm_colors, name = NULL) +
  scale_x_continuous(breaks = c(1990, 2019, 2030, 2060, 2090), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 1, 3, 5, 7), expand = c(0, 0)) +
  coord_cartesian(xlim = c(1990, 2100), ylim = c(-1, 7)) +
  labs(x = "Year", y = "Level of warming (\u00b0C)") +
  theme_classic(base_size = pt(6.5), base_family = font_family) +
  theme(axis.text = element_text(size = pt(6), colour = "black"),
        axis.title = element_text(size = pt(6.5)),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        axis.ticks.length = unit(1, "mm"),
        legend.position = "inside", legend.position.inside = c(0.06, 0.96),
        legend.justification = c(0, 1), legend.text = element_text(size = pt(6)),
        legend.key.height = unit(3, "mm"), legend.key.width = unit(5, "mm"),
        legend.spacing.y = unit(0, "mm"), legend.margin = margin(0, 0, 0, 0),
        legend.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(2, 2, 2, 1, unit = "mm"))

historical_heat_plot <- map_plot(heat, heat$heat_his, cuts$heat_historical,
                                 heat_hist_colors, "Historical (1990\u20132018)",
                                 merge_same_class_tiles = TRUE)
historical_cold_plot <- map_plot(cold, cold$cold_his, cuts$cold_historical,
                                 cold_hist_colors, "Historical (1990\u20132018)",
                                 merge_same_class_tiles = TRUE)
heat_plots <- cold_plots <- vector("list", 12)
k <- 1L
for (s in scenarios) for (yr in decades) {
  heat_plots[[k]] <- map_plot(heat, heat_changes[[paste0("heat_SSP", s, "_", yr)]],
                              cuts$heat_change, heat_change_colors,
                              show_longitude = s == tail(scenarios, 1),
                              show_latitude = yr == decades[1],
                              merge_same_class_tiles = TRUE)
  cold_plots[[k]] <- map_plot(cold, cold_changes[[paste0("cold_SSP", s, "_", yr)]],
                              cuts$cold_change, cold_change_colors,
                              show_longitude = s == tail(scenarios, 1),
                              show_latitude = yr == decades[1],
                              merge_same_class_tiles = TRUE)
  k <- k + 1L
}

# Keep every map panel identical in size despite hiding interior coordinates.
align_map_grobs <- function(plots) {
  grobs <- lapply(plots, ggplotGrob)
  common_widths <- Reduce(unit.pmax, lapply(grobs, function(g) g$widths))
  common_heights <- Reduce(unit.pmax, lapply(grobs, function(g) g$heights))
  lapply(grobs, function(g) {
    g$widths <- common_widths
    g$heights <- common_heights
    g
  })
}
metrics_file <- file.path(output_dir, ".layout_metrics.pdf")
grDevices::cairo_pdf(metrics_file, width = 210 / 25.4, height = 294 / 25.4,
                     family = font_family)
heat_plots <- align_map_grobs(heat_plots)
cold_plots <- align_map_grobs(cold_plots)
dev.off()
unlink(metrics_file)

page_width <- 210
draw_text <- function(label, x, top, size = 6.5, bold = FALSE, rotation = 0,
                       just = "centre", scale_text = TRUE) {
  grid.text(label, x = unit(x, "mm"), y = unit(page_height - top, "mm"), just = just,
            rot = rotation, gp = gpar(fontfamily = font_family, fontsize = if (scale_text) pt(size) else size,
                                     fontface = if (bold) "bold" else "plain", col = "black"))
}
draw_plot <- function(plot, x, top, width, height) {
  vp <- viewport(x = unit(x + width / 2, "mm"), y = unit(page_height - top - height / 2, "mm"),
                 width = unit(width, "mm"), height = unit(height, "mm"))
  if (inherits(plot, "gtable")) {
    pushViewport(vp)
    grid.draw(plot)
    popViewport()
  } else print(plot, vp = vp, newpage = FALSE)
}
draw_bar <- function(title, colors, labels, x, top, width,
                      title_size = 6.3, label_size = 8.0, scale_text = TRUE) {
  title <- sub(" \\(person-days\\)$", "\n(person-days)", title)
  draw_text(title, x + width / 2, top - 3.5, size = title_size, scale_text = scale_text)
  for (i in 1:5) {
    grid.rect(x = unit(x + (i - 0.5) * width / 5, "mm"),
               y = unit(page_height - top - 4.0, "mm"),
               width = unit(width / 5, "mm"), height = unit(2.6, "mm"),
               gp = gpar(fill = colors[i], col = NA))
  }
  label_offset <- if (scale_text) 7.6 else 7.0
  for (i in 1:4) draw_text(labels[i], x + i * width / 5, top + label_offset,
                           size = label_size, scale_text = scale_text)
}
draw_inline_bar <- function(title, colors, labels, top, scale_text = TRUE,
                             width_scale = 1) {
  title_size <- 8.5
  pushViewport(viewport(gp = gpar(fontfamily = font_family,
                                fontsize = if (scale_text) pt(title_size) else title_size)))
  title_width <- convertWidth(stringWidth(title), "mm", valueOnly = TRUE)
  popViewport()
  bar_x <- 14 + title_width + 3
  original_bar_width <- 202 - bar_x
  bar_width <- original_bar_width * width_scale
  block_offset <- (original_bar_width - bar_width) / 2
  bar_x <- bar_x + block_offset
  draw_text(title, 14 + block_offset, top, size = title_size, just = "left",
              scale_text = scale_text)
  stopifnot(bar_width > 50)
  for (i in 1:5) {
    grid.rect(x = unit(bar_x + (i - 0.5) * bar_width / 5, "mm"),
               y = unit(page_height - top, "mm"),
               width = unit(bar_width / 5, "mm"), height = unit(3.0, "mm"),
               gp = gpar(fill = colors[i], col = NA))
  }
  label_offset <- if (scale_text) 4.5 else 3.2
  for (i in 1:4) draw_text(labels[i], bar_x + i * bar_width / 5, top + label_offset,
                           size = 8.0, scale_text = scale_text)
}
draw_future <- function(plots, tag, top, row_height) {
  draw_text(tag, 4, top - 6, size = 8, bold = TRUE, just = "left")
  for (j in 1:3) draw_text(paste0(decades[j], "s"), 13 + (j - 1) * 64 + 30.5,
                            top - 5, size = 7, bold = TRUE)
  k <- 1L
  for (i in 1:4) {
    draw_text(scenario_labels[i], 5, top + (i - 0.5) * row_height,
               size = 6.5, bold = TRUE, rotation = 90)
    for (j in 1:3) {
      draw_plot(plots[[k]], 13 + (j - 1) * 64, top + (i - 1) * row_height,
                 61, row_height - 1)
      k <- k + 1L
    }
  }
}
draw_figure2 <- function() {
  grid.newpage()
  draw_text("a", 4, 6, size = 8, bold = TRUE, just = "left")
  draw_text("b", 106, 6, size = 8, bold = TRUE, just = "left")
  draw_plot(warming_plot, 8, 10, 94, 64)
  draw_plot(historical_heat_plot, 110, 10, 94, 62)
  draw_bar("Average heat exposure (person-days)", heat_hist_colors,
            c("10\u2074", "10\u2075", "10\u2076", "10\u2077"), 164, 54, 38,
            title_size = 8.5, label_size = 8.0, scale_text = FALSE)
  draw_future(heat_plots, "c", top = 80.13, row_height = 38.5)
  draw_inline_bar("Heat Exposure Change vs. Historical (Person-days)", heat_change_colors,
                   c("0", "5\u00d710\u00b3", "5\u00d710\u2074", "5\u00d710\u2075"), 237.34,
                   scale_text = FALSE, width_scale = 0.8)
}
draw_figs6 <- function() {
  grid.newpage()
  draw_text("a", 4, 6, size = 8, bold = TRUE, just = "left")
  draw_plot(historical_cold_plot, 28, 6, 154, 87)
  draw_bar("Average cold exposure (person-days)", cold_hist_colors,
            c("5\u00d710\u2074", "5\u00d710\u2075", "5\u00d710\u2076", "5\u00d710\u2077"), 116, 71, 58)
  draw_future(cold_plots, "b", top = 99.5, row_height = 38.5)
  draw_inline_bar("Cold Exposure Change vs. Historical (person-days)", cold_change_colors,
                   c("\u221210\u2075", "0", "10\u2076", "10\u2077"), 256.73)
}
export_figure <- function(stem, height_mm, draw_fun) {
  page_height <<- height_mm
  pdf_path <- file.path(output_dir, paste0(stem, ".pdf"))
  png_path <- file.path(output_dir, paste0(stem, ".png"))
  grDevices::cairo_pdf(pdf_path, width = page_width / 25.4, height = height_mm / 25.4,
                       family = font_family, onefile = TRUE, bg = "white", fallback_resolution = 600)
  draw_fun()
  dev.off()
  ragg::agg_png(png_path, width = page_width, height = height_mm, units = "mm", res = 600,
                background = "white", scaling = 1)
  draw_fun()
  dev.off()
  cat("Exported:", stem, "210 x", height_mm, "mm\n")
}
if (target %in% c("both", "figure2")) export_figure("Figure2_A4", 249.47, draw_figure2)
if (target %in% c("both", "figs6")) export_figure("FigureS6_A4", 268.73, draw_figs6)

manifest <- list(backend = "R", width_mm = 210, height_mm = c(Figure2 = 249.47, FigS6 = 268.73),
                 png_dpi = 600, font = font_family, grid_cells = nrow(heat),
                 font_scale = font_scale,
                 legend_label_font_pt = pt(8.0), future_legend_title_font_pt = pt(8.5),
                 Figure2_panel_c_legend_label_font_pt = 8.0,
                 Figure2_panel_c_legend_title_font_pt = 8.5,
                 Figure2_panel_b_legend_label_font_pt = 8.0,
                 Figure2_panel_b_legend_title_font_pt = 8.5,
                 Figure2_panel_c_colorbar_width_scale = 0.8,
                 future_legend_title_bar_gap_mm = 3,
                 future_legend_map_whitespace_reduction = "approximately 60%",
                 longitude_ticks = c(0, 50, 100), latitude_ticks = c(-20, 0, 20),
                 Figure2_panel_b_legend = "lower-right ocean space, matching original placement",
                 Figure2_panel_c_legend = "restored to original form: title left, color bar right, thresholds below",
                 map_fill_rendering = "exact union of touching cells in each category; internal white seams removed",
                 Figure2_panel_c_grid_top_mm = 80.13,
                 FigS6_historical_legend = "lower-right ocean area",
                 FigS6_future_legend = "original form: title left, color bar right, thresholds below",
                 FigS6_future_grid_top_mm = 99.5,
                 classification_thresholds = cuts,
                 preserved_already_differenced_column = "cold_SSP119_2030",
                 input_md5 = unname(tools::md5sum(data_file)),
                 R_version = R.version.string,
                 packages = sapply(c("ggplot2", "sf", "ragg", "jsonlite"), function(p) as.character(packageVersion(p))))
write_json(manifest, file.path(output_dir, "export_manifest.json"), pretty = TRUE, auto_unbox = TRUE)
writeLines(c("Reconstructed directly from the supplied numerical grids.",
             "PDF: live Times New Roman serif text, vector tiles and country boundaries, no embedded screenshot panels.",
             "Future grids show latitude only in the leftmost column and longitude only in the bottom row; panel sizes remain aligned.",
             "Figure 2: panel b legend is in the lower-right ocean space; panel c legend follows the original horizontal title-left/color-bar-right format.",
             "All same-class map cells are united as exact vector polygons, with per-class areas checked unchanged; internal white seams are removed.",
             "Figure 2 b-c gap and Supplementary Figure S6 a-b gap are shortened by about 80%; Supplementary Figure S6 historical legend is in the lower-right ocean and its future legend uses the original horizontal format.",
             "The warming ribbons retain source Min and Max; do not relabel them as a 95% confidence interval without checking the analysis.",
             "Historical cold categories use the final figure's thresholds (5e4,5e5,5e6,5e7).",
             "The saved cold SSP119/2030 column is already a change and is not subtracted a second time.",
             "No numerical estimates, time periods, scenario order or source country outlines were changed."),
           file.path(output_dir, "README.txt"))
cat("All exports completed.\n")
