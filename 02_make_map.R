source("00_read_and_prepare__prev_data_and_orog_aspr.R")
source("01_smooth_pr_and_compare_1y_aspr_with_orig.R")

# shape data with SEN_SECC
shape.data <- sf::st_read("eusk/SECCIONES_EUSTAT_5000_ETRS89.shp")

# what are we plotting?
plot_data <- test_data %>% 
  filter(type == "Smooth")

# save(plot_data, file = "tolosa_maps.RData")

# plot order
plot_units <- expand_grid(sex       = unique(plot_data$sex),
                          condition = unique(plot_data$condition),
                          year      = unique(plot_data$year))

# example
plot_data %>%
  filter(sex == plot_units$sex[1]) %>% 
  left_join(shape.data) %>%
  ggplot() +
  scale_fill_viridis_c(option = "inferno", breaks = pretty_breaks(n = 8)) + #inferno
  geom_sf(aes(geometry = geometry, 
              fill     = aspr * 1000),
          color = "white", 
          size  = 0.1) +
  theme_map() +
  labs(title = str_c(plot_units$sex[1],
                     ", ",
                     plot_units$condition[1],
                     ", ",
                     plot_units$year[1],
                     ", ASPR per 1000")) +
  theme(
    legend.direction     = "horizontal",
    legend.position      = "bottom",
    legend.justification = "center",
    legend.key.size      = unit(1, 'cm'),
    legend.key.width     = unit(1, 'cm'),
    legend.background    = element_blank(),
    legend.title         = element_blank(),
    legend.text          = element_text(size = 10, color = "white"),
    plot.title           = element_text(size = 12, color = "white", hjust = 0.5, face = "bold"),
    plot.margin          = unit(c(1, 1, 1, 1), "cm"),
    plot.background      = element_rect(fill  = 'gray20', color = NA),
    panel.background     = element_rect(fill  = 'gray10', color = NA),
    panel.grid           = element_blank())
