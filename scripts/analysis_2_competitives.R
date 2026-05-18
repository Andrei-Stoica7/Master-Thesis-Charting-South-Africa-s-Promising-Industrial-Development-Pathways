# -----------------------------------------------------------------------------
# 0 Preparation
# -----------------------------------------------------------------------------
pre_processed_2024 <- read.csv("data/data_processed/cp_df_gini.csv")
pre_processed_2007 <- read.csv("data/data_processed/cp_df_initial_competitives.csv")

# -----------------------------------------------------------------------------
# 1 Competitive Products for 2024
# -----------------------------------------------------------------------------
zaf_2024 <- pre_processed_2024 %>% 
  filter(country_iso3 == "ZAF", year == 2024)

# -----------------------------------------------------------------------------
# 1.1 Products ranking 
# -----------------------------------------------------------------------------
zaf_2024 <- zaf_2024 %>% 
  mutate(
    DENS_Z    = (dens_cp - mean(dens_cp)) / sd(dens_cp),
    COG_Z     = (cog_cp - mean(cog_cp)) / sd(cog_cp),
  )

# -----------------------------------------------------------------------------
# 1.2 Group Products by Targeted Policy Sector 
# -----------------------------------------------------------------------------
zaf_2024 <- zaf_2024 %>%
  mutate(
    prod_code_str = sprintf("%04d", prod_code),
    hs2 = substr(prod_code_str, 1, 2),
    hs4 = substr(prod_code_str, 1, 4),
    policy_sector = case_when(
      as.integer(hs2) %in% 86:89                      ~ "Motor Vehicles",
      as.integer(hs2) %in% 28:38                      ~ "Chemicals",
      as.integer(hs2) %in% 1:24                       ~ "Agro-processing",
      as.integer(hs2) %in% 72:85                      ~ "Metal Fabrication",
      as.integer(hs2) %in% c(44:48, 94)               ~ "Forestry",
      as.integer(hs2) %in% c(49, 97) | hs4 == "9504"  ~ "CCI",
      as.integer(hs2) %in% c(41:43, 50:65)            ~ "CTFL",
      TRUE                                            ~ "Non-priority Sector"
    )
  )

# -----------------------------------------------------------------------------
# 1.3 Identify Competitive Goods
# -----------------------------------------------------------------------------
candidates_scored_2024 <- zaf_2024 %>% 
  filter(M_cp == 1) %>% # RCA_cp > 0.5 -> competitive products; under < 0.5 -> underdeveloped
  mutate(sector = substr(as.character(prod_code), 1, 2))

head(candidates_scored_2024)

# -----------------------------------------------------------------------------
# 1.4 Export Competitive Goods (2024) to CSV
# -----------------------------------------------------------------------------
competitive_all_2024 <- candidates_scored_2024 %>%
  arrange(desc(PGI_Z)) %>%
  mutate(
    RANK = row_number()
  )

write.csv2(
  competitive_all_2024, 
  file = "outputs/tables/competitive_all_2024.csv", 
  row.names = FALSE
)

competitive_means_2024 <- competitive_all_2024 %>%
  summarise(
    n_products     = n(),
    mean_PCI       = mean(pci_p, na.rm = TRUE),
    median_PCI     = median(pci_p, na.rm = TRUE),
    mean_dens     = mean(dens_cp, na.rm = TRUE),
    median_dens   = median(dens_cp, na.rm = TRUE),
    mean_COG_Z     = mean(COG_Z, na.rm = TRUE),
    median_COG_Z   = median(COG_Z, na.rm = TRUE),
    mean_PGI_Z     = mean(PGI_Z, na.rm = TRUE),
    median_PGI_Z   = median(PGI_Z, na.rm = TRUE)
  ) %>%
  transmute(
    "N (competitive GOODS)"               = n_products,
    "MEAN PCI"                            = mean_PCI,
    "MEDIAN PCI"                          = median_PCI,
    "MEAN DENSITY"                        = mean_dens,
    "MEDIAN DENSITY"                     = median_dens,
    "MEAN OPPORTUNITY GAIN (Z-SCORE)"     = mean_COG_Z,
    "MEDIAN OPPORTUNITY GAIN (Z-SCORE)"   = median_COG_Z,
    "MEAN PGI (Z-SCORE)"                  = mean_PGI_Z,
    "MEDIAN PGI (Z-SCORE)"                = median_PGI_Z,
  )

write.csv2(
  competitive_means_2024,
  file = "outputs/tables/Appendix_2_competitive_all_means_2024.csv",
  row.names = FALSE
)

N_total_2024 <- nrow(candidates_scored_2024)

# -----------------------------------------------------------------------------
# 1.5 Build Plot: Competitive Products (2024)
# -----------------------------------------------------------------------------
competitives_plot_2024 <- ggplot() +
  # POLICY-PREFERRED QUADRANT
  # High feasibility (Density >= 0)
  # Inclusive outcomes (PGI <= 0)
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_2024),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = "red1",
    alpha = 0.05
  ) +
  # Products (grey, full opportunity semantics)
  geom_point(
    data = candidates_scored_2024,
    aes(
      x = PGI_Z,
      y = pci_p,
      colour = policy_sector,
      size  = dens_cp,
    ),
    alpha = 0.6,
  ) +
  # Density legend
  scale_size(
    name = "Density\n(Unstandardised)",
    range = c(0.01, 7),
    limits = range(zaf_2024$dens_cp),
    breaks = c(0.1, 0.3),
    labels = c("0.1", "0.3")
  ) +
  # Sector colours
  scale_colour_manual(
    values = c(
      "Agro-processing"    = "#009E73",
      "Chemicals"          = "#D55E00",
      "Forestry"           = "#6A8F00",
      "Motor Vehicles"     = "#E69F00",
      "CCI"                = "#5C3D99",
      "CTFL"               = "#56B4E9",
      "Metal Fabrication"  = "black",
      "Non-priority Sector" = "grey80"
    ),
    name = "Policy sectors",
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  # Fix legend hollow / filled rendering ONCE
  guides(
    size = guide_legend(
      override.aes = list(
        colour = "grey50",
        alpha = 1
      )
    )
  ) +
  # Labels & theme
  labs(
    title = "Competitive Products (2024) with Sector Positions of Targeted by Industrial Policy",
    x = "PGI (standardised)",
    y = "Product Complexity Index (PCI)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

competitives_plot_2024

# -----------------------------------------------------------------------------
# 1.6 Save Plot: PCI-Density-PGI Competitive Goods 2024
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_D1_PCI-Density-PGI_competitive_2024.png",
  plot = last_plot() + theme(plot.title = element_blank()),
  width = 10,
  height = 7,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 2 Competitive Products for 2007
# -----------------------------------------------------------------------------
# Filter for South Africa
zaf_2007 <- pre_processed_2007 %>% 
  filter(country_iso3 == "ZAF", year == 2007)

# -----------------------------------------------------------------------------
# 2.1 Products ranking 
# -----------------------------------------------------------------------------
zaf_2007 <- zaf_2007 %>% 
  mutate(
    DENS_Z    = (dens_cp - mean(dens_cp)) / sd(dens_cp),
    COG_Z     = (cog_cp - mean(cog_cp)) / sd(cog_cp),
  )

# -----------------------------------------------------------------------------
# 2.2 Group Products by Targeted Policy Sector 
# -----------------------------------------------------------------------------
zaf_2007 <- zaf_2007 %>%
  mutate(
    prod_code_str = sprintf("%04d", prod_code),
    hs2 = substr(prod_code_str, 1, 2),
    hs4 = substr(prod_code_str, 1, 4),
    policy_sector = case_when(
      as.integer(hs2) %in% 86:89                      ~ "Motor Vehicles",
      as.integer(hs2) %in% 28:38                      ~ "Chemicals",
      as.integer(hs2) %in% 1:24                       ~ "Agro-processing",
      as.integer(hs2) %in% 72:85                      ~ "Metal Fabrication",
      as.integer(hs2) %in% c(44:48, 94)               ~ "Forestry",
      as.integer(hs2) %in% c(49, 97) | hs4 == "9504"  ~ "CCI",
      as.integer(hs2) %in% c(41:43, 50:65)            ~ "CTFL",
      TRUE                                            ~ "Non-priority Sector"
    )
  )

# -----------------------------------------------------------------------------
# 2.3 Identify Competitive Goods (2007)
# -----------------------------------------------------------------------------
candidates_scored_2007 <- zaf_2007 %>% 
  filter(M_cp == 1) %>% # RCA_cp > 0.5 -> competitive products; under < 0.5 -> underdeveloped
  mutate(sector = substr(as.character(prod_code), 1, 2))

head(candidates_scored_2007)

# -----------------------------------------------------------------------------
# 2.4 Export Competitive Goods (2007) to CSV
# -----------------------------------------------------------------------------
competitive_all_2007 <- candidates_scored_2007 %>%
  arrange(desc(PGI_Z)) %>%
  mutate(
    RANK = row_number()
  )

write.csv2(
  competitive_all_2007, 
  file = "outputs/tables/competitive_all_2007.csv", 
  row.names = FALSE
)

competitive_means_2007 <- competitive_all_2007 %>%
  summarise(
    n_products     = n(),
    mean_PCI       = mean(pci_p, na.rm = TRUE),
    median_PCI     = median(pci_p, na.rm = TRUE),
    mean_dens      = mean(dens_cp, na.rm = TRUE),
    median_dens    = median(dens_cp, na.rm = TRUE),
    mean_COG_Z     = mean(COG_Z, na.rm = TRUE),
    median_COG_Z   = median(COG_Z, na.rm = TRUE),
    mean_PGI_Z     = mean(PGI_Z, na.rm = TRUE),
    median_PGI_Z   = median(PGI_Z, na.rm = TRUE)
  ) %>%
  transmute(
    "N (competitive GOODS)"               = n_products,
    "MEAN PCI"                            = mean_PCI,
    "MEDIAN PCI"                          = median_PCI,
    "MEAN DENSITY"                        = mean_dens,
    "MEDIAN DENSITY"                      = median_dens,
    "MEAN OPPORTUNITY GAIN (Z-SCORE)"     = mean_COG_Z,
    "MEDIAN OPPORTUNITY GAIN (Z-SCORE)"   = median_COG_Z,
    "MEAN PGI (Z-SCORE)"                  = mean_PGI_Z,
    "MEDIAN PGI (Z-SCORE)"                = median_PGI_Z,
  )

write.csv2(
  competitive_means_2007,
  file = "outputs/tables/competitive_all_means_2007.csv",
  row.names = FALSE
)

N_total_2007 <- nrow(candidates_scored_2007)

# -----------------------------------------------------------------------------
# 2.5 Build Plot: Competitive Products (2007)
# -----------------------------------------------------------------------------
competitives_plot_2007 <- ggplot() +
  # POLICY-PREFERRED QUADRANT
  # High feasibility (Density >= 0)
  # Inclusive outcomes (PGI <= 0)
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_2007),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = "red1",
    alpha = 0.05
  ) +
  # Products (grey, full opportunity semantics)
  geom_point(
    data = candidates_scored_2007,
    aes(
      x = PGI_Z,
      y = pci_p,
      colour = policy_sector,
      size  = dens_cp,
    ),
    alpha = 0.6,
  ) +
  # Density legend
  scale_size(
    name = "Density\n(Unstandardised)",
    range = c(0.01, 7),
    limits = range(zaf_2007$dens_cp),
    breaks = c(0.1, 0.15, 0.3),
    labels = c("0.1", "0.15", "0.3")
  ) +
  # Sector colours
  scale_colour_manual(
    values = c(
      "Agro-processing"    = "#009E73",
      "Chemicals"          = "#D55E00",
      "Forestry"           = "#6A8F00",
      "Motor Vehicles"     = "#E69F00",
      "CCI"                = "#5C3D99",
      "CTFL"               = "#56B4E9",
      "Metal Fabrication"  = "black",
      "Non-priority Sector" = "grey80"
    ),
    name = "Policy sectors", 
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  # Fix legend hollow / filled rendering ONCE
  guides(
    size = guide_legend(
      override.aes = list(
        colour = "grey50",
        alpha = 1
      )
    )
  ) +
  # Labels & theme
  labs(
    title = "Competitive Products (2007) with Sector Positions of Targeted by Industrial Policy",
    x = "PGI (standardised)",
    y = "Product Complexity Index (PCI)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

competitives_plot_2007

# -----------------------------------------------------------------------------
# 2.6 Save Plot: PCI-Density-PGI Competitive Goods 2007
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_D2_PCI-Density-PGI_competitive_2007.png",
  plot = last_plot() + theme(plot.title = element_blank()),
  width = 10,
  height = 7,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 3 Combination of Plots
# -----------------------------------------------------------------------------
## Legend removal and extraction
competitives_plot_2007 <- competitives_plot_2007 +
  theme(legend.position = "none")

competitives_plot_2024 <- competitives_plot_2024 +
  theme(
    legend.background     = element_rect(fill = "white", colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    legend.key            = element_rect(fill = "white", colour = NA),
    legend.position       = "bottom"
  )

get_legend <- function(p) {
  g <- ggplotGrob(p)
  g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
}

legend_competitives <- get_legend(competitives_plot_2024)

legend_competitives_bg <- grobTree(
  rectGrob(gp = gpar(fill = "white", col = NA)),
  legend_competitives
)

competitives_plot_2024 <- competitives_plot_2024 + theme(legend.position = "none")

competitives_plot_2007 <- competitives_plot_2007 +
  labs(title = "(a) 2007",
       subtitle = NULL)
competitives_plot_2024 <- competitives_plot_2024 +
  labs(title = "(b) 2024",
       subtitle = NULL)

strip_margin <- theme(plot.margin = margin(2, 2, 2, 2))

## Title set-up
title_grob <- grobTree(
  rectGrob(gp = gpar(fill = "white", col = NA)),
  textGrob(
    "Density-PCI-PGI of South Africa's Competitive Goods (2007 vs. 2024)",
    y = unit(0.68, "npc"),
    gp = gpar(fontsize = 15, fontface = "bold")
  ),
  vp = viewport(
    layout = grid.layout(1, 1),
    gp = gpar()
  )
)

## Panel set-up
panel_grob <- arrangeGrob(
  competitives_plot_2007,
  competitives_plot_2024,
  ncol = 2
)

## Actual combination
combined_competitives_plot <- arrangeGrob(
  panel_grob,
  legend_competitives_bg,
  ncol = 1,
  heights = unit(c(8.8, 0.9), "null")
)

combined_competitives_plot

# -----------------------------------------------------------------------------
# 3.1 Save Plot: Density-PCI-PGI Competitives Combined (2007-2024)
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_D_Density-PCI-PGI_Competitives_Combined.png",
  combined_competitives_plot,
  width = 14,
  height = 7,
  dpi = 300
)
