# -----------------------------------------------------------------------------
# 0 Preparation
# -----------------------------------------------------------------------------
pre_processed <- read.csv("data/data_processed/cp_df_gini.csv")

# -----------------------------------------------------------------------------
# 1 Transitory Goods for 2024
# -----------------------------------------------------------------------------
zaf <- pre_processed %>% 
  filter(country_iso3 == "ZAF", year == 2024)

# -----------------------------------------------------------------------------
# 1.1 Products ranking 
# -----------------------------------------------------------------------------
## - Feasibility: Relatedness density (dens_cp) -> Z
## - Sophistication: PCI (PCI_p) -> already standardised -> changed into COG
## - Inclusion: PGI (see later once PGI is calculated) -> Z
zaf <- zaf %>% 
  mutate(
    DENS_Z    = (dens_cp - mean(dens_cp)) / sd(dens_cp),
    COG_Z     = (cog_cp - mean(cog_cp)) / sd(cog_cp),
    EBS       = rowMeans(cbind(DENS_Z, COG_Z, -PGI_Z), na.rm = TRUE), # lowest PGI is desirable
  )

# -----------------------------------------------------------------------------
# 1.2 Selecting Transitory Goods 
# -----------------------------------------------------------------------------
candidates_scored_transitory <- zaf %>% 
  filter(M_cp == 0, RCA_cp > 0.5) %>% # RCA_cp > 0.5 -> transitory products; under < 0.5 -> underdeveloped
  mutate(sector = substr(as.character(prod_code), 1, 2))

head(candidates_scored_transitory)

# -----------------------------------------------------------------------------
# 1.3 Group Products by Targeted Policy Sector 
# -----------------------------------------------------------------------------
candidates_scored_transitory <- candidates_scored_transitory %>%
  mutate(
    prod_code_str = sprintf("%04d", prod_code),
    hs2 = as.integer(substr(as.character(prod_code), 1, 2)),
    hs4 = as.integer(substr(as.character(prod_code), 1, 4)),
    policy_sector = case_when(
      hs2 %in% 86:89                    ~ "Motor Vehicles",
      hs2 %in% 28:38                    ~ "Chemicals",
      hs2 %in% 1:24                     ~ "Agro-processing",
      hs2 %in% 72:85                    ~ "Metal Fabrication",
      hs2 %in% c(44:48, 94)             ~ "Forestry",
      hs2 %in% c(49, 97) | hs4 == 9504  ~ "CCI",
      hs2 %in% c(41:43, 50:65)          ~ "CTFL",
      TRUE                              ~ "Non-priority Sector"
      )
  )
 
# -----------------------------------------------------------------------------
# 1.4 Export Transitory Goods to CSV
# -----------------------------------------------------------------------------
transitory_all <- candidates_scored_transitory %>%
  arrange(desc(EBS)) %>%
  mutate(
    RANK = row_number()
  )

write.csv2(
  transitory_all, 
  file = "outputs/tables/transitory_all.csv", 
  row.names = FALSE
)

N_total_transitory <- nrow(transitory_all)

transitory_means <- transitory_all %>%
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
  transitory_means,
  file = "outputs/tables/Appendix_5_Transitory_All_Means.csv",
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 2 Build Plot: Density versus PCI of Transitory Export Goods
# -----------------------------------------------------------------------------
p_G1 <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  # Grey cloud: all transitory goods
  geom_point(
    data = candidates_scored_transitory,
    aes(x = DENS_Z, y = pci_p, colour = policy_sector),
    size = 3,
    alpha = 0.7
  ) +
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
  labs(
    title = "Density versus PCI of Transitory Export Goods",
    x = "Density (standardised)",
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

# -----------------------------------------------------------------------------
# 2.1 Save Plot: PCI-Density Transitory
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_G1_PCI-Density_Transitory.png",
  plot = p_G1 + theme(plot.title = element_blank()),
  width = 10,
  height = 7,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 3 Build Plot: PGI versus PCI of Transitory Export Goods
# -----------------------------------------------------------------------------
p_G2 <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  # Grey cloud: all transitory goods
  geom_point(
    data = candidates_scored_transitory,
    aes(x = PGI_Z, y = pci_p, colour = policy_sector),
    size = 3,
    alpha = 0.7
  ) +
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
  labs(
    title = "PGI versus PCI of Transitory Export Goods",
    x = "Product Gini Index (PGI, standardised)",
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

# -----------------------------------------------------------------------------
# 3.1 Save Plot: PCI-PGI Transitory
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_G2_PCI-PGI_Transitory.png",
  plot = p_G2 + theme(plot.title = element_blank()),
  width = 10,
  height = 7,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 4. Plot: PCI versus Opportunity Gain of Transitory Export Goods
# -----------------------------------------------------------------------------
p_H1 <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  # Grey cloud: all transitory goods
  geom_point(
    data = candidates_scored_transitory,
    aes(x = COG_Z, y = pci_p, colour = policy_sector),
    size = 3,
    alpha = 0.7
  ) +
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
  labs(
    title = "PCI versus Opportunity Gain of Transitory Export Goods",
    x = "Opportunity Gain (standardised)",
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

# -----------------------------------------------------------------------------
# 4.1 Save Plot: PCI-Opportunity Gain Transitory
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_H1_PCI-Opportunity_Gain_Transitory.png",
  plot = p_H1 + theme(plot.title = element_blank()),
  width = 10,
  height = 7,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 5 Build Plot: Density Space Transitory
# -----------------------------------------------------------------------------
p_H2 <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  # POLICY-PREFERRED QUADRANT
  # High feasibility (Density >= 0)
  # Inclusive outcomes (PGI <= 0)
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  # Products (grey, full opportunity semantics)
  geom_point(
    data = candidates_scored_transitory,
    aes(
      x = PGI_Z,
      y = pci_p,
      colour = policy_sector,
      size  = dens_cp,
    ),
    alpha = 0.7,
  ) +
  # Density legend
  scale_size(
    name = "Density\n(unstandardised)",
    range = c(0.1, 10),
    limits = range(zaf$dens_cp),
    breaks = c(0.1, 0.15),
    labels = c("0.1", "0.15")
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
  # Fix legend hollow / filled rendering
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
    title = "Density Space",
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

# -----------------------------------------------------------------------------
# 5.1 Save Plot: PGI-PCI-Density Transitory
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_H2_PGI-PCI-Density_Transitory.png",
  plot = p_H2 + theme(plot.title = element_blank()),
  width = 10,
  height = 7,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 6 Ranking of Candidates Transitory
# -----------------------------------------------------------------------------
shortlist_sectors_gini_transitory <- candidates_scored_transitory %>% 
  filter(pci_p > 0, EBS > 0, PGI_Z < -1, COG_Z > 0) %>%  #previously COG_Z > 1, but doesn't need to be so transformational
  arrange(desc(EBS))

shortlist_transitory_final <- shortlist_sectors_gini_transitory %>%
  arrange(desc(EBS)) %>%
  mutate(
    RANK = row_number()
  ) %>%
  transmute(
    RANK                         = RANK,
    "PRODUCT CODE"               = prod_code,
    "PRODUCT DESCRIPTION"        = prod_descr,
    RCA                          = RCA_cp,
    PCI                          = pci_p,
    "RELATEDNESS DENSITY"        = dens_cp,
    "OPPORTUNITY GAIN (Z-SCORE)" = COG_Z,
    "PGI (Z-SCORE)"              = PGI_Z,
    "ECONOMIC BENEFIT SCORE"     = EBS,
  )

head(shortlist_transitory_final)

# -----------------------------------------------------------------------------
# 6.1 Export Selection of Transitory Goods to CSV
# -----------------------------------------------------------------------------
write.csv2(
  shortlist_transitory_final, 
  file = "outputs/tables/Appendix_6_Shortlist_Transitory_Final.csv", 
  row.names = FALSE,
)

# -----------------------------------------------------------------------------
# 7 Build Plot: Combined Plots
# -----------------------------------------------------------------------------
# 7.1.1 Redefining Subplots (Figure G)
# -----------------------------------------------------------------------------
## Plot A
p_density_pci_transitory <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  geom_point(
    data = candidates_scored_transitory,
    aes(x = DENS_Z, y = pci_p, colour = policy_sector),
    size = 1.5,
    alpha = 0.7
  ) +
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
  labs(
    title = "a)",
    x = "Density (standardised)",
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

p_density_pci_transitory

## Plot B
p_pgi_pci_transitory <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  # Grey cloud: all transitory goods
  geom_point(
    data = candidates_scored_transitory,
    aes(x = PGI_Z, y = pci_p, colour = policy_sector),
    size = 1.5,
    alpha = 0.7
  ) +
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
  labs(
    title = "b)",
    x = "Product Gini Index (PGI, standardised)",
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

p_pgi_pci_transitory

# -----------------------------------------------------------------------------
# 7.1.2 Combination
# -----------------------------------------------------------------------------
## Legend removal and extraction
p_density_pci_transitory <- p_density_pci_transitory +
  theme(legend.position = "none")

p_pgi_pci_transitory <- p_pgi_pci_transitory +
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

legend_density <- get_legend(p_pgi_pci_transitory)

legend_density_bg <- grobTree(
  rectGrob(gp = gpar(fill = "white", col = NA)),
  legend_density
)

p_pgi_pci_transitory <- p_pgi_pci_transitory + theme(legend.position = "none")

strip_margin <- theme(plot.margin = margin(2, 2, 2, 2))

## Panel set-up
panel_grob_G <- arrangeGrob(
  p_density_pci_transitory,
  p_pgi_pci_transitory,
  nrow = 2,
  ncol = 1
)

combined_plot_G <- arrangeGrob(
  panel_grob_G,
  legend_density_bg,
  ncol = 1,
  heights = unit(c(9, 1), "null")
)
# -----------------------------------------------------------------------------
# 7.1.3 Save Plot G
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_G_transitory_SR.png",
  combined_plot_G,
  width = 10,
  height = 12,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 7.2.1 Redefining Subplots (Figure H)
# -----------------------------------------------------------------------------
## Plot A
p_cog_pci_transitory <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  # Grey cloud: all transitory goods
  geom_point(
    data = candidates_scored_transitory,
    aes(x = COG_Z, y = pci_p, colour = policy_sector),
    size = 1.5,
    alpha = 0.7
  ) +
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
  labs(
    title = "a)",
    x = "Opportunity Gain (standardised)",
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

p_cog_pci_transitory

## Plot B
p_density_space_transitory <- ggplot() +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("N = ", N_total_transitory),
    hjust = 1.1,
    vjust = 1.5,
    size  = 4,
    colour = "grey30"
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = 0,
    ymin = 0, ymax = Inf,
    fill = "goldenrod1",
    alpha = 0.25
  ) +
  geom_point(
    data = candidates_scored_transitory,
    aes(
      x = PGI_Z,
      y = pci_p,
      colour = policy_sector,
      size  = dens_cp,
    ),
    alpha = 0.7,
    stroke = 1.2
  ) +
  scale_size(
    name = "(Graph d)\nRelatedness Density\n(unstandardised)",
    range = c(0.1, 10),
    limits = range(zaf$dens_cp),
    breaks = c(0.1, 0.15),
    labels = c("0.1", "0.15")
  ) +
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
  ) +
  guides(
    size = guide_legend(
      order = 1,
      override.aes = list(
        colour = "grey80",
        alpha = 1,
        size = 4
      )
    ),
    colour = guide_legend(
      order = 2,
      override.aes = list(
        size = 4,
        alpha = 0.7
      )
    )
  ) +
  labs(
    title = "b)",
    x = "PGI (standardised)",
    y = "Product Complexity Index (PCI)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    
    legend.spacing.x = unit(0.5, "cm") 
  )

p_density_space_transitory

# -----------------------------------------------------------------------------
# 7.2.2 Combination
# -----------------------------------------------------------------------------
## Legend removal and extraction
p_cog_pci_transitory <- p_cog_pci_transitory +
  theme(legend.position = "none")

p_density_space_transitory <- p_density_space_transitory +
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

legend_density <- get_legend(p_density_space_transitory)

legend_density_bg <- grobTree(
  rectGrob(gp = gpar(fill = "white", col = NA)),
  legend_density
)

p_density_space_transitory <- p_density_space_transitory + theme(legend.position = "none")

strip_margin <- theme(plot.margin = margin(2, 2, 2, 2))

## Title set-up
title_grob <- grobTree(
  rectGrob(gp = gpar(fill = "white", col = NA)),
  textGrob(
    "transitory Goods (South Africa, 2024)",
    y = unit(0.68, "npc"),
    gp = gpar(fontsize = 15, fontface = "bold")
  ),
  vp = viewport(
    layout = grid.layout(1, 1),
    gp = gpar()
  )
)

## Panel set-up
panel_grob_H <- arrangeGrob(
  p_cog_pci_transitory,
  p_density_space_transitory,
  nrow = 2,
  ncol = 1
)

combined_plot_H <- arrangeGrob(
  panel_grob_H,
  legend_density_bg,
  ncol = 1,
  heights = unit(c(9, 1), "null")
)

# -----------------------------------------------------------------------------
# 7.2.3 Save Plot H
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_H_transitory_LR.png",
  combined_plot_H,
  width = 10,
  height = 12,
  dpi = 300
)

