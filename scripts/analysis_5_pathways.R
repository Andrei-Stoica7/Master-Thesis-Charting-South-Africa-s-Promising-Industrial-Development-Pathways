# -----------------------------------------------------------------------------
# ANALYSIS 5: PRODUCT SPACE STRUCTURE (TRANSITORY + UNDEVELOPED)
# -----------------------------------------------------------------------------

# Load processed data
cp_df <- read.csv("data/data_processed/cp_df_gini.csv")

# Extract South Africa 2024
zaf <- cp_df %>% 
  filter(country_iso3 == "ZAF", year == 2024)

# -----------------------------------------------------------------------------
# 1 Load shortlisted products
# -----------------------------------------------------------------------------
transitory <- read.csv2("outputs/tables/Appendix_6_Shortlist_Transitory_Final.csv")
undeveloped <- read.csv2("outputs/tables/Appendix_4_Shortlist_Undeveloped_Final.csv")

# Detect correct product column automatically
prod_col_trans <- "PRODUCT.CODE"
prod_col_undev <- "PRODUCT.CODE"

names(transitory)
names(undeveloped)

# Combine product codes safely
products_all <- unique(c(
  as.character(transitory[[prod_col_trans]]),
  as.character(undeveloped[[prod_col_undev]])
))

products_all <- sprintf("%04d", as.integer(products_all))

# -----------------------------------------------------------------------------
# 2 Extract proximity matrix (GLOBAL → SUBSET)
# -----------------------------------------------------------------------------

# Ensure prox_2024 is available (comes from preprocessing)
prox_mat <- as.matrix(
  read.csv("data/data_processed/proximity_2024.csv", row.names = 1)
)

# Clean names first
clean_col <- gsub("[^0-9]", "", colnames(prox_mat))
clean_row <- gsub("[^0-9]", "", rownames(prox_mat))

# Then format
colnames(prox_mat) <- sprintf("%04d", as.integer(clean_col))
rownames(prox_mat) <- sprintf("%04d", as.integer(clean_row))

rownames(prox_mat) <- sprintf("%04d", as.integer(rownames(prox_mat)))

transitory[[prod_col_trans]] <- sprintf("%04d", as.integer(transitory[[prod_col_trans]]))
undeveloped[[prod_col_undev]] <- sprintf("%04d", as.integer(undeveloped[[prod_col_undev]]))

products_available <- intersect(products_all, colnames(prox_mat))

if (length(products_available) < length(products_all)) {
  warning(paste(
    "Dropped",
    length(products_all) - length(products_available),
    "products not found in proximity matrix"
  ))
}

prox_combined <- prox_mat[
  products_available,
  products_available
]

# Save matrix
write.csv2(
  prox_combined,
  "outputs/tables/proximity_combined_shortlist.csv",
  row.names = TRUE
)

# -----------------------------------------------------------------------------
# 3 Build network
# -----------------------------------------------------------------------------
# Threshold
threshold_moderate <- 0.55
threshold_high <- 0.65

# Keep only moderate and high links
adj <- prox_combined
adj[adj < threshold_moderate] <- 0


g <- graph_from_adjacency_matrix(
  adj,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# -----------------------------------------------------------------------------
# 4 Add node attributes (TYPE: transitory vs undeveloped)
# -----------------------------------------------------------------------------
V(g)$type <- ifelse(
  V(g)$name %in% transitory[[prod_col_trans]],
  "Transitory",
  "Undeveloped"
)

# Remove isolates
g <- delete_vertices(g, degree(g) == 0)

# Convert
g_tbl <- as_tbl_graph(g)


g_tbl <- g_tbl %>%
  activate(edges) %>%
  mutate(
    proximity_type = ifelse(
      weight >= threshold_high,
      "High proximity",
      "Moderate proximity"
    ),
    proximity_type = trimws(proximity_type),   
    proximity_type = factor(
      proximity_type,
      levels = c("High proximity", "Moderate proximity")
    )
  )


g_tbl %>%
  activate(edges) %>%
  pull(proximity_type) %>%
  unique()

# -----------------------------------------------------------------------------
# 5 Plot network
# -----------------------------------------------------------------------------
set.seed(123)

p <- ggraph(g_tbl, layout = "fr", niter = 4000) +
  
  # Edges
    geom_edge_link(aes(
      edge_colour = proximity_type),
      width = 1.2,
      alpha = 0.9,
      show.legend = TRUE,
      )+
  
  # Nodes
  geom_node_point(aes(
    fill = type),
    size = 15,
    shape = 21,
    colour = "black",
    stroke = 0.8,
    position = position_jitter(width = 0.02, height = 0.02)
    ) +
  
  # Labels
  geom_node_text(aes(
    label = name),
    size = 3.5,
    colour = "white",
    fontface = "bold") +
  
  
  # Node colours
  scale_fill_manual(
    name = "Good type",
    breaks = c("Transitory", "Undeveloped"),
    values = c(
      "Transitory" = "#0072B2",
      "Undeveloped" = "#56B5E9"
    ),
    drop = FALSE
  ) +
  
  # Edge colours
  scale_edge_colour_manual(
    name = "Proximity",
    breaks = c("High proximity", "Moderate proximity"),
    labels =  c(
    "High proximity (≥ 0.65)",
    "Moderate proximity (≥ 0.55)"
    ),
    values = c(
      "High proximity" = "#2FB45C",
      "Moderate proximity" = "#F6BB27"
    ),
    drop = FALSE
  ) +

  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    legend.position = "bottom",
    
    legend.box = "horizontal",    
    legend.direction = "horizontal",
    legend.box.just = "left",
  
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.title.position = "top",
    legend.title.align = 0,

    legend.key.width = unit(1.5, "cm"),
    legend.spacing.x = unit(0.8, "cm")
  ) +
  
  guides(
    fill = guide_legend(
      order = 1,
      nrow = 1,
      override.aes = list(
        size = 6,
        alpha = 1
      )
    ),
    edge_colour = guide_legend(
      order = 2,
      nrow = 1,
      override.aes = list(
        linewidth = 4,
        alpha = 1
      )
    )
  )

p

# Save figure
ggsave(
  "outputs/figures/Figure_I_Product_Space_Network.png",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300
)

# -----------------------------------------------------------------------------
# END ANALYSIS 5
# -----------------------------------------------------------------------------
