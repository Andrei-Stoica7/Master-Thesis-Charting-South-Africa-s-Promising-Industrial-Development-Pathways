# -----------------------------------------------------------------------------
# 0 Preparation
# -----------------------------------------------------------------------------
pre_processed <- read.csv("data/data_processed/cp_df_ipap.csv")

# -----------------------------------------------------------------------------
# 1 Descriptives Part 1
# -----------------------------------------------------------------------------
# 1.1 Add ECI rank per country-year (higher ECI = higher rank) and select BRICS & Neighbours (SACU)
# -----------------------------------------------------------------------------
pre_processed <- pre_processed %>%
  group_by(year) %>%
  mutate(
    ECI_rank = dense_rank(desc(eci_c))
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 1.2 Define Country Groups
# -----------------------------------------------------------------------------
brics <- c(
  "BRA", # Brazil
  "CHN", # China
  "IND", # India
  "RUS", # Russia
  "ZAF"  # South Africa
)

neighbours <- c(
  "SWZ", # Eswatini (SACU)
  "NAM", # Namibia (SACU)
  "BWA", # Botswana (SACU)
  "LSO"  # Lesotho (SACU)
)

selected_countries <- c(brics, neighbours)
country_names <- c(
  BRA = "Brazil",
  CHN = "China",
  IND = "India",
  RUS = "Russia",
  ZAF = "South Africa",
  SWZ = "Eswatini",
  NAM = "Namibia",
  BWA = "Botswana",
  LSO = "Lesotho"
)

# -----------------------------------------------------------------------------
# 1.3 Filter Dataset for Selection & perpare ECI Ranking
# -----------------------------------------------------------------------------
cp_df_brics_neighbours <- pre_processed %>%
  filter(country_iso3 %in% selected_countries)

eci_rank_df <- cp_df_brics_neighbours %>%
  distinct(country_iso3, year, ECI_rank)

# -----------------------------------------------------------------------------
# 1.4 Build Plot: ECI Rank Evolution from 2007 to 2024 (BRICS and SACU)
# -----------------------------------------------------------------------------
p_eci_rank <- ggplot(
  eci_rank_df,
  aes(
    x = year,
    y = ECI_rank,
    color = country_iso3,
    group = country_iso3
  )
) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_reverse() +
  labs(
    title = "ECI Rank Evolution from 2007 to 2024 (BRICS and SACU)",
    x = "Year",
    y = "ECI Rank",
    color = "Country"
  ) +
  scale_color_viridis_d(
    option = "C",
    labels = country_names,
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

p_eci_rank

# -----------------------------------------------------------------------------
# 1.5 Save Plot
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_A_ECI_Rank_Evolution.png",
  plot = p_eci_rank + theme(plot.title = element_blank()),
  width = 10,
  height = 8,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 2 Descriptive Part 2
# -----------------------------------------------------------------------------
# Filter for South Africa
zaf <- pre_processed %>% 
  filter(country_iso3 == "ZAF") %>%
  mutate(
    hs2 = as.integer(substr(prod_code, 1, 2)),
    hs3 = as.integer(substr(prod_code, 1, 3))
  )

head(zaf)

# -----------------------------------------------------------------------------
# 2.1 Identify Top 15 Export Products (South Africa)
# -----------------------------------------------------------------------------
top15_products <- zaf %>%
  group_by(prod_code) %>%
  summarise(
    total_exports = sum(tot_exp_p, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_exports)) %>%
  slice_head(n = 15) %>%
  pull(prod_code)

# -------------------------------------------------------------------------
# 2.2 Build RCA time series ONLY
# -------------------------------------------------------------------------
rca_ts <- zaf %>%
  filter(
    prod_code %in% top15_products,
    year >= 2007,
    year <= 2024
  ) %>%
  group_by(prod_code, year) %>%
  summarise(
    RCA = mean(RCA_cp, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 2.3 Facet order by export volume
# -------------------------------------------------------------------------
facet_order <- zaf %>%
  filter(prod_code %in% top15_products) %>%
  group_by(prod_code) %>%
  summarise(total_exports = sum(tot_exp_p, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_exports)) %>%
  pull(prod_code)

rca_ts$prod_code <- factor(rca_ts$prod_code, levels = facet_order)

# -------------------------------------------------------------------------
# 2.4 Plot RCA's of top 15 Export Products
# -------------------------------------------------------------------------
p_rca_top15 <- ggplot(rca_ts, aes(x = year, y = RCA, group = prod_code)) +
  
  # RCA = 1 reference line
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  
  # RCA lines
  geom_line(colour = "black", linewidth = 1) +
  
  # Points
  geom_point(colour = "black", size = 1.5) +
  
  # Facets
  facet_wrap(~ prod_code, scales = "free_y", ncol = 3) +
  
  labs(
    title = "RCA Evolution of South Africa’s Top 15 Export Products (2007–2024)",
    x = "Year",
    y = "Revealed Comparative Advantage (RCA)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

p_rca_top15

# -----------------------------------------------------------------------------
# 2.5 Save Plot
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_B_RCA_Exports_Evolution.png",
  plot = p_rca_top15 + theme(plot.title = element_blank()),
  width = 10,
  height = 8,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 2.6 Appendix: Full HS Product Description for Top 15 Export Products
# -----------------------------------------------------------------------------
appendix_products <- zaf %>%
  filter(prod_code %in% top15_products) %>%
  group_by(prod_code, prod_descr) %>%
  summarise(
    total_exports = sum(tot_exp_p, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_exports)) %>%
  mutate(
    RANK = row_number(),
    total_exports = formatC(
      total_exports,
      format = "f",
      big.mark = ",",
      digits = 0
    )) %>%
  transmute(
    RANK                         = RANK,
    "HS CODE"                    = prod_code,
    "PRODUCT DESCRIPTION"        = prod_descr,
    "TOTAL EXPORTS (2007–2024)\n(in thousands USD; raw)"  = total_exports
  )

appendix_products

# -----------------------------------------------------------------------------
# 2.7 Save Top 15 Export Products as CSV
# -----------------------------------------------------------------------------
write.csv(
  appendix_products,
  "outputs/tables/Appendix_1_Top15_Export_Goods_Full_Descriptions.csv",
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 3 Section Policy Evaluation
# -----------------------------------------------------------------------------
# 3.1 IPAPs
# -----------------------------------------------------------------------------
## Clothing, Textiles, Footwear & Leather
ctfl_hs2 <- c(41:43, 50:65)
ctfl_data <- zaf %>% filter(hs2 %in% ctfl_hs2)

head(ctfl_data)

## Motor vehicles and components
motor_hs2 <- 86:89
motor_data <- zaf %>% filter(hs2 %in% motor_hs2)

head(motor_data)

## Chemicals, Plastics, Pharma
chem_hs2 <- 28:38
chem_data <- zaf %>% filter(hs2 %in% chem_hs2)

head(chem_data)

## Agro-processing
agro_hs2 <- 1:24
agro_data <- zaf %>% filter(hs2 %in% agro_hs2)

head(agro_data)

## Metals, Machinery, Transport
metal_hs2 <- 72:85
metal_data <- zaf %>% filter(hs2 %in% metal_hs2)

head(metal_data)

## Forestry, timber, paper & furniture
forestry_hs2 <- c(44:48, 94) # includes furniture as both are continued in the Master Plans
forestry_data <- zaf %>% filter(hs2 %in% forestry_hs2)

head(forestry_data)

## Green industries -> HS3 code doesn't specify them

# -----------------------------------------------------------------------------
# 3.2 Master Plans
# -----------------------------------------------------------------------------
## R-CTFL, forestry & furntire, poultry, sugar, steel & metals, auto are already included under the IPAPs
## Culture and creative Industries
cci_hs2 <- c(49, 97)   # press, books and works of art
cci_hs4 <- c(9504)     # videogames and consoles
cci_data <- zaf %>%
  filter(hs2 %in% cci_hs2 | prod_code %in% cci_hs4)

head(cci_data)

# -----------------------------------------------------------------------------
# 3.3 Build a dataframe with data by sector and year
# -----------------------------------------------------------------------------
sector_data <- bind_rows(
  ctfl_data      %>% mutate(sector = "CTFL"),              #IPAP
  metal_data     %>% mutate(sector = "Metal Fabrication"), #IPAP
  agro_data      %>% mutate(sector = "Agro-processing"),   #IPAP
  chem_data      %>% mutate(sector = "Chemicals"),         #IPAP
  motor_data     %>% mutate(sector = "Motor Vehicles"),    #IPAP
  forestry_data  %>% mutate(sector = "Forestry"),          #IPAP
  cci_data       %>% mutate(sector = "CCI"),               #Master Plan
  #green_data     %>% mutate(sector = "Green Industries")
) 

# -----------------------------------------------------------------------------
# 3.4 Sector-level RCA–PCI time series (export-weighted)
# -----------------------------------------------------------------
sector_rca_ts <- sector_data %>%
  filter(year >= 2007, year <= 2024) %>%
  group_by(sector, year) %>%
  summarise(
    RCA = sum(RCA_cp * s_cp, na.rm = TRUE),   # export-weighted RCA
    .groups = "drop"
  ) %>%
  arrange(sector, year)

# -----------------------------------------------------------------------------
# 3.5 Subsector (HS2) RCA time series (export-weighted)
# -----------------------------------------------------------------------------
subsector_rca_ts <- sector_data %>%
  filter(year >= 2007, year <= 2024) %>%
  group_by(sector, hs2, year) %>%
  summarise(
    RCA = sum(RCA_cp * s_cp, na.rm = TRUE),
    .groups = "drop"
  )

subsector_labels <- subsector_rca_ts %>%
  filter(year == 2024, RCA > 0.05) %>%
  group_by(sector) %>%
  ungroup() %>%
  mutate(label = paste0("HS", hs2))

# -----------------------------------------------------------------------------
# 3.6 Facet order by total sector export value
# -----------------------------------------------------------------------------
sector_order <- sector_rca_ts %>%
  filter(year == 2024) %>%
  group_by(sector) %>%
  summarise(RCA = mean(RCA), .groups = "drop") %>%  
  arrange(desc(RCA)) %>%
  pull(sector)

sector_rca_ts <- sector_rca_ts %>%
  mutate(sector = factor(sector, levels = sector_order))

subsector_rca_ts <- subsector_rca_ts %>%
  mutate(sector = factor(sector, levels = sector_order))

levels(sector_rca_ts$sector)

# -----------------------------------------------------------------------------
# 3.7 Build Plot: RCA Evolution of Sectors Targeted by Industrial Policy (2007–2024)
# -----------------------------------------------------------------------------
p_rca_sectors <- ggplot(sector_rca_ts, aes(x = year, y = RCA, group = sector)) +
# RCA = 1 reference line
geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
# Subsector lines (HS2)
geom_line(
  data = subsector_rca_ts,
  aes(group = paste(sector, hs2)),
  colour = "grey60",
  linewidth = 0.6,
  linetype = "dotted",
  alpha = 0.7
) +
# Sector RCA (main line)
geom_line(colour = "black", linewidth = 1.2, alpha = 0.7) +
  geom_point(colour = "black", size = 1.5, alpha = 0.7) +
# Axis spacing
scale_x_continuous(
  expand = expansion(mult = c(0.02, 0.25))
) +
# Facets
facet_wrap(
  ~ factor(sector, levels = sector_order),
  scales = "free_y",
  ncol = 4
) +
# Labels
labs(
  title = "RCA Evolution of Sectors Targeted by Industrial Policy (2007–2024)",
  subtitle = "Dotted lines show HS2 subsectors (RCA > 0.05); solid line shows export-weighted sector RCA",
  x = "Year",
  y = "Export-weighted Revealed Comparative Advantage (RCA)"
) +
# Theme
theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(size = 11)
  ) +
# Subsector labels
geom_text_repel(
  data = subsector_labels,
  aes(x = year, y = RCA, label = label),
  colour = "grey40",
  size = 2,
  direction = "y",
  nudge_x = 0.3,
  hjust = 0,
  segment.color = "grey70"
)

p_rca_sectors

# -----------------------------------------------------------------------------
# 3.8 Save Plot
# -----------------------------------------------------------------------------
ggsave(
  "outputs/figures/Figure_C_RCA_Targeted_Sectors.png",
  plot = p_rca_sectors + theme(plot.title = element_blank()) + theme(plot.subtitle = element_blank()),
  width = 10,
  height = 6,
  dpi = 300
)
