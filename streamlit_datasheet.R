# ==============================================================================
# STREAMLIT DATASHEET — MASTER OUTPUT SCRIPT
#
# Produces one Excel file with 4 tabs for Streamlit consumption:
#   Tab 1 — Species_Country_Detail  (from species_relative_growth_v3 logic)
#   Tab 2 — Flagged_Species         (from species_relative_growth_v3 logic)
#   Tab 3 — Jaccard                 (from jaccard_step1 logic)
#   Tab 4 — ENS                     (from StreamlitENS logic, pivoted)
#
# Inputs:
#   FAO_allyears_soc.csv                            → Sections B, C, D (top-40 id)
#   NEI_allocated_2000-2023_ALL_SCENARIOS_OPTIONS.csv → Section D (ENS)
#
# Key decisions:
#   - soc filter: 'yes' and 'yes not ss' only
#   - Taxon filter: PISCES (Finfish) + CRUSTACEA (Crustacean) only
#   - Individual count: VALUE × (Lower.indiv.ton + Upper.indiv.ton) / 2
#   - share_of_group_YYYY: species % of taxon-group total per year, all species
#   - ENS split by taxon group; consensus_trajectory collapses 3 scenarios
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(writexl)

# ==============================================================================
# PARAMETERS
# ==============================================================================

FAO_PATH    <- "/Users/chiaw/Documents/Diversification 2025/FAO/FAO_allyears_soc.csv"
NEI_PATH    <- "/Users/chiaw/Documents/Diversification 2025/FAO/Nei distributedog/NEI_allocated_2000-2023_ALL_SCENARIOS_OPTIONS.csv"
OUTPUT_PATH <- "/Users/chiaw/Documents/Diversification 2025/FAO/Streamlit_Datasheet.xlsx"

START_YEAR      <- 2019
END_YEAR        <- 2023
FLIP_THRESHOLD  <- 15
JACCARD_YEARS   <- 2017:2023
GROWTH_YEARS    <- START_YEAR:END_YEAR

ENS_SCENARIOS <- c("Conservative", "Medium", "Maximum")

# ==============================================================================
# SECTION A: LOAD RAW DATA
# ==============================================================================

cat("Loading FAO raw data...\n")
fao_raw <- read_csv(FAO_PATH, show_col_types = FALSE)
cat("FAO rows:", nrow(fao_raw), "\n")

cat("Loading NEI allocated data...\n")
nei_raw <- read_csv(NEI_PATH, show_col_types = FALSE)
cat("NEI rows:", nrow(nei_raw), "\n")

# Top 40 countries by 2023 total individuals (used by ENS)
top40_codes <- fao_raw %>%
  filter(
    soc %in% c("yes", "yes not ss"),
    Major_Group %in% c("PISCES", "CRUSTACEA"),
    PERIOD == 2023,
    !is.na(VALUE), VALUE > 0
  ) %>%
  mutate(indiv_mid = VALUE * (Lower.indiv.ton + Upper.indiv.ton) / 2) %>%
  filter(indiv_mid > 0) %>%
  group_by(COUNTRY.UN_CODE) %>%
  summarise(total_indiv_2023 = sum(indiv_mid, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total_indiv_2023, n = 40) %>%
  pull(COUNTRY.UN_CODE)

cat("Top 40 countries identified.\n")

# ==============================================================================
# SECTION B: JACCARD (2017-2023)
# ==============================================================================

cat("\n--- Section B: Jaccard ---\n")

df_jac <- fao_raw %>%
  filter(
    soc %in% c("yes", "yes not ss"),
    Major_Group %in% c("PISCES", "CRUSTACEA"),
    PERIOD %in% JACCARD_YEARS,
    !is.na(VALUE), VALUE > 0
  ) %>%
  mutate(indiv_mid = VALUE * (Lower.indiv.ton + Upper.indiv.ton) / 2) %>%
  filter(indiv_mid > 0)

spp_yr_jac <- df_jac %>%
  group_by(
    COUNTRY.UN_CODE,
    country_name = `Name_En.y`,
    SPECIES.ALPHA_3_CODE,
    PERIOD
  ) %>%
  summarise(indiv_mid = sum(indiv_mid, na.rm = TRUE), .groups = "drop")

# Exclude countries missing 2019 or 2023
has_both <- spp_yr_jac %>%
  filter(PERIOD %in% c(2019, 2023)) %>%
  distinct(COUNTRY.UN_CODE, PERIOD) %>%
  group_by(COUNTRY.UN_CODE) %>%
  summarise(n = n_distinct(PERIOD), .groups = "drop") %>%
  filter(n == 2) %>%
  pull(COUNTRY.UN_CODE)

spp_yr_jac <- spp_yr_jac %>% filter(COUNTRY.UN_CODE %in% has_both)
cat("Countries retained for Jaccard:", n_distinct(spp_yr_jac$COUNTRY.UN_CODE), "\n")

# Jaccard helper
compute_jaccard <- function(data, yr_from, yr_to) {
  d <- data %>%
    filter(PERIOD %in% c(yr_from, yr_to)) %>%
    pivot_wider(
      id_cols      = c(COUNTRY.UN_CODE, SPECIES.ALPHA_3_CODE),
      names_from   = PERIOD,
      names_prefix = "y",
      values_from  = indiv_mid,
      values_fill  = 0,
      values_fn    = sum
    )
  col_from <- paste0("y", yr_from)
  col_to   <- paste0("y", yr_to)
  if (!col_from %in% names(d)) d[[col_from]] <- 0
  if (!col_to   %in% names(d)) d[[col_to]]   <- 0

  totals <- d %>%
    group_by(COUNTRY.UN_CODE) %>%
    summarise(total_from = sum(.data[[col_from]]),
              total_to   = sum(.data[[col_to]]), .groups = "drop")

  d %>%
    left_join(totals, by = "COUNTRY.UN_CODE") %>%
    mutate(
      share_from = if_else(total_from > 0, .data[[col_from]] / total_from, 0),
      share_to   = if_else(total_to   > 0, .data[[col_to]]   / total_to,   0)
    ) %>%
    group_by(COUNTRY.UN_CODE) %>%
    summarise(num = sum(pmin(share_from, share_to)),
              den = sum(pmax(share_from, share_to)), .groups = "drop") %>%
    mutate(jaccard = if_else(den > 0, round(num / den, 3), NA_real_)) %>%
    select(COUNTRY.UN_CODE, jaccard)
}

j_main <- compute_jaccard(spp_yr_jac, 2019, 2023) %>%
  rename(jaccard_2019_2023 = jaccard)

yoy_pairs <- list(c(2017,2018), c(2018,2019), c(2019,2020),
                  c(2020,2021), c(2021,2022), c(2022,2023))

yoy_list <- lapply(yoy_pairs, function(p) {
  compute_jaccard(spp_yr_jac, p[1], p[2]) %>%
    rename(!!paste0("J_", p[1], "_", p[2]) := jaccard)
})
yoy_wide <- Reduce(function(a, b) left_join(a, b, by = "COUNTRY.UN_CODE"), yoy_list)

species_counts_jac <- spp_yr_jac %>%
  filter(PERIOD %in% c(2019, 2023)) %>%
  group_by(COUNTRY.UN_CODE, PERIOD) %>%
  summarise(
    n_species   = n_distinct(SPECIES.ALPHA_3_CODE[indiv_mid > 0]),
    total_indiv = sum(indiv_mid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PERIOD,
              values_from = c(n_species, total_indiv),
              names_glue = "{.value}_{PERIOD}")

country_names_jac <- spp_yr_jac %>% distinct(COUNTRY.UN_CODE, country_name)

jaccard_out <- country_names_jac %>%
  left_join(j_main,             by = "COUNTRY.UN_CODE") %>%
  left_join(yoy_wide,           by = "COUNTRY.UN_CODE") %>%
  left_join(species_counts_jac, by = "COUNTRY.UN_CODE") %>%
  mutate(manually_reviewed = COUNTRY.UN_CODE %in% top40_codes) %>%
  arrange(jaccard_2019_2023) %>%
  select(
    country_code = COUNTRY.UN_CODE, country_name,
    jaccard_2019_2023,
    J_2017_2018, J_2018_2019, J_2019_2020,
    J_2020_2021, J_2021_2022, J_2022_2023,
    n_species_2019, n_species_2023,
    total_indiv_2019, total_indiv_2023,
    manually_reviewed
  )

cat("Jaccard rows:", nrow(jaccard_out), "\n")

# ==============================================================================
# SECTION C: SPECIES RELATIVE GROWTH (2019-2023)
# ==============================================================================

cat("\n--- Section C: Species Relative Growth ---\n")

df <- fao_raw %>%
  filter(
    soc %in% c("yes", "yes not ss"),
    Major_Group %in% c("PISCES", "CRUSTACEA"),
    PERIOD %in% GROWTH_YEARS,
    !is.na(VALUE), VALUE > 0
  ) %>%
  rename(Country_name = `Name_En.y`, Species_name = `Name_En.x`) %>%
  mutate(
    taxon_group = case_when(
      Major_Group == "PISCES"    ~ "Finfish",
      Major_Group == "CRUSTACEA" ~ "Crustacean",
      TRUE                       ~ NA_character_
    )
  ) %>%
  filter(!is.na(taxon_group)) %>%
  mutate(
    indiv_lower = VALUE * Lower.indiv.ton,
    indiv_mid   = VALUE * (Lower.indiv.ton + Upper.indiv.ton) / 2,
    indiv_upper = VALUE * Upper.indiv.ton
  )

cat("Rows after filter:", nrow(df), "\n")

# Species x country x year totals
species_year <- df %>%
  group_by(COUNTRY.UN_CODE, Country_name, SPECIES.ALPHA_3_CODE, Species_name,
           Scientific_Name, genus, family, order, class, phylum,
           taxon_group, PERIOD) %>%
  summarise(
    total_lower = sum(indiv_lower, na.rm = TRUE),
    total_mid   = sum(indiv_mid,   na.rm = TRUE),
    total_upper = sum(indiv_upper, na.rm = TRUE),
    .groups = "drop"
  )

years           <- GROWTH_YEARS
year_cols_lower <- paste0("indiv_lower_", years)
year_cols_mid   <- paste0("indiv_mid_",   years)
year_cols_upper <- paste0("indiv_upper_", years)
all_year_cols   <- c(year_cols_lower, year_cols_mid, year_cols_upper)

species_wide <- species_year %>%
  pivot_wider(
    names_from  = PERIOD,
    values_from = c(total_lower, total_mid, total_upper),
    names_glue  = "indiv_{.value}_{PERIOD}"
  ) %>%
  rename_with(~ gsub("indiv_total_lower_", "indiv_lower_", .x)) %>%
  rename_with(~ gsub("indiv_total_mid_",   "indiv_mid_",   .x)) %>%
  rename_with(~ gsub("indiv_total_upper_", "indiv_upper_", .x)) %>%
  mutate(across(all_of(all_year_cols), ~ replace_na(.x, 0)))

# Country rank
col_mid_end  <- paste0("indiv_mid_", END_YEAR)
col_mid_start <- paste0("indiv_mid_", START_YEAR)
col_mid_end1  <- paste0("indiv_mid_", END_YEAR - 1)

country_rank <- species_wide %>%
  group_by(COUNTRY.UN_CODE, Country_name) %>%
  summarise(country_total_mid_end = sum(.data[[col_mid_end]], na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(country_total_mid_end)) %>%
  mutate(country_rank_by_production = row_number())

# Status
species_wide <- species_wide %>%
  mutate(
    status = case_when(
      .data[[col_mid_start]] == 0 &
        (.data[[col_mid_end]] > 0 | .data[[col_mid_end1]] > 0) ~ "Newcomer",
      .data[[col_mid_start]] > 0 &
        .data[[col_mid_end]] == 0 & .data[[col_mid_end1]] == 0  ~ "Exiting",
      .data[[col_mid_start]] > 0 &
        (.data[[col_mid_end]] > 0 | .data[[col_mid_end1]] > 0)  ~ "Stable",
      TRUE ~ "Blip"
    )
  )

# YoY rates and volatility
yoy_pairs_growth <- data.frame(from = years[-length(years)], to = years[-1])
for (r in 1:nrow(yoy_pairs_growth)) {
  cf <- paste0("indiv_mid_", yoy_pairs_growth$from[r])
  ct <- paste0("indiv_mid_", yoy_pairs_growth$to[r])
  rc <- paste0("yoy_", yoy_pairs_growth$from[r], "_", yoy_pairs_growth$to[r])
  species_wide[[rc]] <- ifelse(
    species_wide[[cf]] > 0,
    ((species_wide[[ct]] / species_wide[[cf]]) - 1) * 100,
    NA_real_
  )
}

rate_cols <- paste0("yoy_", yoy_pairs_growth$from, "_", yoy_pairs_growth$to)

species_wide <- species_wide %>%
  rowwise() %>%
  mutate(
    meaningful_flips = {
      rates <- c_across(all_of(rate_cols))
      flips <- 0
      for (j in 2:length(rates)) {
        r1 <- rates[j-1]; r2 <- rates[j]
        if (!is.na(r1) & !is.na(r2) & r1 != 0 & r2 != 0 &
            sign(r1) != sign(r2) &
            (abs(r1) > FLIP_THRESHOLD | abs(r2) > FLIP_THRESHOLD))
          flips <- flips + 1
      }
      flips
    }
  ) %>%
  ungroup() %>%
  mutate(is_volatile = meaningful_flips >= 2)

# CAGR
n_years <- END_YEAR - START_YEAR

species_wide <- species_wide %>%
  mutate(
    CAGR_simple = case_when(
      status != "Stable"                                     ~ NA_real_,
      .data[[col_mid_start]] > 0 & .data[[col_mid_end]] > 0 ~
        ((.data[[col_mid_end]] / .data[[col_mid_start]])^(1/n_years) - 1) * 100,
      TRUE ~ NA_real_
    ),
    baseline_avg_mid = (.data[[col_mid_start]] +
                          .data[[paste0("indiv_mid_", START_YEAR + 1)]]) / 2,
    endpoint_avg_mid = (.data[[col_mid_end1]] + .data[[col_mid_end]]) / 2,
    CAGR_averaged = case_when(
      status != "Stable"                                           ~ NA_real_,
      baseline_avg_mid > 0 & endpoint_avg_mid > 0 ~
        ((endpoint_avg_mid / baseline_avg_mid)^(1/3) - 1) * 100,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    cagr_gap = case_when(
      is.na(CAGR_simple) | is.na(CAGR_averaged) ~ NA_real_,
      TRUE ~ abs(CAGR_simple - CAGR_averaged)
    ),
    is_volatile = case_when(
      is.na(CAGR_simple) | is.na(CAGR_averaged) ~ FALSE,
      sign(CAGR_simple) != sign(CAGR_averaged)   ~ TRUE,
      TRUE                                        ~ FALSE
    )
  ) %>%
  mutate(
    CAGR_averaged = ifelse(is_volatile, NA_real_, CAGR_averaged),
    cagr_gap      = ifelse(is_volatile, NA_real_, cagr_gap),
    cagr_gap_flag = case_when(
      is.na(cagr_gap) ~ FALSE,
      cagr_gap > 10   ~ TRUE,
      TRUE            ~ FALSE
    )
  )

# Group totals — all years
group_year <- df %>%
  group_by(COUNTRY.UN_CODE, Country_name, taxon_group, PERIOD) %>%
  summarise(
    group_lower = sum(indiv_lower, na.rm = TRUE),
    group_mid   = sum(indiv_mid,   na.rm = TRUE),
    group_upper = sum(indiv_upper, na.rm = TRUE),
    .groups = "drop"
  )

group_wide <- group_year %>%
  pivot_wider(
    names_from  = PERIOD,
    values_from = c(group_lower, group_mid, group_upper),
    names_glue  = "{.value}_{PERIOD}"
  ) %>%
  mutate(across(starts_with("group_"), ~ replace_na(.x, 0)))

g_mid_start  <- paste0("group_mid_", START_YEAR)
g_mid_end    <- paste0("group_mid_", END_YEAR)
g_mid_end1   <- paste0("group_mid_", END_YEAR - 1)
g_mid_start1 <- paste0("group_mid_", START_YEAR + 1)
g_low_start  <- paste0("group_lower_", START_YEAR)
g_low_end    <- paste0("group_lower_", END_YEAR)
g_up_start   <- paste0("group_upper_", START_YEAR)
g_up_end     <- paste0("group_upper_", END_YEAR)
g_mid_all    <- paste0("group_mid_", years)   # all years for share computation

group_wide <- group_wide %>%
  mutate(
    group_CAGR_simple = case_when(
      .data[[g_mid_start]] > 0 & .data[[g_mid_end]] > 0 ~
        ((.data[[g_mid_end]] / .data[[g_mid_start]])^(1/n_years) - 1) * 100,
      TRUE ~ NA_real_
    ),
    group_baseline_avg = (.data[[g_mid_start]] + .data[[g_mid_start1]]) / 2,
    group_endpoint_avg = (.data[[g_mid_end1]]  + .data[[g_mid_end]])   / 2,
    group_CAGR_averaged = case_when(
      group_baseline_avg > 0 & group_endpoint_avg > 0 ~
        ((group_endpoint_avg / group_baseline_avg)^(1/3) - 1) * 100,
      TRUE ~ NA_real_
    ),
    group_CAGR_simple_lower = case_when(
      .data[[g_low_start]] > 0 & .data[[g_low_end]] > 0 ~
        ((.data[[g_low_end]] / .data[[g_low_start]])^(1/n_years) - 1) * 100,
      TRUE ~ NA_real_
    ),
    group_CAGR_simple_upper = case_when(
      .data[[g_up_start]] > 0 & .data[[g_up_end]] > 0 ~
        ((.data[[g_up_end]] / .data[[g_up_start]])^(1/n_years) - 1) * 100,
      TRUE ~ NA_real_
    )
  )

# Join group totals (all years) + relative CAGR
group_join_cols <- c(
  "COUNTRY.UN_CODE", "taxon_group",
  g_low_start, g_up_start, g_low_end, g_up_end,
  all_of(g_mid_all),                              # all years for share computation
  "group_CAGR_simple", "group_CAGR_averaged",
  "group_CAGR_simple_lower", "group_CAGR_simple_upper"
)

species_out <- species_wide %>%
  left_join(
    group_wide %>% select(all_of(group_join_cols)),
    by = c("COUNTRY.UN_CODE", "taxon_group")
  ) %>%
  mutate(
    group_CAGR_simple_full       = group_CAGR_simple,
    group_CAGR_simple_lower_full = group_CAGR_simple_lower,
    group_CAGR_simple_upper_full = group_CAGR_simple_upper,
    group_CAGR_simple   = ifelse(status != "Stable" | is_volatile,
                                 NA_real_, group_CAGR_simple),
    group_CAGR_averaged = ifelse(status != "Stable" | is_volatile,
                                 NA_real_, group_CAGR_averaged),
    relative_CAGR_simple = case_when(
      !is.na(CAGR_simple) & !is.na(group_CAGR_simple) ~
        CAGR_simple - group_CAGR_simple,
      TRUE ~ NA_real_
    ),
    relative_CAGR_averaged = case_when(
      !is.na(CAGR_averaged) & !is.na(group_CAGR_averaged) ~
        CAGR_averaged - group_CAGR_averaged,
      TRUE ~ NA_real_
    )
  )

# Share of group (all years, all species)
share_of_group_cols <- paste0("share_of_group_", years)
for (yr in years) {
  col_mid_yr  <- paste0("indiv_mid_", yr)
  g_mid_yr    <- paste0("group_mid_", yr)
  share_col   <- paste0("share_of_group_", yr)
  species_out[[share_col]] <- if_else(
    !is.na(species_out[[g_mid_yr]]) & species_out[[g_mid_yr]] > 0,
    species_out[[col_mid_yr]] / species_out[[g_mid_yr]],
    NA_real_
  )
}

# Share of group start/end (Stable non-volatile only — existing columns)
species_out <- species_out %>%
  mutate(
    share_start = case_when(
      status != "Stable" ~ NA_real_,
      .data[[g_mid_start]] > 0 ~ .data[[col_mid_start]] / .data[[g_mid_start]],
      TRUE ~ NA_real_
    ),
    share_end = case_when(
      status != "Stable" ~ NA_real_,
      .data[[g_mid_end]] > 0   ~ .data[[col_mid_end]]   / .data[[g_mid_end]],
      TRUE ~ NA_real_
    ),
    share_change_pp = case_when(
      !is.na(share_start) & !is.na(share_end) ~
        (share_end - share_start) * 100,
      TRUE ~ NA_real_
    )
  )

# Signal tiering
signal_tier <- species_out %>%
  group_by(COUNTRY.UN_CODE, taxon_group) %>%
  summarise(
    n_valid_relative_CAGR = sum(!is.na(relative_CAGR_simple)),
    sd_relative_CAGR      = sd(relative_CAGR_simple, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    signal_tier = case_when(
      n_valid_relative_CAGR <= 1                                  ~ 3L,
      is.na(sd_relative_CAGR)                                     ~ 3L,
      sd_relative_CAGR > 40                                       ~ 3L,
      n_valid_relative_CAGR >= 3 & sd_relative_CAGR < 20          ~ 1L,
      n_valid_relative_CAGR == 2                                  ~ 2L,
      n_valid_relative_CAGR >= 3 & sd_relative_CAGR >= 20         ~ 2L,
      TRUE                                                        ~ 3L
    )
  )

volatile_share <- species_out %>%
  group_by(COUNTRY.UN_CODE, taxon_group) %>%
  summarise(
    total_production    = sum(.data[[col_mid_end]], na.rm = TRUE),
    volatile_production = sum(.data[[col_mid_end]][is_volatile], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(volatile_share = ifelse(total_production > 0,
                                  volatile_production / total_production, 0))

signal_tier <- signal_tier %>%
  left_join(volatile_share %>% select(COUNTRY.UN_CODE, taxon_group, volatile_share),
            by = c("COUNTRY.UN_CODE", "taxon_group")) %>%
  mutate(signal_tier = ifelse(!is.na(volatile_share) & volatile_share > 0.5,
                               3L, signal_tier))

species_out <- species_out %>%
  left_join(
    signal_tier %>% select(COUNTRY.UN_CODE, taxon_group,
                           n_valid_relative_CAGR, sd_relative_CAGR, signal_tier),
    by = c("COUNTRY.UN_CODE", "taxon_group")
  )

# Tab 1: Species_Country_Detail
species_detail <- species_out %>%
  left_join(
    country_rank %>% select(COUNTRY.UN_CODE, country_total_mid_end,
                            country_rank_by_production),
    by = "COUNTRY.UN_CODE"
  ) %>%
  select(
    country_rank_by_production, country_total_mid_end,
    COUNTRY.UN_CODE, Country_name,
    SPECIES.ALPHA_3_CODE, Species_name, Scientific_Name,
    genus, family, order, class, phylum, taxon_group,
    all_of(year_cols_lower),
    all_of(year_cols_mid),
    all_of(year_cols_upper),
    all_of(share_of_group_cols),         # NEW: year-by-year shares for heatmap
    all_of(c(g_low_start, g_mid_start, g_up_start,
             g_low_end,   g_mid_end,   g_up_end)),
    status, is_volatile, meaningful_flips,
    CAGR_simple, CAGR_averaged,
    group_CAGR_simple, group_CAGR_averaged,
    relative_CAGR_simple, relative_CAGR_averaged,
    share_start, share_end, share_change_pp,
    cagr_gap, cagr_gap_flag,
    signal_tier, n_valid_relative_CAGR, sd_relative_CAGR
  ) %>%
  arrange(country_rank_by_production, taxon_group, desc(relative_CAGR_simple))

cat("Species_Country_Detail rows:", nrow(species_detail), "\n")

# ==============================================================================
# SECTION D: ENS (2019-2023, all countries, all scenarios)
#
# Classification (ENS_2023 − ENS_2019):
#   DIVERSIFYING  : Overall_Change > +0.20
#   STABLE        : Overall_Change within ±0.20
#   CONSOLIDATING : Overall_Change < −0.20
#   Volatile flag removed — NEI scenario sensitivity captures uncertainty.
#
# low_volume = TRUE for countries outside the top 40 by 2023 individuals.
# ==============================================================================

cat("\n--- Section D: ENS ---\n")

ENS_THRESHOLD <- 0.20

calculate_ens <- function(values) {
  values <- values[values > 0]
  if (length(values) == 0) return(NA)
  if (length(values) == 1) return(1)
  shares  <- values / sum(values)
  shannon <- -sum(shares * log(shares))
  return(exp(shannon))
}

ens_list <- list()
for (scenario in ENS_SCENARIOS) {
  cat("  ENS processing:", scenario, "\n")
  sc_data <- nei_raw %>%
    filter(
      Scenario == scenario,
      (Unknown_Sharing == "Regional" | is.na(Unknown_Sharing)),
      Major_Group %in% c("PISCES", "CRUSTACEA"),
      PERIOD %in% c(2019, 2020, 2021, 2022, 2023)
    ) %>%
    mutate(
      taxon_group       = if_else(Major_Group == "PISCES", "Finfish", "Crustacean"),
      Median.indiv.ton  = (Lower.indiv.ton + Upper.indiv.ton) / 2,
      Total_individuals = VALUE * Median.indiv.ton
    )

  ens_yr <- sc_data %>%
    group_by(COUNTRY.UN_CODE, Country_name, taxon_group, PERIOD,
             SPECIES.ALPHA_3_CODE) %>%
    summarise(Total_individuals = sum(Total_individuals, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(COUNTRY.UN_CODE, Country_name, taxon_group, PERIOD) %>%
    summarise(ENS = calculate_ens(Total_individuals), .groups = "drop") %>%
    mutate(Scenario = scenario)

  ens_list[[scenario]] <- ens_yr
}

ens_all <- bind_rows(ens_list)

ens_wide <- ens_all %>%
  select(COUNTRY.UN_CODE, Country_name, taxon_group, Scenario, PERIOD, ENS) %>%
  pivot_wider(names_from = PERIOD, values_from = ENS, names_prefix = "ENS_") %>%
  mutate(
    Overall_Change = ENS_2023 - ENS_2019,
    Trajectory_Category = case_when(
      Overall_Change >  ENS_THRESHOLD  ~ "DIVERSIFYING",
      Overall_Change < -ENS_THRESHOLD  ~ "CONSOLIDATING",
      !is.na(Overall_Change)           ~ "STABLE",
      TRUE                             ~ NA_character_
    )
  )

# Full grid: all countries × both taxon groups × all scenarios
# Countries missing a taxon group entirely get a "No X farmed" label
ens_country_names <- ens_all %>% distinct(COUNTRY.UN_CODE, Country_name)
all_ens_codes     <- ens_country_names %>% pull(COUNTRY.UN_CODE)

full_grid <- expand.grid(
  COUNTRY.UN_CODE = all_ens_codes,
  taxon_group     = c("Finfish", "Crustacean"),
  Scenario        = ENS_SCENARIOS,
  stringsAsFactors = FALSE
) %>%
  left_join(ens_country_names, by = "COUNTRY.UN_CODE") %>%
  left_join(ens_wide, by = c("COUNTRY.UN_CODE", "Country_name",
                              "taxon_group", "Scenario")) %>%
  mutate(
    Trajectory_Category = if_else(
      is.na(Trajectory_Category),
      if_else(taxon_group == "Finfish", "No finfish farmed", "No crustaceans farmed"),
      Trajectory_Category
    )
  )

# Consensus trajectory (per country × taxon_group across the 3 scenarios)
# Majority rule: at least 2 of 3 scenarios must agree for a directional call.
consensus <- full_grid %>%
  group_by(COUNTRY.UN_CODE, taxon_group) %>%
  summarise(
    scenarios_agree   = n_distinct(Trajectory_Category) == 1,
    n_diversifying    = sum(Trajectory_Category == "DIVERSIFYING"),
    n_consolidating   = sum(Trajectory_Category == "CONSOLIDATING"),
    n_stable          = sum(Trajectory_Category == "STABLE"),
    farmed_label      = Trajectory_Category[1],
    .groups = "drop"
  ) %>%
  mutate(
    consensus_trajectory = case_when(
      farmed_label %in% c("No finfish farmed", "No crustaceans farmed") ~ farmed_label,
      n_diversifying  >= 2 ~ "DIVERSIFYING",
      n_consolidating >= 2 ~ "CONSOLIDATING",
      n_stable        >= 2 ~ "STABLE",
      TRUE                 ~ "UNCERTAIN TRAJECTORY"
    )
  ) %>%
  select(COUNTRY.UN_CODE, taxon_group, scenarios_agree, consensus_trajectory)

# Pivot to one row per country × taxon_group; add low_volume flag
# ENS_2019, ENS_2023, Overall_Change vary by scenario so must NOT be in id_cols.
# Join them back from Medium scenario as representative values.
ens_medium_vals <- ens_wide %>%
  filter(Scenario == "Medium") %>%
  select(COUNTRY.UN_CODE, taxon_group, ENS_2019, ENS_2023, Overall_Change)

ens_out <- full_grid %>%
  left_join(consensus, by = c("COUNTRY.UN_CODE", "taxon_group")) %>%
  select(COUNTRY.UN_CODE, Country_name, taxon_group, Scenario, Trajectory_Category) %>%
  pivot_wider(
    id_cols     = c(COUNTRY.UN_CODE, Country_name, taxon_group),
    names_from  = Scenario,
    values_from = Trajectory_Category,
    names_glue  = "trajectory_{Scenario}"
  ) %>%
  rename(
    trajectory_conservative = trajectory_Conservative,
    trajectory_medium       = trajectory_Medium,
    trajectory_maximum      = trajectory_Maximum
  ) %>%
  left_join(consensus,        by = c("COUNTRY.UN_CODE", "taxon_group")) %>%
  left_join(ens_medium_vals,  by = c("COUNTRY.UN_CODE", "taxon_group")) %>%
  mutate(low_volume = !(COUNTRY.UN_CODE %in% top40_codes)) %>%
  arrange(low_volume, COUNTRY.UN_CODE, taxon_group)

cat("ENS rows:", nrow(ens_out), "\n")
cat("  Top-40 countries:", sum(!ens_out$low_volume & ens_out$taxon_group == "Finfish"), "finfish,",
    sum(!ens_out$low_volume & ens_out$taxon_group == "Crustacean"), "crustacean\n")
cat("  Low-volume countries:", sum(ens_out$low_volume & ens_out$taxon_group == "Finfish"), "finfish,",
    sum(ens_out$low_volume & ens_out$taxon_group == "Crustacean"), "crustacean\n")

# ==============================================================================
# SECTION E: EXPORT
# ==============================================================================

cat("\nWriting Excel...\n")
write_xlsx(
  list(
    Species_Country_Detail = species_detail,
    Jaccard                = jaccard_out,
    ENS                    = ens_out
  ),
  OUTPUT_PATH
)
cat("Saved:", OUTPUT_PATH, "\n")

# ==============================================================================
# DIAGNOSTICS
# ==============================================================================

cat("\n=== DIAGNOSTICS ===\n")
cat("Species_Country_Detail:", nrow(species_detail), "rows,",
    n_distinct(species_detail$COUNTRY.UN_CODE), "countries\n")
cat("Jaccard:", nrow(jaccard_out), "rows\n")
cat("ENS:", nrow(ens_out), "rows\n")

cat("\nStatus breakdown:\n")
print(table(species_detail$status))

cat("\nENS consensus distribution — top-40 countries (Finfish):\n")
print(table(ens_out$consensus_trajectory[ens_out$taxon_group == "Finfish" & !ens_out$low_volume]))

cat("\nENS consensus distribution — top-40 countries (Crustacean):\n")
print(table(ens_out$consensus_trajectory[ens_out$taxon_group == "Crustacean" & !ens_out$low_volume]))

cat("\nScenario disagreements (top-40 only):\n")
ens_out %>%
  filter(!low_volume, !scenarios_agree,
         !consensus_trajectory %in% c("No finfish farmed", "No crustaceans farmed")) %>%
  select(Country_name, taxon_group, trajectory_conservative,
         trajectory_medium, trajectory_maximum, consensus_trajectory) %>%
  print()

cat("\nDone.\n")
