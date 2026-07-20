library(tidycensus)
library(tidyverse)

# Written with the help of Claude AI

# Created April 30, 2026

# Set your Census API key 
census_api_key("api-key")

# Lookup table
dhc_vars <- load_variables(year = 2020, dataset = "dhc")

# ── Variables: Under 10 ────────────────────────────────────────────────────────
vars_under10 <- c(
  male_under5   = "P12_003N",
  male_5to9     = "P12_004N",
  female_under5 = "P12_027N",
  female_5to9   = "P12_028N"
)

# ── Variables: Over 50 ────────────────────────────────────────────────────────
# Male 50+ : P12_016N (50-54) through P12_025N (85+)
# Female 50+: P12_040N (50-54) through P12_049N (85+)

vars_over50 <- c(
  male_50to54     = "P12_016N",
  male_55to59     = "P12_017N",
  male_60to61     = "P12_018N",
  male_62to64     = "P12_019N",
  male_65to66     = "P12_020N",
  male_67to69     = "P12_021N",
  male_70to74     = "P12_022N",
  male_75to79     = "P12_023N",
  male_80to84     = "P12_024N",
  male_85plus     = "P12_025N",
  female_50to54   = "P12_040N",
  female_55to59   = "P12_041N",
  female_60to61   = "P12_042N",
  female_62to64   = "P12_043N",
  female_65to66   = "P12_044N",
  female_67to69   = "P12_045N",
  female_70to74   = "P12_046N",
  female_75to79   = "P12_047N",
  female_80to84   = "P12_048N",
  female_85plus   = "P12_049N"
)

# ── Download data (all variables in one call) ──────────────────────────────────
all_vars <- c(vars_under10, vars_over50)

ny_data_raw <- get_decennial(
  geography = "county",
  variables = all_vars,
  year      = 2020,
  sumfile   = "dhc",
  state     = "NY",
  output    = "wide"
)

# ── Calculate totals ───────────────────────────────────────────────────────────
ny_data <- ny_data_raw |>
  mutate(
    total_under10 = male_under5 + male_5to9 + female_under5 + female_5to9,
    total_over50  = male_50to54 + male_55to59 + male_60to61 + male_62to64 +
      male_65to66 + male_67to69 + male_70to74 + male_75to79 +
      male_80to84 + male_85plus +
      female_50to54 + female_55to59 + female_60to61 + female_62to64 +
      female_65to66 + female_67to69 + female_70to74 + female_75to79 +
      female_80to84 + female_85plus
  ) |>
  select(GEOID, NAME, total_under10, total_over50) |>
  arrange(desc(total_over50))

# ── Preview results ────────────────────────────────────────────────────────────
print(ny_data)

# Save data
write.csv(ny_data,
          paste0("C:/Users/dthill/OneDrive - Syracuse University/RSV paper/", 
                 "Data/ny_pop_numbers.csv")
          )
