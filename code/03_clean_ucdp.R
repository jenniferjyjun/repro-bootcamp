# ------------------------------------------------------------------
# 03_clean_ucdp.R
# Purpose: Turn the UCDP/PRIO conflict-year data into a country-year
#          data set: one row per country and year in which at least one
#          armed conflict (>= 25 battle-related deaths) took place there.
# Inputs:  data/raw/ucdp_acd.csv
# Outputs: data/clean/ucdp.rds
# Author:  Hennes Barnehl
# Date:    2026-09-18
# ------------------------------------------------------------------

# 1. Setup ------------------------------------------------------------
library(tidyverse)
library(here)

# UCDP/PRIO ACD rows are already conflict-years with >= 25 battle-related
# deaths (that threshold is the dataset's own inclusion rule), so no extra
# death filter is needed here to match the PAP's IV definition.
ucdp_raw <- read_csv(here("data", "raw", "ucdp_acd.csv"), na = "",
                     show_col_types = FALSE)

# 2. One row per country --------------------------------------------
# `gwno_loc` can list several Gleditsch-Ward codes separated by ", " when a
# conflict is located in more than one country; split so each row is one
# conflict-year-country, since that's the unit we collapse to next.
ucdp_long <- ucdp_raw |>
  select(conflict_id, year, intensity_level, gwno_loc) |>
  separate_longer_delim(gwno_loc, delim = ",") |>
  mutate(gwno_loc = as.numeric(str_trim(gwno_loc)))

n_bad_gwno <- sum(is.na(ucdp_long$gwno_loc))
if (n_bad_gwno > 0) {
  message("03_clean_ucdp.R: dropping ", n_bad_gwno,
          " conflict-year-country rows with unparseable gwno_loc")
}
ucdp_long <- ucdp_long |> filter(!is.na(gwno_loc))

# 3. Collapse to country-year ---------------------------------------
ucdp <- ucdp_long |>
  group_by(gwno_loc, year) |>
  summarise(
    conflict      = 1L,
    n_conflicts   = n_distinct(conflict_id),
    max_intensity = max(intensity_level),
    .groups = "drop"
  )

# 4. Save -------------------------------------------------------------
saveRDS(ucdp, here("data", "clean", "ucdp.rds"))
message("03_clean_ucdp.R: ", nrow(ucdp), " country-years, ",
        n_distinct(ucdp$gwno_loc), " countries, ",
        min(ucdp$year), "-", max(ucdp$year))

# 5. Checks -----------------------------------------------------------
stopifnot(
  !anyDuplicated(ucdp[c("gwno_loc", "year")]),        # unique key
  all(!is.na(ucdp$gwno_loc)),
  all(!is.na(ucdp$year)),
  all(ucdp$conflict == 1),
  all(ucdp$year >= 1946 & ucdp$year <= 2025),
  all(ucdp$n_conflicts >= 1),
  all(ucdp$max_intensity %in% c(1, 2))
)
