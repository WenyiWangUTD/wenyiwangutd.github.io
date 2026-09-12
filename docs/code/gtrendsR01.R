# EPPS 6302 — Assignment 2
# Reproducible Google Trends pulls. Successful calls are cached immediately.

library(gtrendsR)
library(readr)

data_dir <- file.path("outputs", "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
pull_date <- as.character(Sys.Date())

cache_gtrends <- function(stem, ..., pause = 6) {
  rds_path <- file.path(data_dir, paste0(stem, ".rds"))
  csv_path <- file.path(data_dir, paste0(stem, ".csv"))

  if (file.exists(rds_path)) {
    message("Using cache: ", rds_path)
    return(readRDS(rds_path))
  }

  message("Pulling: ", stem)
  result <- gtrends(...)
  saveRDS(result, rds_path)
  if ("interest_over_time" %in% names(result)) {
    write_csv(result$interest_over_time, csv_path)
  }
  Sys.sleep(pause)
  result
}

# 1. The query supplied in the assignment script (Harris, not Kamala Harris).
TrumpHarrisElection <- cache_gtrends(
  paste0("r_trump_harris-election_us_5y_", pull_date),
  keyword = c("Trump", "Harris", "election"),
  onlyInterest = TRUE,
  geo = "US", gprop = "web", time = "today+5-y", category = 0
)
the_df <- TrumpHarrisElection$interest_over_time

# 2. Exact match to the website query, for method-to-method comparison.
TrumpKamalaElection <- cache_gtrends(
  paste0("r_trump_kamala-election_us_5y_", pull_date),
  keyword = c("Trump", "Kamala Harris", "Election"),
  onlyInterest = TRUE,
  geo = "US", gprop = "web", time = "today+5-y", category = 0
)

# 3. One term over its full Google Trends history.
taylor_all <- cache_gtrends(
  paste0("r_taylor-swift_world_all_", pull_date),
  keyword = "Taylor Swift", onlyInterest = TRUE,
  geo = "", gprop = "web", time = "all", category = 0
)

# 4. The same term in another country.
taylor_gb <- cache_gtrends(
  paste0("r_taylor-swift_gb_5y_", pull_date),
  keyword = "Taylor Swift", onlyInterest = TRUE,
  geo = "GB", gprop = "web", time = "today+5-y", category = 0
)

# 5–6. One term in three countries. onlyInterest remains FALSE, so the
# object also keeps the geographic and related-search data frames.
tct <- cache_gtrends(
  paste0("r_taylor-swift_us-gb-tw_5y_full-object_", pull_date),
  keyword = "Taylor Swift",
  geo = c("US", "GB", "TW"), gprop = "web",
  time = "today+5-y", category = 0
)
print(names(tct))

# Three terms worldwide.
three_worldwide <- cache_gtrends(
  paste0("r_taylor-beyonce-adele_world_5y_", pull_date),
  keyword = c("Taylor Swift", "Beyonce", "Adele"),
  onlyInterest = TRUE,
  geo = "", gprop = "web", time = "today+5-y", category = 0
)

# Search-term wording check: both terms are in one query, so they share a scale.
harris_wording <- cache_gtrends(
  paste0("r_harris-v-kamala-harris_us_5y_", pull_date),
  keyword = c("Harris", "Kamala Harris"),
  onlyInterest = TRUE,
  geo = "US", gprop = "web", time = "today+5-y", category = 0
)

# Section 7: identical calls separated by the work above and a final pause.
repeat_a <- cache_gtrends(
  paste0("r_repeat-a_federal-reserve_us_90d_", pull_date),
  keyword = "Federal Reserve", onlyInterest = TRUE,
  geo = "US", gprop = "web", time = "today 3-m", category = 0,
  pause = 10
)
repeat_b <- cache_gtrends(
  paste0("r_repeat-b_federal-reserve_us_90d_", pull_date),
  keyword = "Federal Reserve", onlyInterest = TRUE,
  geo = "US", gprop = "web", time = "today 3-m", category = 0,
  pause = 0
)

# Save a machine-readable pull log.
log <- data.frame(
  pulled_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  gtrendsR_version = as.character(packageVersion("gtrendsR")),
  stringsAsFactors = FALSE
)
write_csv(log, file.path(data_dir, paste0("r_pull_log_", pull_date, ".csv")))
