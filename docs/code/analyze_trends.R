library(readr)
library(dplyr)
library(tidyr)
library(stringr)

data_dir <- file.path("outputs", "data")
stamp <- "2026-09-12"

trend_num <- function(x) {
  x <- as.character(x)
  out <- suppressWarnings(as.numeric(x))
  out[x == "<1"] <- 0
  out
}

read_website <- function(path) {
  read_csv(path, skip = 2, col_types = cols(.default = col_character())) |>
    rename(date = 1) |>
    pivot_longer(-date, names_to = "keyword", values_to = "hits") |>
    mutate(
      date = as.Date(date),
      keyword = str_remove(keyword, ":.*$"),
      hits = trend_num(hits)
    )
}

read_r_iot <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    transmute(
      date = as.Date(date),
      keyword,
      hits = trend_num(hits)
    )
}

web_main <- read_website(file.path(
  data_dir, paste0("website_trump_kamala-election_us_5y_", stamp, ".csv")
))
r_main <- read_r_iot(file.path(
  data_dir, paste0("r_trump_kamala-election_us_5y_", stamp, ".csv")
))

method_compare <- inner_join(
  rename(web_main, website = hits),
  rename(r_main, gtrendsR = hits),
  by = c("date", "keyword")
) |>
  mutate(difference = gtrendsR - website)
write_csv(method_compare, file.path(data_dir, "comparison_website_vs_r.csv"))

web_two <- read_website(file.path(
  data_dir, paste0("website_trump-election_us_5y_", stamp, ".csv")
))
term_set_compare <- inner_join(
  web_main |> filter(keyword %in% c("Trump", "Election")) |>
    rename(three_term_query = hits),
  web_two |> rename(two_term_query = hits),
  by = c("date", "keyword")
) |>
  mutate(difference = two_term_query - three_term_query)
write_csv(term_set_compare, file.path(data_dir, "comparison_removed_term.csv"))

repeat_a <- read_r_iot(file.path(
  data_dir, paste0("r_repeat-a_federal-reserve_us_90d_", stamp, ".csv")
))
repeat_b <- read_r_iot(file.path(
  data_dir, paste0("r_repeat-b_federal-reserve_us_90d_", stamp, ".csv")
))
repeat_compare <- inner_join(
  rename(repeat_a, pull_a = hits),
  rename(repeat_b, pull_b = hits),
  by = c("date", "keyword")
) |>
  mutate(difference = pull_b - pull_a)
write_csv(repeat_compare, file.path(data_dir, "comparison_repeat_pulls.csv"))

harris <- read_r_iot(file.path(
  data_dir, paste0("r_harris-v-kamala-harris_us_5y_", stamp, ".csv")
)) |>
  select(date, keyword, hits) |>
  pivot_wider(names_from = keyword, values_from = hits) |>
  mutate(difference = Harris - `Kamala Harris`)
write_csv(harris, file.path(data_dir, "comparison_harris_wording.csv"))

web_short <- read_website(file.path(
  data_dir, paste0("website_trump_kamala-election_us_90d_", stamp, ".csv")
))

summary <- tibble(
  measure = c(
    "website_5y_start", "website_5y_end", "website_5y_interval_days",
    "website_90d_start", "website_90d_end", "website_90d_interval_days",
    "method_rows", "method_identical_pct", "method_mean_abs_difference",
    "method_max_abs_difference", "method_mean_signed_difference",
    "removed_identical_pct", "removed_mean_abs_difference",
    "removed_max_abs_difference", "repeat_identical_pct",
    "repeat_mean_abs_difference", "repeat_max_abs_difference",
    "harris_identical_pct", "harris_mean_abs_difference",
    "harris_max_abs_difference"
  ),
  value = c(
    as.character(min(web_main$date)), as.character(max(web_main$date)),
    median(diff(sort(unique(web_main$date)))),
    as.character(min(web_short$date)), as.character(max(web_short$date)),
    median(diff(sort(unique(web_short$date)))),
    nrow(method_compare), mean(method_compare$difference == 0) * 100,
    mean(abs(method_compare$difference)), max(abs(method_compare$difference)),
    mean(method_compare$difference),
    mean(term_set_compare$difference == 0) * 100,
    mean(abs(term_set_compare$difference)), max(abs(term_set_compare$difference)),
    mean(repeat_compare$difference == 0) * 100,
    mean(abs(repeat_compare$difference)), max(abs(repeat_compare$difference)),
    mean(harris$difference == 0) * 100,
    mean(abs(harris$difference)), max(abs(harris$difference))
  ) |> as.character()
)
write_csv(summary, file.path(data_dir, "analysis_summary.csv"))
print(summary, n = Inf)
