# Assignment 4: reproducible collection and cleaning

library(countrycode)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(robotstxt)
library(rvest)
library(stringr)
library(tibble)
library(WDI)

dir.create("data/assignment04", recursive = TRUE, showWarnings = FALSE)

books_base <- "https://books.toscrape.com/catalogue/page-%s.html"
reserves_url <- paste0(
  "https://en.wikipedia.org/wiki/",
  "List_of_countries_by_foreign-exchange_reserves"
)
population_url <- paste0(
  "https://en.wikipedia.org/wiki/",
  "List_of_countries_and_dependencies_by_population"
)

# Check permission before the first request to each host/path.
permission_log <- tibble(
  checked_at = format(Sys.time(), tz = "America/Chicago", usetz = TRUE),
  url = c(sprintf(books_base, 1), reserves_url, population_url),
  allowed = map_lgl(url, robotstxt::paths_allowed)
)
write_csv(permission_log, "data/assignment04/robots_permission_log.csv")
stopifnot(all(permission_log$allowed))

rating_levels <- c(
  One = 1L, Two = 2L, Three = 3L, Four = 4L, Five = 5L
)

scrape_books_page <- function(page_number) {
  Sys.sleep(0.5)
  page <- read_html(sprintf(books_base, page_number))
  cards <- html_elements(page, "article.product_pod")

  tibble(
    page = page_number,
    title = cards |>
      html_element("h3 a") |>
      html_attr("title"),
    price_gbp = cards |>
      html_element("p.price_color") |>
      html_text2() |>
      parse_number(),
    rating_word = cards |>
      html_element("p.star-rating") |>
      html_attr("class") |>
      str_remove("^star-rating\\s+")
  ) |>
    mutate(rating = unname(rating_levels[rating_word]))
}

books <- map_dfr(1:5, scrape_books_page)
stopifnot(nrow(books) == 100L, !anyDuplicated(books$title))
write_csv(books, "data/assignment04/books_first_five_pages.csv")

clean_text <- function(x) {
  x |>
    str_replace_all("[\\u00A0\\u2009\\u202F]", " ") |>
    str_squish()
}

parse_reserve <- function(x) {
  x <- if_else(
    str_detect(x, regex("N/?a|not available", ignore_case = TRUE)),
    NA_character_, x
  )
  parse_number(
    str_replace_all(x, "[−–—]", "-"),
    na = c("", "—", "–", "N/A", "N/a")
  )
}

# The first Wikipedia table contains two header rows inside the body and four
# columns that rvest gives the same name. Position-based renaming is explicit.
reserves_raw <- read_html(reserves_url) |>
  html_elements("table.wikitable") |>
  pluck(1) |>
  html_table(fill = TRUE)

stopifnot(ncol(reserves_raw) == 8L)
names(reserves_raw) <- c(
  "country", "continent", "reserves_with_gold_usd_millions",
  "change_with_gold_usd_millions", "reserves_without_gold_usd_millions",
  "change_without_gold_usd_millions", "last_reported_date", "reference"
)

reserves <- reserves_raw |>
  slice(-(1:2)) |>
  mutate(across(everything(), clean_text)) |>
  mutate(
    country = str_remove_all(country, "\\[[^]]+\\]") |> str_squish(),
    continent = str_remove_all(continent, "\\[[^]]+\\]") |> str_squish(),
    across(
      ends_with("usd_millions"),
      parse_reserve
    ),
    last_reported_date = suppressWarnings(dmy(last_reported_date)),
    iso3c = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Taiwan" = "TWN", "Kosovo" = "XKX",
        "South Korea" = "KOR", "North Korea" = "PRK"
      )
    )
  ) |>
  filter(!country %in% c("", "Total", "World")) |>
  select(-reference)

stopifnot(
  nrow(reserves) > 150L,
  # Wikipedia currently contains one impossible source date: 31 April 2024.
  # Keep it missing rather than silently inventing a replacement date.
  sum(is.na(reserves$last_reported_date)) <= 1L,
  sum(is.na(reserves$reserves_with_gold_usd_millions)) == 0L
)
write_csv(reserves, "data/assignment04/wikipedia_reserves_clean.csv")

# A deliberately different table: unlike the reserve table, it has one header
# row, six columns, a World row, percent signs, and citations in source/notes.
population_raw <- read_html(population_url) |>
  html_element("table.wikitable") |>
  html_table(fill = TRUE)

population <- population_raw |>
  rename(
    location = 1, population = 2, world_share = 3,
    estimate_date = 4, source = 5, notes = 6
  ) |>
  mutate(across(everything(), clean_text)) |>
  mutate(
    location = str_remove_all(location, "\\[[^]]+\\]") |> str_squish(),
    population = parse_number(population),
    world_share_pct = parse_number(world_share),
    estimate_date = suppressWarnings(dmy(estimate_date))
  ) |>
  filter(location != "World") |>
  select(location, population, world_share_pct, estimate_date, source, notes)

stopifnot(nrow(population) > 200L, !any(population$location == "World"))
write_csv(population, "data/assignment04/wikipedia_population_clean.csv")

# One WDI call returns the same indicator for every country and recent year.
options(timeout = 240)
wdi_raw <- WDI(
  country = "all", indicator = "FI.RES.TOTL.CD",
  start = 2023, end = 2026, extra = TRUE
)
write_csv(wdi_raw, "data/assignment04/wdi_reserves_2023_2026_raw.csv")

wdi_latest <- wdi_raw |>
  filter(region != "Aggregates", !is.na(FI.RES.TOTL.CD)) |>
  group_by(iso3c) |>
  slice_max(year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    iso3c, wb_country = country, wb_year = year,
    wb_reserves_usd_millions = FI.RES.TOTL.CD / 1e6
  )

comparison <- reserves |>
  filter(!is.na(iso3c)) |>
  inner_join(wdi_latest, by = "iso3c") |>
  mutate(
    wiki_year = year(last_reported_date),
    difference_usd_millions =
      reserves_with_gold_usd_millions - wb_reserves_usd_millions,
    absolute_difference_usd_millions = abs(difference_usd_millions),
    percent_difference = 100 * difference_usd_millions /
      wb_reserves_usd_millions,
    same_vintage = wiki_year == wb_year
  ) |>
  arrange(desc(absolute_difference_usd_millions))

stopifnot(nrow(comparison) > 100L)
write_csv(comparison, "data/assignment04/reserves_source_comparison.csv")

summary_lines <- c(
  paste("books_rows", nrow(books)),
  paste("wiki_reserve_rows", nrow(reserves)),
  paste("population_rows", nrow(population)),
  paste("matched_countries", nrow(comparison)),
  paste("same_vintage_countries", sum(comparison$same_vintage, na.rm = TRUE)),
  paste("wiki_date_min", min(reserves$last_reported_date, na.rm = TRUE)),
  paste("wiki_date_max", max(reserves$last_reported_date, na.rm = TRUE)),
  paste("wdi_years", paste(sort(unique(wdi_latest$wb_year)), collapse = ","))
)
write_lines(summary_lines, "data/assignment04/run_summary.txt")
