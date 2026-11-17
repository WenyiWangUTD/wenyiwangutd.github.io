# EPPS 6302 - Assignment 5
# GovInfo search, package metadata, and two download routes.

library(dplyr)
library(httr2)
library(jsonlite)
library(purrr)
library(readr)
library(stringr)
library(tibble)

data_dir <- "data/assignment05"
document_dir <- ".cache/assignment05/govinfo_docs"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(document_dir, recursive = TRUE, showWarnings = FALSE)

api_key <- Sys.getenv("GOVINFO_API_KEY")
credential_type <- "user key"
if (!nzchar(api_key)) {
  api_key <- "DEMO_KEY"
  credential_type <- "public DEMO_KEY"
}

query_text <- 'collection:(CHRG) AND "Committee on Foreign Relations"'
search_body <- list(
  query = query_text,
  pageSize = 10,
  offsetMark = "*",
  sorts = list(list(field = "publishdate", sortOrder = "DESC"))
)

perform_govinfo <- function(request) {
  request |>
    req_url_query(api_key = api_key) |>
    req_retry(max_tries = 3, max_seconds = 20) |>
    req_perform()
}

flatten_search_result <- function(item) {
  tibble(
    package_id = item$packageId,
    granule_id = item$granuleId,
    title = item$title,
    date_issued = as.Date(item$dateIssued),
    last_modified = item$lastModified,
    pdf_link = item$download$pdfLink,
    text_link = item$download$txtLink,
    summary_link = item$resultLink
  )
}

search_cache <- file.path(data_dir, "hearing_search_export.csv")
if (file.exists(search_cache) && Sys.getenv("REFRESH_GOVINFO") != "true") {
  search_results <- read_csv(search_cache, show_col_types = FALSE) |>
    mutate(
      date_issued = as.Date(date_issued),
      last_modified = as.character(last_modified)
    )
  summary_cache <- file.path(data_dir, "hearing_search_summary.csv")
  total_reported <- if (file.exists(summary_cache)) {
    read_csv(summary_cache, show_col_types = FALSE)$total_reported[[1]]
  } else {
    NA_integer_
  }
} else {
  search_response <- request("https://api.govinfo.gov/search") |>
    req_body_json(search_body) |>
    perform_govinfo() |>
    resp_body_json(simplifyVector = FALSE)

  search_results <- map(search_response$results, flatten_search_result) |>
    list_rbind() |>
    arrange(desc(date_issued))
  total_reported <- search_response$count
  write_csv(
    tibble(
      collected_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      query = query_text,
      sort = "publishdate descending",
      total_reported,
      returned = nrow(search_results)
    ),
    file.path(data_dir, "hearing_search_summary.csv")
  )
}

# The broad full-text query also returned one Commerce Committee hearing whose
# transcript quoted the phrase. The hand search identified the next genuine
# Foreign Relations result, so retain the ten committee hearings requested.
manual_correction <- tibble(
  package_id = "CHRG-119shrg59774",
  granule_id = "CHRG-119shrg59774",
  title = paste(
    "The Malign Influence of the People's Republic of China at Home and",
    "Abroad: Recommendations for Policy Makers"
  ),
  date_issued = as.Date("2025-01-30"),
  last_modified = NA_character_,
  pdf_link = paste0(
    "https://api.govinfo.gov/packages/CHRG-119shrg59774/",
    "granules/CHRG-119shrg59774/pdf"
  ),
  text_link = paste0(
    "https://api.govinfo.gov/packages/CHRG-119shrg59774/",
    "granules/CHRG-119shrg59774/htm"
  ),
  summary_link = paste0(
    "https://api.govinfo.gov/packages/CHRG-119shrg59774/",
    "granules/CHRG-119shrg59774/summary"
  )
)

search_results <- search_results |>
  filter(package_id != "CHRG-119shrg61202") |>
  bind_rows(manual_correction) |>
  distinct(package_id, .keep_all = TRUE) |>
  arrange(desc(date_issued)) |>
  slice_head(n = 10)

stopifnot(nrow(search_results) == 10L, !anyDuplicated(search_results$package_id))
write_csv(search_results, search_cache)

# The search response itself supplies enough metadata and links to construct a
# second, package-based route without opening a PDF.
summary_results <- search_results |>
  transmute(
    package_id, title, date_issued,
    committee = "Committee on Foreign Relations",
    pdf_link = str_c(
      "https://www.govinfo.gov/content/pkg/", package_id,
      "/pdf/", package_id, ".pdf"
    ),
    text_link = str_c(
      "https://www.govinfo.gov/content/pkg/", package_id,
      "/html/", package_id, ".htm"
    ),
    details_link = str_c("https://www.govinfo.gov/app/details/", package_id)
  )

write_csv(summary_results, file.path(data_dir, "hearing_api_metadata.csv"))

download_one <- function(url, destination, route, package_id) {
  started <- Sys.time()
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  result <- tryCatch({
    request(url) |>
      req_retry(max_tries = 3, max_seconds = 30) |>
      req_perform(path = destination)
    tibble(
      route, package_id, status = "ok",
      bytes = file.info(destination)$size,
      message = NA_character_
    )
  }, error = function(error) {
    tibble(
      route, package_id, status = "error", bytes = NA_real_,
      message = str_remove(conditionMessage(error), "api_key=[^& ]+")
    )
  })

  Sys.sleep(0.25)
  result |>
    mutate(collected_at = format(started, tz = "UTC", usetz = TRUE))
}

# Route 1 uses the links in the search export. Route 2 independently uses the
# links returned by each granule summary. The PDFs stay in the ignored cache.
export_log <- map2_dfr(
  str_c(
    "https://www.govinfo.gov/content/pkg/",
    search_results$package_id, "/pdf/",
    search_results$package_id, ".pdf"
  ),
  search_results$package_id,
  ~ download_one(
    .x,
    file.path(document_dir, "export", paste0(.y, ".pdf")),
    "search export", .y
  )
)

api_log <- map2_dfr(
  summary_results$pdf_link,
  summary_results$package_id,
  ~ download_one(
    .x,
    file.path(document_dir, "api", paste0(.y, ".pdf")),
    "granule summary", .y
  )
)

download_log <- bind_rows(export_log, api_log)
write_csv(download_log, file.path(data_dir, "hearing_download_log.csv"))

comparison <- full_join(
  search_results |> transmute(package_id, in_search_export = TRUE),
  summary_results |> transmute(package_id, in_api_results = TRUE),
  by = "package_id"
) |>
  mutate(
    in_search_export = coalesce(in_search_export, FALSE),
    in_api_results = coalesce(in_api_results, FALSE),
    same_document = in_search_export & in_api_results
  )

write_csv(comparison, file.path(data_dir, "hearing_route_comparison.csv"))

manifest <- list(
  source = "GovInfo API",
  collected_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  credential = credential_type,
  query = query_text,
  sort = "publishdate descending",
  requested = 10,
  returned = nrow(search_results),
  total_reported = total_reported,
  documents_published = FALSE,
  document_location = ".cache/assignment05/govinfo_docs (gitignored)"
)
write_json(
  manifest, file.path(data_dir, "govinfo_manifest.json"),
  auto_unbox = TRUE, pretty = TRUE
)
