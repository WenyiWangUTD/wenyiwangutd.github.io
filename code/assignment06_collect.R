# EPPS 6302 - Assignment 6
# Text analytics for summit tweets, GovInfo presidential documents,
# and US presidential inaugural addresses.

library(dplyr)
library(ggplot2)
library(httr2)
library(jsonlite)
library(purrr)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(readr)
library(stringr)
library(tibble)

data_dir <- "data/assignment06"
cache_dir <- ".cache/assignment06"
cpd_dir <- file.path(cache_dir, "cpd_text")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cpd_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Part 1: archived tweets from the November 2021 Biden-Xi summit
# -----------------------------------------------------------------------------

summit_file <- file.path(cache_dir, "summit_11162021.csv")
summit_url <- paste0(
  "https://raw.githubusercontent.com/datageneration/datamethods/",
  "master/textanalytics/summit_11162021.csv"
)

if (!file.exists(summit_file)) {
  dir.create(dirname(summit_file), recursive = TRUE, showWarnings = FALSE)
  download.file(summit_url, summit_file, mode = "wb", quiet = TRUE)
}

summit <- read_csv(summit_file, show_col_types = FALSE) |>
  filter(!is.na(text), nzchar(text)) |>
  mutate(doc_id = str_c("tweet_", row_number()))

summit_corpus <- corpus(summit, docid_field = "doc_id", text_field = "text")
summit_tokens <- summit_corpus |>
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) |>
  tokens_tolower()
summit_dfm <- summit_tokens |>
  tokens_remove(stopwords("en")) |>
  dfm()

summit_top_words <- topfeatures(summit_dfm, 30) |>
  enframe(name = "feature", value = "frequency")

summit_hashtags <- summit_corpus |>
  tokens(remove_punct = FALSE, remove_url = TRUE) |>
  tokens_tolower() |>
  dfm() |>
  dfm_select(pattern = "#*") |>
  topfeatures(30) |>
  enframe(name = "hashtag", value = "frequency")

summit_mentions <- summit_corpus |>
  tokens(remove_punct = FALSE, remove_url = TRUE) |>
  tokens_tolower() |>
  dfm() |>
  dfm_select(pattern = "@*") |>
  topfeatures(30) |>
  enframe(name = "mention", value = "frequency")

# LSA is descriptive: it maps the archived corpus supplied by the course, not
# an independently drawn sample of public opinion.
summit_lsa_dfm <- summit_dfm |>
  dfm_trim(min_docfreq = 5, docfreq_type = "count")
summit_lsa <- textmodel_lsa(summit_lsa_dfm, nd = 4)
summit_lsa_docs <- as_tibble(summit_lsa$docs, rownames = "doc_id")

write_csv(summit_top_words, file.path(data_dir, "summit_top_words.csv"))
write_csv(summit_hashtags, file.path(data_dir, "summit_top_hashtags.csv"))
write_csv(summit_mentions, file.path(data_dir, "summit_top_mentions.csv"))
write_csv(summit_lsa_docs, file.path(data_dir, "summit_lsa_documents.csv"))

# -----------------------------------------------------------------------------
# Part 2: Compilation of Presidential Documents mentioning Xi Jinping
# -----------------------------------------------------------------------------

cpd_query <- paste(
  'collection:(CPD) AND "Xi Jinping"',
  'AND publishdate:range(2021-01-20,2026-12-31)'
)
cpd_metadata_file <- file.path(data_dir, "cpd_xi_metadata.csv")
collect_cpd <- identical(tolower(Sys.getenv("COLLECT_CPD")), "true")

perform_govinfo <- function(request, api_key) {
  request |>
    req_url_query(api_key = api_key) |>
    req_retry(max_tries = 3, max_seconds = 30) |>
    req_perform()
}

flatten_cpd_result <- function(item) {
  tibble(
    package_id = item$packageId %||% NA_character_,
    title = item$title %||% NA_character_,
    date_issued = as.Date(item$dateIssued %||% NA_character_),
    last_modified = item$lastModified %||% NA_character_,
    text_link = item$download$txtLink %||% NA_character_,
    pdf_link = item$download$pdfLink %||% NA_character_,
    result_link = item$resultLink %||% NA_character_
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

if (collect_cpd) {
  govinfo_key <- Sys.getenv("GOVINFO_API_KEY")
  if (!nzchar(govinfo_key)) {
    stop(
      "GOVINFO_API_KEY is empty. Add it to .Renviron, restart R, and rerun ",
      "with COLLECT_CPD=true. The key is never stored in this project."
    )
  }

  offset_mark <- "*"
  result_pages <- list()
  page_number <- 1L

  repeat {
    body <- list(
      query = cpd_query,
      pageSize = 100,
      offsetMark = offset_mark,
      sorts = list(list(field = "publishdate", sortOrder = "ASC"))
    )

    response <- request("https://api.govinfo.gov/search") |>
      req_body_json(body) |>
      perform_govinfo(govinfo_key) |>
      resp_body_json(simplifyVector = FALSE)

    if (length(response$results) == 0) break
    result_pages[[page_number]] <- map_dfr(
      response$results,
      flatten_cpd_result
    )

    next_mark <- response$nextOffsetMark %||% response$offsetMark %||% NULL
    if (is.null(next_mark) || identical(next_mark, offset_mark)) break
    offset_mark <- next_mark
    page_number <- page_number + 1L
  }

  cpd_metadata <- bind_rows(result_pages) |>
    filter(!is.na(package_id)) |>
    distinct(package_id, .keep_all = TRUE) |>
    arrange(date_issued)
  write_csv(cpd_metadata, cpd_metadata_file)

  download_text <- function(package_id, text_link) {
    destination <- file.path(cpd_dir, str_c(package_id, ".txt"))
    if (file.exists(destination)) return(destination)

    candidate_links <- c(
      text_link,
      str_c(
        "https://www.govinfo.gov/content/pkg/", package_id,
        "/html/", package_id, ".htm"
      )
    ) |>
      discard(is.na) |>
      discard(~ !nzchar(.x))

    for (url in candidate_links) {
      ok <- tryCatch({
        request(url) |>
          req_retry(max_tries = 3, max_seconds = 30) |>
          req_perform(path = destination)
        TRUE
      }, error = function(error) FALSE)
      if (ok) break
    }
    Sys.sleep(0.25)
    if (file.exists(destination)) destination else NA_character_
  }

  cpd_metadata <- cpd_metadata |>
    mutate(local_text = map2_chr(package_id, text_link, download_text))
  write_csv(cpd_metadata, cpd_metadata_file)
}

if (file.exists(cpd_metadata_file)) {
  cpd_metadata <- read_csv(cpd_metadata_file, show_col_types = FALSE) |>
    mutate(date_issued = as.Date(date_issued))

  read_cpd_text <- function(path) {
    text <- read_file(path)
    if (str_detect(str_sub(text, 1, 500), regex("<html|<!doctype", TRUE))) {
      text <- rvest::read_html(text) |>
        rvest::html_text2()
    }
    text
  }

  cpd_documents <- cpd_metadata |>
    filter(!is.na(local_text), file.exists(local_text)) |>
    mutate(
      text = map_chr(local_text, read_cpd_text),
      administration = if_else(
        date_issued < as.Date("2025-01-20"), "Biden", "Trump"
      )
    ) |>
    filter(nchar(text) > 0)

  if (nrow(cpd_documents) > 0) {
    cpd_corpus <- corpus(
      cpd_documents,
      docid_field = "package_id",
      text_field = "text"
    )
    cpd_tokens <- cpd_corpus |>
      tokens(remove_punct = TRUE, remove_url = TRUE) |>
      tokens_tolower()
    cpd_dfm <- cpd_tokens |>
      tokens_remove(stopwords("en")) |>
      dfm()

    cpd_kwic <- kwic(
      cpd_tokens,
      pattern = phrase("xi jinping"),
      window = 10
    ) |>
      as_tibble()

    cpd_keyness <- cpd_dfm |>
      dfm_group(groups = administration) |>
      textstat_keyness(target = "Biden") |>
      as_tibble()

    write_csv(cpd_kwic, file.path(data_dir, "cpd_xi_kwic.csv"))
    write_csv(cpd_keyness, file.path(data_dir, "cpd_keyness_biden.csv"))
  }
}

# -----------------------------------------------------------------------------
# Part 3: inaugural addresses, 1945 onward
# -----------------------------------------------------------------------------

data(data_corpus_inaugural, package = "quanteda")
inaugural <- data_corpus_inaugural |>
  corpus_subset(Year >= 1945)

inaugural_tokens <- inaugural |>
  tokens(remove_punct = TRUE, remove_url = TRUE) |>
  tokens_tolower()
inaugural_dfm <- inaugural_tokens |>
  tokens_remove(stopwords("en")) |>
  dfm()

inaugural_lexdiv <- textstat_lexdiv(inaugural_dfm, measure = "TTR") |>
  as_tibble()
inaugural_readability <- textstat_readability(
  inaugural,
  measure = "Flesch.Kincaid"
) |>
  as_tibble()

inaugural_metrics <- tibble(
  document = docnames(inaugural),
  year = docvars(inaugural, "Year"),
  president = docvars(inaugural, "President"),
  party = docvars(inaugural, "Party"),
  tokens = ntoken(inaugural_tokens)
) |>
  left_join(inaugural_lexdiv, by = c("document" = "document")) |>
  left_join(inaugural_readability, by = c("document" = "document"))

inaugural_similarity <- textstat_simil(
  inaugural_dfm,
  margin = "documents",
  method = "cosine"
) |>
  as.matrix()
diag(inaugural_similarity) <- NA_real_
nearest_index <- apply(inaugural_similarity, 1, which.max)
inaugural_nearest <- tibble(
  document = rownames(inaugural_similarity),
  nearest_document = colnames(inaugural_similarity)[nearest_index],
  cosine_similarity = inaugural_similarity[
    cbind(seq_len(nrow(inaugural_similarity)), nearest_index)
  ]
)

inaugural_keyness <- inaugural_dfm |>
  dfm_group(groups = Party) |>
  textstat_keyness(target = "Democratic") |>
  as_tibble()

wordfish_dfm <- inaugural_dfm |>
  dfm_trim(min_docfreq = 3, docfreq_type = "count", min_termfreq = 5)
wordfish_direction <- match(
  c("1945-Roosevelt", "1981-Reagan"),
  docnames(wordfish_dfm)
)
stopifnot(!anyNA(wordfish_direction))
wordfish <- textmodel_wordfish(
  wordfish_dfm,
  dir = wordfish_direction
)
wordfish_documents <- tibble(
  document = docnames(wordfish_dfm),
  wordfish_position = unname(wordfish$theta)
)

write_csv(inaugural_metrics, file.path(data_dir, "inaugural_metrics.csv"))
write_csv(inaugural_nearest, file.path(data_dir, "inaugural_nearest.csv"))
write_csv(inaugural_keyness, file.path(data_dir, "inaugural_keyness_party.csv"))
write_csv(wordfish_documents, file.path(data_dir, "inaugural_wordfish.csv"))

manifest <- list(
  summit = list(
    source = summit_url,
    selection = "Course archive of tweets collected during the 16 November 2021 summit",
    documents = nrow(summit),
    recollectable = FALSE
  ),
  cpd = list(
    source = "GovInfo Compilation of Presidential Documents",
    query = cpd_query,
    credential = "GOVINFO_API_KEY environment variable",
    collected = file.exists(cpd_metadata_file),
    documents = if (exists("cpd_metadata")) nrow(cpd_metadata) else 0L,
    raw_documents_published = FALSE
  ),
  inaugural = list(
    source = "quanteda data_corpus_inaugural",
    selection = "Addresses from 1945 onward",
    first_year = min(docvars(inaugural, "Year")),
    last_year = max(docvars(inaugural, "Year")),
    documents = ndoc(inaugural)
  ),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
write_json(
  manifest,
  file.path(data_dir, "assignment06_manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
