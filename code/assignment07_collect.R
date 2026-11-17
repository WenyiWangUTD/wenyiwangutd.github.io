# EPPS 6302 - Assignment 7
# YouTube searches, channel uploads, comments, manifests, and aggregate text
# analysis for the 2026 US midterm election.

required_packages <- c(
  "dplyr", "jsonlite", "purrr", "quanteda", "quanteda.textstats",
  "readr", "stringr", "tibble", "tidyr", "tuber"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    ". For the required development version of tuber, run: ",
    'pak::pak(c("gojiplus/tuber", "httr2"))'
  )
}

library(dplyr)
library(jsonlite)
library(purrr)
library(quanteda)
library(quanteda.textstats)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(tuber)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

if (packageVersion("tuber") < "2.0.0.9000") {
  stop('tuber 2.0.0.9000 or later is required. Run pak::pak("gojiplus/tuber").')
}
if (!nzchar(Sys.getenv("YOUTUBE_KEY"))) {
  stop(
    "YOUTUBE_KEY is empty. Add it to ~/.Renviron, restart R, and rerun. ",
    "The key is never written to this project."
  )
}

data_dir <- "data/assignment07"
private_dir <- "data/youtube/private"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(private_dir, recursive = TRUE, showWarnings = FALSE)
refresh_youtube <- identical(
  tolower(Sys.getenv("REFRESH_YOUTUBE")),
  "true"
)

search_term <- "US midterm election"
pre_start <- as.POSIXct("2026-09-01 00:00:00", tz = "UTC")
pre_end <- as.POSIXct("2026-10-28 00:00:00", tz = "UTC")
post_start <- as.POSIXct("2026-11-04 00:00:00", tz = "UTC")
post_end <- as.POSIXct(
  str_c(Sys.getenv("YOUTUBE_POST_END", unset = as.character(Sys.Date())),
        " 23:59:59"),
  tz = "UTC"
)

# Write the quota plan before making the first request.
quota_plan <- tribble(
  ~operation, ~calls_or_pages, ~unit_cost, ~planned_units, ~search_calls,
  "pre-election search: relevance", 2L, 100L, 200L, 2L,
  "pre-election search: viewCount", 2L, 100L, 200L, 2L,
  "post-election search: relevance", 2L, 100L, 200L, 2L,
  "post-election search: viewCount", 2L, 100L, 200L, 2L,
  "three channel upload playlists", 200L, 1L, 200L, 0L,
  "video and channel statistics", 12L, 1L, 12L, 0L,
  "comments: 18 videos x up to 3 pages", 54L, 1L, 54L, 0L
)
write_csv(quota_plan, file.path(data_dir, "quota_plan.csv"))

search_window <- function(period, start, end, order) {
  result <- yt_search(
    term = search_term,
    published_after = format(start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    published_before = format(end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    order = order,
    max_results = 100
  )
  total_reported <- attr(result, "total_results") %||% NA_integer_
  result |>
    as_tibble() |>
    mutate(period, order, total_reported, .before = 1)
}

search_cache <- file.path(private_dir, "search_results.rds")
if (file.exists(search_cache) && !refresh_youtube) {
  search_sets <- readRDS(search_cache)
} else {
  search_sets <- list(
    pre_relevance = search_window("before", pre_start, pre_end, "relevance"),
    pre_views = search_window("before", pre_start, pre_end, "viewCount"),
    post_relevance = search_window("after", post_start, post_end, "relevance"),
    post_views = search_window("after", post_start, post_end, "viewCount")
  )
  saveRDS(search_sets, search_cache)
}

search_summary <- imap_dfr(search_sets, function(x, name) {
  tibble(
    search = name,
    period = first(x$period),
    order = first(x$order),
    returned = nrow(x),
    total_reported = first(x$total_reported),
    duplicate_video_ids = sum(duplicated(x$video_id)),
    unique_video_ids = n_distinct(x$video_id)
  )
})

search_overlap <- tibble(
  period = c("before", "after"),
  overlap = c(
    length(intersect(
      search_sets$pre_relevance$video_id,
      search_sets$pre_views$video_id
    )),
    length(intersect(
      search_sets$post_relevance$video_id,
      search_sets$post_views$video_id
    ))
  )
)

search_channel_counts <- bind_rows(search_sets) |>
  distinct(period, order, video_id, .keep_all = TRUE) |>
  count(period, order, channel_title, sort = TRUE)

write_csv(search_summary, file.path(data_dir, "search_summary.csv"))
write_csv(search_overlap, file.path(data_dir, "search_overlap.csv"))
write_csv(
  search_channel_counts,
  file.path(data_dir, "search_channel_counts.csv")
)

# Three publisher types. These IDs identify the publishing channels, not
# commenters: national network, cable news, and international broadcaster.
channels <- tribble(
  ~channel_id, ~channel_title, ~channel_kind,
  "UCeY0bbntWzzVIaj2z3QigXg", "NBC News", "national network",
  "UCupvZG-5ko_eiXAupbDfxWw", "CNN", "cable news",
  "UC16niRr50-MSBwiO3YDb3RA", "BBC News", "international broadcaster"
)
title_pattern_text <- "midterm|senate race|house race|election day|ballot"
title_pattern <- regex(
  title_pattern_text,
  ignore_case = TRUE
)

collect_channel_uploads <- function(channel_id, channel_title, channel_kind) {
  uploads <- list_channel_videos(channel_id, max_results = 4000) |>
    as_tibble() |>
    mutate(
      channel_id,
      channel_title,
      channel_kind,
      date = as.Date(video_published_at),
      title_kept = str_detect(title, title_pattern)
    ) |>
    arrange(desc(date))

  reached_index <- which(uploads$date <= as.Date(pre_start))[1]
  pages_used <- if (is.na(reached_index)) {
    ceiling(nrow(uploads) / 50)
  } else {
    ceiling(reached_index / 50)
  }
  tibble(
    channel_id,
    channel_title,
    channel_kind,
    pages_used,
    pages_downloaded = ceiling(nrow(uploads) / 50),
    earliest_date = min(uploads$date, na.rm = TRUE),
    reached_september = any(uploads$date <= as.Date(pre_start)),
    kept = sum(uploads$title_kept),
    total = nrow(uploads),
    uploads = list(uploads)
  )
}

channel_cache <- file.path(private_dir, "channel_uploads.rds")
if (file.exists(channel_cache) && !refresh_youtube) {
  channel_uploads <- readRDS(channel_cache)
} else {
  channel_runs <- pmap_dfr(channels, collect_channel_uploads)
  channel_uploads <- channel_runs |>
    select(uploads) |>
    unnest(uploads)
  write_csv(
    channel_runs |> select(-uploads),
    file.path(data_dir, "channel_route_summary.csv")
  )
  saveRDS(channel_uploads, channel_cache)
}

# Reapply the current title rule even when the raw upload list is cached.
channel_uploads <- channel_uploads |>
  mutate(title_kept = str_detect(title, title_pattern))
channel_route_summary <- channel_uploads |>
  group_by(channel_id, channel_title, channel_kind) |>
  summarise(
    pages_used = {
      reached_index <- which(date <= as.Date(pre_start))[1]
      if (is.na(reached_index)) ceiling(n() / 50) else ceiling(reached_index / 50)
    },
    pages_downloaded = ceiling(n() / 50),
    earliest_date = min(date, na.rm = TRUE),
    reached_september = any(date <= as.Date(pre_start)),
    kept = sum(title_kept),
    total = n(),
    .groups = "drop"
  )
write_csv(
  channel_route_summary,
  file.path(data_dir, "channel_route_summary.csv")
)

candidate_videos <- channel_uploads |>
  filter(title_kept) |>
  mutate(
    period = case_when(
      date >= as.Date(pre_start) & date < as.Date(pre_end) ~ "before",
      date >= as.Date(post_start) & date <= as.Date(post_end) ~ "after",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period)) |>
  distinct(video_id, .keep_all = TRUE)

title_filter_review <- channel_uploads |>
  mutate(
    period = case_when(
      date >= as.Date(pre_start) & date < as.Date(pre_end) ~ "before",
      date >= as.Date(post_start) & date <= as.Date(post_end) ~ "after",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period)) |>
  group_by(channel_title, period, title_kept) |>
  slice_head(n = 20) |>
  ungroup() |>
  select(channel_title, period, title_kept, date, title, video_id)
write_csv(
  title_filter_review,
  file.path(data_dir, "title_filter_review.csv")
)

search_video_ids <- bind_rows(search_sets) |>
  distinct(period, video_id)
channel_route_comparison <- candidate_videos |>
  left_join(
    search_video_ids |> mutate(found_by_search = TRUE),
    by = c("period", "video_id")
  ) |>
  mutate(found_by_search = coalesce(found_by_search, FALSE)) |>
  group_by(channel_title, period) |>
  summarise(
    eligible_channel_videos = n(),
    also_in_search = sum(found_by_search),
    missed_by_search = sum(!found_by_search),
    .groups = "drop"
  )
write_csv(
  channel_route_comparison,
  file.path(data_dir, "channel_route_comparison.csv")
)

saveRDS(candidate_videos, file.path(private_dir, "candidate_videos.rds"))

stats_cache <- file.path(private_dir, "video_stats.rds")
if (file.exists(stats_cache) && !refresh_youtube) {
  video_stats <- readRDS(stats_cache)
} else {
  video_stats <- get_video_stats(
    candidate_videos$video_id,
    include_content_details = TRUE
  ) |>
    as_tibble() |>
    distinct(video_id, .keep_all = TRUE)
  saveRDS(video_stats, stats_cache)
}

video_info <- candidate_videos |>
  left_join(video_stats, by = "video_id", relationship = "one-to-one")

targets <- video_info |>
  filter(!is.na(comment_count), comment_count > 0) |>
  group_by(channel_title, period) |>
  slice_max(comment_count, n = 3, with_ties = FALSE) |>
  ungroup() |>
  distinct(video_id, .keep_all = TRUE)
saveRDS(targets, file.path(private_dir, "targets.rds"))

collect_comments <- function(video_id, n_max = 300) {
  status <- "ok"
  out <- tryCatch(
    withCallingHandlers(
      get_all_comments(video_id, max_results = n_max),
      tuber_comments_disabled = function(warning) {
        status <<- "disabled"
        invokeRestart("muffleWarning")
      },
      tuber_no_comments = function(warning) {
        if (status == "ok") status <<- "none"
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) error
  )
  Sys.sleep(1)

  if (inherits(out, "error")) {
    return(list(
      data = NULL,
      log = tibble(
        video_id, status = "error", received = 0L,
        api_calls = NA_real_, message = conditionMessage(out)
      )
    ))
  }

  list(
    data = as_tibble(out),
    log = tibble(
      video_id, status, received = nrow(out),
      api_calls = attr(out, "tuber_api_calls") %||% NA_real_,
      message = NA_character_
    )
  )
}

comment_runs_cache <- file.path(private_dir, "comment_runs.rds")
comment_targets_cache <- file.path(private_dir, "comment_run_targets.rds")
comment_cache_matches <-
  file.exists(comment_runs_cache) &&
  file.exists(comment_targets_cache) &&
  identical(readRDS(comment_targets_cache), targets$video_id)
if (comment_cache_matches && !refresh_youtube) {
  comment_runs <- readRDS(comment_runs_cache)
} else {
  comment_runs <- map(targets$video_id, collect_comments, .progress = TRUE)
  saveRDS(comment_runs, comment_runs_cache)
  saveRDS(targets$video_id, comment_targets_cache)
}
collection_log <- map(comment_runs, "log") |>
  bind_rows() |>
  left_join(
    targets |>
      select(video_id, channel_title, period, reported = comment_count),
    by = "video_id",
    relationship = "many-to-one"
  ) |>
  mutate(
    difference = reported - received,
    collected_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
write_csv(collection_log, file.path(data_dir, "collection_log.csv"))

comment_data <- map2(
  comment_runs,
  targets$video_id,
  function(run, target_video_id) {
    if (is.null(run$data)) return(NULL)
    as_tibble(run$data) |>
      mutate(video_id = coalesce(video_id, target_video_id))
  }
) |>
  compact()
if (length(comment_data) == 0) {
  stop(
    "No comments were returned. Inspect data/assignment07/collection_log.csv."
  )
}

comments <- comment_data |>
  bind_rows() |>
  distinct(comment_id, .keep_all = TRUE) |>
  left_join(
    targets |>
      select(video_id, video_title = title, channel_title, period),
    by = "video_id",
    relationship = "many-to-one"
  ) |>
  mutate(
    is_reply = !is.na(parent_id),
    date = as.Date(published_at)
  )

saveRDS(comments, file.path(private_dir, "comments.rds"))

top_one_percent <- max(1L, ceiling(0.01 * nrow(comments)))
top_ten_percent_authors <- max(
  1L,
  ceiling(0.10 * n_distinct(comments$author_name))
)

comment_count_total <- nrow(comments)
video_count_total <- n_distinct(comments$video_id)
channel_count_total <- n_distinct(comments$channel_title)
reply_share_total <- mean(comments$is_reply)
top_like_share <- comments |>
  arrange(desc(like_count)) |>
  summarise(
    share = if_else(
      sum(like_count, na.rm = TRUE) > 0,
      sum(head(like_count, top_one_percent), na.rm = TRUE) /
        sum(like_count, na.rm = TRUE),
      0
    )
  ) |>
  pull(share)
top_author_share <- comments |>
  count(author_name, sort = TRUE) |>
  summarise(share = sum(head(n, top_ten_percent_authors)) / sum(n)) |>
  pull(share)
duplicate_text_count <- sum(duplicated(comments$text_original))

collection_summary <- tibble(
  comments = comment_count_total,
  videos = video_count_total,
  channels = channel_count_total,
  reply_share = reply_share_total,
  top_one_percent_like_share = top_like_share,
  top_ten_percent_author_share = top_author_share,
  exact_duplicate_texts = duplicate_text_count
)
write_csv(collection_summary, file.path(data_dir, "collection_summary.csv"))

# One document per comment. Author names stay in the private RDS and are not
# docvars in the published analytical object.
comment_corpus <- comments |>
  select(
    comment_id, text_original, channel_title, video_id,
    is_reply, like_count, date, period
  ) |>
  corpus(docid_field = "comment_id", text_field = "text_original")

comment_tokens <- comment_corpus |>
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) |>
  tokens_tolower()

phrases <- phrase(c(
  "house majority", "senate race", "early voting", "mail in",
  "election fraud", "voter turnout", "swing state"
))
analysis_tokens <- comment_tokens |>
  tokens_compound(phrases) |>
  tokens_remove(stopwords("en"))
comment_dfm <- dfm(analysis_tokens)

kwic_private <- map_dfr(
  c("democrat*", "republican*", "trump", "vote*", "ballot*"),
  ~ kwic(comment_tokens, pattern = .x, window = 8) |>
    as_tibble() |>
    mutate(pattern = .x, .before = 1) |>
    slice_head(n = 20)
)
write_csv(kwic_private, file.path(private_dir, "kwic_review.csv"))

channel_keyness <- map_dfr(
  unique(na.omit(comments$channel_title)),
  function(target_channel) {
    comment_dfm |>
      dfm_group(groups = channel_title) |>
      textstat_keyness(target = target_channel) |>
      as_tibble() |>
      slice_max(abs(chi2), n = 20) |>
      mutate(target_channel, .before = 1)
  }
)
period_keyness <- comment_dfm |>
  dfm_group(groups = period) |>
  textstat_keyness(target = "after") |>
  as_tibble()

sentiment <- comment_dfm |>
  dfm_lookup(dictionary = data_dictionary_LSD2015) |>
  convert(to = "data.frame") |>
  as_tibble() |>
  rename(comment_id = doc_id) |>
  left_join(
    comments |> select(comment_id, channel_title, period),
    by = "comment_id",
    relationship = "one-to-one"
  ) |>
  mutate(
    positive = positive + neg_negative,
    negative = negative + neg_positive,
    sentiment = positive - negative
  )

sentiment_summary <- sentiment |>
  group_by(channel_title, period) |>
  summarise(
    comments = n(),
    mean_sentiment = mean(sentiment),
    positive_share = mean(sentiment > 0),
    negative_share = mean(sentiment < 0),
    .groups = "drop"
  )

set.seed(6302)
validation_path <- file.path(
  private_dir,
  "sentiment_validation_template.csv"
)
existing_validation <- if (file.exists(validation_path)) {
  read_csv(validation_path, show_col_types = FALSE)
} else {
  NULL
}
has_manual_labels <- !is.null(existing_validation) &&
  "human_label" %in% names(existing_validation) &&
  any(!is.na(existing_validation$human_label))

if (has_manual_labels) {
  validation_sample <- existing_validation
} else {
  validation_sample <- comments |>
    slice_sample(n = min(40L, nrow(comments))) |>
    transmute(
      comment_id,
      text = text_original,
      dictionary_label = case_when(
        sentiment$sentiment[match(comment_id, sentiment$comment_id)] > 0 ~ "positive",
        sentiment$sentiment[match(comment_id, sentiment$comment_id)] < 0 ~ "negative",
        TRUE ~ "neither"
      ),
      human_label = NA_character_
    )
  write_csv(validation_sample, validation_path)
}

valid_labels <- c("positive", "negative", "neither")
manual_validation_complete <- nrow(validation_sample) == 40L &&
  all(validation_sample$human_label %in% valid_labels)
if (manual_validation_complete) {
  validation_summary <- tibble(
    sample_size = nrow(validation_sample),
    agreement = mean(
      validation_sample$dictionary_label == validation_sample$human_label
    )
  )
  validation_confusion <- validation_sample |>
    count(human_label, dictionary_label, name = "comments")
  write_csv(
    validation_summary,
    file.path(data_dir, "sentiment_validation_summary.csv")
  )
  write_csv(
    validation_confusion,
    file.path(data_dir, "sentiment_validation_confusion.csv")
  )
}

write_csv(channel_keyness, file.path(data_dir, "channel_keyness.csv"))
write_csv(period_keyness, file.path(data_dir, "period_keyness.csv"))
write_csv(sentiment_summary, file.path(data_dir, "sentiment_summary.csv"))

write_period_manifest <- function(period_name, start, end) {
  period_targets <- targets |> filter(period == period_name)
  period_comments <- comments |> filter(period == period_name)
  manifest <- list(
    source = "YouTube Data API v3 via tuber",
    permission = paste(
      "API key; quota plan; non-authorized comments retained no more",
      "than 30 days under Developer Policies III.E.4"
    ),
    collected_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    search = list(
      term = search_term,
      orders = c("relevance", "viewCount"),
      published_after = format(start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      published_before = format(end, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    channels = channels,
    title_filter = title_pattern_text,
    selection = "Three highest-comment-count eligible videos per channel and period",
    comment_cap_per_video = 300,
    videos = nrow(period_targets),
    comments = nrow(period_comments),
    known_gaps = paste(
      "Search ranking, title filter, disabled or moderated comments, deleted",
      "comments, replies beyond the cap, and collection timing"
    ),
    delete_by = as.character(Sys.Date() + 30),
    raw_comments_published = FALSE,
    key_recorded = FALSE
  )
  write_json(
    manifest,
    file.path(data_dir, str_c("manifest_", period_name, ".json")),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

write_period_manifest("before", pre_start, pre_end)
write_period_manifest("after", post_start, post_end)

quota_actual_file <- file.path(data_dir, "quota_actual.csv")

# Preserve the audit record from the collection run. A cache-only rerun makes
# no API calls, so its in-memory counter is zero and must not overwrite the
# quota actually used to create the cached dataset.
if (refresh_youtube || !file.exists(quota_actual_file)) {
  quota_actual <- yt_get_quota_usage() |>
    as_tibble() |>
    mutate(
      reset_time = format(reset_time, tz = "America/Chicago", usetz = TRUE)
    )
  write_csv(quota_actual, quota_actual_file)
}
