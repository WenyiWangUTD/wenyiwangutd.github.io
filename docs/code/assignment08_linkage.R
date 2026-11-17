# EPPS 6302 - Assignment 8
# Deterministic and probabilistic linkage of simulated TRS and TEA records.
# Based on record_linkage_workshop.R supplied by the instructor.

library(dplyr)
library(ggplot2)
library(jsonlite)
library(purrr)
library(readr)
library(stringdist)
library(stringr)
library(tibble)
library(tidyr)

set.seed(42)

data_dir <- "data/assignment08"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Data generator and complete answer key
# -----------------------------------------------------------------------------

create_trs_data <- function(n = 500) {
  first_names <- c(
    "John", "Maria", "Robert", "Jennifer", "Michael", "Sarah",
    "David", "Patricia", "James", "Linda", "Daniel", "Barbara"
  )
  last_names <- c(
    "Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia",
    "Miller", "Davis", "Rodriguez", "Martinez", "Hernandez", "Lopez"
  )

  tibble(
    record_id = str_c("TRS_", seq_len(n)),
    first_name = sample(first_names, n, replace = TRUE),
    last_name = sample(last_names, n, replace = TRUE),
    dob = sample(
      seq(as.Date("1950-01-01"), as.Date("1985-12-31"), by = "day"),
      n
    ),
    years_service = sample(5:40, n, replace = TRUE),
    salary_group = sample(
      c("Elementary", "Secondary", "Administration"),
      n,
      replace = TRUE
    )
  )
}

create_tea_data <- function(
    trs,
    n = 500,
    num_matches = 100,
    corruption_rate = 0.20) {
  first_names <- c(
    "John", "Maria", "Robert", "Jennifer", "Michael", "Sarah",
    "David", "Patricia", "James", "Linda", "Daniel", "Barbara",
    "Jon", "Mary", "Rob", "Jen", "Mike", "Sara"
  )
  last_names <- c(
    "Smith", "Smithe", "Smyth", "Johnson", "Jonson", "Williams",
    "Brown", "Jones", "Garcia", "Miller", "Davis", "Rodriguez",
    "Martinez", "Hernandez", "Lopez", "Sanchez"
  )

  tea <- tibble(
    record_id = str_c("TEA_", seq_len(n)),
    first_name = sample(first_names, n, replace = TRUE),
    last_name = sample(last_names, n, replace = TRUE),
    dob = sample(
      seq(as.Date("1950-01-01"), as.Date("1985-12-31"), by = "day"),
      n
    ),
    certification = sample(
      c("Science", "Math", "English", "Social Studies", "Special Ed"),
      n,
      replace = TRUE
    ),
    school_district = sample(str_c("District_", 1:50), n, replace = TRUE)
  )

  tea$first_name[seq_len(num_matches)] <-
    trs$first_name[seq_len(num_matches)]
  tea$last_name[seq_len(num_matches)] <-
    trs$last_name[seq_len(num_matches)]
  tea$dob[seq_len(num_matches)] <- trs$dob[seq_len(num_matches)]

  corrupt_n <- round(num_matches * corruption_rate)
  corruption_indices <- sample(
    seq_len(num_matches),
    size = corrupt_n,
    replace = FALSE
  )

  for (index in corruption_indices) {
    field <- sample(1:3, 1)
    if (field == 1) {
      tea$first_name[index] <- abbreviate(
        tea$first_name[index],
        minlength = 3
      )
    } else if (field == 2) {
      length_name <- nchar(tea$last_name[index])
      position <- sample(seq_len(length_name), 1)
      used_letters <- str_split(tea$last_name[index], "", simplify = TRUE)
      replacement <- sample(setdiff(letters, used_letters), 1)
      tea$last_name[index] <- str_c(
        str_sub(tea$last_name[index], 1, position - 1),
        replacement,
        str_sub(tea$last_name[index], position + 1)
      )
    } else {
      tea$dob[index] <- tea$dob[index] + sample(c(-1, 1), 1)
    }
  }

  tea
}

create_validation_set <- function(num_matches = 100) {
  tibble(
    record_id_a = str_c("TRS_", seq_len(num_matches)),
    record_id_b = str_c("TEA_", seq_len(num_matches))
  )
}

standardize_records <- function(data) {
  data |>
    mutate(
      first_name_std = first_name |>
        str_to_lower() |>
        str_squish() |>
        str_remove_all("[^a-z ]"),
      last_name_std = last_name |>
        str_to_lower() |>
        str_squish() |>
        str_remove_all("[^a-z ]"),
      first_initial = str_sub(first_name_std, 1, 1),
      last_initial = str_sub(last_name_std, 1, 1),
      dob_std = as.Date(dob)
    )
}

# -----------------------------------------------------------------------------
# Deterministic linkage
# -----------------------------------------------------------------------------

keep_one_to_one <- function(pairs) {
  pairs |>
    arrange(record_id_a, record_id_b) |>
    distinct(record_id_a, .keep_all = TRUE) |>
    distinct(record_id_b, .keep_all = TRUE)
}

deterministic_linkage <- function(trs, tea) {
  remaining_a <- trs
  remaining_b <- tea

  rule1 <- remaining_a |>
    inner_join(
      remaining_b,
      by = c(
        "last_name_std" = "last_name_std",
        "first_name_std" = "first_name_std",
        "dob_std" = "dob_std"
      ),
      suffix = c("_a", "_b")
    ) |>
    transmute(
      record_id_a,
      record_id_b,
      rule = "Rule 1: full name + DOB"
    ) |>
    keep_one_to_one()

  remaining_a <- remaining_a |>
    anti_join(rule1, by = c("record_id" = "record_id_a"))
  remaining_b <- remaining_b |>
    anti_join(rule1, by = c("record_id" = "record_id_b"))

  rule2 <- remaining_a |>
    inner_join(
      remaining_b,
      by = c(
        "last_name_std" = "last_name_std",
        "first_initial" = "first_initial",
        "dob_std" = "dob_std"
      ),
      suffix = c("_a", "_b")
    ) |>
    transmute(
      record_id_a,
      record_id_b,
      rule = "Rule 2: last name + initial + DOB"
    ) |>
    keep_one_to_one()

  remaining_a <- remaining_a |>
    anti_join(rule2, by = c("record_id" = "record_id_a"))
  remaining_b <- remaining_b |>
    anti_join(rule2, by = c("record_id" = "record_id_b"))

  rule3 <- remaining_a |>
    transmute(
      record_id_a = record_id,
      first_name_std_a = first_name_std,
      last_name_std_a = last_name_std,
      dob_std_a = dob_std
    ) |>
    cross_join(
      remaining_b |>
        transmute(
          record_id_b = record_id,
          first_name_std_b = first_name_std,
          last_name_std_b = last_name_std,
          dob_std_b = dob_std
        )
    ) |>
    filter(
      first_name_std_a == first_name_std_b,
      last_name_std_a == last_name_std_b,
      abs(as.numeric(dob_std_a - dob_std_b)) <= 1
    ) |>
    transmute(
      record_id_a,
      record_id_b,
      rule = "Rule 3: full name + DOB within one day"
    ) |>
    keep_one_to_one()

  remaining_a <- remaining_a |>
    anti_join(rule3, by = c("record_id" = "record_id_a"))
  remaining_b <- remaining_b |>
    anti_join(rule3, by = c("record_id" = "record_id_b"))

  matches <- bind_rows(rule1, rule2, rule3)
  list(
    matches = matches,
    remaining_a = remaining_a,
    remaining_b = remaining_b,
    summary = matches |>
      count(rule, name = "links") |>
      complete(
        rule = c(
          "Rule 1: full name + DOB",
          "Rule 2: last name + initial + DOB",
          "Rule 3: full name + DOB within one day"
        ),
        fill = list(links = 0L)
      )
  )
}

# -----------------------------------------------------------------------------
# Pair construction, weights, and quality measures
# -----------------------------------------------------------------------------

create_pair_features <- function(data_a, data_b, blocked = FALSE) {
  left <- data_a |>
    transmute(
      record_id_a = record_id,
      first_name_a = first_name_std,
      last_name_a = last_name_std,
      last_initial = last_initial,
      dob_a = dob_std
    )
  right <- data_b |>
    transmute(
      record_id_b = record_id,
      first_name_b = first_name_std,
      last_name_b = last_name_std,
      last_initial = last_initial,
      dob_b = dob_std
    )

  pairs <- if (blocked) {
    inner_join(
      left,
      right,
      by = "last_initial",
      relationship = "many-to-many"
    )
  } else {
    cross_join(
      left |> select(-last_initial),
      right |> select(-last_initial)
    )
  }

  pairs |>
    mutate(
      last_name_sim = 1 - stringdist(
        last_name_a,
        last_name_b,
        method = "jw"
      ),
      first_name_sim = 1 - stringdist(
        first_name_a,
        first_name_b,
        method = "jw"
      ),
      dob_days = abs(as.numeric(dob_a - dob_b)),
      dob_sim = case_when(
        dob_days == 0 ~ 1,
        dob_days == 1 ~ 0.95,
        TRUE ~ 0
      )
    ) |>
    select(
      record_id_a, record_id_b,
      last_name_sim, first_name_sim, dob_sim
    )
}

estimate_agreement_weights <- function(pairs, validation) {
  labeled <- pairs |>
    mutate(
      true_match = paste(record_id_a, record_id_b) %in%
        paste(validation$record_id_a, validation$record_id_b),
      last_agree = last_name_sim >= 0.95,
      first_agree = first_name_sim >= 0.95,
      dob_agree = dob_sim == 1
    )

  estimate_field <- function(agree) {
    m <- (sum(agree[labeled$true_match]) + 0.5) /
      (sum(labeled$true_match) + 1)
    u <- (sum(agree[!labeled$true_match]) + 0.5) /
      (sum(!labeled$true_match) + 1)
    tibble(m, u, log_m_over_u = log(m / u))
  }

  estimates <- bind_rows(
    estimate_field(labeled$last_agree) |> mutate(field = "last_name"),
    estimate_field(labeled$first_agree) |> mutate(field = "first_name"),
    estimate_field(labeled$dob_agree) |> mutate(field = "dob")
  ) |>
    select(field, everything()) |>
    mutate(weight = log_m_over_u / sum(log_m_over_u))

  weights <- setNames(estimates$weight, estimates$field)
  list(table = estimates, weights = weights)
}

score_pairs <- function(pairs, weights) {
  pairs |>
    mutate(
      match_score =
        last_name_sim * weights[["last_name"]] +
        first_name_sim * weights[["first_name"]] +
        dob_sim * weights[["dob"]]
    )
}

choose_one_to_one <- function(scored_pairs, threshold) {
  eligible <- scored_pairs |>
    filter(match_score >= threshold) |>
    arrange(desc(match_score), record_id_a, record_id_b)

  if (nrow(eligible) == 0) {
    return(eligible)
  }

  keep <- logical(nrow(eligible))
  used_a <- character()
  used_b <- character()
  for (row in seq_len(nrow(eligible))) {
    id_a <- eligible$record_id_a[row]
    id_b <- eligible$record_id_b[row]
    if (!id_a %in% used_a && !id_b %in% used_b) {
      keep[row] <- TRUE
      used_a <- c(used_a, id_a)
      used_b <- c(used_b, id_b)
    }
  }
  eligible[keep, ]
}

assess_matches <- function(predicted, validation) {
  predicted <- predicted |>
    distinct(record_id_a, record_id_b)
  true_positive <- predicted |>
    inner_join(validation, by = c("record_id_a", "record_id_b")) |>
    nrow()
  false_positive <- nrow(predicted) - true_positive
  false_negative <- nrow(validation) - true_positive
  precision <- if_else(
    true_positive + false_positive > 0,
    true_positive / (true_positive + false_positive),
    NA_real_
  )
  recall <- true_positive / (true_positive + false_negative)
  f1 <- if_else(
    !is.na(precision) && precision + recall > 0,
    2 * precision * recall / (precision + recall),
    NA_real_
  )

  tibble(
    links = nrow(predicted),
    true_positive,
    false_positive,
    false_negative,
    precision,
    recall,
    f1
  )
}

generate_scenario <- function(corruption_rate) {
  set.seed(42)
  trs <- create_trs_data()
  tea <- create_tea_data(trs, corruption_rate = corruption_rate)
  validation <- create_validation_set()
  list(
    trs = standardize_records(trs),
    tea = standardize_records(tea),
    validation = validation
  )
}

run_linkage <- function(
    corruption_rate,
    weights,
    threshold = 0.95,
    blocked = FALSE) {
  scenario <- generate_scenario(corruption_rate)
  deterministic <- deterministic_linkage(scenario$trs, scenario$tea)
  candidates <- create_pair_features(
    deterministic$remaining_a,
    deterministic$remaining_b,
    blocked = blocked
  ) |>
    score_pairs(weights)
  probabilistic <- choose_one_to_one(candidates, threshold) |>
    mutate(rule = "Probabilistic")
  combined <- bind_rows(
    deterministic$matches,
    probabilistic |> select(record_id_a, record_id_b, rule)
  )

  list(
    scenario = scenario,
    deterministic = deterministic,
    candidates = candidates,
    probabilistic = probabilistic,
    combined = combined,
    metrics = assess_matches(combined, scenario$validation)
  )
}

# -----------------------------------------------------------------------------
# Corruption-rate experiment using the workshop's original weights
# -----------------------------------------------------------------------------

original_weights <- c(last_name = 0.40, first_name = 0.30, dob = 0.30)
corruption_runs <- map(
  c(0.10, 0.20, 0.40),
  ~ run_linkage(.x, original_weights, threshold = 0.95, blocked = FALSE)
)

corruption_results <- map2_dfr(
  corruption_runs,
  c(0.10, 0.20, 0.40),
  function(run, rate) {
    rule_counts <- run$deterministic$summary
    tibble(
      corruption_rate = rate,
      rule1_links = rule_counts$links[
        rule_counts$rule == "Rule 1: full name + DOB"
      ],
      rule2_links = rule_counts$links[
        rule_counts$rule == "Rule 2: last name + initial + DOB"
      ],
      rule3_links = rule_counts$links[
        rule_counts$rule == "Rule 3: full name + DOB within one day"
      ],
      probabilistic_links = nrow(run$probabilistic)
    ) |>
      bind_cols(run$metrics |> select(-links))
  }
)
write_csv(
  corruption_results,
  file.path(data_dir, "corruption_rate_results.csv")
)

# -----------------------------------------------------------------------------
# Baseline data-driven weights and last-name-initial blocking
# -----------------------------------------------------------------------------

baseline <- generate_scenario(0.20)
baseline_det <- deterministic_linkage(baseline$trs, baseline$tea)
all_full_pairs <- create_pair_features(baseline$trs, baseline$tea)
weight_fit <- estimate_agreement_weights(all_full_pairs, baseline$validation)
data_weights <- weight_fit$weights

unblocked_pairs <- create_pair_features(
  baseline_det$remaining_a,
  baseline_det$remaining_b,
  blocked = FALSE
) |>
  score_pairs(data_weights)
blocked_pairs <- create_pair_features(
  baseline_det$remaining_a,
  baseline_det$remaining_b,
  blocked = TRUE
) |>
  score_pairs(data_weights)

unmatched_truth <- baseline$validation |>
  anti_join(
    baseline_det$matches,
    by = c("record_id_a", "record_id_b")
  )
unreachable_truth <- unmatched_truth |>
  anti_join(blocked_pairs, by = c("record_id_a", "record_id_b"))

blocking_summary <- tibble(
  candidates_before = nrow(unblocked_pairs),
  candidates_after = nrow(blocked_pairs),
  reduction_share = 1 - nrow(blocked_pairs) / nrow(unblocked_pairs),
  unmatched_true_pairs = nrow(unmatched_truth),
  true_pairs_removed = nrow(unreachable_truth)
)

# The original function used slice(1:100). In the supplied workshop the answer
# key itself has exactly 100 rows, so removing that shortcut changes no point.
legacy_validation <- baseline$validation |>
  slice(seq_len(min(100, n())))

make_curve <- function(validation) {
  map_dfr(seq(0.70, 0.99, by = 0.01), function(threshold) {
    probabilistic <- choose_one_to_one(blocked_pairs, threshold) |>
      select(record_id_a, record_id_b)
    combined <- bind_rows(
      baseline_det$matches |> select(record_id_a, record_id_b),
      probabilistic
    )
    assess_matches(combined, validation) |>
      mutate(threshold, .before = 1)
  })
}

threshold_curve <- make_curve(baseline$validation)
legacy_curve <- make_curve(legacy_validation)
curve_change <- threshold_curve |>
  select(threshold, precision, recall, f1) |>
  left_join(
    legacy_curve |>
      select(
        threshold,
        legacy_precision = precision,
        legacy_recall = recall,
        legacy_f1 = f1
      ),
    by = "threshold"
  ) |>
  mutate(
    precision_change = precision - legacy_precision,
    recall_change = recall - legacy_recall,
    f1_change = f1 - legacy_f1
  )

# Select the most conservative threshold among those attaining the best F1.
# This prioritizes avoiding false links without sacrificing measured recall.
best_f1 <- max(threshold_curve$f1, na.rm = TRUE)
chosen_threshold <- threshold_curve |>
  filter(abs(f1 - best_f1) < 1e-12) |>
  slice_max(threshold, n = 1, with_ties = FALSE) |>
  pull(threshold)

selected_probabilistic <- choose_one_to_one(
  blocked_pairs,
  chosen_threshold
) |>
  mutate(rule = "Probabilistic")
selected_combined <- bind_rows(
  baseline_det$matches,
  selected_probabilistic |> select(record_id_a, record_id_b, rule)
)

unblocked_selected <- choose_one_to_one(
  unblocked_pairs,
  chosen_threshold
) |>
  select(record_id_a, record_id_b)
blocked_selected <- selected_probabilistic |>
  select(record_id_a, record_id_b)
blocking_quality <- bind_rows(
  assess_matches(
    bind_rows(
      baseline_det$matches |> select(record_id_a, record_id_b),
      unblocked_selected
    ),
    baseline$validation
  ) |>
    mutate(method = "Unblocked"),
  assess_matches(
    bind_rows(
      baseline_det$matches |> select(record_id_a, record_id_b),
      blocked_selected
    ),
    baseline$validation
  ) |>
    mutate(method = "Blocked")
) |>
  select(method, everything())

review_lower <- max(0.70, chosen_threshold - 0.10)
review_pairs <- blocked_pairs |>
  filter(match_score >= review_lower, match_score < chosen_threshold) |>
  mutate(
    true_match = paste(record_id_a, record_id_b) %in%
      paste(
        baseline$validation$record_id_a,
        baseline$validation$record_id_b
      )
  )
review_summary <- tibble(
  lower_threshold = review_lower,
  upper_threshold = chosen_threshold,
  pairs = nrow(review_pairs),
  true_matches = sum(review_pairs$true_match),
  correct_share = if_else(nrow(review_pairs) > 0, mean(review_pairs$true_match), NA_real_)
)

write_csv(weight_fit$table, file.path(data_dir, "estimated_weights.csv"))
write_csv(blocking_summary, file.path(data_dir, "blocking_summary.csv"))
write_csv(blocking_quality, file.path(data_dir, "blocking_quality.csv"))
write_csv(threshold_curve, file.path(data_dir, "threshold_curve.csv"))
write_csv(curve_change, file.path(data_dir, "full_key_curve_change.csv"))
write_csv(review_summary, file.path(data_dir, "review_band.csv"))

# -----------------------------------------------------------------------------
# Method comparison, errors, and downstream count
# -----------------------------------------------------------------------------

deterministic_metrics <- assess_matches(
  baseline_det$matches,
  baseline$validation
) |>
  mutate(method = "Deterministic only")
probabilistic_metrics <- assess_matches(
  selected_probabilistic,
  baseline$validation
) |>
  mutate(method = "Probabilistic additions only")
combined_metrics <- assess_matches(
  selected_combined,
  baseline$validation
) |>
  mutate(method = "Combined")
method_comparison <- bind_rows(
  deterministic_metrics,
  probabilistic_metrics,
  combined_metrics
) |>
  select(method, everything())

scored_full_pairs <- score_pairs(all_full_pairs, data_weights)
predicted_ids <- selected_combined |>
  select(record_id_a, record_id_b)

false_positives <- predicted_ids |>
  anti_join(baseline$validation, by = c("record_id_a", "record_id_b")) |>
  left_join(scored_full_pairs, by = c("record_id_a", "record_id_b")) |>
  mutate(
    responsible_field = case_when(
      dob_sim * data_weights[["dob"]] >=
        pmax(
          last_name_sim * data_weights[["last_name"]],
          first_name_sim * data_weights[["first_name"]]
        ) ~ "DOB agreement",
      last_name_sim * data_weights[["last_name"]] >=
        first_name_sim * data_weights[["first_name"]] ~ "last-name similarity",
      TRUE ~ "first-name similarity"
    ),
    cause = "unrelated people with similar identifiers",
    error_type = "False positive"
  )

false_negatives <- baseline$validation |>
  anti_join(predicted_ids, by = c("record_id_a", "record_id_b")) |>
  left_join(scored_full_pairs, by = c("record_id_a", "record_id_b")) |>
  mutate(
    responsible_field = case_when(
      dob_sim < pmin(last_name_sim, first_name_sim) ~ "DOB",
      last_name_sim <= first_name_sim ~ "last name",
      TRUE ~ "first name"
    ),
    cause = case_when(
      dob_sim < 1 ~ "date corruption",
      last_name_sim < 0.95 ~ "last-name typo",
      first_name_sim < 0.95 ~ "first-name abbreviation",
      TRUE ~ "threshold or one-to-one competition"
    ),
    error_type = "False negative"
  )

error_examples <- bind_rows(
  false_positives |> slice_head(n = 10),
  false_negatives |> slice_head(n = 10)
) |>
  select(
    error_type, record_id_a, record_id_b,
    last_name_sim, first_name_sim, dob_sim, match_score,
    responsible_field, cause
  )
error_causes <- bind_rows(false_positives, false_negatives) |>
  count(error_type, cause, name = "errors")

# The chosen threshold happens to make no errors in this simple simulation, so
# there are not ten false positives or ten false negatives to list. Diagnose
# the nearest thresholds that do fail instead: 0.89 admits the closest false
# link, while 0.99 misses the most borderline true links.
diagnostic_low_threshold <- 0.89
diagnostic_high_threshold <- 0.99
diagnostic_low_predicted <- bind_rows(
  baseline_det$matches |> select(record_id_a, record_id_b),
  choose_one_to_one(blocked_pairs, diagnostic_low_threshold) |>
    select(record_id_a, record_id_b)
) |>
  distinct()
diagnostic_high_predicted <- bind_rows(
  baseline_det$matches |> select(record_id_a, record_id_b),
  choose_one_to_one(blocked_pairs, diagnostic_high_threshold) |>
    select(record_id_a, record_id_b)
) |>
  distinct()

sensitivity_false_positives <- diagnostic_low_predicted |>
  anti_join(baseline$validation, by = c("record_id_a", "record_id_b")) |>
  left_join(scored_full_pairs, by = c("record_id_a", "record_id_b")) |>
  mutate(
    diagnostic_threshold = diagnostic_low_threshold,
    error_type = "False positive",
    responsible_field = "DOB and last-name agreement",
    cause = "unrelated people with similar identifiers"
  )
sensitivity_false_negatives <- baseline$validation |>
  anti_join(
    diagnostic_high_predicted,
    by = c("record_id_a", "record_id_b")
  ) |>
  left_join(scored_full_pairs, by = c("record_id_a", "record_id_b")) |>
  mutate(
    diagnostic_threshold = diagnostic_high_threshold,
    error_type = "False negative",
    responsible_field = case_when(
      dob_sim < pmin(last_name_sim, first_name_sim) ~ "DOB",
      last_name_sim <= first_name_sim ~ "last name",
      TRUE ~ "first name"
    ),
    cause = case_when(
      dob_sim < 1 ~ "date corruption",
      last_name_sim < 0.95 ~ "last-name typo",
      first_name_sim < 0.95 ~ "first-name abbreviation",
      TRUE ~ "threshold or one-to-one competition"
    )
  )
sensitivity_errors <- bind_rows(
  sensitivity_false_positives,
  sensitivity_false_negatives
) |>
  select(
    diagnostic_threshold, error_type, record_id_a, record_id_b,
    last_name_sim, first_name_sim, dob_sim, match_score,
    responsible_field, cause
  )
sensitivity_error_causes <- sensitivity_errors |>
  count(error_type, cause, name = "errors")

combined_row <- combined_metrics
downstream <- tibble(
  true_overlap = nrow(baseline$validation),
  estimated_overlap = combined_row$links,
  false_positive_overcount = combined_row$false_positive,
  false_negative_undercount = combined_row$false_negative,
  net_bias = combined_row$false_positive - combined_row$false_negative,
  direction = case_when(
    net_bias > 0 ~ "overcount",
    net_bias < 0 ~ "undercount",
    TRUE ~ "no net bias"
  )
)

det_true <- deterministic_metrics$true_positive
prob_additions <- tibble(
  deterministic_true_matches = det_true,
  additional_true_matches = combined_metrics$true_positive - det_true,
  additional_false_matches = combined_metrics$false_positive -
    deterministic_metrics$false_positive
)

write_csv(method_comparison, file.path(data_dir, "method_comparison.csv"))
write_csv(error_examples, file.path(data_dir, "error_examples.csv"))
write_csv(error_causes, file.path(data_dir, "error_causes.csv"))
write_csv(
  sensitivity_errors,
  file.path(data_dir, "sensitivity_error_examples.csv")
)
write_csv(
  sensitivity_error_causes,
  file.path(data_dir, "sensitivity_error_causes.csv")
)
write_csv(downstream, file.path(data_dir, "downstream_count_bias.csv"))
write_csv(prob_additions, file.path(data_dir, "probabilistic_additions.csv"))

manifest <- list(
  source = "Simulated TRS and TEA records from record_linkage_workshop.R",
  seed = 42,
  records_per_file = 500,
  true_matches = 100,
  corruption_rates = c(0.10, 0.20, 0.40),
  deterministic_rules = c(
    "full standardized name and DOB",
    "standardized last name, first initial, and DOB",
    "full standardized name and DOB within one day"
  ),
  blocking = "first letter of standardized last name",
  weights = as.list(data_weights),
  threshold = chosen_threshold,
  review_band = c(review_lower, chosen_threshold),
  selection = "one-to-one greedy selection in descending score order",
  raw_records_published = FALSE,
  known_gaps = paste(
    "Simulation has a complete answer key, limited name diversity, one",
    "corruption per affected match, and no missing fields or group-specific",
    "error process. Real administrative files need clerical review and",
    "independent validation."
  ),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
write_json(
  manifest,
  file.path(data_dir, "assignment08_manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
