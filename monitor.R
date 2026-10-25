
# monitor.R
# Daily UN General Assembly voting digest
#
# Required GitHub Actions secrets:
# SMTP_USER, SMTP_PASS, SMTP_HOST, SMTP_PORT, EMAIL_TO
#
# First run creates a baseline without emailing the entire historical dataset.
# Subsequent runs email newly detected resolution records.

packages <- c("readr", "dplyr", "stringr", "rvest", "xml2", "blastula")
missing <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(packages, library, character.only = TRUE))

dir.create("state", showWarnings = FALSE, recursive = TRUE)

record_url <- "https://digitallibrary.un.org/record/4060887?ln=en"
state_file <- "state/resolution_counts.csv"

# 1. Discover the current CSV download from the official UN record.
page <- rvest::read_html(record_url)
hrefs <- rvest::html_attr(
  rvest::html_elements(page, "a[href]"), "href"
)

csv_links <- hrefs[
  stringr::str_detect(hrefs, stringr::regex("\\.csv($|\\?)", ignore_case = TRUE)) &
    stringr::str_detect(hrefs, stringr::regex("ga_voting", ignore_case = TRUE))
]

if (!length(csv_links)) {
  stop(
    "Could not discover the UN voting CSV. Check the dataset page and update ",
    "the link-discovery logic: ", record_url
  )
}

csv_url <- utils::URLdecode(csv_links[[1]])
if (startsWith(csv_url, "/")) {
  csv_url <- paste0("https://digitallibrary.un.org", csv_url)
} else if (!startsWith(csv_url, "http")) {
  csv_url <- paste0("https://digitallibrary.un.org/", csv_url)
}

message("Downloading: ", csv_url)
votes <- readr::read_csv(
  csv_url,
  show_col_types = FALSE,
  progress = FALSE,
  name_repair = "unique"
)

# 2. Map common column names to a standard schema.
# If the UN changes its schema, this deliberately stops rather than
# silently sending a misleading report.
normalise_name <- function(x) {
  tolower(gsub("[^a-z0-9]", "", x))
}

find_col <- function(data, candidates, required = TRUE) {
  actual <- normalise_name(names(data))
  wanted <- normalise_name(candidates)
  found <- which(actual %in% wanted)

  if (!length(found)) {
    if (required) {
      stop(
        "Cannot identify a required column. Available columns: ",
        paste(names(data), collapse = ", "),
        ". Update the column mapping in monitor.R."
      )
    }
    return(NULL)
  }

  names(data)[found[[1]]]
}

resolution_col <- find_col(
  votes,
  c("resolution_symbol", "document_symbol", "resolution", "symbol",
    "resolution_id", "resolution_number")
)
country_col <- find_col(
  votes,
  c("country", "member_state", "country_name", "member_state_name")
)
vote_col <- find_col(
  votes,
  c("vote", "vote_position", "voting", "vote_value")
)
date_col <- find_col(
  votes,
  c("date", "resolution_date", "vote_date", "meeting_date"),
  required = FALSE
)
title_col <- find_col(
  votes,
  c("title", "resolution_title", "document_title", "subject"),
  required = FALSE
)

clean <- tibble::tibble(
  resolution = as.character(votes[[resolution_col]]),
  country = as.character(votes[[country_col]]),
  vote = as.character(votes[[vote_col]]),
  date = if (is.null(date_col)) NA_character_ else
    as.character(votes[[date_col]]),
  title = if (is.null(title_col)) NA_character_ else
    as.character(votes[[title_col]])
) |>
  dplyr::mutate(
    resolution = stringr::str_trim(resolution),
    country = stringr::str_trim(country),
    vote = toupper(stringr::str_trim(vote))
  ) |>
  dplyr::filter(
    !is.na(resolution), resolution != "",
    !is.na(country), country != ""
  )

# 3. Count rows per resolution. This compact state file allows us to
# detect new resolutions or additional country-vote rows on old ones.
current_counts <- clean |>
  dplyr::count(resolution, name = "row_count")

if (!file.exists(state_file)) {
  readr::write_csv(current_counts, state_file)
  message(
    "Initial baseline saved. No email sent to avoid reporting the entire ",
    "historical dataset. The next run will detect additions."
  )
  quit(save = "no", status = 0)
}

previous_counts <- readr::read_csv(state_file, show_col_types = FALSE)

changes <- current_counts |>
  dplyr::left_join(previous_counts, by = "resolution",
                   suffix = c("_now", "_before")) |>
  dplyr::mutate(
    row_count_before = dplyr::coalesce(row_count_before, 0L),
    added_rows = row_count_now - row_count_before
  ) |>
  dplyr::filter(added_rows > 0)

# Update state only after successfully downloading and parsing the dataset.
# Email failure can be retried manually; the state is committed by the workflow.
if (nrow(changes) == 0) {
  readr::write_csv(current_counts, state_file)
  message("No new voting records detected.")
  quit(save = "no", status = 0)
}

new_symbols <- changes$resolution

# 4. Build a concise digest for affected resolutions.
digest_data <- clean |>
  dplyr::filter(resolution %in% new_symbols) |>
  dplyr::group_by(resolution) |>
  dplyr::summarise(
    date = dplyr::first(stats::na.omit(date), default = NA_character_),
    title = dplyr::first(stats::na.omit(title), default = NA_character_),
    yes = sum(vote %in% c("Y", "YES"), na.rm = TRUE),
    no = sum(vote %in% c("N", "NO"), na.rm = TRUE),
    abstain = sum(vote %in% c("A", "ABSTAIN"), na.rm = TRUE),
    countries_recorded = dplyr::n_distinct(country),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(date))

# Use plain Markdown for a portable HTML email.
lines <- c(
  "# UN General Assembly voting digest",
  "",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M UTC")),
  paste("Resolutions with new or additional records:", nrow(changes)),
  "",
  "## New or updated resolutions",
  ""
)

for (i in seq_len(nrow(digest_data))) {
  x <- digest_data[i, ]

  lines <- c(
    lines,
    paste0("### ", x$resolution),
    paste0("**Date:** ", ifelse(is.na(x$date), "Not supplied", x$date)),
    paste0("**Title:** ", ifelse(is.na(x$title), "Not supplied", x$title)),
    paste0(
      "**Recorded positions:** Yes ", x$yes,
      " | No ", x$no,
      " | Abstain ", x$abstain,
      " | Countries with records ", x$countries_recorded
    ),
    paste0(
      "**Source:** https://digitallibrary.un.org/search?ln=en&p=",
      utils::URLencode(x$resolution, reserved = TRUE)
    ),
    ""
  )
}

lines <- c(
  lines,
  "---",
  "Source: UN Digital Library General Assembly voting dataset.",
  "This digest detects dataset additions; it does not establish political intent."
)

email <- blastula::compose_email(
  body = blastula::md(paste(lines, collapse = "\n"))
)

# 5. Send using credentials stored in GitHub Actions secrets.
required_env <- c("SMTP_USER", "SMTP_PASS", "SMTP_HOST",
                  "SMTP_PORT", "EMAIL_TO")
missing_env <- required_env[
  !nzchar(vapply(required_env, Sys.getenv, character(1)))
]
if (length(missing_env)) {
  stop("Missing environment variables: ", paste(missing_env, collapse = ", "))
}

credentials <- blastula::creds_envvar(
  user = Sys.getenv("SMTP_USER"),
  pass_envvar = "SMTP_PASS",
  host = Sys.getenv("SMTP_HOST"),
  port = as.integer(Sys.getenv("SMTP_PORT")),
  use_ssl = TRUE
)

blastula::smtp_send(
  email,
  to = Sys.getenv("EMAIL_TO"),
  from = Sys.getenv("SMTP_USER"),
  subject = paste0(
    "UN voting digest: ", nrow(changes), " resolution update(s)"
  ),
  credentials = credentials
)

# Save state after a successful email send.
readr::write_csv(current_counts, state_file)
message("Digest sent; state updated.")
