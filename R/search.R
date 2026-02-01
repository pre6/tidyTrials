
#' Fetch clinical trial records from ClinicalTrials.gov
#'
#' Retrieves clinical trial records from the ClinicalTrials.gov v2 API
#' using keyword-based search and automatic pagination.
#'
#' @param query Character string of search keywords.
#' @param max_records Maximum number of studies to retrieve.
#' @param .page_size Number of records per API request (internal).
#' @param phase Optional phase filter for the clinical trials.
#' @param country Optional country filter for the clinical trials.
#' @param from_date for filtering trials (YYYY-MM-DD or default "MIN")
#' @param to_date for filtering trials (YYYY-MM-DD or default "MAX").
#' @param date_filter Date field to filter on.
#'   One of `"LastUpdatePostDate"` (default), `"StartDate"`,
#'   `"PrimaryCompletionDate"`, or `"CompletionDate"`.
#' @param save_json Logical; if `TRUE`, saves raw JSON to file instead of returning R objects.
#' @param json_file File path to save JSON output (if `save_json` is `TRUE`).
#' @return A list with two elements:
#' \describe{
#'   \item{studies}{A list of raw study records (JSON parsed as R lists).}
#'   \item{meta}{Metadata describing the query and retrieval.}
#' }
#'
#' @export
trials_fetch <- function(
  query,
  max_records = 1000,
  .page_size = 1000,
  phase = NULL,
  country = NULL,
  from_date = "MIN",
  to_date = "MAX",
  date_filter = "LastUpdatePostDate",
  save_json = FALSE,
  json_file = NULL
) {
  if (!is.character(query) || length(query) != 1 || nchar(query) == 0) {
    stop("`query` must be a single non-empty string.")
  }

  allowed <- c("LastUpdatePostDate", "StartDate", "PrimaryCompletionDate", "CompletionDate")
  if (!date_filter %in% allowed) {
    stop(paste0("`date_filter` must be one of: ", paste(allowed, collapse = ", ")))
  }

  if (save_json) {
    if (is.null(json_file)) {
      json_file <- "output.json"
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package `jsonlite` is required.")
    }
  }

  url <- "https://clinicaltrials.gov/api/v2/studies"
  date_range <- .build_date_range_filter(from_date, to_date, date_filter)

  advanced_filters <- if (is.null(phase)) {
    date_range
  } else {
    paste0(date_range, ' AND AREA[Phase] "', phase, '"')
  }

  params <- list(
    "query.term" = query,
    "pageSize" = as.integer(min(.page_size, max_records)),
    "query.locn" = country,
    "filter.advanced" = advanced_filters
  )

  # streaming / accumulation setup
  con <- NULL
  first <- TRUE
  fetched <- 0L

  if (save_json) {
    con <- file(json_file, open = "wt", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    # Start JSON array
    writeLines("[", con)
  } else {
    all_studies <- list()
  }

  # Fetch loop
  while (fetched < max_records) {
    data <- .fetch_page(url, params)
    studies <- data$studies

    if (is.null(studies) || length(studies) == 0) break

    remaining <- max_records - fetched
    to_take <- min(length(studies), remaining)
    studies <- studies[seq_len(to_take)]

    if (save_json) {
      for (s in studies) {
        if (!first) writeLines(",", con)
        writeLines(
          jsonlite::toJSON(s, auto_unbox = TRUE, null = "null"),
          con
        )
        first <- FALSE
        fetched <- fetched + 1L
      }
    } else {
      all_studies <- c(all_studies, studies)
      fetched <- fetched + to_take
    }

    next_token <- data$nextPageToken
    if (is.null(next_token) || fetched >= max_records) break
    params[["pageToken"]] <- next_token
  }
  meta = list(
    query = query,
    max_records = max_records,
    fetched = fetched,
    phase = phase,
    country = country,
    from_date = from_date,
    to_date = to_date,
    date_filter = date_filter
  )

  if (save_json) {
    # End JSON array
    writeLines("]", con)
    print(meta)
    print(paste("Saving JSON output to", json_file))
    return(invisible(json_file))
  }

  list(
    studies = all_studies,
    meta = meta
  )
}
# `trial_run_spec` is a helper function for testing purposes.
# It takes a list of parameters and calls `trials_fetch` with them.
trials_run_spec <- function(spec) {
  do.call(trials_fetch, spec)
}

