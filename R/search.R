
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
  date_filter = "LastUpdatePostDate"
) {
  if (!is.character(query) || length(query) != 1 || nchar(query) == 0) {
    stop("`query` must be a single non-empty string.")
  }

  allowed <- c("LastUpdatePostDate", "StartDate", "PrimaryCompletionDate", "CompletionDate")

  if (!date_filter %in% allowed) {
    stop(paste0("`date_filter` must be one of: ", paste(allowed, collapse = ", ")))
  }

  url <- "https://clinicaltrials.gov/api/v2/studies"

  date_range <- .build_date_range_filter(from_date, to_date, date_filter)

  #Building advanced filters
  if (is.null(phase)) {
    advanced_filters <- date_range
  } else {
    advanced_filters <- paste0(date_range, ' AND AREA[Phase] "', phase, '"')
  }

  params <- list(
    "query.term" = query,
    "pageSize" = as.integer(min(.page_size, max_records)),
    "query.locn" = country,
    "filter.advanced" = advanced_filters
  )

  all_studies <- list()

  # Fetch pages of results until we reach the maximum number of records
  while (length(all_studies) < max_records) {
    data <- .fetch_page(url, params)

    if (is.null(data$studies) || length(data$studies) == 0) break

    all_studies <- c(all_studies, data$studies)

    next_token <- data$nextPageToken
    if (is.null(next_token) || length(all_studies) >= max_records) break
    params[["pageToken"]] <- next_token
  }

  studies_out <- all_studies[seq_len(min(length(all_studies), max_records))]

  list(
    studies = studies_out,
    meta = list(
      query = query,
      filters = list(
      phase = phase,
      country = country,
      from_date = from_date,
      to_date = to_date,
      date_filter = date_filter
    ),
    records_fetched = length(studies_out)
)
  )
}

# `trial_run_spec` is a helper function for testing purposes.
# It takes a list of parameters and calls `trials_fetch` with them.
trials_run_spec <- function(spec) {
  do.call(trials_fetch, spec)
}

