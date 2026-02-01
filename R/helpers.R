#' Helper function to fetch a single page of results from the API
.fetch_page <- function(url, params) {
  resp <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform()

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

.build_date_range_filter <- function(from_date, to_date, date_filter) {
  if (is.null(from_date) || is.null(to_date)) return(NULL)

  if (!is.character(date_filter) || length(date_filter) != 1) {
    stop("`date_filter` must be a single character string.")
  }

  # from_date
  if (!identical(from_date, "MIN")) {
    if (!.is_ymd(from_date)) {
      stop("`from_date` must be in 'YYYY-MM-DD' format or 'MIN'.")
    }
    from_date <- as.Date(from_date)
    if (is.na(from_date)) {
      stop("`from_date` is not a valid calendar date.")
    }
  }

  # to_date
  if (!identical(to_date, "MAX")) {
    if (!.is_ymd(to_date)) {
      stop("`to_date` must be in 'YYYY-MM-DD' format or 'MAX'.")
    }
    to_date <- as.Date(to_date)
    if (is.na(to_date)) {
      stop("`to_date` is not a valid calendar date.")
    }
  }

  # ordering check (guard MIN/MAX)
  if (!identical(from_date, "MIN") && !identical(to_date, "MAX")) {
    if (from_date > to_date) {
      stop("`from_date` must be <= `to_date`.")
    }
  }

  paste0("AREA[", date_filter, "]RANGE[", from_date, ",", to_date, "]")
}

# Helper function to check if a string is in 'YYYY-MM-DD' format
.is_ymd <- function(x) {
  is.character(x) &&
    length(x) == 1 &&
    grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
}

## Helper functions for get_study.R:simplifyVector
validate_nct_id <- function(x) {
  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    stop(
      "`nct_id` must be a single non-empty character string.",
      call. = FALSE
    )
  }
}

validate_module <- function(x) {
  if (!is.null(x) &&
      (!is.character(x) || length(x) != 1 || !nzchar(x))) {
    stop(
      "`module` must be NULL or a single non-empty character string.",
      call. = FALSE
    )
  }
}

fetch_study <- function(nct_id) {
  url <- paste0("https://clinicaltrials.gov/api/v2/studies/", nct_id)

  resp <- httr2::request(url) |>
    httr2::req_user_agent("tidyTrials (R)") |>
    httr2::req_error(is_error = function(r) httr2::resp_status(r) >= 400) |>
    httr2::req_perform()

  jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = FALSE
  )
}

is_empty <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (is.character(x) && all(!nzchar(x))) ||
    (is.list(x) && length(x) == 0)
}

flatten_any <- function(x, prefix = "") {
  out <- list()
  if (is_empty(x)) return(out)

  if (is.atomic(x) && !is.list(x)) {
    val <- if (length(x) > 1) {
      paste(as.character(x), collapse = " | ")
    } else {
      as.character(x)[1]
    }
    if (!is_empty(val)) out[[prefix]] <- val
    return(out)
  }

  if (is.list(x)) {
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      for (nm in names(x)) {
        if (!nzchar(nm)) next
        key <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
        out <- c(out, flatten_any(x[[nm]], key))
      }
      return(out)
    }

    all_scalar <- all(vapply(
      x,
      function(z) is.atomic(z) && !is.list(z),
      logical(1)
    ))

    if (all_scalar) {
      vals <- unlist(lapply(x, as.character), use.names = FALSE)
      vals <- vals[nzchar(vals)]
      if (length(vals)) out[[prefix]] <- paste(vals, collapse = " | ")
    } else {
      out[[prefix]] <- jsonlite::toJSON(
        x,
        auto_unbox = TRUE,
        null = "null"
      )
    }
  }

  out
}

clean_names <- function(x) {
  x <- gsub("\\s+", "_", x)
  gsub("[^A-Za-z0-9_.]+", "_", x)
}





