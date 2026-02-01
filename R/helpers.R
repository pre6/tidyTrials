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

## helper function for studies to table script:

# helpers.R

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}

get_in <- function(x, path, default = NA_character_) {
  for (nm in path) {
    if (is.null(x[[nm]])) return(default)
    x <- x[[nm]]
  }
  if (is.null(x) || length(x) == 0) return(default)
  if (is.atomic(x)) return(as.character(x)[1])
  default
}





