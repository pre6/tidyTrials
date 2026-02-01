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


# helper functions for get study


#' Check whether an object is empty
#' @keywords internal
is_empty <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (is.character(x) && all(!nzchar(x))) ||
    (is.list(x) && length(x) == 0)
}

#' Recursively flatten nested API responses
#' @keywords internal
flatten_any <- function(x, prefix = "") {

  out <- list()
  if (is_empty(x)) return(out)

  # atomic leaf
  if (is.atomic(x) && !is.list(x)) {
    val <- if (length(x) > 1) {
      paste(as.character(x), collapse = " | ")
    } else {
      as.character(x)[1]
    }

    if (nzchar(val)) out[[prefix]] <- val
    return(out)
  }

  # list
  if (is.list(x)) {

    # named list
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      for (nm in names(x)) {
        if (!nzchar(nm)) next
        key <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
        out <- c(out, flatten_any(x[[nm]], key))
      }
      return(out)
    }

    # unnamed list
    if (all(vapply(x, function(z) is.atomic(z) && !is.list(z), logical(1)))) {
      vals <- unlist(lapply(x, as.character), use.names = FALSE)
      vals <- vals[nzchar(vals)]
      if (length(vals)) out[[prefix]] <- paste(vals, collapse = " | ")
      return(out)
    }

    # complex objects → JSON
    out[[prefix]] <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
    return(out)
  }

  out
}



## helper functions for get_metadata
is_empty <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    (is.character(x) && all(!nzchar(x))) ||
    (is.list(x) && length(x) == 0)
}

truncate_100 <- function(x) {
  x <- as.character(x)
  ifelse(nchar(x) > 100, paste0(substr(x, 1, 100), "..."), x)
}

leaf_type <- function(x) {
  if (is.null(x)) return("null")
  if (is.atomic(x) && !is.list(x)) {
    if (is.logical(x)) return("logical")
    if (is.integer(x)) return("integer")
    if (is.double(x))  return("double")
    if (is.character(x)) return("character")
    return(typeof(x))
  }
  if (is.list(x)) return("list")
  typeof(x)
}

flatten_to_rows <- function(x, prefix = "") {

  if (is_empty(x)) {
    return(tibble::tibble(
      path = character(),
      value_type = character(),
      example_value = character()
    ))
  }

  if (is.atomic(x) && !is.list(x)) {
    val <- if (length(x) > 1) {
      paste(as.character(x), collapse = " | ")
    } else {
      as.character(x)[1]
    }

    if (is_empty(val)) {
      return(tibble::tibble(
        path = character(),
        value_type = character(),
        example_value = character()
      ))
    }

    return(tibble::tibble(
      path = prefix,
      value_type = leaf_type(x),
      example_value = truncate_100(val)
    ))
  }

  if (is.list(x)) {

    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      rows <- lapply(
        names(x)[nzchar(names(x))],
        function(nm) {
          key <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
          flatten_to_rows(x[[nm]], key)
        }
      )
      return(dplyr::bind_rows(rows))
    }

    if (all(vapply(x, function(z) is.atomic(z) && !is.list(z), logical(1)))) {
      vals <- unlist(lapply(x, as.character), use.names = FALSE)
      vals <- vals[nzchar(vals)]

      if (!length(vals)) {
        return(tibble::tibble(
          path = character(),
          value_type = character(),
          example_value = character()
        ))
      }

      return(tibble::tibble(
        path = prefix,
        value_type = "vector",
        example_value = truncate_100(paste(vals, collapse = " | "))
      ))
    }

    return(tibble::tibble(
      path = prefix,
      value_type = "list_of_objects",
      example_value = truncate_100(
        jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
      )
    ))
  }

  tibble::tibble(
    path = character(),
    value_type = character(),
    example_value = character()
  )
}

module_from_path <- function(p) {
  parts <- strsplit(p, "\\.", fixed = FALSE)[[1]]
  if (length(parts) >= 2 && identical(parts[1], "protocolSection")) {
    parts[2]
  } else {
    NA_character_
  }
}
