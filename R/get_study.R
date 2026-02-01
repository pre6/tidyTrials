#' Retrieve and flatten a ClinicalTrials.gov study record
#'
#' Fetches a study from the ClinicalTrials.gov v2 API and returns a flattened
#' one-row tibble. Optionally limits output to a specific protocol module.
#'
#' @param nct_id A single NCT identifier (e.g. "NCT04267848").
#' @param module Optional protocolSection module name
#'   (e.g. "statusModule"). If NULL, the full record is flattened.
#'
#' @return A tibble with one row per study.
#'
#' @examples
#' \dontrun{
#' get_study_module_table("NCT04267848", "statusModule")
#' }
#'
#' @export
#' @importFrom httr2 request req_user_agent req_error req_perform resp_status resp_body_string
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom tibble as_tibble tibble
get_study_module_table <- function(nct_id, module = NULL) {

  # --- validation ---
  if (!is.character(nct_id) || length(nct_id) != 1 || !nzchar(nct_id)) {
    stop("`nct_id` must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.null(module) &&
      (!is.character(module) || length(module) != 1 || !nzchar(module))) {
    stop("`module` must be NULL or a single non-empty character string.", call. = FALSE)
  }

  # --- fetch ---
  url <- paste0("https://clinicaltrials.gov/api/v2/studies/", nct_id)

  resp <- httr2::request(url) |>
    httr2::req_user_agent("tidyTrials (R)") |>
    httr2::req_error(is_error = function(r) httr2::resp_status(r) >= 400) |>
    httr2::req_perform()

  study <- jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = FALSE
  )

  # --- select target ---
  if (is.null(module)) {
    target <- study
    prefix <- ""
  } else {
    protocol <- study$protocolSection

    if (is.null(protocol) || !is.list(protocol)) {
      stop("No protocolSection found in API response.", call. = FALSE)
    }

    target <- protocol[[module]]

    if (is.null(target)) {
      stop(
        "Module '", module, "' not found. Available modules: ",
        paste(names(protocol), collapse = ", "),
        call. = FALSE
      )
    }

    prefix <- module
  }

  # --- flatten ---
  flat <- flatten_any(target, prefix)

  if (length(flat) == 0) {
    return(tibble::tibble(nct_id = nct_id))
  }

  # sanitize names
  names(flat) <- names(flat) |>
    gsub("\\s+", "_", x = _) |>
    gsub("[^A-Za-z0-9_.]+", "_", x = _)

  flat <- c(list(nct_id = nct_id), flat)

  tibble::as_tibble(flat)
}
