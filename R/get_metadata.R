#' Inspect available protocol fields for a ClinicalTrials.gov study
#'
#' Fetches a study from the ClinicalTrials.gov v2 API and returns metadata
#' describing available protocol modules and flattened field paths.
#'
#' @param nct_id A single NCT identifier (e.g. "NCT04267848").
#'
#' @return A list with elements:
#' \describe{
#'   \item{nct_id}{Cleaned NCT identifier}
#'   \item{url}{API URL used}
#'   \item{modules}{Character vector of protocol modules}
#'   \item{fields}{Tibble describing available fields}
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_user_agent req_perform resp_body_string
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows mutate filter arrange
get_metadata <- function(nct_id) {

  # ---- validate ----
  nct_clean <- toupper(trimws(nct_id))

  if (!is.character(nct_clean) ||
      length(nct_clean) != 1 ||
      !grepl("^NCT\\d{6,12}$", nct_clean)) {
    stop(
      "`nct_id` must look like 'NCT04267848'. You provided: ",
      shQuote(nct_id),
      call. = FALSE
    )
  }

  url <- paste0("https://clinicaltrials.gov/api/v2/studies/", nct_clean)

  # ---- fetch ----
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("tidyTrials (R)") |>
      httr2::req_perform(),
    error = function(e) {
      stop(
        "ClinicalTrials.gov API request failed.\n",
        "NCT ID: ", nct_clean, "\n",
        "URL: ", url, "\n",
        "Error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  study <- jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = FALSE
  )

  protocol <- study$protocolSection
  if (is.null(protocol) || !is.list(protocol)) {
    stop("No protocolSection found in API response.", call. = FALSE)
  }

  modules <- names(protocol)
  modules <- modules[!is.na(modules) & nzchar(modules)]

  fields <- flatten_to_rows(protocol, prefix = "protocolSection") |>
    dplyr::mutate(
      module = vapply(path, module_from_path, character(1))
    ) |>
    dplyr::filter(!is.na(module)) |>
    dplyr::arrange(module, path)

  list(
    nct_id = nct_clean,
    url = url,
    modules = modules,
    fields = tibble::as_tibble(fields)
  )
}
