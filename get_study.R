#' Retrieve a ClinicalTrials.gov study or module as a tibble
#'
#' @param nct_id A single ClinicalTrials.gov identifier (e.g., "NCT04267848")
#' @param module Optional protocolSection module name (e.g., "statusModule").
#'   If NULL, the full study record is returned.
#'
#' @return A tibble with one row containing flattened study data.
#' @export
#'
#' @examples
#' \dontrun{
#' ct_get_study_module_table("NCT04267848", "statusModule")
#' }
ct_get_study_module_table <- function(nct_id, module = NULL) {

  validate_nct_id(nct_id)
  validate_module(module)

  study <- fetch_study(nct_id)

  if (is.null(module)) {
    target <- study
    prefix <- ""
  } else {
    protocol <- study$protocolSection
    if (!is.list(protocol)) {
      stop("No protocolSection found in the API response.", call. = FALSE)
    }

    target <- protocol[[module]]
    if (is.null(target)) {
      available <- names(protocol)
      stop(
        "Module '", module, "' not found.\nAvailable modules: ",
        paste(available, collapse = ", "),
        call. = FALSE
      )
    }

    prefix <- module
  }

  flat <- flatten_any(target, prefix)

  if (length(flat) == 0) {
    return(tibble::tibble(nct_id = nct_id))
  }

  names(flat) <- clean_names(names(flat))
  flat <- c(list(nct_id = nct_id), flat)

  tibble::as_tibble(flat)
}
