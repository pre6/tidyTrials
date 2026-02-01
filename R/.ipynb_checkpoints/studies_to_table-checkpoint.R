library(tibble)
library(purrr)
library(dplyr)

#' Build module-level tables (one tibble per module) from CT.gov v2 JSON
#'
#' @param studies List of study records (e.g., result$studies).
#' @param modules Character vector of protocolSection module names to include.
#'   Use "all" to include all supported modules.
#' @return Named list of tibbles. Each tibble has 1 row per study.
studies_to_tables_by_module <- function(
    studies,
    modules = c("identificationModule", "statusModule", "sponsorCollaboratorsModule", "designModule")
) {
  
  extractors <- list(
    
    identificationModule = function(st) tibble(
      nct_id        = get_in(st, c("protocolSection","identificationModule","nctId")),
      org_study_id   = get_in(st, c("protocolSection","identificationModule","orgStudyIdInfo","id")),
      brief_title    = get_in(st, c("protocolSection","identificationModule","briefTitle")),
      official_title = get_in(st, c("protocolSection","identificationModule","officialTitle")),
      acronym        = get_in(st, c("protocolSection","identificationModule","acronym"))
    ),
    
    statusModule = function(st) tibble(
      nct_id                   = get_in(st, c("protocolSection","identificationModule","nctId")),
      overall_status           = get_in(st, c("protocolSection","statusModule","overallStatus")),
      status_verified_ym       = get_in(st, c("protocolSection","statusModule","statusVerifiedDate")),
      start_date               = get_in(st, c("protocolSection","statusModule","startDateStruct","date")),
      start_date_type          = get_in(st, c("protocolSection","statusModule","startDateStruct","type")),
      primary_completion_date  = get_in(st, c("protocolSection","statusModule","primaryCompletionDateStruct","date")),
      primary_completion_type  = get_in(st, c("protocolSection","statusModule","primaryCompletionDateStruct","type")),
      completion_date          = get_in(st, c("protocolSection","statusModule","completionDateStruct","date")),
      completion_date_type     = get_in(st, c("protocolSection","statusModule","completionDateStruct","type")),
      last_update_posted       = get_in(st, c("protocolSection","statusModule","lastUpdatePostDateStruct","date")),
      last_update_posted_type  = get_in(st, c("protocolSection","statusModule","lastUpdatePostDateStruct","type")),
      why_stopped              = get_in(st, c("protocolSection","statusModule","whyStopped"))
    ),
    
    sponsorCollaboratorsModule = function(st) {
      collabs <- st$protocolSection$sponsorCollaboratorsModule$collaborators %||% list()
      collab_names <- unique(vapply(collabs, function(z) z$name %||% NA_character_, character(1)))
      collab_names <- collab_names[!is.na(collab_names)]
      
      tibble(
        nct_id             = get_in(st, c("protocolSection","identificationModule","nctId")),
        lead_sponsor_name  = get_in(st, c("protocolSection","sponsorCollaboratorsModule","leadSponsor","name")),
        lead_sponsor_class = get_in(st, c("protocolSection","sponsorCollaboratorsModule","leadSponsor","class")),
        collaborators      = if (length(collab_names) == 0) NA_character_ else paste(collab_names, collapse = "; ")
      )
    },
    
    designModule = function(st) {
      phases <- st$protocolSection$designModule$phases %||% character(0)
      tibble(
        nct_id             = get_in(st, c("protocolSection","identificationModule","nctId")),
        study_type         = get_in(st, c("protocolSection","designModule","studyType")),
        phases             = if (length(phases) == 0) NA_character_ else paste(unlist(phases), collapse = ", "),
        allocation         = get_in(st, c("protocolSection","designModule","designInfo","allocation")),
        intervention_model = get_in(st, c("protocolSection","designModule","designInfo","interventionModel")),
        primary_purpose    = get_in(st, c("protocolSection","designModule","designInfo","primaryPurpose")),
        masking            = get_in(st, c("protocolSection","designModule","designInfo","maskingInfo","masking")),
        enrollment_count   = get_in(st, c("protocolSection","designModule","enrollmentInfo","count")),
        enrollment_type    = get_in(st, c("protocolSection","designModule","enrollmentInfo","type"))
      )
    },
    
    eligibilityModule = function(st) tibble(
      nct_id                = get_in(st, c("protocolSection","identificationModule","nctId")),
      eligibility_criteria  = get_in(st, c("protocolSection","eligibilityModule","eligibilityCriteria")),
      healthy_volunteers    = get_in(st, c("protocolSection","eligibilityModule","healthyVolunteers")),
      sex                   = get_in(st, c("protocolSection","eligibilityModule","sex")),
      minimum_age           = get_in(st, c("protocolSection","eligibilityModule","minimumAge")),
      maximum_age           = get_in(st, c("protocolSection","eligibilityModule","maximumAge")),
      std_ages              = {
        ages <- st$protocolSection$eligibilityModule$stdAges %||% character(0)
        if (length(ages) == 0) NA_character_ else paste(unlist(ages), collapse = ", ")
      }
    ),
    
    contactsLocationsModule = function(st) {
      locs <- st$protocolSection$contactsLocationsModule$locations %||% list()
      countries <- unique(vapply(locs, function(z) z$country %||% NA_character_, character(1)))
      countries <- countries[!is.na(countries)]
      cities <- unique(vapply(locs, function(z) z$city %||% NA_character_, character(1)))
      cities <- cities[!is.na(cities)]
      
      tibble(
        nct_id     = get_in(st, c("protocolSection","identificationModule","nctId")),
        n_sites    = length(locs),
        countries  = if (length(countries) == 0) NA_character_ else paste(countries, collapse = "; "),
        cities     = if (length(cities) == 0) NA_character_ else paste(cities, collapse = "; ")
      )
    },
    
    referencesModule = function(st) {
      refs <- st$protocolSection$referencesModule$references %||% list()
      
      pmids <- vapply(refs, function(r) r$pmid %||% NA_character_, character(1))
      pmids <- pmids[!is.na(pmids) & nzchar(pmids)]
      
      citations <- vapply(refs, function(r) r$citation %||% NA_character_, character(1))
      citations <- citations[!is.na(citations) & nzchar(citations)]
      
      tibble(
        nct_id        = get_in(st, c("protocolSection","identificationModule","nctId")),
        n_references  = length(refs),
        pmids         = if (length(pmids) == 0) NA_character_ else paste(unique(pmids), collapse = "; "),
        citations     = if (length(citations) == 0) NA_character_ else paste(unique(citations), collapse = " | ")
      )
    }
  )
  
  supported <- names(extractors)
  
  if (length(modules) == 1 && identical(modules, "all")) {
    modules <- supported
  }
  
  bad <- setdiff(modules, supported)
  if (length(bad) > 0) {
    stop(
      "Unknown module(s): ", paste(bad, collapse = ", "),
      "\nSupported modules: ", paste(supported, collapse = ", ")
    )
  }
  
  # build one tibble per module (named list)
  tables <- set_names(modules) |>
    map(function(m) {
      tab <- map_dfr(studies, extractors[[m]])
      
      # de-dup by nct_id when present
      if ("nct_id" %in% names(tab)) {
        tab <- distinct(tab, nct_id, .keep_all = TRUE)
      }
      
      tab
    })
  
  tables
}

# helpers.R (same as yours)

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



