# A tiny fake "study" object that looks like CT.gov v2 structure
make_fake_study <- function(nct = "NCT04267848") {
  list(
    protocolSection = list(
      identificationModule = list(
        nctId = nct,
        orgStudyIdInfo = list(id = "ORG1"),
        briefTitle = "Brief",
        officialTitle = "Official",
        acronym = "ABC"
      ),
      statusModule = list(
        overallStatus = "RECRUITING",
        statusVerifiedDate = "2025-01",
        startDateStruct = list(date = "2025-01-01", type = "ACTUAL"),
        primaryCompletionDateStruct = list(date = "2026-01-01", type = "ESTIMATED"),
        completionDateStruct = list(date = "2027-01-01", type = "ESTIMATED"),
        lastUpdatePostDateStruct = list(date = "2025-02-01", type = "ACTUAL"),
        whyStopped = NULL
      ),
      sponsorCollaboratorsModule = list(
        leadSponsor = list(name = "Sponsor", class = "INDUSTRY"),
        collaborators = list(list(name = "Collab1"), list(name = "Collab2"))
      ),
      designModule = list(
        studyType = "INTERVENTIONAL",
        phases = c("PHASE2", "PHASE3"),
        designInfo = list(
          allocation = "RANDOMIZED",
          interventionModel = "PARALLEL",
          primaryPurpose = "TREATMENT",
          maskingInfo = list(masking = "DOUBLE")
        ),
        enrollmentInfo = list(count = 100, type = "ESTIMATED")
      )
    )
  )
}

test_that("studies_to_tables_by_module returns named list of tibbles", {
  studies <- list(make_fake_study("NCT1"), make_fake_study("NCT2"))
  
  tabs <- studies_to_tables_by_module(studies, modules = "identificationModule")
  
  expect_type(tabs, "list")
  expect_named(tabs, "identificationModule")
  expect_s3_class(tabs$identificationModule, "tbl_df")
  expect_equal(nrow(tabs$identificationModule), 2)
  expect_true(all(c("nct_id", "brief_title") %in% names(tabs$identificationModule)))
})

test_that("modules='all' returns all supported modules", {
  studies <- list(make_fake_study("NCT1"))
  tabs <- studies_to_tables_by_module(studies, modules = "all")
  
  # should include at least these (based on your extractor list)
  expect_true(all(c("identificationModule", "statusModule", "designModule") %in% names(tabs)))
})

test_that("unknown module name throws an error", {
  studies <- list(make_fake_study("NCT1"))
  
  expect_error(
    studies_to_tables_by_module(studies, modules = "notARealModule"),
    "Unknown module"
  )
})

test_that("deduplicates by nct_id when present", {
  studies <- list(make_fake_study("NCT_DUP"), make_fake_study("NCT_DUP"))
  tabs <- studies_to_tables_by_module(studies, modules = "identificationModule")
  
  expect_equal(nrow(tabs$identificationModule), 1)
  expect_equal(tabs$identificationModule$nct_id[[1]], "NCT_DUP")
})
