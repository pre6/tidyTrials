test_that("get_metadata rejects invalid NCT ids", {
  expect_error(get_metadata(""), "must look like")
  expect_error(get_metadata("NCT123"), "must look like")
  expect_error(get_metadata("abc"), "must look like")
  expect_error(get_metadata("NCTABCDEF"), "must look like")
})

test_that("get_metadata returns modules + fields using mocked API response", {
  
  # ---- Fake protocolSection returned from the API ----
  fake_study <- list(
    protocolSection = list(
      identificationModule = list(nctId = "NCT04267848"),
      statusModule = list(overallStatus = "RECRUITING")
    )
  )
  
  # ---- Fake flattened rows that your flatten_to_rows() would return ----
  fake_rows <- tibble::tibble(
    path = c(
      "protocolSection.identificationModule.nctId",
      "protocolSection.statusModule.overallStatus"
    ),
    example_value = c("NCT04267848", "RECRUITING")
  )
  
  # ---- Mock the functions used inside get_metadata() so no network happens ----
  testthat::local_mocked_bindings(
    # Make httr2 pipeline return a dummy response object
    request = function(url) structure(list(url = url), class = "fake_req"),
    req_user_agent = function(req, ...) req,
    req_perform = function(req, ...) structure(list(req = req), class = "fake_resp"),
    resp_body_string = function(resp, ...) "{}",  # not used, because we mock fromJSON
    .package = "httr2"
  )
  
  testthat::local_mocked_bindings(
    # Return our fake study regardless of JSON input
    fromJSON = function(txt, simplifyVector = FALSE) fake_study,
    .package = "jsonlite"
  )
  
  # These two are in YOUR package namespace, so mock them there
  testthat::local_mocked_bindings(
    flatten_to_rows = function(x, prefix) fake_rows,
    module_from_path = function(p) {
      # simple: pull the second segment after "protocolSection."
      # e.g. "protocolSection.identificationModule.nctId" -> "identificationModule"
      strsplit(p, "\\.")[[1]][2]
    },
    .package = "tidytrials"
  )
  
  out <- get_metadata("NCT04267848")
  
  # ---- Assertions ----
  expect_type(out, "list")
  expect_equal(out$nct_id, "NCT04267848")
  expect_true(is.character(out$url))
  expect_true(all(c("identificationModule", "statusModule") %in% out$modules))
  
  expect_s3_class(out$fields, "tbl_df")
  expect_true(all(c("path", "module") %in% names(out$fields)))
  expect_equal(nrow(out$fields), 2)
  
  # module column should be computed from path via module_from_path()
  expect_equal(sort(unique(out$fields$module)), c("identificationModule", "statusModule"))
})
