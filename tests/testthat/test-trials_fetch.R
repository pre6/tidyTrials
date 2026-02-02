test_that("trials_fetch validates inputs", {
  # query must be a single non-empty string
  expect_error(trials_fetch(query = ""), "single non-empty string")
  expect_error(trials_fetch(query = character(0)), "single non-empty string")
  expect_error(trials_fetch(query = c("a", "b")), "single non-empty string")
  
  # date_filter must be one of allowed options
  expect_error(
    trials_fetch(query = "diabetes", date_filter = "BadField"),
    "`date_filter` must be one of"
  )
})

test_that("trials_fetch returns studies + meta and respects max_records", {
  # Fake pages returned by .fetch_page()
  page1 <- list(
    studies = list(list(id = 1), list(id = 2), list(id = 3)),
    nextPageToken = "TOKEN2"
  )
  page2 <- list(
    studies = list(list(id = 4), list(id = 5)),
    nextPageToken = NULL
  )
  pages <- list(page1, page2)
  i <- 0L
  
  testthat::local_mocked_bindings(
    .build_date_range_filter = function(from_date, to_date, date_filter) {
      "DATE_RANGE_FILTER"
    },
    .fetch_page = function(url, params) {
      i <<- i + 1L
      pages[[i]]
    },
    .package = "tidytrials"  # change if your Package: is different
  )
  
  out <- trials_fetch(
    query = "diabetes",
    max_records = 4,   # should take 3 from page1 + 1 from page2
    .page_size = 1000,
    phase = NULL,
    country = NULL
  )
  
  expect_type(out, "list")
  expect_true(all(c("studies", "meta") %in% names(out)))
  
  expect_length(out$studies, 4)
  expect_equal(vapply(out$studies, `[[`, double(1), "id"), 1:4)
  
  expect_type(out$meta, "list")
  expect_equal(out$meta$query, "diabetes")
  expect_equal(out$meta$max_records, 4)
  expect_equal(out$meta$fetched, 4)
})

test_that("trials_fetch builds advanced filter correctly when phase is set", {
  captured_params <- NULL
  
  testthat::local_mocked_bindings(
    .build_date_range_filter = function(from_date, to_date, date_filter) {
      'AREA[LastUpdatePostDate]RANGE[MIN,MAX]'
    },
    .fetch_page = function(url, params) {
      captured_params <<- params
      list(studies = list(), nextPageToken = NULL)  # stop loop immediately
    },
    .package = "tidytrials"
  )
  
  trials_fetch(
    query = "asthma",
    max_records = 10,
    phase = "Phase 2",
    country = "Canada"
  )
  
  expect_true(!is.null(captured_params))
  expect_equal(captured_params[["query.term"]], "asthma")
  expect_equal(captured_params[["query.locn"]], "Canada")
  
  # Ensure phase was appended to filter.advanced
  expect_true(grepl('AREA\\[Phase\\] "Phase 2"', captured_params[["filter.advanced"]]))
})

test_that("trials_run_spec passes arguments through to trials_fetch", {
  # capture the spec that reaches trials_fetch via do.call
  captured <- NULL
  
  testthat::local_mocked_bindings(
    trials_fetch = function(...) {
      captured <<- list(...)
      list(studies = list(), meta = list(fetched = 0))
    },
    .package = "tidytrials"
  )
  
  spec <- list(
    query = "covid",
    max_records = 25,
    phase = "Phase 3",
    country = "United States"
  )
  
  out <- trials_run_spec(spec)
  
  expect_true(is.list(out))
  expect_equal(captured$query, "covid")
  expect_equal(captured$max_records, 25)
  expect_equal(captured$phase, "Phase 3")
  expect_equal(captured$country, "United States")
})
