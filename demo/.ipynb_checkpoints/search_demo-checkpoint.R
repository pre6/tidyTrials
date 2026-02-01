spec <- list(
  query = "cancer",
  max_records = 10,
  phase = "Phase 2",
  country = "Canada",
  from_date = "2022-01-01",
  to_date = "2023-01-01",
  date_filter = "StartDate"
)

out <- trials_run_spec(spec)
str(out)