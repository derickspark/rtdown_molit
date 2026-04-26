test_that(".expand_month_range expands single + multi-month ranges", {
  expect_equal(.expand_month_range("202604"), "202604")
  expect_equal(.expand_month_range("202501", "202501"), "202501")
  expect_equal(
    .expand_month_range("202501", "202604"),
    sprintf("%04d%02d",
            c(rep(2025L, 12L), rep(2026L, 4L)),
            c(seq_len(12L), seq_len(4L)))
  )
  expect_length(.expand_month_range("202501", "202604"), 16L)
})

test_that(".expand_month_range auto-sorts reversed input", {
  expect_equal(
    .expand_month_range("202604", "202501"),
    .expand_month_range("202501", "202604")
  )
})

test_that(".expand_month_range handles year-boundary correctly", {
  expect_equal(
    .expand_month_range("202511", "202602"),
    c("202511", "202512", "202601", "202602")
  )
})

test_that(".expand_month_range rejects malformed input", {
  expect_error(.expand_month_range("2025"))
  expect_error(.expand_month_range("202513"))
  expect_error(.expand_month_range("202500"))
  expect_error(.expand_month_range("abcdef"))
})

test_that(".expand_month_range NULL to_ymd defaults to single month", {
  expect_equal(.expand_month_range("202604", NULL), "202604")
})
