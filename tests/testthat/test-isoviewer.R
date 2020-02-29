# file info ====

context("Isoviewer GUI")

test_that("test that viewer start has appropriate safety checks", {

  expect_error(iso_start_viewer(42), "variable.*does not exist")
  x <- 42
  expect_error(iso_start_viewer(x), "variable.*not an iso object")
  expect_error(iso_start_viewer("x"), "variable.*not an iso object")

})
