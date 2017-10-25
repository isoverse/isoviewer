# file info ====

context("Data Storage")

test_that("test that local storage objects work", {

  # type checking
  expect_false(isoviewer:::is_storage(5))
  expect_error(isoviewer:::local_storage())
  expect_true(isoviewer::is_storage(loc <- local_storage(".")))
  expect_true(isoviewer::is_local_storage(loc))
  expect_false(isoviewer::is_gdrive_storage(loc))

  # initialization
  expect_error(local_storage("DNE") %>% init_storage(), "does not exist")
  expect_false(loc$initialized)
  expect_true(isoviewer::is_local_storage(loc <- init_storage(loc)))
  expect_true(loc$initialized)

})

test_that("test that google drive storage objects work", {

  # type checking
  expect_error(isoviewer:::gdrive_storage())
  expect_true(isoviewer::is_storage(loc <- gdrive_storage(".")))
  expect_false(isoviewer::is_local_storage(loc))
  expect_true(isoviewer::is_gdrive_storage(loc))

  # initialization
  expect_error(gdrive_storage(".", local_dir = "DNE") %>% init_storage(), "does not exist")

})
