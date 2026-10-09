source("helper.R")

test_that("Calculate one play", {
  # add_4th_probs relies on model downloads. Better skip this on cran
  testthat::skip_on_cran()

  probs <- cfb4th::add_4th_probs(play)

  # positive go boost
  testthat::expect_gt(probs$go_boost, 0)

})

test_that("Make the table", {
  # add_4th_probs relies on model downloads. Better skip this on cran
  testthat::skip_on_cran()

  probs <- cfb4th::add_4th_probs(play)
  table <- cfb4th::make_table_data(probs)

  fg_row <- table %>% dplyr::filter(choice == "Field goal attempt")
  go_row <- table %>% dplyr::filter(choice == "Go for it")

  # succeeding is better than failing
  testthat::expect_gt(go_row$success_wp, go_row$fail_wp)
  testthat::expect_gt(fg_row$success_wp, fg_row$fail_wp)

})

test_that("a failed model download is one informative error", {
  # point the fd_model URL at a port nothing listens on: no network needed
  testthat::local_mocked_bindings(
    model_url = function(name) "http://127.0.0.1:9/fd_model.rds",
    .package = "cfb4th"
  )
  testthat::expect_error(cfb4th:::download_model("fd_model"), "could not be downloaded")
})

test_that("cfb4th_clear_cache() clears the session copy and the disk copy", {
  testthat::skip_on_cran()

  invisible(cfb4th:::fd_model())
  testthat::expect_true(exists("fd_model", envir = cfb4th:::.models))

  testthat::expect_true(cfb4th_clear_cache("fd_model"))
  testthat::expect_false(exists("fd_model", envir = cfb4th:::.models))
  testthat::expect_false(file.exists(cfb4th:::cfb4th_model_path("fd_model")))
})
