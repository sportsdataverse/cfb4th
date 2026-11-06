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
  # the failed connection also warns ("cannot open URL"); only the error matters
  suppressWarnings(
    testthat::expect_error(cfb4th:::download_model("fd_model"), "could not be downloaded")
  )
})

test_that("a truncated cache file is replaced, not fatal", {
  testthat::skip_on_cran()

  cfb4th_clear_cache("wp_model")
  path <- cfb4th:::cfb4th_model_path("wp_model")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(1:10), path)   # what an interrupted saveRDS() leaves behind

  testthat::expect_s3_class(cfb4th:::wp_model(), "xgb.Booster")
  testthat::expect_type(readRDS(path), "raw")   # re-downloaded and re-cached
})

test_that("cfb4th_clear_cache() clears the session copy and the disk copy", {
  testthat::skip_on_cran()

  invisible(cfb4th:::fd_model())
  testthat::expect_true(exists("fd_model", envir = cfb4th:::.models))

  testthat::expect_true(cfb4th_clear_cache("fd_model"))
  testthat::expect_false(exists("fd_model", envir = cfb4th:::.models))
  testthat::expect_false(file.exists(cfb4th:::cfb4th_model_path("fd_model")))
})

test_that("caching a model removes other versions' caches", {
  root <- tempfile("cfb4th-cache")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(cfb4th_cache_root = function() root, .package = "cfb4th")
  current <- cfb4th:::cfb4th_cache_dir()
  dir.create(file.path(root, "0.0.1"))
  dir.create(current)
  file.create(file.path(root, "fd_model.rds"))   # pre-versioned layout

  cfb4th:::prune_cache()

  testthat::expect_identical(list.files(root), basename(current))
})

test_that("load_4th_pbp() joins betting lines onto pbp that already has spread/over_under", {
  pbp <- tibble::tibble(
    game_id = c(1L, 2L), spread = c(99, 99), over_under = c(99, 99),
    rush = c(1, 0), pass = c(0, 0), play_type = c("Rush", "Punt")
  )
  lines <- function(year, season_type = "regular", ...) {
    if (season_type != "regular") return(tibble::tibble())
    tibble::tibble(
      game_id = c(1L, 1L, 2L), provider = c("Bovada", "consensus", "Bovada"),
      spread = c("-3", "-7", "2.5"), over_under = c("50", "55", "60")
    )
  }
  testthat::local_mocked_bindings(
    load_cfb_pbp = function(...) pbp, cfbd_betting_lines = lines, .package = "cfbfastR"
  )
  testthat::local_mocked_bindings(add_4th_probs = function(df, ...) df, .package = "cfb4th")

  out <- suppressMessages(cfb4th::load_4th_pbp(2020))

  testthat::expect_false(any(c("spread.x", "spread.y", "over_under.x") %in% names(out)))
  testthat::expect_equal(out$spread, c(-7, 2.5))      # consensus preferred over Bovada
  testthat::expect_equal(out$over_under, c(55, 60))
  testthat::expect_equal(out$provider, c("consensus", "Bovada"))
  testthat::expect_equal(out$go, c(100, 0))
})

test_that("load_4th_pbp() prefers a provider with a spread and falls back to the pbp line", {
  pbp <- tibble::tibble(
    game_id = c(1L, 2L), spread = c(99, -4), over_under = c(99, 45),
    provider = c("pbp", "pbp"), formatted_spread = c("x", "y"),
    rush = c(1, 0), pass = c(0, 0), play_type = c("Rush", "Punt")
  )
  lines <- function(year, season_type = "regular", ...) {
    if (season_type != "regular") return(tibble::tibble())
    tibble::tibble(
      game_id = c(1L, 1L, 2L), provider = c("consensus", "Bovada", "consensus"),
      spread = c(NA, "-3", NA), over_under = c("55", "50", NA)
    )
  }
  testthat::local_mocked_bindings(
    load_cfb_pbp = function(...) pbp, cfbd_betting_lines = lines, .package = "cfbfastR"
  )
  testthat::local_mocked_bindings(add_4th_probs = function(df, ...) df, .package = "cfb4th")

  out <- suppressMessages(cfb4th::load_4th_pbp(2020))

  testthat::expect_equal(out$spread, c(-3, -4))       # Bovada has a spread; game 2 keeps the pbp line
  testthat::expect_equal(out$over_under, c(50, 45))
  testthat::expect_equal(out$provider, c("Bovada", "consensus"))
  testthat::expect_false(any(c("pbp_spread", "pbp_over_under", "formatted_spread") %in% names(out)))
})
