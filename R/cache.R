# Model download + package cache, modelled on nfl4th's cached_model().
#
# The expected-points (nnet) and field-goal (mgcv) models are small and ship
# in R/sysdata.rda (see data-raw/sysdata.R). The two xgboost models are 17 MB
# together, so they are not bundled and are not loaded at package load: each
# is downloaded the first time it is needed, parsed once per session, and
# saved under tools::R_user_dir() so the next session reads it from disk. On a
# CRAN check machine (probably_cran()) nothing is written: the model is
# downloaded and used in memory only.

cfb4th_cache_dir <- function() {
  tools::R_user_dir("cfb4th", "cache")
}

cfb4th_model_path <- function(name) {
  file.path(cfb4th_cache_dir(), paste0(name, ".rds"))
}

model_names <- c("fd_model", "wp_model")

# raw UBJ byte vectors saved with saveRDS() on this package's model_archive
# release, read back with xgboost::xgb.load.raw()
model_url <- function(name) {
  switch(
    name,
    fd_model = "https://github.com/sportsdataverse/cfb4th/releases/download/model_archive/fd_model.rds",
    wp_model = "https://github.com/sportsdataverse/cfb4th/releases/download/model_archive/wp_model.rds"
  )
}

# one parsed copy per session, so a cached 15 MB model is read from disk once
.models <- new.env(parent = emptyenv())

fd_model <- function() cached_model("fd_model")
wp_model <- function() cached_model("wp_model")

cached_model <- function(name) {
  if (!is.null(.models[[name]])) {
    return(.models[[name]])
  }
  path <- cfb4th_model_path(name)
  use_cache <- !probably_cran() || force_cache()

  obj <- NULL
  if (use_cache && file.exists(path)) {
    # an interrupted earlier write can leave a truncated file: drop it and
    # download again instead of failing on every later session
    obj <- tryCatch(readRDS(path), error = function(e) {
      unlink(path)
      NULL
    })
  }
  if (is.null(obj)) {
    obj <- download_model(name)
    if (use_cache) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      # write next to the final path and rename, so the cache file only ever
      # appears once it is complete
      tmp <- tempfile(tmpdir = dirname(path), fileext = ".rds")
      saveRDS(obj, tmp)
      if (!file.rename(tmp, path)) unlink(tmp)
    }
  }

  parsed <- xgboost::xgb.load.raw(obj)
  assign(name, parsed, envir = .models)
  parsed
}

download_model <- function(name) {
  url <- model_url(name)
  con <- url(url)
  on.exit(close(con))
  obj <- try(readRDS(con), silent = TRUE)
  if (inherits(obj, "try-error")) {
    stop(
      "The cfb4th ", name, " could not be downloaded from <", url, ">.\n",
      "The models are fetched on first use and cached in ", cfb4th_cache_dir(),
      "; check your network connection and try again.",
      call. = FALSE
    )
  }
  obj
}

# TRUE while R CMD check --as-cran (and CRAN's own check machines) run, so the
# package never writes a cache there. Same detection as nfl4th.
probably_cran <- function() {
  envvars <- c(
    "_R_CHECK_EXAMPLE_TIMING_CPU_TO_ELAPSED_THRESHOLD_",
    "_R_CHECK_THINGS_IN_OTHER_DIRS_",
    "_R_CHECK_THINGS_IN_OTHER_DIRS_XTRA_"
  )
  any(nzchar(Sys.getenv(envvars, unset = "")))
}

# allow a user to keep the cache even if probably_cran() is TRUE
force_cache <- function() {
  isTRUE(getOption("cfb4th.force_cache", FALSE))
}

#' Reset the cfb4th model cache
#'
#' The fourth-down conversion and win-probability models are downloaded the
#' first time they are needed and cached under
#' `tools::R_user_dir("cfb4th", "cache")`. Clear the cache to force a fresh
#' download, for example after a model update.
#'
#' @param type One of `"all"` (the default), `"fd_model"` or `"wp_model"`.
#'
#' @return Returns `TRUE` invisibly once the cache has been cleared.
#' @export
#'
#' @examples
#' cfb4th_clear_cache()
cfb4th_clear_cache <- function(type = c("all", "fd_model", "wp_model")) {
  type <- match.arg(type)
  names <- if (type == "all") model_names else type
  rm(list = intersect(names, ls(.models)), envir = .models)
  paths <- cfb4th_model_path(names)
  file.remove(paths[file.exists(paths)])
  invisible(TRUE)
}
