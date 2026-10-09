# Rebuild R/sysdata.rda.
#
# ep_model (nnet::multinom) and fg_model (mgcv::bam) are the legacy cfbfastR
# models published on the cfbfastR-data repository; they are bundled here so
# scoring never needs them downloaded (nfl4th bundles its fg_model the same
# way). punt_df and team_info are carried over from the existing sysdata.
# Re-run from the package root when cfbfastR-data republishes either model.

base <- "https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/models/"
fetch <- function(name) {
  e <- new.env()
  con <- url(paste0(base, name, ".Rdata"))
  on.exit(close(con))
  load(con, envir = e)
  get(name, envir = e)
}

old <- new.env()
load("R/sysdata.rda", envir = old)
punt_df <- old$punt_df
team_info <- old$team_info

ep_model <- fetch("ep_model")
fg_model <- fetch("fg_model")
stopifnot(inherits(ep_model, "multinom"), inherits(fg_model, "bam"))

usethis::use_data(ep_model, fg_model, punt_df, team_info, internal = TRUE, overwrite = TRUE)
