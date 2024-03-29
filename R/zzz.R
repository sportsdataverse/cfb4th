.onLoad <- function(libname, pkgname) {
  ep_model <- load_ep_model()
  fg_model <- load_fg_model()
  assign("ep_model", ep_model, envir = parent.env(environment()))
  assign("fg_model", fg_model, envir = parent.env(environment()))
}
load_ep_model <- function(){
  ep_model <- NULL
  # load the model from GitHub because it is too large for the package
  .url = url("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/models/ep_model.Rdata")
  on.exit(close(.url))
  try(
    load(.url),
    silent = TRUE
  )
  return(ep_model)
}
load_fg_model <- function(){
  fg_model <- NULL
  .url = url("https://raw.githubusercontent.com/sportsdataverse/cfbfastR-data/main/models/fg_model.Rdata")
  on.exit(close(.url))
  try(
    load(.url),
    silent = TRUE
  )
  return(fg_model)
}

