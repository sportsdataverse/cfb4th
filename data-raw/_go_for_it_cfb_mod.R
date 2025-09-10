library(tidyverse)
library(tidymodels)

if (!exists("pbp")) {
  pbp = cfbfastR::load_cfb_pbp(2014:2024)
}

model_vars <- pbp %>%
  filter(
    down %in% c(3,4),
    rush == 1 | pass == 1,
    !is.na(offense_play),
    !is.na(yards_to_goal),
    !is.na(score_diff)
  ) %>%
  # left_join(lines,by = c("game_id")) %>%
  mutate(first_down_penalty = firstD_by_penalty) %>%
  mutate(#yards_gained =

           # we need a way to account for defensive penalties that give auto first downs
           # hacky "solution" is saying here that a penalty that gives a first down goes for the yards to go
           # unless the actual penalty yardage is higher

           # the drawback is that a defensive holding on eg 4th and 8 is coded as an 8 yard gain
           # the alternative is to estimate a separate model for penalties or have their own category
           # but first down penalties on 4th and long are very rare:
           # https://twitter.com/benbbaldwin/status/1322530446371074050
           # case_when(
           #   first_down_penalty == 1 & penalty_yards < ydstogo ~ ydstogo,
           #   first_down_penalty == 1 & penalty_yards >= ydstogo ~ penalty_yards,
           #   TRUE ~ yards_gained
           # ),
         # truncate to make model training easier
         yards_gained = ifelse(yards_gained < -10, -10, yards_gained),
         yards_gained = ifelse(yards_gained > 65, 65, yards_gained),
         home_total = (spread + over_under) / 2,
         away_total = (over_under - spread) / 2,
         posteam_total = if_else(offense_play == home_team, home_total, away_total),
         posteam_spread = dplyr::if_else(offense_play == home_team, spread, -1 * spread)
  ) %>%
  # look at when an actual play is run or a defensive penalty gives a first down
  filter(rush+pass == 1 | first_down_penalty == 1,
         distance > 0,
         yards_to_goal > 0,
         distance <= yards_to_goal,
         !is.na(posteam_spread),
         !is.na(posteam_total)) %>%
  mutate(label = as.double(yards_gained)) %>%
  select(
    #game_id,offense_play, #mine for debugging
    label,
    down,
    distance,
    yards_to_goal,
    #era3, era4,
    #outdoors, retractable, dome,
    #spread_line,home_total,away_total,home_team,away_team, #mine for debugging
    posteam_spread,
    #total_line,
    posteam_total
  ) %>%
  # 0 = 10 yard loss
  mutate(label = label + 10)

set.seed(2013)

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_vars %>% dplyr::select(-label)), label = as.integer(model_vars$label))

nrounds = 5000

grid <- grid_latin_hypercube(
  finalize(mtry(), model_vars),
  # min_n(),
  # tree_depth(),
  # learn_rate(),
  # loss_reduction(),

  dials::min_n(),
  # force tree depth to be between 3 and 5
  dials::tree_depth(range = c(4L, 8L)),
  # to force learn_rate to not be crazy small like dials defaults to
  dials::learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),

  # sample_size = sample_prop(),
  size = 20
)

grid <- grid %>%
  mutate(
    # it was making dumb learn rates
    learn_rate = .025 + .1 * ((1 : nrow(grid)) / nrow(grid)),
    # has to be between 0 and 1
    mtry = mtry / length(model_vars)
  )

grid

get_metrics <- function(df, row = 1) {

  # testing only
  # df <- grid %>% dplyr::slice(1)

  params <-
    list(
      booster = "gbtree",
      objective = "multi:softprob",
      eval_metric = c("mlogloss"),
      num_class = 76,
      eta = df$learn_rate,
      gamma = df$loss_reduction,
      subsample= df$sample_size,
      colsample_bytree= df$mtry,
      max_depth = df$tree_depth,
      min_child_weight = df$min_n
    )

  # tuning with cv
  fd_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                              nfold = 5, metrics = list("mlogloss"),
                              early_stopping_rounds = 10, print_every_n = 10)

  output <- params
  output$iter = fd_model$best_iteration
  output$logloss = fd_model$evaluation_log[output$iter]$test_mlogloss_mean
  output$error = fd_model$evaluation_log[output$iter]$test_merror_mean

  this_param <- bind_rows(output)

  # if (row == 1) {
  #   saveRDS(this_param, "data/modeling.rds")
  # } else {
  #   prev <- readRDS("data/modeling.rds")
  #   for_save <- bind_rows(prev, this_param)
  #   saveRDS(for_save, "data/modeling.rds")
  # }

  return(this_param)

}

results <- map_df(1 : nrow(grid), function(x) {

  message(glue::glue("Row {x}"))
  get_metrics(grid %>% dplyr::slice(x), row = x)

})
saveRDS(results, "data/modeling.rds")

# plot
results %>%
  select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  pivot_longer(eta:min_child_weight,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

best_model <- results %>%
  dplyr::arrange(logloss) %>%
  dplyr::slice(1)

# **************************************************************************************
# train

nrounds = best_model$iter
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 76,
    eta = best_model$eta,
    gamma = best_model$gamma,
    subsample = best_model$subsample,
    colsample_bytree = best_model$colsample_bytree,
    max_depth = best_model$max_depth,
    min_child_weight = best_model$min_child_weight
  )

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_vars %>% dplyr::select(-label)), label = as.integer(model_vars$label))
fd_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

#saveRDS(fd_model, file = 'data/fd_model.RDS')
usethis::use_data(fd_model, internal = TRUE, overwrite = T)

importance <- xgboost::xgb.importance(feature_names = colnames(fd_model), model = fd_model)
xgboost::xgb.ggplot.importance(importance_matrix = importance)



# yard_model_2 = MASS::polr(as.factor(label) ~ .,data = model_vars)
# saveRDS(yard_model,"yard_model.RDS")


#"https://github.com/sportsdataverse/sportsdataverse-py/blob/main/sportsdataverse/cfb/models/wp_spread.model"
# Spread WP model from gameonpaper.com
wp_model <- xgboost::xgb.load(paste0(here::here(), "/data/wp_spread.model"))
usethis::use_data(wp_model, internal = TRUE, overwrite = T)


# EP model from cfbfastR
ep_model <- cfbfastR:::ep_model
usethis::use_data(ep_model, internal = TRUE, overwrite = T)


# Team Info from cfbfastR
team_info <- cfbfastR::cfbd_team_info()


# ADD EVERYTHING
usethis::use_data(team_info, internal = TRUE, overwrite = TRUE)
