library(tidyverse)
library(MASS)

if (!exists("pbp")) {
  pbp = cfbfastR::load_cfb_pbp(2014:2024)
}

get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

points <- pbp %>%
  dplyr::select(play_text, yards_to_goal,punt, kick_distance=yds_punted, return_yards=yds_punt_return, punt_blocked) %>%
  dplyr::mutate(
    # give return yards too
    yards_to_goal_end = yards_to_goal - kick_distance + return_yards,
    yards_to_goal_end =
      ifelse(
        stringr::str_detect(play_text, "end zone") & is.na(kick_distance), 20, yards_to_goal_end
      ),
    # for blocked punts, just give them the ball there
    yards_to_goal_end = ifelse(punt_blocked == 1 & is.na(yards_to_goal_end), yards_to_goal, yards_to_goal_end),
    # make it in the actual field of play
    yards_to_goal_end = ifelse(yards_to_goal_end > 100, 100, yards_to_goal_end),
    # there's 2 safeties that are too annoying to deal with
    yards_to_goal_end = ifelse(yards_to_goal_end <= 0, 1, yards_to_goal_end),
    blocked = ifelse(punt_blocked == 1, 1, 0),
    return_td = ifelse(yards_to_goal_end == 100, 1, 0),
    # there's 1 play where there was a fumble lost after a blocked punt
    # this isn't a muffed punt
  ) %>%
  # there's like 10 of these for some reason
  dplyr::filter(!is.na(yards_to_goal_end)) %>%
  dplyr::select(play_text, yards_to_goal, yards_to_goal_end, blocked, return_td)

points

outliers <- points %>%
  dplyr::group_by(yards_to_goal) %>%
  dplyr::summarize(
    #muffed = sum(fumble_lost),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    bin = case_when(
      yards_to_goal < 40 ~ 0,
      between(yards_to_goal, 40, 49) ~ 1,
      between(yards_to_goal, 50, 59) ~ 2,
      between(yards_to_goal, 60, 69) ~ 3,
      between(yards_to_goal, 70, 79) ~ 4,
      between(yards_to_goal, 80, 89) ~ 5,
      between(yards_to_goal, 90, 99) ~ 6
    )
  ) %>%
  dplyr::group_by(bin) %>%
  dplyr::mutate(
   # muffed = sum(muffed),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = sum(n),
    #bin_muffed_pct = muffed / n,
    bin_blocked_pct = blocked / n,
    bin_td_pct = return_td / n,
  ) %>%
  dplyr::ungroup()

return_tds <- outliers %>%
  dplyr::mutate(
    yards_to_goal_end = 100,
    density = bin_blocked_pct
  ) %>%
  dplyr::select(yards_to_goal, yards_to_goal_end, density) %>%
  dplyr::filter(density > 0)

blocks <- outliers %>%
  dplyr::mutate(
    # not used for anything except to pick these out later
    yards_to_goal_end = 999,
    density = bin_td_pct
  ) %>%
  dplyr::select(yards_to_goal, yards_to_goal_end, density) %>%
  dplyr::filter(density > 0)

# get density excluding blocks and returns. will add those later
density_map_normal <- points %>%
  dplyr::filter(blocked == 0 & return_td == 0) %>%
  dplyr::select(yards_to_goal, yards_to_goal_end) %>%
  dplyr::mutate(density = get_density(yards_to_goal, yards_to_goal_end, n = 100))

# get final percentages
df <- density_map_normal %>%
  dplyr::group_by(yards_to_goal, yards_to_goal_end) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(yards_to_goal, yards_to_goal_end) %>%
  dplyr::group_by(yards_to_goal) %>%
  dplyr::mutate(
    tot_dens = sum(density),
    pct = density / tot_dens
  ) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(blocks) %>%
  dplyr::bind_rows(return_tds) %>%
  dplyr::arrange(yards_to_goal, yards_to_goal_end) %>%
  dplyr::group_by(yards_to_goal) %>%
  dplyr::mutate(
    outlier_pct = sum(density * (yards_to_goal_end == 100)) + sum(density * (yards_to_goal_end == 999)),
    non_outlier_pct = 1 - outlier_pct,
    pct = pct * non_outlier_pct,
    pct = dplyr::if_else(is.na(pct), density, pct),
    yards_to_goal_end = dplyr::if_else(yards_to_goal_end == 999, yards_to_goal, yards_to_goal_end)
  ) %>%
  dplyr::ungroup() %>%
  # left_join(
  #   outliers %>% select(yards_to_goal, bin_muffed_pct), by = "yards_to_goal"
  # ) %>%
  dplyr::arrange(yards_to_goal, yards_to_goal_end) %>%
  dplyr::select(yards_to_goal, yards_to_goal_end, pct) %>% #, bin_muffed_pct) %>%
  dplyr::filter(yards_to_goal > 30)

punt_df <- bind_rows(
  # get a df without the return and blocked probs
  df %>%
    dplyr::filter(yards_to_goal_end != 100 & yards_to_goal != yards_to_goal_end),
  df
) %>%
  dplyr::arrange(yards_to_goal, yards_to_goal_end) %>%
  dplyr::group_by(yards_to_goal, yards_to_goal_end) %>%
  # mutate(
  #   muff = 1 : n() - 1,
  #   #pct = ifelse(muff == 1, bin_muffed_pct * pct, pct),
  #   pct = ifelse(
  #     muff == 0 & yards_to_goal_end != 100 & yards_to_goal != yards_to_goal_end, (1 - bin_muffed_pct) * pct, pct
  #   )
  # ) %>%
  # one last making sure all the pct add up to 1
  dplyr::group_by(yards_to_goal) %>%
  dplyr::mutate(tot_pct = sum(pct), pct = pct / tot_pct) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    yards_to_goal, yards_to_goal_end, pct#, muff
  )

#saveRDS(punt_df,"punt_df.RDS")

usethis::use_data(punt_df, internal = TRUE, overwrite = TRUE)




