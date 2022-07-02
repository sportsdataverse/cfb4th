# punt_df <- pbp %>%
#   filter(str_detect(play_text,"punt"))
#punt_df <- readRDS("punt_df.RDS")



get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

points <- pbp %>%
  select(play_text, yards_to_goal,punt, kick_distance=yds_punted, return_yards=yds_punt_return,punt_blocked) %>%
  mutate(
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
  filter(!is.na(yards_to_goal_end)) %>%
  select(play_text, yards_to_goal, yards_to_goal_end, blocked, return_td)

points

outliers <- points %>%
  group_by(yards_to_goal) %>%
  summarize(
    #muffed = sum(fumble_lost),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
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
  group_by(bin) %>%
  mutate(
   # muffed = sum(muffed),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = sum(n),
    #bin_muffed_pct = muffed / n,
    bin_blocked_pct = blocked / n,
    bin_td_pct = return_td / n,
  ) %>%
  ungroup()

return_tds <- outliers %>%
  mutate(
    yards_to_goal_end = 100,
    density = bin_blocked_pct
  ) %>%
  select(yards_to_goal, yards_to_goal_end, density) %>%
  filter(density > 0)

blocks <- outliers %>%
  mutate(
    # not used for anything except to pick these out later
    yards_to_goal_end = 999,
    density = bin_td_pct
  ) %>%
  select(yards_to_goal, yards_to_goal_end, density) %>%
  filter(density > 0)

# get density excluding blocks and returns. will add those later
density_map_normal <- points %>%
  filter(blocked == 0 & return_td == 0) %>%
  select(yards_to_goal, yards_to_goal_end) %>%
  mutate(density = get_density(yards_to_goal, yards_to_goal_end, n = 100))

# get final percentages
df <- density_map_normal %>%
  group_by(yards_to_goal, yards_to_goal_end) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  group_by(yards_to_goal) %>%
  mutate(
    tot_dens = sum(density),
    pct = density / tot_dens
  ) %>%
  ungroup() %>%
  bind_rows(blocks) %>%
  bind_rows(return_tds) %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  group_by(yards_to_goal) %>%
  mutate(
    outlier_pct = sum(density * (yards_to_goal_end == 100)) + sum(density * (yards_to_goal_end == 999)),
    non_outlier_pct = 1 - outlier_pct,
    pct = pct * non_outlier_pct,
    pct = ifelse(is.na(pct), density, pct),
    yards_to_goal_end = ifelse(yards_to_goal_end == 999, yards_to_goal, yards_to_goal_end)
  ) %>%
  ungroup() %>%
  # left_join(
  #   outliers %>% select(yards_to_goal, bin_muffed_pct), by = "yards_to_goal"
  # ) %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  select(yards_to_goal, yards_to_goal_end, pct) %>% #, bin_muffed_pct) %>%
  filter(yards_to_goal > 30)

punt_df <- bind_rows(
  # get a df without the return and blocked probs
  df %>%
    filter(yards_to_goal_end != 100 & yards_to_goal != yards_to_goal_end),
  df
) %>%
  arrange(yards_to_goal, yards_to_goal_end) %>%
  group_by(yards_to_goal, yards_to_goal_end) %>%
  # mutate(
  #   muff = 1 : n() - 1,
  #   #pct = ifelse(muff == 1, bin_muffed_pct * pct, pct),
  #   pct = ifelse(
  #     muff == 0 & yards_to_goal_end != 100 & yards_to_goal != yards_to_goal_end, (1 - bin_muffed_pct) * pct, pct
  #   )
  # ) %>%
  # one last making sure all the pct add up to 1
  group_by(yards_to_goal) %>%
  mutate(tot_pct = sum(pct), pct = pct / tot_pct) %>%
  ungroup() %>%
  select(
    yards_to_goal, yards_to_goal_end, pct#, muff
  )

#saveRDS(punt_df,"punt_df.RDS")

usethis::use_data(punt_df, internal = TRUE, overwrite = TRUE)




