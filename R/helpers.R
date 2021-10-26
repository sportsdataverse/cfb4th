# # # # a couple basic helpers

drop.cols <- c(
  "fg_make_prob"
)

# data prep function
prepare_df <- function(df) {

  df %>%
    # if an nflfastR df is passed, need to drop all this so we don't get redundant cols
    dplyr::select(-tidyselect::any_of(drop.cols)) %>%
    #left_join(.games_nfl4th, by = c("home_team", "away_team", "type", "season")) %>%
    mutate(
      # simple way to deal with random errors from mismatched names
      home_team = home,
      away_team = away,
      # pos_team_receives_2H_kickoff = case_when(
      #   # 1st half, home team opened game with kickoff, away team has ball
      #   period <= 2 & home_opening_kickoff == 1 & pos_team == away_team ~ 1,
      #   # 1st half, away team opened game with kickoff, home team has ball
      #   period <= 2 & home_opening_kickoff == 0 & pos_team == home_team ~ 1,
      #   TRUE ~ 0
      # ),
      down = 4,
      #half_seconds_remaining = if_else(qtr == 2 | qtr == 4, quarter_seconds_remaining, quarter_seconds_remaining + 900),
      #game_seconds_remaining = if_else(qtr <= 2, half_seconds_remaining + 1800, half_seconds_remaining),
      pos_team_spread = if_else(pos_team == home, spread, -spread),
      home_total = (spread + over_under) / 2,
      away_total = (over_under - spread) / 2,
      pos_team_total = dplyr::if_else(pos_team == home, home_total, away_total),
      pos_team_spread = dplyr::if_else(pos_team == home, spread, -1 * spread),

      # set timeouts to default if data says it's negative. May not be accurate but fewer timeouts could give drastically different results in end of game situations
      pos_team_timeouts_rem_before = as.double(pos_team_timeouts_rem_before),
      def_pos_team_timeouts_rem_before = as.double(def_pos_team_timeouts_rem_before),
      pos_team_timeouts_rem_before = dplyr::if_else(pos_team_timeouts_rem_before < 0, 3, pos_team_timeouts_rem_before),
      def_pos_team_timeouts_rem_before = dplyr::if_else(def_pos_team_timeouts_rem_before < 0, 3, def_pos_team_timeouts_rem_before),
      # useful for lots of stuff later

      home_timeouts_remaining = dplyr::if_else(pos_team == home, pos_team_timeouts_rem_before, def_pos_team_timeouts_rem_before),
      away_timeouts_remaining = dplyr::if_else(pos_team == away, pos_team_timeouts_rem_before, def_pos_team_timeouts_rem_before),
      original_pos_team = pos_team
    ) %>%
    return()

}

# Functions to change teams based on calculator sim

flip_team <- function(df) {
  # df %>%
  #   mutate(
  #     down = 1,
  #     distance = 10,
  #     TimeSecsRem = ifelse(TimeSecsRem<=6,0,TimeSecsRem - 6),
  #     pos_team_score_temp = def_pos_team_score,
  #     def_pos_team_score = pos_team_score,
  #     pos_team_score = pos_team_score_temp,
  #     pos_to_temp = def_pos_team_timeouts_rem_before,
  #     def_pos_team_timeouts_rem_before = pos_team_timeouts_rem_before,
  #     pos_team_timeouts_rem_before = pos_to_temp,
  #     score_differential = pos_team_score - def_pos_team_score,
  #     pos_score_diff_start = pos_team_score - def_pos_team_score,
  #     pos_team = if_else(home == pos_team, away, home),
  #     is_home = if_else(home == pos_team,TRUE,FALSE),
  #     pos_team_spread = -1*pos_team_spread,
  #     pos_team_receives_2H_kickoff = if_else(pos_team_receives_2H_kickoff == 1,0,1)
  #   )%>%
  #   mutate(Under_two = TimeSecsRem < 120,
  #          log_ydstogo = log(yards_to_goal),
  #          Goal_To_Go = distance == yards_to_goal
  #   )
  #

  df %>%
    mutate(
      # switch posteam
      pos_team = if_else(home == original_pos_team, away, home),
      def_pos_team = if_else(home == original_pos_team, home, away),
      # update timeouts
      pos_team_timeouts_rem_before = if_else(pos_team == away, away_timeouts_remaining, home_timeouts_remaining),
      def_pos_team_timeouts_rem_before = if_else(pos_team == home, away_timeouts_remaining, home_timeouts_remaining),
      # swap score
      pos_score_diff_start = -pos_score_diff_start,
      # 1st and 10
      down = 1,
      distance = 10,
      # run off 6 seconds
      TimeSecsRem = TimeSecsRem - 6,
      adj_TimeSecsRem = adj_TimeSecsRem - 6,
      # don't let seconds go negative
      TimeSecsRem = if_else(TimeSecsRem < 0, 0, TimeSecsRem),
      adj_TimeSecsRem = if_else(adj_TimeSecsRem < 0, 0, adj_TimeSecsRem),
      # flip receive_2h_ko var
      pos_team_receives_2H_kickoff = case_when(
        period <= 2 & pos_team_receives_2H_kickoff == 0 ~ 1,
        period <= 2 & pos_team_receives_2H_kickoff == 1 ~ 0,
        TRUE ~ pos_team_receives_2H_kickoff
      ),
      # flip spread
      pos_team_spread = -1*pos_team_spread,
    ) %>%
    return()
}

# helper function to move the game to start of 3rd Q on an end-of-half play
# on the plays where we find that the half has ended
flip_half <- function(df) {

  df %>%
    mutate(
      prior_pos_team = pos_team,
      end_of_half = ifelse(
        period == 2 & TimeSecsRem == 0, 1, 0
      ),
      pos_team = case_when(
        pos_team_receives_2H_kickoff == 1 & end_of_half == 1 ~ pos_team,
        pos_team_receives_2H_kickoff == 0 & end_of_half == 1 ~ def_pos_team,
        TRUE ~ pos_team
      ),
      def_pos_team = case_when(
        pos_team_receives_2H_kickoff == 1 & end_of_half == 1 ~ def_pos_team,
        pos_team_receives_2H_kickoff == 0 & end_of_half == 1 ~ pos_team,
        TRUE ~ def_pos_team
      ),
      period = ifelse(end_of_half == 1, 3L, period),
      half = ifelse(end_of_half == 1, 2L, half),
      pos_team_timeouts_rem_before = ifelse(end_of_half == 1, 3L, pos_team_timeouts_rem_before),
      def_pos_team_timeouts_rem_before = ifelse(end_of_half == 1, 3L, def_pos_team_timeouts_rem_before),
      down = ifelse(end_of_half == 1, 1, down),
      distance = ifelse(end_of_half == 1, 10L, distance),
      yards_to_goal = ifelse(end_of_half == 1, 75L, yards_to_goal),
      TimeSecsRem = ifelse(end_of_half == 1, 1800, TimeSecsRem),
      adj_TimeSecsRem = ifelse(end_of_half == 1, 1800, adj_TimeSecsRem),
      pos_score_diff_start = ifelse(
        pos_team != prior_pos_team & end_of_half == 1, -pos_score_diff_start, pos_score_diff_start
      ),
      pos_team_spread = case_when(
        pos_team_receives_2H_kickoff == 1 & end_of_half == 1 ~ pos_team_spread,
        pos_team_receives_2H_kickoff == 0 & end_of_half == 1 ~ -1*pos_team_spread,
        TRUE ~ pos_team_spread
      ),

      pos_team_receives_2H_kickoff = ifelse(
        pos_team != prior_pos_team & end_of_half == 1, 1, pos_team_receives_2H_kickoff
      )
    ) %>%
    select(-prior_pos_team, -end_of_half) %>%
    return()

}

# fill in end of game situation when team can kneel out clock
# discourages punting or fg when the other team can end the game
end_game_fn <- function(pbp) {

  pbp %>%
    mutate(
      wp = case_when(
        pos_score_diff_start > 0 & adj_TimeSecsRem < 120 & period == 4 & def_pos_team_timeouts_rem_before == 0 ~ 0,
        pos_score_diff_start > 0 & adj_TimeSecsRem < 80 & period == 4 & def_pos_team_timeouts_rem_before == 1 ~ 0,
        pos_score_diff_start > 0 & adj_TimeSecsRem < 40 & period == 4 & def_pos_team_timeouts_rem_before == 2 ~ 0,
        TRUE ~ wp
      )
    ) %>%
    return()
}


# #########################################################################################
# other: this isn't used by the bot or shiny app but helps get an cfbfastR df read

# prepare raw pbp data
prepare_cfbfastr_data <- function(pbp) {

  # some prep
  data <- pbp %>%
    dplyr::mutate(
      qtr = period
      #type = ifelse(tolower(season_type) == "reg", "reg", "post")
    ) %>%
    dplyr::filter(
      adj_TimeSecsRem > 30,
      !is.na(TimeSecsRem),
      !is.na(period),
      !is.na(pos_team),
      !is.na(spread),
      yards_to_goal > 0,
      period <= 4
    )

  return(data)

}

# Functions to add WP for calculator
prep_ep <- function(game_state) {
  suppressWarnings({
    game_state %>%
      mutate(log_ydstogo = log(yards_to_goal),
             Goal_To_Go = distance >= yards_to_goal,
             #pos_score_diff_start = pos_team_score - def_pos_team_score,
             Under_two = TimeSecsRem <= 120,
             down = factor(down,levels = c(1,2,3,4)))
  })

}

add_ep <- function(game_state) {
  ep_vars <- game_state %>%
    select(TimeSecsRem,
           yards_to_goal,
           down,
           log_ydstogo,
           Goal_To_Go,
           pos_score_diff_start,
           Under_two)

  for_return <- predict(object = cfbfastR:::ep_model,newdata = ep_vars,type = "probs")
  if (nrow(game_state) > 1) {
    colnames(for_return) <- c("prob_no_score",
                              "prob_fg",
                              "prob_def_fg",
                              "prob_def_safety",
                              "prob_def_td",
                              "prob_safety",
                              "prob_td")
  } else {
    names(for_return) <- c("prob_no_score",
                           "prob_fg",
                           "prob_def_fg",
                           "prob_def_safety",
                           "prob_def_td",
                           "prob_safety",
                           "prob_td")
    for_return <- for_return %>%
      as.list() %>%
      data.frame()
  }

  opp_field_pos_value <- 0

  for_return <- for_return %>%
    as_tibble() %>%
    mutate(ep =
             (3-opp_field_pos_value)*prob_fg +
             (-3+opp_field_pos_value)*prob_def_fg +
             (-2-opp_field_pos_value)*prob_def_safety +
             (-6.95+opp_field_pos_value)*prob_def_td +
             (2+opp_field_pos_value)*prob_safety +
             (6.95-opp_field_pos_value)*prob_td
    )
  return(bind_cols(game_state,for_return))
}

prep_wp <- function(game_state) {
  game_state %>%
    mutate(ExpScoreDiff = pos_score_diff_start + ep,
           #adj_TimeSecsRem = TimeSecsRem + ifelse(half == 1,1800,0),
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/(adj_TimeSecsRem+1),
           elapsed_share = ((3600 - adj_TimeSecsRem) / 3600),
           spread_time = (-1 * pos_team_spread) * exp(-4 * elapsed_share),
           is_home = ifelse(pos_team == home, TRUE, FALSE)
    )
}

add_wp <- function(game_state) {
  wp_vars <- game_state %>%
    select(
      "pos_team_receives_2H_kickoff",
      "spread_time",
      "TimeSecsRem",
      "adj_TimeSecsRem",
      "ExpScoreDiff_Time_Ratio",
      "pos_score_diff_start",
      "down",
      "distance",
      "yards_to_goal",
      "is_home",
      "pos_team_timeouts_rem_before",
      "def_pos_team_timeouts_rem_before",
      "period"
    ) %>%
    # Down is encoded as a factor, this prevents creating a character matrix
    mutate(down = as.numeric(down)) %>%
    as.matrix()
  game_state$wp <- predict(wp_model, newdata = wp_vars)
  return(game_state)
}

add_probs <- function(df) {

  df %>%
    get_go_wp() %>%
    get_fg_wp() %>%
    get_punt_wp() %>%
    return()

}
