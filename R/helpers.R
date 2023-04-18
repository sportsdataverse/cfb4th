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
    dplyr::mutate(
      # simple way to deal with random errors from mismatched names
      home_team = .data$home,
      away_team = .data$away,
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
      pos_team_spread = dplyr::if_else(.data$pos_team == .data$home, .data$spread, -.data$spread),
      home_total = (.data$spread + .data$over_under) / 2,
      away_total = (.data$over_under - .data$spread) / 2,
      pos_team_total = dplyr::if_else(.data$pos_team == .data$home, .data$home_total, .data$away_total),
      pos_team_spread = dplyr::if_else(.data$pos_team == .data$home, .data$spread, -1 * .data$spread),

      # set timeouts to default if data says it's negative. May not be accurate but fewer timeouts could give drastically different results in end of game situations
      pos_team_timeouts_rem_before = as.double(.data$pos_team_timeouts_rem_before),
      def_pos_team_timeouts_rem_before = as.double(.data$def_pos_team_timeouts_rem_before),
      pos_team_timeouts_rem_before = dplyr::if_else(
        .data$pos_team_timeouts_rem_before < 0,
        3,
        .data$pos_team_timeouts_rem_before),
      def_pos_team_timeouts_rem_before = dplyr::if_else(
        .data$def_pos_team_timeouts_rem_before < 0,
        3,
        .data$def_pos_team_timeouts_rem_before),
      # useful for lots of stuff later

      home_timeouts_remaining = dplyr::if_else(
        .data$pos_team == .data$home,
        .data$pos_team_timeouts_rem_before,
        .data$def_pos_team_timeouts_rem_before),
      away_timeouts_remaining = dplyr::if_else(
        .data$pos_team == .data$away,
        .data$pos_team_timeouts_rem_before,
        .data$def_pos_team_timeouts_rem_before),
      original_pos_team = .data$pos_team
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
    dplyr::mutate(
      # switch posteam
      pos_team = dplyr::if_else(.data$home == .data$original_pos_team, .data$away, .data$home),
      def_pos_team = dplyr::if_else(.data$home == .data$original_pos_team, .data$home, .data$away),
      # update timeouts
      pos_team_timeouts_rem_before = dplyr::if_else(
        .data$pos_team == .data$away,
        .data$away_timeouts_remaining,
        .data$home_timeouts_remaining),
      def_pos_team_timeouts_rem_before = dplyr::if_else(
        .data$pos_team == .data$home,
        .data$away_timeouts_remaining,
        .data$home_timeouts_remaining),
      # swap score
      pos_score_diff_start = -.data$pos_score_diff_start,
      # 1st and 10
      down = 1,
      distance = 10,
      # run off 6 seconds
      TimeSecsRem = .data$TimeSecsRem - 6,
      adj_TimeSecsRem = .data$adj_TimeSecsRem - 6,
      # don't let seconds go negative
      TimeSecsRem = dplyr::if_else(.data$TimeSecsRem < 0, 0, .data$TimeSecsRem),
      adj_TimeSecsRem = dplyr::if_else(.data$adj_TimeSecsRem < 0, 0, .data$adj_TimeSecsRem),
      # flip receive_2h_ko var
      pos_team_receives_2H_kickoff = dplyr::case_when(
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 0 ~ 1,
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 1 ~ 0,
        TRUE ~ .data$pos_team_receives_2H_kickoff
      ),
      # flip spread
      pos_team_spread = -1*.data$pos_team_spread,
    ) %>%
    return()
}

# helper function to move the game to start of 3rd Q on an end-of-half play
# on the plays where we find that the half has ended
flip_half <- function(df) {

  df %>%
    dplyr::mutate(
      prior_pos_team = .data$pos_team,
      end_of_half = ifelse(
        .data$period == 2 & .data$TimeSecsRem == 0, 1, 0
      ),
      pos_team = dplyr::case_when(
        .data$pos_team_receives_2H_kickoff == 1 & .data$end_of_half == 1 ~ .data$pos_team,
        .data$pos_team_receives_2H_kickoff == 0 & .data$end_of_half == 1 ~ .data$def_pos_team,
        TRUE ~ .data$pos_team
      ),
      def_pos_team = dplyr::case_when(
        .data$pos_team_receives_2H_kickoff == 1 & .data$end_of_half == 1 ~ .data$def_pos_team,
        .data$pos_team_receives_2H_kickoff == 0 & .data$end_of_half == 1 ~ .data$pos_team,
        TRUE ~ .data$def_pos_team
      ),
      period = ifelse(.data$end_of_half == 1, 3L, .data$period),
      half = ifelse(.data$end_of_half == 1, 2L, .data$half),
      pos_team_timeouts_rem_before = ifelse(.data$end_of_half == 1, 3L, .data$pos_team_timeouts_rem_before),
      def_pos_team_timeouts_rem_before = ifelse(.data$end_of_half == 1, 3L, .data$def_pos_team_timeouts_rem_before),
      down = ifelse(.data$end_of_half == 1, 1, .data$down),
      distance = ifelse(.data$end_of_half == 1, 10L, .data$distance),
      yards_to_goal = ifelse(.data$end_of_half == 1, 75L, .data$yards_to_goal),
      TimeSecsRem = ifelse(.data$end_of_half == 1, 1800, .data$TimeSecsRem),
      adj_TimeSecsRem = ifelse(.data$end_of_half == 1, 1800, .data$adj_TimeSecsRem),
      pos_score_diff_start = ifelse(
        .data$pos_team != .data$prior_pos_team & .data$end_of_half == 1,
        -.data$pos_score_diff_start,
        .data$pos_score_diff_start
      ),
      pos_team_spread = dplyr::case_when(
        .data$pos_team_receives_2H_kickoff == 1 & .data$end_of_half == 1 ~ .data$pos_team_spread,
        .data$pos_team_receives_2H_kickoff == 0 & .data$end_of_half == 1 ~ -1*.data$pos_team_spread,
        TRUE ~ .data$pos_team_spread
      ),

      pos_team_receives_2H_kickoff = ifelse(
        .data$pos_team != .data$prior_pos_team & .data$end_of_half == 1,
        1,
        .data$pos_team_receives_2H_kickoff
      )
    ) %>%
    dplyr::select(-"prior_pos_team", -"end_of_half") %>%
    return()

}

# fill in end of game situation when team can kneel out clock
# discourages punting or fg when the other team can end the game
end_game_fn <- function(pbp) {

  pbp %>%
    mutate(
      wp = dplyr::case_when(
        .data$pos_score_diff_start > 0 & .data$adj_TimeSecsRem < 120 & .data$period == 4 &
          .data$def_pos_team_timeouts_rem_before == 0 ~ 0,
        .data$pos_score_diff_start > 0 & .data$adj_TimeSecsRem < 80 & .data$period == 4 &
          .data$def_pos_team_timeouts_rem_before == 1 ~ 0,
        .data$pos_score_diff_start > 0 & .data$adj_TimeSecsRem < 40 & .data$period == 4 &
          .data$def_pos_team_timeouts_rem_before == 2 ~ 0,
        TRUE ~ .data$wp
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
      qtr = .data$period
      #type = ifelse(tolower(season_type) == "reg", "reg", "post")
    ) %>%
    dplyr::filter(
      .data$adj_TimeSecsRem > 30,
      !is.na(.data$TimeSecsRem),
      !is.na(.data$period),
      !is.na(.data$pos_team),
      !is.na(.data$spread),
      .data$yards_to_goal > 0,
      .data$period <= 4
    )

  return(data)

}

# Functions to add WP for calculator
prep_ep <- function(game_state) {
  suppressWarnings({
    game_state %>%
      dplyr::mutate(
        log_ydstogo = log(.data$yards_to_goal),
        Goal_To_Go = .data$distance >= .data$yards_to_goal,
        #pos_score_diff_start = pos_team_score - def_pos_team_score,
        Under_two = .data$TimeSecsRem <= 120,
        down = factor(.data$down,levels = c(1, 2, 3, 4)))
  })

}

add_ep <- function(game_state) {
  ep_vars <- game_state %>%
    dplyr::select(
      "TimeSecsRem",
      "yards_to_goal",
      "down",
      "log_ydstogo",
      "Goal_To_Go",
      "pos_score_diff_start",
      "Under_two")

  for_return <- predict(object = ep_model, newdata = ep_vars, type = "probs")
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
    dplyr::as_tibble() %>%
    dplyr::mutate(
      ep =
        (3 - opp_field_pos_value) * .data$prob_fg +
        (-3 + opp_field_pos_value) * .data$prob_def_fg +
        (-2 - opp_field_pos_value) * .data$prob_def_safety +
        (-6.95 + opp_field_pos_value) * .data$prob_def_td +
        (2 + opp_field_pos_value) * .data$prob_safety +
        (6.95 - opp_field_pos_value) * .data$prob_td
    )
  return(dplyr::bind_cols(game_state,for_return))
}

prep_wp <- function(game_state) {
  game_state %>%
    dplyr::mutate(
      ExpScoreDiff = .data$pos_score_diff_start + .data$ep,
      #adj_TimeSecsRem = TimeSecsRem + ifelse(half == 1,1800,0),
      ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff/(.data$adj_TimeSecsRem + 1),
      elapsed_share = ((3600 - .data$adj_TimeSecsRem) / 3600),
      spread_time = (-1 * .data$pos_team_spread) * exp(-4 * .data$elapsed_share),
      is_home = ifelse(.data$pos_team == .data$home, TRUE, FALSE)
    )
}

add_wp <- function(game_state) {
  wp_vars <- game_state %>%
    dplyr::select(
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
    dplyr::mutate(down = as.numeric(.data$down)) %>%
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
