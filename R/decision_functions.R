#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom stats predict
#' @importFrom nnet multinom
#' @importFrom xgboost getinfo


get_punt_wp <- function(pbp) {
  # to join later
  pbp <- pbp %>%
    mutate(punt_index = 1 : n ())
           # original_pos_team = pos_team,
           # home_timeouts_remaining = ifelse(pos_team == home, pos_team_timeouts_rem_before, def_pos_team_timeouts_rem_before),
           # away_timeouts_remaining = ifelse(pos_team == away, pos_team_timeouts_rem_before, def_pos_team_timeouts_rem_before))
  if ("yards_to_goal_end" %in% names(pbp)) {
    pbp <- pbp %>% select(-yards_to_goal_end)
  }

  # get wp associated with punt
  probs <- pbp %>%
    left_join(punt_df, by = "yards_to_goal") %>%
    flip_team() %>%
    mutate(
      yards_to_goal = 100 - yards_to_goal_end,

      # deal with punt return TD (yards_to_goal_end == 100) or muff (muff == 1) #DOESNT HANDLE MUFFS YET
      # we want punting team to be receiving a kickoff so have to flip everything back
      pos_team = case_when(
        (yards_to_goal_end == 100) & original_pos_team == away ~ away,
        (yards_to_goal_end == 100) & original_pos_team == home ~ home,
        TRUE ~ pos_team
      ),
      def_pos_team = case_when(
        (yards_to_goal_end == 100) & original_pos_team == away ~ home,
        (yards_to_goal_end == 100) & original_pos_team == home ~ away,
        TRUE ~ def_pos_team
      ),

      pos_team_timeouts_remaining_before = case_when(
        (yards_to_goal_end == 100) & original_pos_team == home ~ home_timeouts_remaining,
        (yards_to_goal_end == 100) & original_pos_team == away ~ away_timeouts_remaining,
        TRUE ~ pos_team_timeouts_rem_before
      ),

      def_pos_team_timeouts_remaining_before = case_when(
        (yards_to_goal_end == 100) & original_pos_team == home ~ away_timeouts_remaining,
        (yards_to_goal_end == 100) & original_pos_team == away ~ home_timeouts_remaining,
        TRUE ~ def_pos_team_timeouts_rem_before
      ),

      # return TD and muff for yardline
      yards_to_goal = if_else(yards_to_goal_end == 100, as.integer(75), as.integer(yards_to_goal)),
      #yards_to_goal = if_else(muff == 1, as.integer(100 - yards_to_goal), yards_to_goal),

      pos_score_diff_start = if_else(yards_to_goal_end == 100, as.integer(-pos_score_diff_start - 7), as.integer(pos_score_diff_start)),
      #score_differential = if_else(muff == 1, as.integer(-score_differential), as.integer(score_differential)),

      pos_team_receives_2H_kickoff = case_when(
        period <= 2 & pos_team_receives_2H_kickoff == 0 & (yards_to_goal_end == 100 ) ~ 1,
        period <= 2 & pos_team_receives_2H_kickoff == 1 & (yards_to_goal_end == 100 ) ~ 0,
        TRUE ~ pos_team_receives_2H_kickoff
      ),
      pos_team_spread = if_else(yards_to_goal_end == 100, -pos_team_spread, pos_team_spread),

      distance = if_else(yards_to_goal < 10, yards_to_goal, as.integer(distance))

    ) %>%
    flip_half()
  # to deal with a single play where you shouldn't punt
  if(sum(!is.na(probs$yards_to_goal)) == 0) {
    probs <- probs %>%
      mutate(wp = NA_real_)
  } else {
    probs <- probs %>%
      prep_ep() %>%
      add_ep() %>%
      prep_wp() %>%
      add_wp() %>%
      mutate(
        wp = if_else(pos_team != original_pos_team, 1 - wp, wp)
      )
  }

  probs <- probs %>%
    end_game_fn() %>%
    mutate(
      wt_wp = pct * wp
    ) %>%
    group_by(punt_index) %>%
    summarize(punt_wp = sum(wt_wp)) %>%
    ungroup()

  pbp %>%
    left_join(probs, by = "punt_index") %>%
    select(-punt_index) #%>% view()
}

get_fg_wp <- function(pbp) {
  # probability field goal is made
  # pbp <- pbp %>%
  #   select(-.data$fg_make_prob)
  fg_prob <- as.numeric(mgcv::predict.bam(fg_model, newdata = pbp, type="response")) %>%
    as_tibble() %>%
    dplyr::rename(fg_make_prob = value)

  dat <- bind_cols(
    pbp,
    fg_prob
  ) %>%
    mutate(
      # don't recommend kicking when fg is over 60 yards
      fg_make_prob = ifelse(yards_to_goal > 42, 0, fg_make_prob),
      # hacky way to not have crazy high probs for long kicks
      # because the bot should be conservative about recommending kicks in this region
      # for 56 through 60 yards

      # note: if you're implementing this for your own team, provide your own estimates of your kicker's
      # true probs
      fg_make_prob = ifelse(yards_to_goal >= 35, .9 * fg_make_prob, fg_make_prob),

      fg_index = 1 : n()
    )

  make_df <- dat %>%
    flip_team() %>%
    # win prob after receiving kickoff for touchback and other team has 3 more points
    mutate(
      yards_to_goal = 75,
      pos_score_diff_start = pos_score_diff_start - 3
    ) %>%
    # for end of 1st half stuff
    flip_half() %>%
    prep_ep() %>%
    add_ep() %>%
    prep_wp() %>%
    add_wp() %>%

    mutate(
      wp = if_else(pos_team != original_pos_team, 1 - wp, wp)
    ) %>%
    end_game_fn() %>%
    select(fg_index, make_fg_wp = wp)

  miss_df <- dat %>%
    flip_team() %>%
    mutate(
      yards_to_goal = (100 - yards_to_goal) - 8,
      # yards_to_goal can't be bigger than 80 due to some weird nfl rule *Check rule for college?
      yards_to_goal = if_else(yards_to_goal > 80, 80, yards_to_goal),
      yards_to_goal = ifelse(yards_to_goal < 1, 1, yards_to_goal)
    ) %>%
    # for end of 1st half stuff
    flip_half() %>%
    prep_ep() %>%
    add_ep() %>%
    prep_wp() %>%
    add_wp() %>%
    mutate(
      wp = if_else(pos_team != original_pos_team, 1 - wp, wp)
    ) %>%
    end_game_fn() %>%
    select(fg_index, miss_fg_wp = wp)

  dat %>%
    left_join(make_df, by = "fg_index") %>%
    left_join(miss_df, by = "fg_index") %>%
    mutate(fg_wp = fg_make_prob * make_fg_wp + (1 - fg_make_prob) * miss_fg_wp) %>%
    select(-fg_index) %>%
    return()
}

get_go_wp <- function(pbp) {
  n_plays <- nrow(pbp)
  pbp <- pbp %>% mutate(go_index = 1 : n(),
                        pos_team_receives_2H_kickoff = as.numeric(pos_team_receives_2H_kickoff))

  # stuff in the go for it model
  data <- pbp %>%
    transmute(down = as.numeric(down),
              distance,
              yards_to_goal,
              posteam_spread = pos_team_spread,
              posteam_total = pos_team_total) %>%
    as.matrix()

  # get model output from situation
  preds_df <- stats::predict(
    fd_model,
    data
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(prob = "value") %>%
    dplyr::bind_cols(
      tibble::tibble(
        "gain" = rep_len(-10:65, length.out = n_plays * 76),
        "go_index" = rep(pbp$go_index, times = rep_len(76, length.out = n_plays))
      ) %>%
        dplyr::left_join(pbp, by = "go_index")
    ) %>%
    dplyr::mutate(
      # if predicted gain is more than possible, call it a TD
      gain = ifelse(gain > yards_to_goal, as.integer(yards_to_goal), as.integer(gain))
    ) %>%

    # this step is to combine all the TD probs into one (for gains longer than possible)
    group_by(go_index, gain) %>%
    mutate(prob = sum(prob)) %>%
    dplyr::slice(1) %>%

    # needed for the max() step later
    group_by(go_index) %>%

    # update situation based on play result
    mutate(
      yards_to_goal = yards_to_goal - gain,
      # for figuring out if it was a td later
      final_yardline = yards_to_goal,
      turnover = ifelse(gain < distance, 1, 0),
      down = 1,

      # flip a bunch of columns on turnover on downs where other team gets ball
      # # note: touchdowns are dealt with separately later
      yards_to_goal = ifelse(turnover == 1, 100 - yards_to_goal, yards_to_goal),

      pos_team_timeouts_rem_before = case_when(
        turnover == 1 & original_pos_team == home ~ away_timeouts_remaining,
        turnover == 1 & original_pos_team == away ~ home_timeouts_remaining,
        TRUE ~ pos_team_timeouts_rem_before
      ),

      def_pos_team_timeouts_rem_before = case_when(
        turnover == 1 & original_pos_team == home ~ home_timeouts_remaining,
        turnover == 1 & original_pos_team == away ~ away_timeouts_remaining,
        TRUE ~ def_pos_team_timeouts_rem_before
      ),

      # flip receive_2h_ko var if turnover
      pos_team_receives_2H_kickoff = case_when(
        period <= 2 & pos_team_receives_2H_kickoff == 0 & turnover == 1 ~ 1,
        period <= 2 & pos_team_receives_2H_kickoff == 1 & turnover == 1 ~ 0,
        TRUE ~ pos_team_receives_2H_kickoff
      ),

      # switch posteam if turnover
      pos_team = case_when(
        home == pos_team & turnover == 1 ~ away,
        away == pos_team & turnover == 1 ~ home,
        TRUE ~ pos_team
      ),
      def_pos_team = case_when(
        home == def_pos_team & turnover == 1 ~ away,
        away == def_pos_team & turnover == 1 ~ home,
        TRUE ~ def_pos_team
      ),

      # swap spread if turnover
      pos_team_spread = ifelse(turnover == 1, -pos_team_spread, pos_team_spread),

      # swap score diff if turnover on downs
      pos_score_diff_start = ifelse(turnover == 1, -pos_score_diff_start, pos_score_diff_start),


      touchdown = ifelse(yards_to_goal == 0, 1,0),
      # give 7 points for the TD play # CHANGE TO 6 WHEN 2-pt IMPLEMENTED
      pos_score_diff_start = ifelse(touchdown == 1, -pos_score_diff_start - 7, pos_score_diff_start),

      # change variables for TD
      yards_to_goal = ifelse(touchdown == 1, 75, yards_to_goal),
      pos_team_timeouts_rem_before = case_when(
        touchdown == 1 & original_pos_team == home ~ away_timeouts_remaining,
        touchdown == 1 & original_pos_team == away ~ home_timeouts_remaining,
        TRUE ~ pos_team_timeouts_rem_before
      ),

      def_pos_team_timeouts_rem_before = case_when(
        touchdown == 1 & original_pos_team == home ~ home_timeouts_remaining,
        touchdown == 1 & original_pos_team == away ~ away_timeouts_remaining,
        TRUE ~ def_pos_team_timeouts_rem_before
      ),

      # flip receive_2h_ko var if touchdown
      pos_team_receives_2H_kickoff = case_when(
        period <= 2 & pos_team_receives_2H_kickoff == 0 & touchdown == 1 ~ 1,
        period <= 2 & pos_team_receives_2H_kickoff == 1 & touchdown == 1 ~ 0,
        TRUE ~ pos_team_receives_2H_kickoff
      ),
      pos_team = case_when(
        home == pos_team & touchdown == 1 ~ away,
        away == pos_team & touchdown == 1 ~ home,
        TRUE ~ pos_team
      ),
      def_pos_team = case_when(
        home == def_pos_team & touchdown == 1 ~ away,
        away == def_pos_team & touchdown == 1 ~ home,
        TRUE ~ def_pos_team
      ),
      pos_team_spread = ifelse(touchdown == 1, -pos_team_spread, pos_team_spread),


      # run off 6 seconds
      TimeSecsRem = TimeSecsRem - 6,
      adj_TimeSecsRem = adj_TimeSecsRem - 6,

      # additional runoff after successful non-td conversion (entered from user input)
      #TimeSecsRem = ifelse(turnover == 0 & yards_to_goal > 0, TimeSecsRem - runoff, TimeSecsRem),
      #adj_TimeSecsRem = ifelse(turnover == 0 & yards_to_goal > 0, adj_TimeSecsRem - runoff, adj_TimeSecsRem),

      # after all that, make sure these aren't negative
      TimeSecsRem = max(TimeSecsRem, 0),
      adj_TimeSecsRem = max(adj_TimeSecsRem, 0),

      # if now goal to go for either team, use yardline for yards to go, otherwise it's 1st and 10
      distance = ifelse(yards_to_goal < 10, yards_to_goal, 10)

    ) %>%
    ungroup()

  ## THIS IS FOR WHEN 2-pt IS IMPLEMENTED
  # separate df of just the TDs to calculate WP after TD
  # this step is needed to deal with the option of 1pt or 2pt choice
  # if (nrow(preds_df %>% filter(yards_to_goal == 0)) > 0) {
  #   tds_df <- preds_df %>%
  #     filter(yards_to_goal == 0) %>%
  #     #get_2pt_wp() %>%
  #     select(go_index, yards_to_goal, wp_td)
  # } else {
  #   # avoids errors when one play is fed that doesn't have TD in range
  #   tds_df <- tibble::tibble(
  #     "go_index" = 0,
  #     "yardline_100" = 99999
  #   )
  # }


  # join TD WPs back to original df and use those WPs
  preds <- preds_df %>%
    #left_join(tds_df, by = c("go_index", "yardline_100")) %>%
    flip_half() %>%
    prep_ep() %>%
    add_ep() %>%
    prep_wp() %>%
    add_wp() %>%
    mutate(
      # flip WP for possession change (turnover)
      wp = if_else(pos_team != original_pos_team, 1 - wp, wp),

      # get the TD probs computed separately
      #wp = ifelse(yards_to_goal == 0, wp_td, wp),

      # fill in end of game situation when team can kneel out clock after successful non-td conversion
      wp = case_when(
        pos_score_diff_start > 0 & turnover == 0 & yards_to_goal > 0 & adj_TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 1,
        pos_score_diff_start > 0 & turnover == 0 & yards_to_goal > 0 & adj_TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 1,
        pos_score_diff_start > 0 & turnover == 0 & yards_to_goal > 0 & adj_TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 1,
        TRUE ~ wp
      ),
      # fill in end of game situation when other team can kneel out clock after failed attempt
      wp = case_when(
        pos_score_diff_start > 0 & turnover == 1 & adj_TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 0,
        pos_score_diff_start > 0 & turnover == 1 & adj_TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 0,
        pos_score_diff_start > 0 & turnover == 1 & adj_TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 0,
        TRUE ~ wp
      ),

      wt_wp = prob * wp
    )

  report <- preds %>%
    group_by(go_index, turnover) %>%
    mutate(
      fd_pct = sum(prob),
      new_prob = prob / fd_pct,
      wt_wp = new_prob * wp
    ) %>%
    summarize(
      pct = sum(prob),
      wp = sum(wt_wp)
    ) %>%
    pivot_wider(
      names_from = turnover, values_from = c("pct", "wp")
    ) %>%
    dplyr::rename(
      first_down_prob = pct_0,
      wp_fail = wp_1,
      wp_succeed = wp_0
    ) %>%
    ungroup() %>%
    dplyr::select(go_index, first_down_prob, wp_fail, wp_succeed)

  wp_go_df <- preds %>%
    group_by(go_index) %>%
    summarize(go_wp = sum(wt_wp)) %>%
    ungroup()

  pbp %>%
    left_join(report, by = "go_index") %>%
    left_join(wp_go_df, by = "go_index") %>%
    select(-go_index) %>%
    return()

}

