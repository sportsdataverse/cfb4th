#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom stats predict
#' @importFrom nnet multinom
#' @importFrom xgboost getinfo


get_punt_wp <- function(pbp) {
  # to join later
  pbp <- pbp %>%
    dplyr::mutate(punt_index = 1:dplyr::n())
  # original_pos_team = pos_team,
  # home_timeouts_remaining = ifelse(pos_team == home, pos_team_timeouts_rem_before, def_pos_team_timeouts_rem_before),
  # away_timeouts_remaining = ifelse(pos_team == away, pos_team_timeouts_rem_before, def_pos_team_timeouts_rem_before))
  if ("yards_to_goal_end" %in% names(pbp)) {
    pbp <- pbp %>% dplyr::select(-"yards_to_goal_end")
  }

  # get wp associated with punt
  probs <- pbp %>%
    dplyr::left_join(punt_df, by = "yards_to_goal") %>%
    flip_team() %>%
    dplyr::mutate(
      yards_to_goal = 100 - .data$yards_to_goal_end,

      # deal with punt return TD (yards_to_goal_end == 100) or muff (muff == 1) #DOESNT HANDLE MUFFS YET
      # we want punting team to be receiving a kickoff so have to flip everything back
      pos_team = dplyr::case_when(
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$away ~ .data$away,
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$home ~ .data$home,
        TRUE ~ .data$pos_team
      ),
      def_pos_team = dplyr::case_when(
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$away ~ .data$home,
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$home ~ .data$away,
        TRUE ~ .data$def_pos_team
      ),

      pos_team_timeouts_remaining_before = dplyr::case_when(
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$home ~ .data$home_timeouts_remaining,
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$away ~ .data$away_timeouts_remaining,
        TRUE ~ .data$pos_team_timeouts_rem_before
      ),

      def_pos_team_timeouts_remaining_before = dplyr::case_when(
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$home ~ .data$away_timeouts_remaining,
        (.data$yards_to_goal_end == 100) & .data$original_pos_team == .data$away ~ .data$home_timeouts_remaining,
        TRUE ~ .data$def_pos_team_timeouts_rem_before
      ),

      # return TD and muff for yardline
      yards_to_goal = dplyr::if_else(.data$yards_to_goal_end == 100, as.integer(75), as.integer(.data$yards_to_goal)),
      #yards_to_goal = if_else(muff == 1, as.integer(100 - yards_to_goal), yards_to_goal),

      pos_score_diff_start = dplyr::if_else(.data$yards_to_goal_end == 100, as.integer(-.data$pos_score_diff_start - 7), as.integer(.data$pos_score_diff_start)),
      #score_differential = if_else(muff == 1, as.integer(-score_differential), as.integer(score_differential)),

      pos_team_receives_2H_kickoff = dplyr::case_when(
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 0 & (.data$yards_to_goal_end == 100 ) ~ 1,
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 1 & (.data$yards_to_goal_end == 100 ) ~ 0,
        TRUE ~ .data$pos_team_receives_2H_kickoff
      ),
      pos_team_spread = dplyr::if_else(.data$yards_to_goal_end == 100, -.data$pos_team_spread, .data$pos_team_spread),

      distance = dplyr::if_else(.data$yards_to_goal < 10, .data$yards_to_goal, as.integer(.data$distance))

    ) %>%
    flip_half()
  # to deal with a single play where you shouldn't punt
  if (sum(!is.na(probs$yards_to_goal)) == 0) {
    probs <- probs %>%
      dplyr::mutate(wp = NA_real_)
  } else {
    probs <- probs %>%
      prep_ep() %>%
      add_ep() %>%
      prep_wp() %>%
      add_wp() %>%
      dplyr::mutate(
        wp = dplyr::if_else(.data$pos_team != .data$original_pos_team, 1 - .data$wp, .data$wp)
      )
  }

  probs <- probs %>%
    end_game_fn() %>%
    dplyr::mutate(
      wt_wp = .data$pct * .data$wp
    ) %>%
    dplyr::group_by(.data$punt_index) %>%
    dplyr::summarize(punt_wp = sum(.data$wt_wp)) %>%
    dplyr::ungroup()

  pbp %>%
    dplyr::left_join(probs, by = "punt_index") %>%
    dplyr::select(-"punt_index") #%>% view()
}

get_fg_wp <- function(pbp) {
  # probability field goal is made
  # pbp <- pbp %>%
  #   select(-.data$fg_make_prob)
  fg_prob <- as.numeric(mgcv::predict.bam(fg_model, newdata = pbp, type = "response")) %>%
    dplyr::as_tibble() %>%
    dplyr::rename("fg_make_prob" = "value")

  dat <- dplyr::bind_cols(
    pbp,
    fg_prob
  ) %>%
    dplyr::mutate(
      # don't recommend kicking when fg is over 60 yards
      fg_make_prob = ifelse(.data$yards_to_goal > 42, 0, .data$fg_make_prob),
      # hacky way to not have crazy high probs for long kicks
      # because the bot should be conservative about recommending kicks in this region
      # for 56 through 60 yards

      # note: if you're implementing this for your own team, provide your own estimates of your kicker's
      # true probs
      fg_make_prob = ifelse(.data$yards_to_goal >= 35, .9 * .data$fg_make_prob, .data$fg_make_prob),

      fg_index = 1:dplyr::n()
    )

  make_df <- dat %>%
    flip_team() %>%
    # win prob after receiving kickoff for touchback and other team has 3 more points
    dplyr::mutate(
      yards_to_goal = 75,
      pos_score_diff_start = .data$pos_score_diff_start - 3
    ) %>%
    # for end of 1st half stuff
    flip_half() %>%
    prep_ep() %>%
    add_ep() %>%
    prep_wp() %>%
    add_wp() %>%

    dplyr::mutate(
      wp = dplyr::if_else(.data$pos_team != .data$original_pos_team, 1 - .data$wp, .data$wp)
    ) %>%
    end_game_fn() %>%
    dplyr::select("fg_index", "make_fg_wp" = "wp")

  miss_df <- dat %>%
    flip_team() %>%
    dplyr::mutate(
      yards_to_goal = (100 - .data$yards_to_goal),
      # yards_to_goal can't be bigger than 80 due to some weird nfl rule *Check rule for college?
      yards_to_goal = dplyr::if_else(.data$yards_to_goal > 80, 80, .data$yards_to_goal),
      yards_to_goal = ifelse(.data$yards_to_goal < 1, 1, .data$yards_to_goal)
    ) %>%
    # for end of 1st half stuff
    flip_half() %>%
    prep_ep() %>%
    add_ep() %>%
    prep_wp() %>%
    add_wp() %>%
    dplyr::mutate(
      wp = dplyr::if_else(.data$pos_team != .data$original_pos_team, 1 - .data$wp, .data$wp)
    ) %>%
    end_game_fn() %>%
    dplyr::select("fg_index", "miss_fg_wp" = "wp")

  dat %>%
    dplyr::left_join(make_df, by = "fg_index") %>%
    dplyr::left_join(miss_df, by = "fg_index") %>%
    dplyr::mutate(fg_wp = .data$fg_make_prob * .data$make_fg_wp + (1 - .data$fg_make_prob) * .data$miss_fg_wp) %>%
    dplyr::select(-"fg_index") %>%
    return()
}

get_go_wp <- function(pbp) {
  n_plays <- nrow(pbp)
  pbp <- pbp %>%
    dplyr::mutate(
      go_index = 1:dplyr::n(),
      pos_team_receives_2H_kickoff = as.numeric(.data$pos_team_receives_2H_kickoff))

  # stuff in the go for it model
  data <- pbp %>%
    dplyr::transmute(
      down = as.numeric(.data$down),
                     .data$distance,
                     .data$yards_to_goal,
              posteam_spread = .data$pos_team_spread,
              posteam_total = .data$pos_team_total) %>%
    as.matrix()

  # get model output from situation
  preds_df <- stats::predict(
    fd_model,
    data
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename("prob" = "value") %>%
    dplyr::bind_cols(
      tibble::tibble(
        "gain" = rep_len(-10:65, length.out = n_plays * 76),
        "go_index" = rep(pbp$go_index, times = rep_len(76, length.out = n_plays))
      ) %>%
        dplyr::left_join(pbp, by = "go_index")
    ) %>%
    dplyr::mutate(
      # if predicted gain is more than possible, call it a TD
      gain = ifelse(.data$gain > .data$yards_to_goal, as.integer(.data$yards_to_goal), as.integer(.data$gain)),
      # if predicted loss is more than possible, call it on the 1
      gain = ifelse(.data$yards_to_goal - .data$gain >= 100, as.integer(.data$yards_to_goal - 99), as.integer(.data$gain))
    ) %>%
    # this step is to combine all the TD probs into one (for gains longer than possible)
    dplyr::group_by(.data$go_index, .data$gain) %>%
    dplyr::mutate(prob = sum(.data$prob)) %>%
    dplyr::slice(1) %>%

    # needed for the max() step later
    dplyr::group_by(.data$go_index) %>%

    # update situation based on play result
    dplyr::mutate(
      yards_to_goal = .data$yards_to_goal - .data$gain,
      # for figuring out if it was a td later
      final_yardline = .data$yards_to_goal,
      turnover = ifelse(.data$gain < .data$distance, 1, 0),
      down = 1,

      # flip a bunch of columns on turnover on downs where other team gets ball
      # # note: touchdowns are dealt with separately later
      yards_to_goal = ifelse(.data$turnover == 1, 100 - .data$yards_to_goal, .data$yards_to_goal),

      pos_team_timeouts_rem_before = dplyr::case_when(
        .data$turnover == 1 & .data$original_pos_team == .data$home ~ .data$away_timeouts_remaining,
        .data$turnover == 1 & .data$original_pos_team == .data$away ~ .data$home_timeouts_remaining,
        TRUE ~ .data$pos_team_timeouts_rem_before
      ),

      def_pos_team_timeouts_rem_before = dplyr::case_when(
        .data$turnover == 1 & .data$original_pos_team == .data$home ~ .data$home_timeouts_remaining,
        .data$turnover == 1 & .data$original_pos_team == .data$away ~ .data$away_timeouts_remaining,
        TRUE ~ .data$def_pos_team_timeouts_rem_before
      ),

      # flip receive_2h_ko var if turnover
      pos_team_receives_2H_kickoff = dplyr::case_when(
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 0 & .data$turnover == 1 ~ 1,
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 1 & .data$turnover == 1 ~ 0,
        TRUE ~ .data$pos_team_receives_2H_kickoff
      ),

      # switch posteam if turnover
      pos_team = dplyr::case_when(
        .data$home == .data$pos_team & .data$turnover == 1 ~ .data$away,
        .data$away == .data$pos_team & .data$turnover == 1 ~ .data$home,
        TRUE ~ .data$pos_team
      ),
      def_pos_team = dplyr::case_when(
        .data$home == .data$def_pos_team & .data$turnover == 1 ~ .data$away,
        .data$away == .data$def_pos_team & .data$turnover == 1 ~ .data$home,
        TRUE ~ .data$def_pos_team
      ),

      # swap spread if turnover
      pos_team_spread = ifelse(.data$turnover == 1, -.data$pos_team_spread, .data$pos_team_spread),

      # swap score diff if turnover on downs
      pos_score_diff_start = ifelse(.data$turnover == 1, -.data$pos_score_diff_start, .data$pos_score_diff_start),


      touchdown = ifelse(.data$yards_to_goal == 0, 1,0),
      # give 6 points for the TD play
      pos_score_diff_start = ifelse(.data$touchdown == 1, -.data$pos_score_diff_start - 6, .data$pos_score_diff_start),

      # change variables for TD
      yards_to_goal = ifelse(.data$touchdown == 1, 75, .data$yards_to_goal),
      pos_team_timeouts_rem_before = dplyr::case_when(
        .data$touchdown == 1 & .data$original_pos_team == .data$home ~ .data$away_timeouts_remaining,
        .data$touchdown == 1 & .data$original_pos_team == .data$away ~ .data$home_timeouts_remaining,
        TRUE ~ .data$pos_team_timeouts_rem_before
      ),

      def_pos_team_timeouts_rem_before = dplyr::case_when(
        .data$touchdown == 1 & .data$original_pos_team == .data$home ~ .data$home_timeouts_remaining,
        .data$touchdown == 1 & .data$original_pos_team == .data$away ~ .data$away_timeouts_remaining,
        TRUE ~ .data$def_pos_team_timeouts_rem_before
      ),

      # flip receive_2h_ko var if touchdown
      pos_team_receives_2H_kickoff = dplyr::case_when(
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 0 & .data$touchdown == 1 ~ 1,
        .data$period <= 2 & .data$pos_team_receives_2H_kickoff == 1 & .data$touchdown == 1 ~ 0,
        TRUE ~ .data$pos_team_receives_2H_kickoff
      ),
      pos_team = dplyr::case_when(
        .data$home == .data$pos_team & .data$touchdown == 1 ~ .data$away,
        .data$away == .data$pos_team & .data$touchdown == 1 ~ .data$home,
        TRUE ~ .data$pos_team
      ),
      def_pos_team = dplyr::case_when(
        .data$home == .data$def_pos_team & .data$touchdown == 1 ~ .data$away,
        .data$away == .data$def_pos_team & .data$touchdown == 1 ~ .data$home,
        TRUE ~ .data$def_pos_team
      ),
      pos_team_spread = ifelse(.data$touchdown == 1, -.data$pos_team_spread, .data$pos_team_spread),


      # run off 6 seconds
      TimeSecsRem = .data$TimeSecsRem - 6,
      adj_TimeSecsRem = .data$adj_TimeSecsRem - 6,

      # additional runoff after successful non-td conversion (entered from user input)
      #TimeSecsRem = ifelse(turnover == 0 & yards_to_goal > 0, TimeSecsRem - runoff, TimeSecsRem),
      #adj_TimeSecsRem = ifelse(turnover == 0 & yards_to_goal > 0, adj_TimeSecsRem - runoff, adj_TimeSecsRem),

      # after all that, make sure these aren't negative
      TimeSecsRem = max(.data$TimeSecsRem, 0),
      adj_TimeSecsRem = max(.data$adj_TimeSecsRem, 0),

      # if now goal to go for either team, use yardline for yards to go, otherwise it's 1st and 10
      distance = ifelse(.data$yards_to_goal < 10, .data$yards_to_goal, 10)

    ) %>%
    ungroup()

  ## THIS IS FOR WHEN 2-pt IS IMPLEMENTED
  # separate df of just the TDs to calculate WP after TD
  # this step is needed to deal with the option of 1pt or 2pt choice
  if (nrow(preds_df %>% dplyr::filter(.data$touchdown == 1)) > 0) {
    tds_df <- preds_df %>%
      dplyr::filter(.data$touchdown == 1) %>%
      get_2pt_wp() %>%
      dplyr::select("go_index", "yards_to_goal", "wp_td")
  } else {
    # avoids errors when one play is fed that doesn't have TD in range
    tds_df <- tibble::tibble(
      "go_index" = 0,
      "yards_to_goal" = 99999
    )
  }


  # join TD WPs back to original df and use those WPs
  preds <- preds_df %>%
    dplyr::left_join(tds_df, by = c("go_index", "yards_to_goal")) %>%
    flip_half() %>%
    prep_ep() %>%
    add_ep() %>%
    prep_wp() %>%
    add_wp() %>%
    dplyr::mutate(
      # flip WP for possession change (turnover)
      wp = dplyr::if_else(.data$pos_team != .data$original_pos_team, 1 - .data$wp, .data$wp),

      # get the TD probs computed separately
      wp = ifelse(.data$touchdown == 1, .data$wp_td, .data$wp),

      # fill in end of game situation when team can kneel out clock after successful non-td conversion
      wp = dplyr::case_when(
        .data$pos_score_diff_start > 0 & .data$turnover == 0 & .data$yards_to_goal > 0 & .data$adj_TimeSecsRem < 120 & .data$def_pos_team_timeouts_rem_before == 0 ~ 1,
        .data$pos_score_diff_start > 0 & .data$turnover == 0 & .data$yards_to_goal > 0 & .data$adj_TimeSecsRem < 80 & .data$def_pos_team_timeouts_rem_before == 1 ~ 1,
        .data$pos_score_diff_start > 0 & .data$turnover == 0 & .data$yards_to_goal > 0 & .data$adj_TimeSecsRem < 40 & .data$def_pos_team_timeouts_rem_before == 2 ~ 1,
        TRUE ~ .data$wp
      ),
      # fill in end of game situation when other team can kneel out clock after failed attempt
      wp = dplyr::case_when(
        .data$pos_score_diff_start > 0 & .data$turnover == 1 & .data$adj_TimeSecsRem < 120 & .data$def_pos_team_timeouts_rem_before == 0 ~ 0,
        .data$pos_score_diff_start > 0 & .data$turnover == 1 & .data$adj_TimeSecsRem < 80 & .data$def_pos_team_timeouts_rem_before == 1 ~ 0,
        .data$pos_score_diff_start > 0 & .data$turnover == 1 & .data$adj_TimeSecsRem < 40 & .data$def_pos_team_timeouts_rem_before == 2 ~ 0,
        TRUE ~ .data$wp
      ),

      wt_wp = .data$prob * .data$wp
    )

  report <- preds %>%
    dplyr::group_by(.data$go_index, .data$turnover) %>%
    dplyr::mutate(
      fd_pct = sum(.data$prob),
      new_prob = .data$prob / .data$fd_pct,
      wt_wp = .data$new_prob * .data$wp
    ) %>%
    dplyr::summarize(
      pct = sum(.data$prob),
      wp = sum(.data$wt_wp)
    ) %>%
    tidyr::pivot_wider(
      names_from = "turnover", values_from = c("pct", "wp")
    ) %>%
    dplyr::rename(
      "first_down_prob" = "pct_0",
      "wp_fail" = "wp_1",
      "wp_succeed" = "wp_0"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("go_index", "first_down_prob", "wp_fail", "wp_succeed")

  wp_go_df <- preds %>%
    dplyr::group_by(.data$go_index) %>%
    dplyr::summarize(go_wp = sum(.data$wt_wp)) %>%
    dplyr::ungroup()

  pbp %>%
    dplyr::left_join(report, by = "go_index") %>%
    dplyr::left_join(wp_go_df, by = "go_index") %>%
    dplyr::select(-"go_index") %>%
    return()

}

get_2pt_wp <- function(pbp) {
  pbp <- pbp %>%
    dplyr::mutate(index_2pt = 1:dplyr::n())

  # stuff in the 2pt model
  # data <- pbp %>%
  #   transmute(down = as.numeric(down),
  #             distance,
  #             yards_to_goal,
  #             posteam_spread = pos_team_spread,
  #             posteam_total = pos_team_total) %>%
  #   as.matrix()

  # get probability of converting 2pt attempt from model
  prob_2pt <- tibble::tibble(prob_2pt = .45)
  #   stats::predict(
  #   two_pt_model,
  #   data
  # )  %>%
  #   tibble::as_tibble() %>%
  #   dplyr::rename(prob_2pt = "value") %>%
  #   select(prob_2pt)

  # probability of making PAT
  xp_prob <- as.numeric(mgcv::predict.bam(fg_model, newdata = pbp %>%
                                            dplyr::mutate(yards_to_goal = 2), type = "response")) %>%
    dplyr::as_tibble() %>%
    dplyr::rename("prob_1pt" = "value") %>%
    dplyr::select("prob_1pt")

  pbp <- dplyr::bind_cols(
    pbp, prob_2pt, xp_prob
  )

  probs <- dplyr::bind_rows(
    pbp %>% dplyr::mutate(pts = 0, pos_score_diff_start = .data$pos_score_diff_start),
    pbp %>% dplyr::mutate(pts = 1, pos_score_diff_start = .data$pos_score_diff_start - 1),
    pbp %>% dplyr::mutate(pts = 2, pos_score_diff_start = .data$pos_score_diff_start - 2)
  ) %>%
    #This switch was all handled in get_go_wp()
    # mutate(
    #   # switch posteam, timeouts and kickoff indicator
    #   posteam = case_when(
    #     home == pos_team ~ away,
    #     away == pos_team ~ home
    #   ),
    #
    #   pos_team_timeouts_rem_before = ifelse(original_posteam == home, away_timeouts_remaining, home_timeouts_remaining),
    #   def_pos_team_timeouts_rem_before = ifelse(original_posteam == home, home_timeouts_remaining, away_timeouts_remaining),
    #
  #   receive_2h_ko = case_when(
  #     qtr <= 2 & receive_2h_ko == 0 ~ 1,
  #     qtr <= 2 & receive_2h_ko == 1 ~ 0,
  #     TRUE ~ receive_2h_ko
  #   ),
  #
  #   # 1st & 10 after touchback
  #   yardline_100 = 75,
  #   down = 1,
  #   ydstogo = 10
  #
  # ) %>%
  flip_half() %>%
    prep_ep() %>%
    add_ep() %>%
    prep_wp() %>%
    add_wp() %>%
    dplyr::mutate(wp = dplyr::if_else(.data$pos_team != .data$original_pos_team, 1 - .data$wp, .data$wp)) %>%
    dplyr::arrange(.data$index_2pt, .data$pts) %>%
    dplyr::select("index_2pt", "pts", "wp", "prob_2pt", "prob_1pt") %>%
    dplyr::group_by(.data$index_2pt) %>%
    dplyr::summarize(
      wp_0 = dplyr::first(.data$wp),
      wp_1 = dplyr::nth(.data$wp, 2),
      wp_2 = dplyr::last(.data$wp),
      conv_2pt = dplyr::first(.data$prob_2pt),
      conv_1pt = dplyr::first(.data$prob_1pt)
    ) %>%
    dplyr::mutate(
      wp_go2 = .data$conv_2pt * .data$wp_2 + (1 - .data$conv_2pt) * .data$wp_0,
      wp_go1 = .data$conv_1pt * .data$wp_1 + (1 - .data$conv_1pt) * .data$wp_0,

      # convenience column useful for other things
      wp_td = ifelse(.data$wp_go1 > .data$wp_go2, .data$wp_go1, .data$wp_go2)
    ) %>%
    dplyr::ungroup()

  pbp %>%
    dplyr::left_join(probs, by = "index_2pt") %>%
    dplyr::select(-"index_2pt") %>%
    return()
}
