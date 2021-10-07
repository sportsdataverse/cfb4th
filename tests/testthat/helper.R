play <-
  tibble::tibble(
    # Game Info
    home = "Utah",
    away = "BYU",
    pos_team = "Utah",
    def_pos_team = "BYU",
    spread = -7,
    over_under = 55,

    # Situation Info
    half = 2,
    period = 3, # Quarter
    TimeSecsRem = 900, # Half Seconds Remaining
    adj_TimeSecsRem = 900, # Game Seconds Remaining
    down = 4,
    distance = 4,
    yards_to_goal = 40,
    pos_score_diff_start = 7,

    pos_team_receives_2H_kickoff = 1,
    pos_team_timeouts_rem_before = 3,
    def_pos_team_timeouts_rem_before = 3

  )
