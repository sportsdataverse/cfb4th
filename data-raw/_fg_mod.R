fgs_data <- pbp %>%
  filter(str_detect(play_type,"Field Goal")) %>%
  transmute(yards_to_goal,result = ifelse(str_detect(play_type,"Good"),1,0))



fg_model <- mgcv::bam(result ~ s(yards_to_goal),
                      data = fgs_data, family = "binomial")
#saveRDS(fg_model,"fg_model.RDS")
usethis::use_data(fg_model, internal = TRUE, overwrite = FALSE)

# function to get WP for field goal attempt
get_fg_wp <- function(df) {

  # probability field goal is made
  fg_prob <- as.numeric(mgcv::predict.bam(fg_model, newdata = df, type="response"))

  # don't recommend kicking when fg is over 60 yards
  fg_prob <- if_else(df$yards_to_goal > 42, 0, fg_prob)

  # hacky way to not have crazy high probs for long kicks
  # because the bot should be conservative about recommending kicks in this region
  # for 56 through 60 yards
  fg_prob <- if_else(df$yards_to_goal >= 38 & df$yards_to_goal <= 42, fg_prob * .9, fg_prob)

  # win probability of kicking team if field goal is made
  probs <-
    df %>%
    flip_team() %>%
    # win prob after receiving kickoff for touchback and other team has 3 more points
    mutate(
      yards_to_goal = 75,
      score_differential = -(pos_score_diff_start + 3),
      pos_score_diff_start = -(pos_score_diff_start + 3),
      down = as.factor(down)
    ) #%>%
  probs$ep <- sum(predict(object = cfbscrapR:::ep_model,newdata = probs,type = "probs")*c(0,3,-3,2,-7,-2,7) )
  probs <- probs %>%
    mutate(ExpScoreDiff = pos_score_diff_start + ep,
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/TimeSecsRem,
           wp = NA)
  probs$wp <- predict(object = cfbscrapR:::wp_model,newdata = probs,type = "response")


    # for end of 1st half stuff
    #flip_half() %>%
  fg_make_wp<-1-probs %>%
    mutate(

      # fill in end of game situation when team can kneel out clock
      # discourages punting when the other team can end the game
      wp = case_when(
        score_differential > 0 & TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 1,
        score_differential > 0 & TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 1,
        score_differential > 0 & TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 1,
        TRUE ~ wp
      )

    ) %>%
    pull(wp)

  # win probability of kicking team if field goal is missed
  probs <-
    df %>%
    flip_team() %>%
    mutate(
      yards_to_goal = (100 - yards_to_goal) - 8,
      score_differential = -(pos_score_diff_start),
      pos_score_diff_start = -(pos_score_diff_start),
      # yards_to_goal can't be bigger than 80 due to some weird nfl rule
      yards_to_goal = if_else(yards_to_goal > 80, 80, yards_to_goal),
      down = as.factor(down)
    )

  probs$ep <- sum(predict(object = cfbscrapR:::ep_model,newdata = probs,type = "probs")*c(0,3,-3,2,-7,-2,7) )
  probs <- probs %>%
    mutate(ExpScoreDiff = pos_score_diff_start + ep,
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/TimeSecsRem,
           wp = NA)
  probs$wp <- predict(object = cfbscrapR:::wp_model,newdata = probs,type = "response")
    # for end of 1st half stuff
    #flip_half() %>%
  fg_miss_wp <- 1-probs %>%
    mutate(

      # fill in end of game situation when team can kneel out clock
      # discourages punting when the other team can end the game
      wp = case_when(
        score_differential > 0 & TimeSecsRem < 120 & def_pos_team_timeouts_rem_before == 0 ~ 1,
        score_differential > 0 & TimeSecsRem < 80 & def_pos_team_timeouts_rem_before == 1 ~ 1,
        score_differential > 0 & TimeSecsRem < 40 & def_pos_team_timeouts_rem_before == 2 ~ 1,
        TRUE ~ wp
      )

    ) %>%
    pull(wp)

  # for end of half situations
  # when team gets ball again after halftime
  # if (df %>% flip_team() %>% pull(half_seconds_remaining) == 0 & df$qtr == 2 &
  #     df %>% flip_team() %>% flip_half() %>% pull(posteam) == df$posteam) {
  #   # message("test")
  #   fg_make_wp <- 1 - fg_make_wp
  #   fg_miss_wp <- 1 - fg_miss_wp
  # }

  # FG win prob is weighted avg of make and miss WPs
  fg_wp <- fg_prob * fg_make_wp + (1 - fg_prob) * fg_miss_wp

  # bind up the probs to return for table
  results <- list(fg_wp, fg_prob, fg_miss_wp, fg_make_wp)

  return(results)
}

