################################################################################
# Author: Ben Baldwin, Sebastian Carl, Jared Lee
# Purpose: Top-Level function made available through the package
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get 4th down decision probabilities
#'
#' @description Get various probabilities associated with each option on 4th downs (go
#' for it, kick field goal, punt).
#'
#' @param df A data frame of decisions to be computed for.
#' @return Original data frame Data frame plus the following columns added:
#' \describe{
#' \item{go_boost}{Gain (or loss) in win prob associated with choosing to go for it (percentage points).}
#' \item{first_down_prob}{Probability of earning a first down if going for it on 4th down.}
#' \item{wp_fail}{Win probability in the event of a failed 4th down attempt.}
#' \item{wp_succeed}{Win probability in the event of a successful 4th down attempt.}
#' \item{go_wp}{Average win probability when going for it on 4th down.}
#' \item{fg_make_prob}{Probability of making field goal.}
#' \item{miss_fg_wp}{Win probability in the event of a missed field goal.}
#' \item{make_fg_wp}{Win probability in the event of a made field goal.}
#' \item{fg_wp}{Average win probability when attempting field goal.}
#' \item{punt_wp}{Average win probability when punting.}
#' }
#' @export
#' @examples
#' \donttest{
#'  play <-
#'   tibble::tibble(
#'     # Game Info
#'     home = "Utah",
#'     away = "BYU",
#'     pos_team = "Utah",
#'     def_pos_team = "BYU",
#'     spread = -7,
#'     over_under = 55,
#'
#'     # Situation Info
#'     half = 2,
#'     period = 3, # Quarter
#'     TimeSecsRem = 900, # Half Seconds Remaining
#'     adj_TimeSecsRem = 900, # Game Seconds Remaining
#'     down = 4,
#'     distance = 4,
#'     yards_to_goal = 40,
#'     pos_score_diff_start = 7,
#'
#'     pos_team_receives_2H_kickoff = 1,
#'     pos_team_timeouts_rem_before = 3,
#'     def_pos_team_timeouts_rem_before = 3
#'
#'   )
#'
#'  cfb4th::add_4th_probs(play)
#'
#' }

add_4th_probs <- function(df) {

  original_df <- df %>%
    dplyr::mutate(index = 1:dplyr::n())

  modified_df <- original_df
  if("fg_make_prob" %in% names(original_df)) {
    original_df <- original_df %>%
      dplyr::select(-.data$fg_make_prob)
  }

  if (!"type" %in% names(df)) {
    # message("type not found. Assuming an cfbfastR df and doing necessary cleaning . . .")
    modified_df <- original_df %>%
      dplyr::filter(.data$down == 4) %>%
      prepare_cfbfastr_data()
  }

  # message("Performing final preparation . . .")
  df <- modified_df %>%
    prepare_df()

  if (!"runoff" %in% names(df)) {
    df$runoff <- 0L
  }

  message(glue::glue("Computing probabilities for {nrow(df)} plays. . ."))
  df <- df %>%
    add_probs() %>%
    dplyr::mutate(play_no = 1 : dplyr::n()) %>%
    dplyr::group_by(.data$play_no) %>%
    dplyr::mutate(
      punt_prob = dplyr::if_else(is.na(.data$punt_wp), 0, .data$punt_wp),
      max_non_go = max(.data$fg_wp, .data$punt_prob, na.rm = T),
      go_boost = 100 * (.data$go_wp - .data$max_non_go)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$index, .data$go_boost,
      .data$first_down_prob, .data$wp_fail,
      .data$wp_succeed, .data$go_wp,
      .data$fg_make_prob, .data$miss_fg_wp,
      .data$make_fg_wp, .data$fg_wp,
      .data$punt_wp
    )

  original_df %>%
    dplyr::left_join(df, by = c("index")) %>%
    dplyr::select(-.data$index) %>%
    return()

}

#' Load calculated 4th down probabilities from `cfbfastR` data
#'
#' @description Load calculated 4th down probabilities from `cfbfastR` data.
#'
#' @param seasons Seasons to load. Must be 2014 and later.
#' @return `cfbfastR` data on 4th downs with the `add_4th_probs()` columns added and also the following:
#' \describe{
#' \item{go}{100 if a team went for it on 4th down, 0 otherwise. It's 100 and 0 as a convenience for obtaining percent of times going for it.}
#' }
#' @export
#' @examples
#' \donttest{
#'   cfb4th::load_4th_pbp(2020)
#' }

load_4th_pbp <- function(seasons) {

  if (min(seasons) < 2014) {
    stop("Season before 2014 supplied. Please try again with nothing before 2014.")
  }

  # this is less likely to result in crashes due to memory
  purrr::map_df(seasons, ~{
    message(glue::glue("Loading season {.x}"))
    suppressMessages({bets <- cfbfastR::cfbd_betting_lines(year = .x) %>%
      dplyr::bind_rows(cfbfastR::cfbd_betting_lines(year = .x, season_type = "postseason")) %>%
      dplyr::mutate(
        provider = factor(.data$provider,
                               c(
                                 "consensus",
                                 "teamrankings",
                                 "numberfire",
                                 "Caesars",
                                 "Caesars (Pennsylvania)",
                                 "William Hill (New Jersey)",
                                 "SugarHouse",
                                 "Bovada"
                               )),
             spread = as.numeric(.data$spread),
             over_under = as.numeric(.data$over_under)
             ) %>%
      dplyr::group_by(.data$game_id) %>%
      dplyr::arrange(.data$provider) %>%
      dplyr::slice(1) %>%
      dplyr::select(.data$game_id,.data$spread,.data$over_under)
      }
    )
    cfbfastR::load_cfb_pbp(.x) %>%
      dplyr::left_join(bets, by = "game_id") %>%
      cfb4th::add_4th_probs() %>%
      return()
  }) %>%
    dplyr::mutate(
      # choice <- dplyr::case_when(
      #   # football to punt
      #   fullInput$play_type %in% c("Blocked Punt", "Punt","Safety",
      #                              "Blocked Punt Touchdown","Punt Return Touchdown") ~ "Punt",
      #   # field goal
      #   fullInput$play_type %in% c("Field Goal Good", "Field Goal Missed","Blocked Field Goal") ~ "Field goal attempt",
      #   # go for it
      #   fullInput$play_type %in% c("Pass Incompletion", "Pass Reception", "Passing Touchdown",
      #                              "Rush", "Rushing Touchdown", "Sack","Interception",
      #                              "Fumble Recovery (Opponent)","Pass Interception Return",
      #                              "Fumble Return Touchdown") ~ "Go for it",
      #   # penalty
      #   fullInput$play_type %in% c("Penalty") ~ "Penalty",
      #   TRUE ~ ""
      # ),
      go = ifelse(

        (.data$rush == 1 |.data$pass == 1),# & !play_type_nfl %in% c("PUNT", "FIELD_GOAL"),
        100, 0
      ),
      # Penalties and Timeouts are NA
      go = ifelse(
        .data$play_type %in% c("Timeout","Penalty"),
        NA_integer_, .data$go
      )
      # if it's an aborted snap in punt formation, call it a punt
      # go = ifelse(
      #   aborted_play == 1 & stringr::str_detect(desc, "Punt formation"),
      #   0, go
      # )
    ) %>%
    return()

}

