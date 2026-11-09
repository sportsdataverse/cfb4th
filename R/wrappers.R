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
#' @return The input data frame with the following columns added:
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
#'  # the fourth-down and win-probability models are downloaded the first time
#'  # they are needed; try() keeps the example from failing without network
#'  try(cfb4th::add_4th_probs(play))
#'
#' }

add_4th_probs <- function(df) {

  original_df <- df %>%
    dplyr::mutate(index = seq_len(dplyr::n()))

  modified_df <- original_df
  if ("fg_make_prob" %in% names(original_df)) {
    original_df <- original_df %>%
      dplyr::select(-"fg_make_prob")
  }

  if (nrow(original_df) > 0 && !"type" %in% names(df)) {
    # message("type not found. Assuming an cfbfastR df and doing necessary cleaning . . .")
    modified_df <- original_df %>%
      dplyr::filter(.data$down == 4) %>%
      prepare_cfbfastr_data()
  }

  # nothing to score: return the input with the probability columns as NA
  # instead of failing inside a model call
  if (nrow(modified_df) == 0) {
    if (nrow(original_df) > 0) {
      cli::cli_warn(c(
        "None of the {nrow(original_df)} play{?s} can be scored.",
        "i" = "Scored plays are 4th downs in regulation, with more than 30 seconds left, a {.field spread}, a {.field pos_team} and {.field yards_to_goal} > 0."
      ))
    }
    original_df[prob_cols] <- NA_real_
    return(dplyr::select(original_df, -"index"))
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
    dplyr::mutate(play_no = seq_len(dplyr::n())) %>%
    dplyr::group_by(.data$play_no) %>%
    dplyr::mutate(
      punt_prob = dplyr::if_else(is.na(.data$punt_wp), 0, .data$punt_wp),
      max_non_go = max(.data$fg_wp, .data$punt_prob, na.rm = T),
      go_boost = 100 * (.data$go_wp - .data$max_non_go)) %>%
    dplyr::ungroup() %>%
    dplyr::select("index", dplyr::all_of(prob_cols))

  original_df %>%
    dplyr::left_join(df, by = c("index")) %>%
    dplyr::select(-"index") %>%
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
#' @examplesIf interactive() && cfbfastR::has_cfbd_key()
#' # interactive only: one season is the smallest call, and scoring its ~10,000
#' # fourth downs (mostly the field-goal model) takes about a minute of CPU.
#' # The betting lines come from the CFBD API, which needs a CFBD_API_KEY.
#' try(cfb4th::load_4th_pbp(2020))

load_4th_pbp <- function(seasons) {

  if (min(seasons) < 2014) {
    stop("Season before 2014 supplied. Please try again with nothing before 2014.")
  }

  # this is less likely to result in crashes due to memory
  purrr::map_df(seasons, ~{
    season <- .x
    message(glue::glue("Loading season {season}"))
    bets <- season_betting_lines(season)
    pbp <- cfbfastR::load_cfb_pbp(season)
    if (NROW(pbp) == 0) {
      cli::cli_abort(c(
        "No {.pkg cfbfastR} play-by-play was found for season {season}.",
        "i" = "Check your network connection, or whether the season is published yet."
      ))
    }
    # cfbfastR >= 3 pbp already carries a line (spread, over_under, provider,
    # formatted_spread). The provider-ranked line replaces it, so the
    # metadata describes the line used, and the pbp's own spread/over_under
    # fill in where that line has none.
    pbp %>%
      dplyr::rename(dplyr::any_of(c(pbp_spread = "spread", pbp_over_under = "over_under"))) %>%
      dplyr::select(-dplyr::any_of(c("provider", "formatted_spread"))) %>%
      dplyr::left_join(bets, by = "game_id") %>%
      fill_line("spread") %>%
      fill_line("over_under") %>%
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

        (.data$rush == 1 | .data$pass == 1),# & !play_type_nfl %in% c("PUNT", "FIELD_GOAL"),
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


# columns add_4th_probs() adds
prob_cols <- c(
  "go_boost", "first_down_prob", "wp_fail", "wp_succeed", "go_wp",
  "fg_make_prob", "miss_fg_wp", "make_fg_wp", "fg_wp", "punt_wp"
)

# betting-line providers, most preferred first; unlisted providers rank last
provider_rank <- c(
  "consensus", "teamrankings", "numberfire", "Caesars",
  "Caesars (Pennsylvania)", "William Hill (New Jersey)", "SugarHouse", "Bovada"
)

# One CFBD betting line per game for a season: a provider with a spread
# first, then by provider rank. cfbfastR::cfbd_betting_lines() returns an
# empty data.frame() with no columns on any API error (an HTTP 429 included)
# and its message is hidden here, so an empty response warns and returns no
# lines, and the pbp's own line is used. A response without `provider` is
# ranked on spread alone, with `provider` NA.
season_betting_lines <- function(season) {
  bets <- suppressMessages(dplyr::bind_rows(
    cfbfastR::cfbd_betting_lines(year = season),
    cfbfastR::cfbd_betting_lines(year = season, season_type = "postseason")
  ))
  if (NROW(bets) == 0 || !"game_id" %in% names(bets)) {
    cli::cli_warn(c(
      "CFBD returned no betting lines for {season}, so the play-by-play's own {.field spread} and {.field over_under} are used.",
      "i" = "The CFBD API may be rate-limiting (HTTP 429); try again later."
    ))
    return(tibble::tibble(
      game_id = integer(), provider = character(),
      spread = numeric(), over_under = numeric()
    ))
  }
  if (!"provider" %in% names(bets)) {
    cli::cli_warn(c(
      "CFBD's betting lines for {season} have no {.field provider} column.",
      "i" = "Each game's first line with a spread is used, and {.field provider} is {.val NA}."
    ))
    bets$provider <- NA_character_
  }
  for (col in c("spread", "over_under")) {
    if (!col %in% names(bets)) bets[[col]] <- NA_real_
  }
  bets %>%
    dplyr::mutate(
      game_id = as.integer(.data$game_id),
      spread = suppressWarnings(as.numeric(.data$spread)),
      over_under = suppressWarnings(as.numeric(.data$over_under))
    ) %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::arrange(
      is.na(.data$spread), factor(.data$provider, provider_rank),
      .by_group = TRUE
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(provider = as.character(.data$provider)) %>%
    dplyr::select(
      "game_id", "provider", "spread", "over_under",
      dplyr::any_of("formatted_spread")
    )
}

# `col` from the joined betting line, falling back to the pbp's own value
# (`pbp_<col>`) where the line has none; drops `pbp_<col>`.
fill_line <- function(df, col) {
  pbp_col <- paste0("pbp_", col)
  if (!col %in% names(df)) df[[col]] <- NA_real_
  if (pbp_col %in% names(df)) {
    df[[col]] <- dplyr::coalesce(df[[col]], as.numeric(df[[pbp_col]]))
    df[[pbp_col]] <- NULL
  }
  df
}
