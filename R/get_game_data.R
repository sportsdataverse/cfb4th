
################################################################################
# Author: Ben Baldwin, Sebastian Carl, Jared Lee
# Purpose: Function that can pull 4th downs from a live game courtesy of ESPN
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get 4th down plays from a game
#'
#' @description Get 4th down plays from a game.
#'
#' @param df A data frame of a game to get 4th down decisions of.
#' @details Obtains a data frame that can be used with `add_4th_probs()`. The following columns
#' must be present:
#' \itemize{
#' \item{game_id : ESPN game ID from ESPN or cfbfastR (eg '401114223')}
#' \item{home_team : Name of the home team}
#' \item{away_team : Name of the away team}
#' }
#' @return Original data frame Data frame plus the following columns added:
#' \describe{
#' \item{desc}{Play description from ESPN.}
#' \item{type_text}{Play type text from ESPN.}
#' \item{index}{Index number of play from a given game. Useful for tracking plays (e.g. for 4th down bot).}
#' \item{The rest}{All the columns needed for `add_4th_probs().`}
#' }
#' @export
#' @examples
#' \donttest{
#'
#' game <- cfbfastR::cfbd_game_info(2019, team = "Utah", week = 3)
#' plays <- cfb4th::get_4th_plays(game)
#'
#' dplyr::glimpse(plays)
#' }

get_4th_plays <- function(df) {

  espn_game_id <- df$game_id
  home <- df$home_team
  away <- df$away_team
  home_team <- df$home_team
  away_team <- df$away_team
  if (is.null(df$week)) {
    week <- NA
  } else {
    week <- df$week
  }


  plays <- data.frame()

  tryCatch(
    expr = {

      warn <- 0
      pbp <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event={espn_game_id}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      if ("code" %in% names(pbp)) {
        warning(warn <- 1)
      }

      # get plays out of the drives lists
      # i think current drive duplicates a drive in previous drive so might be ok to cut this
      if ("current" %in% names(pbp$drives) & "previous" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        current_drive <- current_drive[['plays']] %>%
          dplyr::bind_rows() %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(team.abbreviation = current_drive$team$abbreviation)

        previous_drives <- pbp$drives$previous

        drives <- dplyr::bind_rows(
          previous_drives %>%
            dplyr::select("team.abbreviation", "plays") %>%
            tidyr::unnest("plays"),
          current_drive
        )
      } else if ("current" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        drives <- current_drive[['plays']] %>%
          dplyr::bind_rows() %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(team.abbreviation = current_drive$team$abbreviation)
      } else {
        previous_drives <- pbp$drives$previous
        drives <- previous_drives %>%
          dplyr::select("team.abbreviation", "plays") %>%
          tidyr::unnest("plays")
      }

      line <- pbp$pickcenter %>%
        dplyr::mutate(provider = factor(.data$provider.name,
                                        c(
                                          "consensus",
                                          "teamrankings",
                                          "numberfire",
                                          "Caesars",
                                          "Caesars (Pennsylvania)",
                                          "William Hill (New Jersey)",
                                          "SugarHouse",
                                          "Bovada"
                                        ))) %>%
        dplyr::arrange(.data$provider) %>%
        slice(1)

      suppressWarnings(

        plays <- drives %>%
          tibble::as_tibble() %>%
          dplyr::group_by(.data$id) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          janitor::clean_names() %>%
          dplyr::rename(
            "abbreviation" = "team_abbreviation",
            "qtr" = "period_number",
            "yardline_100" = "start_yards_to_endzone",
            "yardline" = "start_possession_text",
            "down" = "start_down",
            "ydstogo" = "start_distance",
            "desc" = "text",
            "time" = "clock_display_value"
          ) %>%
          # dplyr::left_join(team_info %>%
          #                    select(abbreviation,pos_team = school) %>%
          #                    mutate(abbreviation = case_when(pos_team == "Louisiana" ~ "UL",
          #                                                    pos_team == "Wisconsin" ~ "WISC",
          #                                                    pos_team == "Oklahoma" ~ "OU",
          #                                                    pos_team == "Indiana" ~ "IU",
          #                                                    pos_team == "Charlotte" ~ "CLT",
          #                                                    pos_team == "UMass" ~ "MASS",
          #                                                    pos_team == "Northwestern" ~ "NU",
          #                                                    pos_team == "Miami" ~ "MIA",
          #                                                    TRUE ~ abbreviation)),
        #                  by = "abbreviation") %>%
        dplyr::filter(.data$qtr <= 4) %>%
          dplyr::mutate(
            # time column is wacky so extract it from play description when possible
            play_time = stringr::str_extract(.data$desc, "\\([^()]+(?=\\)\\s)"),
            play_time = substr(.data$play_time, 2, nchar(.data$play_time)),
            play_min = stringr::str_extract(.data$play_time, "[^()]+(?=\\:)") %>% as.integer(),
            play_min = if_else(is.na(.data$play_min) & !is.na(.data$play_time), as.integer(0), .data$play_min),
            play_sec = substr(.data$play_time, nchar(.data$play_time) - 1, nchar(.data$play_time)) %>% as.integer(),
            mins = if_else(nchar(.data$time) == 5, substr(.data$time, 1, 2), substr(.data$time, 1, 1)) %>% as.integer(),
            secs = if_else(nchar(.data$time) == 5, substr(.data$time, 4, 5), substr(.data$time, 3, 4)) %>% as.integer(),
            mins = if_else(is.na(.data$play_min), .data$mins, .data$play_min),
            secs = if_else(is.na(.data$play_sec), .data$secs, .data$play_sec)
          ) %>%
          dplyr::arrange(.data$qtr, dplyr::desc(.data$mins), dplyr::desc(.data$secs), .data$id) %>%
          dplyr::mutate(
            home_team = home,
            away_team = away,
            home_team_abbrv = pbp$header$competitions$competitors[[1]]$team.abbreviation[[1]],
            home_team_nick = pbp$header$competitions$competitors[[1]]$team.name[[1]],
            home_team_alt = pbp$header$competitions$competitors[[1]]$team.nickname[[1]],
            away_team_abbrv = pbp$header$competitions$competitors[[1]]$team.abbreviation[[2]],
            away_team_nick = pbp$header$competitions$competitors[[1]]$team.name[[2]],
            away_team_alt = pbp$header$competitions$competitors[[1]]$team.nickname[[2]],

            pos_team = dplyr::if_else(.data$abbreviation == .data$home_team_abbrv, .data$home_team, .data$away_team),
            defteam = dplyr::if_else(.data$pos_team == .data$home_team, .data$away_team, .data$home_team),
            half = dplyr::if_else(.data$qtr <= 2, 1, 2),
            challenge_team = stringr::str_extract(.data$desc, "[:alpha:]*\\s*[:alpha:]*\\s*[:alpha:]*[:alpha:]+(?=\\schallenged)"),
            challenge_team = stringr::str_replace_all(.data$challenge_team, "[\r\n]" , ""),
            challenge_team = stringr::str_trim(.data$challenge_team, side = c("both")),
            desc_timeout = dplyr::if_else(stringr::str_detect(.data$desc, "Timeout "), 1, 0),
            timeout_team = stringr::str_extract(.data$desc, "(?<=Timeout ).{2,20}(?=, )"),

            home_timeout_used = dplyr::case_when(
              .data$timeout_team == toupper(.data$home_team) ~ 1,
              .data$timeout_team == toupper(.data$home_team_abbrv) ~ 1,
              .data$timeout_team == toupper(.data$home_team_nick) ~ 1,
              .data$timeout_team == toupper(.data$home_team_alt) ~ 1,
              .data$timeout_team != .data$home_team ~ 0,
              is.na(.data$timeout_team) ~ 0
            ),
            away_timeout_used = dplyr::case_when(
              .data$timeout_team == toupper(.data$away_team) ~ 1,
              .data$timeout_team == toupper(.data$away_team_abbrv) ~ 1,
              .data$timeout_team == toupper(.data$away_team_nick) ~ 1,
              .data$timeout_team == toupper(.data$away_team_alt) ~ 1,
              .data$timeout_team != .data$away_team ~ 0,
              is.na(.data$timeout_team) ~ 0
            ),
            home_timeouts_remaining = 3,
            away_timeouts_remaining = 3
          ) %>%
          dplyr::group_by(.data$half) %>%
          dplyr::arrange(.data$qtr, dplyr::desc(.data$mins), dplyr::desc(.data$secs), .data$id) %>%
          dplyr::mutate(
            total_home_timeouts_used = dplyr::if_else(cumsum(.data$home_timeout_used) > 3, 3, cumsum(.data$home_timeout_used)),
            total_away_timeouts_used = dplyr::if_else(cumsum(.data$away_timeout_used) > 3, 3, cumsum(.data$away_timeout_used))
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            home_timeouts_remaining = .data$home_timeouts_remaining - .data$total_home_timeouts_used,
            away_timeouts_remaining = .data$away_timeouts_remaining - .data$total_away_timeouts_used,
            pos_team_timeouts_remaining = dplyr::if_else(
              .data$pos_team == .data$home_team,
              .data$home_timeouts_remaining,
              .data$away_timeouts_remaining
            ),
            defteam_timeouts_remaining = dplyr::if_else(
              .data$defteam == .data$home_team,
              .data$home_timeouts_remaining,
              .data$away_timeouts_remaining
            ),
            time = 60 * as.integer(.data$mins) + as.integer(.data$secs),
            home_score = dplyr::lag(.data$home_score),
            away_score = dplyr::lag(.data$away_score),
            score_differential = dplyr::if_else(.data$pos_team == .data$home_team, .data$home_score - .data$away_score, .data$away_score - .data$home_score),
            runoff = 0,
            yr = 2021,
            home_opening_kickoff = dplyr::if_else(dplyr::first(stats::na.omit(.data$pos_team)) == .data$home_team, 1, 0),
            week = week,
            type = dplyr::if_else(.data$week <= 17, "reg", "post")
          ) %>%
          dplyr::filter(
            .data$down == 4,
            !stringr::str_detect(.data$desc,"kickoff"),
            !stringr::str_detect(.data$desc,"on-side"),
            !(.data$time < 30 & .data$qtr %in% c(4)),
            !stringr::str_detect(.data$desc,"Timeout"),
            is.na(.data$timeout_team),
            .data$type_text != "Two-minute warning",
            .data$type_text != "End Period"
          ) %>%
          dplyr::group_by(.data$qtr, .data$time, .data$ydstogo) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(.data$qtr, dplyr::desc(.data$time), .data$ydstogo) %>%
          dplyr::mutate(
            season = pbp$header$season$year,
            week = pbp$header$week,
            game_id = df$game_id,
            yardline_side = purrr::map_chr(
              stringr::str_split(.data$yardline, " "),
              function(x) x[1]
            ),
            yardline_side = dplyr::case_when(
              .data$yardline_side == "WSH" ~ "WAS",
              .data$yardline_side == "LAR" ~ "LA",
              TRUE ~ .data$yardline_side
            ),
            yardline_number = as.numeric(purrr::map_chr(
              stringr::str_split(.data$yardline, " "),
              function(x) x[2]
            )),
            temp_yardline = dplyr::if_else(
              .data$yardline_side == .data$abbreviation | .data$yardline_100 == 50,
              100 - .data$yardline_number,
              .data$yardline_number
            ),
            yardline_100 = dplyr::if_else(
              !is.na(.data$temp_yardline), as.integer(.data$temp_yardline), .data$yardline_100
            ),
            TimeSecsRem = .data$time + ifelse(.data$qtr == 1 | .data$qtr == 3, 900, 0),
            half = ifelse(.data$qtr < 3, 1, 2),
            pos_score = ifelse(.data$pos_team == .data$home_team, .data$home_score, .data$away_score),
            def_pos_score = ifelse(.data$pos_team == .data$home_team, .data$away_score, .data$home_score),
            score_differential = .data$pos_score - .data$def_pos_score,
            period = .data$qtr,
            total_line = line$overUnder,
            spread_line = line$spread,
            home_total = (.data$spread_line + .data$total_line) / 2,
            away_total = (.data$total_line - .data$spread_line) / 2,
            pos_team_total = dplyr::if_else(.data$pos_team == .data$home_team, .data$home_total, .data$away_total),
            pos_team_spread = dplyr::if_else(.data$pos_team == .data$home_team, .data$spread_line, -1 * .data$spread_line),
            play_text = .data$desc,
            play_type = .data$type_text
          ) %>%
          dplyr::select(
            "game_id",
            "play_id" = "id",
            "yr",
            "desc",
            "play_text",
            "type",
            "qtr",
            "period",
            "half",
            "TimeSecsRem",
            "time",
            "pos_team",
            "def_pos_team" = "defteam",
            "abbreviation",
            # yardline_side,
            "away_team",
            "home_team",
            "away" = "away_team",
            "home" = "home_team",
            "yards_to_goal" = "yardline_100",
            "down",
            "yardline",
            "distance" = "ydstogo",
            "pos_team_timeouts_rem_before" = "pos_team_timeouts_remaining",
            "def_pos_team_timeouts_rem_before" = "defteam_timeouts_remaining",
            "home_opening_kickoff",
            "score_differential",
            "pos_team_total",
            "pos_team_spread",
            "spread" = "spread_line",
            "over_under" = "total_line",
            "runoff",
            "home_score",
            "away_score",
            "pos_team_score" = "pos_score",
            "def_pos_team_score" = "def_pos_score",
            "type_text",
            "play_type",
            "yr"
          ) %>%
          dplyr::mutate(
            Under_two = .data$TimeSecsRem < 120,
            distance = ifelse(.data$distance == 0, 1, .data$distance),
            period = .data$qtr,
            adj_TimeSecsRem = ifelse(.data$period < 3, .data$TimeSecsRem + 1800, .data$TimeSecsRem),
            log_ydstogo = log(.data$yards_to_goal),
            Goal_To_Go = .data$distance == .data$yards_to_goal,
            pos_score_diff_start = .data$pos_team_score - .data$def_pos_team_score,
            pos_team_receives_2H_kickoff = dplyr::case_when(
              # 1st half, home team opened game with kickoff, away team has ball
              .data$period <= 2 & .data$home_opening_kickoff == 1 & .data$pos_team == .data$away_team ~ 1,
              # 1st half, away team opened game with kickoff, home team has ball
              .data$period <= 2 & .data$home_opening_kickoff == 0 & .data$pos_team == .data$home_team ~ 1,
              TRUE ~ 0
            ),
          ) %>%
          # put in end of game conditions
          dplyr::mutate(
            # if there's a conversion with fewer than 5 minutes left and a lead, run off 40 seconds
            runoff = ifelse(between(.data$time, 167, 300) & .data$score_differential > 0 & .data$qtr == 4, 40, .data$runoff),
            # if there's a conversion right before 2 minute warning, run down to 2 minute warning
            runoff = ifelse(between(.data$time, 127, 166) & .data$score_differential > 0 & .data$qtr == 4, .data$time - 120 - 6, .data$runoff),
            # if conversion after 2 minute warning, run down 40 seconds
            runoff = ifelse(.data$time <= 120 & .data$score_differential > 0 & .data$qtr == 4, 40, .data$runoff)
          )
      )


      if (nrow(plays) > 0) {
        plays <- plays %>%
          dplyr::mutate(
            index = 1:dplyr::n()
          )
      } else {
        plays$index <- NA_real_
      }

    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested GameID {espn_game_id} ({df$game_id}) is invalid!"))
      }
    },
    finally = {
    }

  )

  return(plays)
}





