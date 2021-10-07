
################################################################################
# Author: Ben Baldwin, Sebastian Carl, Jared Lee
# Purpose: Function that can pull 4th downs from a live game courtesy of ESPN
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get 4th down plays from a game
#'
#' @description Get 4th down plays from a game.
#'
#' @param gid A game to get 4th down decisions of.
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
        current_drive <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)

        previous_drives <- pbp$drives$previous

        drives <- bind_rows(
          previous_drives %>% dplyr::select(team.abbreviation, plays) %>% tidyr::unnest(plays),
          current_drive
        )
      } else if ("current" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        drives <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)
      } else {
        previous_drives <- pbp$drives$previous
        drives <- previous_drives %>% dplyr::select(team.abbreviation, plays) %>% tidyr::unnest(plays)
      }

      line <- pbp$pickcenter %>%
        mutate(provider = factor(provider.name,
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
        arrange(provider) %>%
        slice(1)

      suppressWarnings(

        plays <- drives %>%
          tibble::as_tibble() %>%
          dplyr::group_by(id) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          janitor::clean_names() %>%
          dplyr::rename(
            abbreviation = team_abbreviation,
            qtr = period_number,
            yardline_100 = start_yards_to_endzone,
            yardline = start_possession_text,
            down = start_down,
            ydstogo = start_distance,
            desc = text,
            time = clock_display_value
          ) %>%
          dplyr::left_join(team_info %>%
                      select(abbreviation,pos_team = school) %>%
                      mutate(abbreviation = case_when(pos_team == "Louisiana" ~ "UL",
                                                      pos_team == "Wisconsin" ~ "WISC",
                                                      pos_team == "Oklahoma" ~ "OU",
                                                      pos_team == "Indiana" ~ "IU",
                                                      pos_team == "Charlotte" ~ "CLT",
                                                      pos_team == "UMass" ~ "MASS",
                                                      pos_team == "Northwestern" ~ "NU",
                                                      pos_team == "Miami" ~ "MIA",
                                                      TRUE ~ abbreviation)),
                    by = "abbreviation") %>%
          dplyr::filter(qtr <= 4) %>%
          dplyr::mutate(
            # time column is wacky so extract it from play description when possible
            play_time = stringr::str_extract(desc, "\\([^()]+(?=\\)\\s)"),
            play_time = substr(play_time, 2, nchar(play_time)),
            play_min = stringr::str_extract(play_time, "[^()]+(?=\\:)") %>% as.integer(),
            play_min = if_else(is.na(play_min) & !is.na(play_time), as.integer(0), play_min),
            play_sec = substr(play_time, nchar(play_time) - 1, nchar(play_time)) %>% as.integer(),
            mins = if_else(nchar(time) == 5, substr(time, 1, 2), substr(time, 1, 1)) %>% as.integer(),
            secs = if_else(nchar(time) == 5, substr(time, 4, 5), substr(time, 3, 4)) %>% as.integer(),
            mins = if_else(is.na(play_min), mins, play_min),
            secs = if_else(is.na(play_sec), secs, play_sec)
          ) %>%
          dplyr::arrange(qtr, desc(mins), desc(secs), id) %>%
          dplyr::mutate(
            home_team = home,
            away_team = away,
            defteam = if_else(pos_team == home_team, away_team, home_team),
            half = if_else(qtr <= 2, 1, 2),
            challenge_team = stringr::str_extract(desc, "[:alpha:]*\\s*[:alpha:]*\\s*[:alpha:]*[:alpha:]+(?=\\schallenged)"),
            challenge_team = stringr::str_replace_all(challenge_team, "[\r\n]" , ""),
            challenge_team = stringr::str_trim(challenge_team, side = c("both")),
            desc_timeout = if_else(stringr::str_detect(desc, "Timeout #[:digit:]"), 1, 0),
            timeout_team = stringr::str_extract(desc, "(?<=Timeout #[:digit:] by )[:upper:]{2,3}"),

            home_timeout_used = case_when(
              timeout_team == home_team ~ 1,
              timeout_team != home_team ~ 0,
              is.na(timeout_team) ~ 0
            ),
            away_timeout_used = case_when(
              timeout_team == away_team ~ 1,
              timeout_team != away_team ~ 0,
              is.na(timeout_team) ~ 0
            ),
            home_timeouts_remaining = 3,
            away_timeouts_remaining = 3
          ) %>%
          dplyr::group_by(half) %>%
          dplyr::arrange(qtr, desc(mins), desc(secs), id) %>%
          dplyr::mutate(
            total_home_timeouts_used = dplyr::if_else(cumsum(home_timeout_used) > 3, 3, cumsum(home_timeout_used)),
            total_away_timeouts_used = dplyr::if_else(cumsum(away_timeout_used) > 3, 3, cumsum(away_timeout_used))
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            home_timeouts_remaining = home_timeouts_remaining - total_home_timeouts_used,
            away_timeouts_remaining = away_timeouts_remaining - total_away_timeouts_used,
            pos_team_timeouts_remaining = dplyr::if_else(
              pos_team == home_team,
              home_timeouts_remaining,
              away_timeouts_remaining
            ),
            defteam_timeouts_remaining = dplyr::if_else(
              defteam == home_team,
              home_timeouts_remaining,
              away_timeouts_remaining
            ),
            time = 60 * as.integer(mins) + as.integer(secs),
            home_score = dplyr::lag(home_score),
            away_score = dplyr::lag(away_score),
            score_differential = if_else(pos_team == home_team, home_score - away_score, away_score - home_score),
            runoff = 0,
            yr = 2020,
            home_opening_kickoff = if_else(dplyr::first(na.omit(pos_team)) == home_team, 1, 0),
            week = week,
            type = if_else(week <= 17, "reg", "post")
          ) %>%
          dplyr::filter(
            down == 4,
            !stringr::str_detect(desc,"kickoff"),
            !stringr::str_detect(desc,"on-side"),
            !(time < 30 & qtr %in% c(4)),
            !stringr::str_detect(desc,"Timeout"),
            is.na(timeout_team),
            type_text != "Two-minute warning",
            type_text != "End Period"
          ) %>%
          dplyr::group_by(qtr, time, ydstogo) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(qtr, desc(time), ydstogo) %>%
          dplyr::mutate(
            game_id = df$game_id,
            yardline_side = purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[1]
            ),
            yardline_side = case_when(
              yardline_side == "WSH" ~ "WAS",
              yardline_side == "LAR" ~ "LA",
              TRUE ~ yardline_side
            ),
            yardline_number = as.numeric(purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[2]
            )),
            temp_yardline = dplyr::if_else(
              yardline_side == abbreviation | yardline_100 == 50,
              100 - yardline_number,
              yardline_number
            ),
            yardline_100 = if_else(
              !is.na(temp_yardline), as.integer(temp_yardline), yardline_100
            ),
            TimeSecsRem = time + ifelse(qtr == 1 | qtr == 3,900,0),
            half = ifelse(qtr<3,1,2),
            pos_score = ifelse(pos_team == home_team,home_score,away_score),
            def_pos_score = ifelse(pos_team == home_team,away_score,home_score),
            score_differential = pos_score - def_pos_score,
            period = qtr,
            total_line = line$overUnder,
            spread_line = line$spread,
            home_total = (spread_line + total_line) / 2,
            away_total = (total_line - spread_line) / 2,
            pos_team_total = if_else(pos_team == home_team, home_total, away_total),
            pos_team_spread = dplyr::if_else(pos_team == home_team, spread_line, -1 * spread_line)
          ) %>%
          dplyr::select(
            game_id,
            play_id = id,
            yr,
            desc,
            type,
            qtr,
            period,
            half,
            TimeSecsRem,
            time,
            pos_team,
            def_pos_team = defteam,
            abbreviation,
            # yardline_side,
            away_team,
            home_team,
            away = away_team,
            home = home_team,
            yards_to_goal = yardline_100,
            down,
            yardline,
            distance = ydstogo,
            pos_team_timeouts_rem_before = pos_team_timeouts_remaining,
            def_pos_team_timeouts_rem_before = defteam_timeouts_remaining,
            home_opening_kickoff,
            score_differential,
            pos_team_total,
            pos_team_spread,
            spread = spread_line,
            over_under = total_line,
            runoff,
            home_score,
            away_score,
            pos_team_score = pos_score,
            def_pos_team_score = def_pos_score,
            type_text,
            yr
          ) %>%
          dplyr::mutate(Under_two = TimeSecsRem < 120,
                 distance = ifelse(distance == 0,1,distance),
                 period = qtr,
                 adj_TimeSecsRem = ifelse(period < 3, TimeSecsRem + 1800, TimeSecsRem),
                 log_ydstogo = log(yards_to_goal),
                 Goal_To_Go = distance == yards_to_goal,
                 pos_score_diff_start = pos_team_score-def_pos_team_score,
                 pos_team_receives_2H_kickoff = case_when(
                   # 1st half, home team opened game with kickoff, away team has ball
                   period <= 2 & home_opening_kickoff == 1 & pos_team == away_team ~ 1,
                   # 1st half, away team opened game with kickoff, home team has ball
                   period <= 2 & home_opening_kickoff == 0 & pos_team == home_team ~ 1,
                   TRUE ~ 0
                 ),
                 ) %>%
          # put in end of game conditions
          dplyr::mutate(
            # if there's a conversion with fewer than 5 minutes left and a lead, run off 40 seconds
            runoff = ifelse(between(time, 167, 300) & score_differential > 0 & qtr == 4, 40, runoff),
            # if there's a conversion right before 2 minute warning, run down to 2 minute warning
            runoff = ifelse(between(time, 127, 166) & score_differential > 0 & qtr == 4, time - 120 - 6, runoff),
            # if conversion after 2 minute warning, run down 40 seconds
            runoff = ifelse(time <= 120 & score_differential > 0 & qtr == 4, 40, runoff)
          )
      )


      if (nrow(plays) > 0) {
        plays <- plays %>%
          mutate(
            index = 1 : n()
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
