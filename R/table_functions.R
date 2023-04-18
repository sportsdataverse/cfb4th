#' Get 4th down decision probabilities
#'
#' @description Get a table with the probabilities on 4th down.
#'
#' @param df A data frame consisting of one play that has had `add_4th_probs()` already run on it.
#' @return A table showing the probabilities associated with each possible choice.
#' @export
#' @examples
#' \donttest{
#' play <-
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
#' probs <- cfb4th::add_4th_probs(play)
#'
#' cfb4th::make_table_data(probs)
#'}

make_table_data <- function(df) {
  go <- tibble::tibble(
    "choice_prob" = df$go_wp,
    "choice" = "Go for it",
    "success_prob" = df$first_down_prob,
    "fail_wp" = df$wp_fail,
    "success_wp" = df$wp_succeed
  ) %>%
    dplyr::select(
      "choice",
      "choice_prob",
      "success_prob",
      "fail_wp",
      "success_wp")

  punt <- tibble::tibble(
    "choice_prob" = dplyr::if_else(is.na(df$punt_wp), NA_real_, df$punt_wp),
    "choice" = "Punt",
    "success_prob" = NA_real_,
    "fail_wp" = NA_real_,
    "success_wp" = NA_real_
  ) %>%
    dplyr::select(
      "choice",
      "choice_prob",
      "success_prob",
      "fail_wp",
      "success_wp")

  fg <- tibble::tibble(
    "choice_prob" = df$fg_wp,
    "choice" = "Field goal attempt",
    "success_prob" = df$fg_make_prob,
    "fail_wp" = df$miss_fg_wp,
    "success_wp" = df$make_fg_wp
  ) %>%
    dplyr::select(
      "choice",
      "choice_prob",
      "success_prob",
      "fail_wp",
      "success_wp")

  tableData <- dplyr::bind_rows(
    go, fg, punt
  ) %>%
    dplyr::mutate(
      choice_prob = 100 * .data$choice_prob,
      success_prob = 100 * .data$success_prob,
      fail_wp = 100 * .data$fail_wp,
      success_wp = 100 * .data$success_wp
    ) %>%
    dplyr::arrange(dplyr::desc(.data$choice_prob))
  return(tableData)
}
