# fixtures/espn_summary_401114220.json.gz: the drives, pickcenter and header of
# ESPN's game summary for Utah at USC, 2019 week 4 (game 401114220), captured
# 2026-11-09. get_4th_plays() reads only those three parts.
espn_fixture <- function() {
  jsonlite::fromJSON(gzfile(test_path("fixtures", "espn_summary_401114220.json.gz")), flatten = TRUE)
}

usc_utah <- data.frame(game_id = 401114220, home_team = "USC", away_team = "Utah", week = 4)

test_that("get_4th_plays() returns its documented columns (offline ESPN fixture)", {
  summary <- espn_fixture()
  testthat::local_mocked_bindings(espn_summary = function(game_id) summary, .package = "cfb4th")

  plays <- get_4th_plays(usc_utah)

  expect_named(plays, c(
    "game_id", "play_id", "yr", "desc", "play_text", "type", "qtr",
    "period", "half", "TimeSecsRem", "time", "pos_team", "def_pos_team",
    "abbreviation", "away", "home", "yards_to_goal", "down", "yardline",
    "distance", "pos_team_timeouts_rem_before", "def_pos_team_timeouts_rem_before",
    "home_opening_kickoff", "score_differential", "pos_team_total",
    "pos_team_spread", "spread", "over_under", "runoff", "home_score",
    "away_score", "pos_team_score", "def_pos_team_score", "type_text",
    "play_type", "Under_two", "adj_TimeSecsRem", "log_ydstogo", "Goal_To_Go",
    "pos_score_diff_start", "pos_team_receives_2H_kickoff", "index"
  ))
  expect_equal(nrow(plays), 13)
  expect_true(all(plays$down == 4))
  expect_type(plays$desc, "character")
  expect_false(anyNA(plays$desc))
  expect_equal(unique(plays$spread), 3.5)
  expect_equal(unique(plays$over_under), 53.5)
})

test_that("get_4th_plays() errors by name on a failed game lookup", {
  # what cfbfastR::cfbd_game_info() returns when the CFBD API rejects the key
  expect_error(get_4th_plays(usc_utah[0, ]), "got 0 rows")
  expect_error(get_4th_plays(NULL), "got NULL")
  expect_error(get_4th_plays(usc_utah["game_id"]), "got no home_team, away_team column")
})
