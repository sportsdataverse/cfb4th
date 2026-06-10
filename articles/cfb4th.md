# Get Started with cfb4th

First load the packages and some data.
[`load_4th_pbp()`](https://cfb4th.sportsdataverse.org/reference/load_4th_pbp.md)
loads `cfbfastR` data and computes 4th down probabilities (depending on
your computer, this may take up to a minute or two per season).

``` r

library(cfb4th)
library(dplyr)
library(knitr)
library(tibble)
library(gt)
```

## Easy mode: using cfbfastR data

Here’s what the data obtained using
[`load_4th_pbp()`](https://cfb4th.sportsdataverse.org/reference/load_4th_pbp.md)
looks like:

``` r

suppressWarnings(
  data <- cfb4th::load_4th_pbp(2020) %>% 
    dplyr::filter(.data$week %in% 1:14)
)
data %>%
  dplyr::filter(!is.na(go_boost)) %>%
  utils::head(10) %>%
  dplyr::select(
    pos_team, distance, yards_to_goal, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) %>%
  knitr::kable(digits = 2)
```

Or we can add some filters to look up a certain game:

``` r

data %>%
  dplyr::filter(week == 12, pos_team == "Utah", down == 4) %>%
  dplyr::select(
    pos_team, distance, yards_to_goal, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) %>%
  knitr::kable(digits = 2)
```

## Calculations from user input

The below shows the bare minimum amount of information that has to be
fed to `cfb4th` in order to compute 4th down decision recommendations.
The main function on user-input data is
[`add_4th_probs()`](https://cfb4th.sportsdataverse.org/reference/add_4th_probs.md).

Teams are included to help the model easily track the simulations.

``` r

one_play <-
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
one_play %>%
  cfb4th::add_4th_probs() %>%
  dplyr::select(
    pos_team, distance, yards_to_goal, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) %>%
  knitr::kable(digits = 2)
#> Computing probabilities for 1 plays. . .
```

| pos_team | distance | yards_to_goal | go_boost | first_down_prob | wp_fail | wp_succeed | go_wp | fg_make_prob | miss_fg_wp | make_fg_wp | fg_wp | punt_wp |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Utah | 4 | 40 | 1.4 | 0.49 | 0.81 | 0.92 | 0.87 | 0.3 | 0.81 | 0.91 | 0.84 | 0.85 |

## Make a summary table

Let’s put the play above into a table using the provided function
[`make_table_data()`](https://cfb4th.sportsdataverse.org/reference/make_table_data.md),
which makes it easier to interpret the recommendations for a play. This
function only works with one play at a time since it makes a table using
the results from the play.

``` r

one_play %>%
  cfb4th::add_4th_probs() %>%
  cfb4th::make_table_data() %>%
  knitr::kable(digits = 1)
#> Computing probabilities for 1 plays. . .
```

| choice             | choice_prob | success_prob | fail_wp | success_wp |
|:-------------------|------------:|-------------:|--------:|-----------:|
| Go for it          |        86.5 |         49.4 |    81.2 |       92.0 |
| Punt               |        85.1 |           NA |      NA |         NA |
| Field goal attempt |        84.4 |         30.4 |    81.4 |       91.3 |

Looking at the table, the offense would be expected to have 86.5% win
probability if they had gone for it and 85% if they punted.

## Getting 4th down plays from a live game

`cfbfastR` isn’t available for live games and typing all the plays in by
hand is annoying. So how does the 4th down bot work? With thanks to the
ESPN API, which can be accessed using
[`get_4th_plays()`](https://cfb4th.sportsdataverse.org/reference/get_4th_plays.md).

``` r

game <- cfbfastR::cfbd_game_info(year = 2019, team = "Utah", week = 4)
plays <- cfb4th::get_4th_plays(game) %>% 
  tail(1)
plays %>% 
  dplyr::select("desc", "TimeSecsRem")
#> # A tibble: 1 × 2
#>   desc                        TimeSecsRem
#>   <chr>                             <dbl>
#> 1 Jadon Redding 38 yd FG GOOD         241
plays %>% 
  cfb4th::add_4th_probs() %>%
  cfb4th::make_table_data() %>%
  knitr::kable(digits = 1)
#> Computing probabilities for 1 plays. . .
```

| choice             | choice_prob | success_prob | fail_wp | success_wp |
|:-------------------|------------:|-------------:|--------:|-----------:|
| Field goal attempt |         7.6 |         72.3 |     2.2 |        9.7 |
| Go for it          |         7.5 |         29.0 |     2.3 |       20.2 |
| Punt               |          NA |           NA |      NA |         NA |
