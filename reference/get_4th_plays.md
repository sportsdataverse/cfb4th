# Get 4th down plays from a game

Get 4th down plays from a game.

## Usage

``` r
get_4th_plays(df)
```

## Arguments

- df:

  A one-row data frame identifying the game to get 4th down decisions
  of.

## Value

A data frame of the game's fourth down plays, one row per play (the
input data frame is only used to identify the game), with the columns:

- desc:

  Play description from ESPN.

- type_text:

  Play type text from ESPN.

- index:

  Index number of play from a given game. Useful for tracking plays
  (e.g. for 4th down bot).

- The rest:

  All the columns needed for `add_4th_probs().`

## Details

Obtains a data frame that can be used with
[`add_4th_probs()`](https://cfb4th.sportsdataverse.org/reference/add_4th_probs.md).
`df` must have exactly one row (otherwise `get_4th_plays()` errors) and
the following columns:

- game_id : ESPN game ID from ESPN or cfbfastR (e.g. '401114223')

- home_team : Name of the home team

- away_team : Name of the away team

## Examples

``` r
# \donttest{
# Utah at BYU, 2019 week 1, identified by its ESPN game id
game <- data.frame(game_id = 401114223, home_team = "BYU", away_team = "Utah", week = 1)
try(dplyr::glimpse(cfb4th::get_4th_plays(game)))
#> Rows: 12
#> Columns: 42
#> $ game_id                          <dbl> 401114223, 401114223, 401114223, 4011…
#> $ play_id                          <chr> "401114223101875801", "40111422310191…
#> $ yr                               <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2…
#> $ desc                             <chr> "Zach Wilson pass incomplete to Talon…
#> $ play_text                        <chr> "Zach Wilson pass incomplete to Talon…
#> $ type                             <chr> "reg", "reg", "reg", "reg", "reg", "r…
#> $ qtr                              <int> 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 4, 4
#> $ period                           <int> 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 4, 4
#> $ half                             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2
#> $ TimeSecsRem                      <dbl> 1661, 1387, 1218, 1089, 855, 663, 352…
#> $ time                             <dbl> 761, 487, 318, 189, 855, 663, 352, 24…
#> $ pos_team                         <chr> "BYU", "Utah", "BYU", "Utah", "BYU", …
#> $ def_pos_team                     <chr> "Utah", "BYU", "Utah", "BYU", "Utah",…
#> $ abbreviation                     <chr> "BYU", "UTAH", "BYU", "UTAH", "BYU", …
#> $ away                             <chr> "Utah", "Utah", "Utah", "Utah", "Utah…
#> $ home                             <chr> "BYU", "BYU", "BYU", "BYU", "BYU", "B…
#> $ yards_to_goal                    <int> 40, 23, 74, 26, 15, 90, 20, 51, 68, 6…
#> $ down                             <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
#> $ yardline                         <chr> "UTAH 40", "BYU 23", "BYU 26", "BYU 2…
#> $ distance                         <int> 4, 5, 4, 3, 8, 19, 15, 1, 2, 10, 3, 1
#> $ pos_team_timeouts_rem_before     <dbl> 3, 3, 3, 3, 3, 3, 3, 1, 2, 3, 3, 3
#> $ def_pos_team_timeouts_rem_before <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 0, 3, 3, 2
#> $ home_opening_kickoff             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> $ score_differential               <int> 0, 0, -3, 3, -3, 0, -6, 3, -3, -10, 1…
#> $ pos_team_total                   <dbl> 27.0, 21.5, 27.0, 21.5, 27.0, 21.5, 2…
#> $ pos_team_spread                  <dbl> 5.5, -5.5, 5.5, -5.5, 5.5, -5.5, 5.5,…
#> $ spread                           <dbl> 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.…
#> $ over_under                       <dbl> 48.5, 48.5, 48.5, 48.5, 48.5, 48.5, 4…
#> $ runoff                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6
#> $ home_score                       <int> 0, 0, 0, 0, 0, 3, 3, 6, 6, 6, 6, 12
#> $ away_score                       <int> 0, 0, 3, 3, 3, 3, 9, 9, 9, 16, 16, 30
#> $ pos_team_score                   <int> 0, 0, 0, 3, 0, 3, 3, 9, 6, 6, 16, 30
#> $ def_pos_team_score               <int> 0, 0, 3, 0, 3, 3, 9, 6, 9, 16, 6, 12
#> $ type_text                        <chr> "Pass Incompletion", "Field Goal Good…
#> $ play_type                        <chr> "Pass Incompletion", "Field Goal Good…
#> $ Under_two                        <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FA…
#> $ adj_TimeSecsRem                  <dbl> 3461, 3187, 3018, 2889, 2655, 2463, 2…
#> $ log_ydstogo                      <dbl> 3.688879, 3.135494, 4.304065, 3.25809…
#> $ Goal_To_Go                       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FA…
#> $ pos_score_diff_start             <int> 0, 0, -3, 3, -3, 0, -6, 3, -3, -10, 1…
#> $ pos_team_receives_2H_kickoff     <dbl> 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0
#> $ index                            <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
# }
```
