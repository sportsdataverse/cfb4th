# Get 4th down decision probabilities

Get a table with the probabilities on 4th down.

## Usage

``` r
make_table_data(df)
```

## Arguments

- df:

  A data frame consisting of one play that has had \`add_4th_probs()\`
  already run on it.

## Value

A table showing the probabilities associated with each possible choice.

## Examples

``` r
# \donttest{
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

probs <- cfb4th::add_4th_probs(play)
#> Computing probabilities for 1 plays. . .

cfb4th::make_table_data(probs)
#> # A tibble: 3 × 5
#>   choice             choice_prob success_prob fail_wp success_wp
#>   <chr>                    <dbl>        <dbl>   <dbl>      <dbl>
#> 1 Go for it                 86.5         49.4    81.2       92.0
#> 2 Punt                      85.1         NA      NA         NA  
#> 3 Field goal attempt        84.4         30.4    81.4       91.3
# }
```
