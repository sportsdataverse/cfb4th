# Get 4th down decision probabilities

Get various probabilities associated with each option on 4th downs (go
for it, kick field goal, punt).

## Usage

``` r
add_4th_probs(df)
```

## Arguments

- df:

  A data frame of decisions to be computed for.

## Value

Original data frame Data frame plus the following columns added:

- go_boost:

  Gain (or loss) in win prob associated with choosing to go for it
  (percentage points).

- first_down_prob:

  Probability of earning a first down if going for it on 4th down.

- wp_fail:

  Win probability in the event of a failed 4th down attempt.

- wp_succeed:

  Win probability in the event of a successful 4th down attempt.

- go_wp:

  Average win probability when going for it on 4th down.

- fg_make_prob:

  Probability of making field goal.

- miss_fg_wp:

  Win probability in the event of a missed field goal.

- make_fg_wp:

  Win probability in the event of a made field goal.

- fg_wp:

  Average win probability when attempting field goal.

- punt_wp:

  Average win probability when punting.

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

 cfb4th::add_4th_probs(play)
#> Computing probabilities for 1 plays. . .
#> # A tibble: 1 × 27
#>   home  away  pos_team def_pos_team spread over_under  half period TimeSecsRem
#>   <chr> <chr> <chr>    <chr>         <dbl>      <dbl> <dbl>  <dbl>       <dbl>
#> 1 Utah  BYU   Utah     BYU              -7         55     2      3         900
#> # ℹ 18 more variables: adj_TimeSecsRem <dbl>, down <dbl>, distance <dbl>,
#> #   yards_to_goal <dbl>, pos_score_diff_start <dbl>,
#> #   pos_team_receives_2H_kickoff <dbl>, pos_team_timeouts_rem_before <dbl>,
#> #   def_pos_team_timeouts_rem_before <dbl>, go_boost <dbl>,
#> #   first_down_prob <dbl>, wp_fail <dbl>, wp_succeed <dbl>, go_wp <dbl>,
#> #   fg_make_prob <dbl>, miss_fg_wp <dbl>, make_fg_wp <dbl>, fg_wp <dbl>,
#> #   punt_wp <dbl>

# }
```
