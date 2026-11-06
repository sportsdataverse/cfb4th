# Load calculated 4th down probabilities from `cfbfastR` data

Load calculated 4th down probabilities from `cfbfastR` data.

## Usage

``` r
load_4th_pbp(seasons)
```

## Arguments

- seasons:

  Seasons to load. Must be 2014 and later.

## Value

`cfbfastR` data on 4th downs with the
[`add_4th_probs()`](https://cfb4th.sportsdataverse.org/reference/add_4th_probs.md)
columns added and also the following:

- go:

  100 if a team went for it on 4th down, 0 otherwise. It's 100 and 0 as
  a convenience for obtaining percent of times going for it.

## Examples

``` r
# \donttest{
# the betting lines come from the CFBD API, which needs a CFBD_API_KEY
if (cfbfastR::has_cfbd_key()) try(cfb4th::load_4th_pbp(2020))
#> Loading season 2020
#> Computing probabilities for 9752 plays. . .
#> Caching the cfb4th fd_model in /home/runner/.cache/R/cfb4th/0.2.0
#> Caching the cfb4th wp_model in /home/runner/.cache/R/cfb4th/0.2.0
#> ── PBP from data repo and CollegeFootballData.com ──────────── cfbfastR 3.0.0 ──
#> ℹ Data updated: 2026-11-06 08:25:28 UTC
#> # A tibble: 99,955 × 372
#>     year  week id_play   game_id game_play_number half_play_number
#>    <int> <int>   <dbl>     <int>            <dbl>            <dbl>
#>  1  2020     1 4.01e17 401207098                1                1
#>  2  2020     1 4.01e17 401207098                2                2
#>  3  2020     1 4.01e17 401207098                3                3
#>  4  2020     1 4.01e17 401207098                4                4
#>  5  2020     1 4.01e17 401207098                5                5
#>  6  2020     1 4.01e17 401207098                6                6
#>  7  2020     1 4.01e17 401207098                7                7
#>  8  2020     1 4.01e17 401207098                8                8
#>  9  2020     1 4.01e17 401207098                9                9
#> 10  2020     1 4.01e17 401207098               10               10
#> # ℹ 99,945 more rows
#> # ℹ 366 more variables: drive_play_number <dbl>, pos_team <chr>,
#> #   def_pos_team <chr>, pos_team_score <int>, def_pos_team_score <int>,
#> #   half <dbl>, period <int>, clock_minutes <int>, clock_seconds <int>,
#> #   play_type <chr>, play_text <chr>, down <dbl>, distance <dbl>,
#> #   yards_to_goal <dbl>, yards_gained <dbl>, EPA <dbl>, ep_before <dbl>,
#> #   ep_after <dbl>, wpa <dbl>, wp_before <dbl>, wp_after <dbl>, …
# }
```
