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

`spread` and `over_under` come from the CFBD betting lines, one per game
ranked by `provider`, with the play-by-play's own line where CFBD has
none. If CFBD returns no lines, or lines without a provider, a warning
says so and `provider` is `NA`.

## Examples

``` r
if (FALSE) { # interactive() && cfbfastR::has_cfbd_key()
# interactive only: one season is the smallest call, and scoring its ~10,000
# fourth downs (mostly the field-goal model) takes about a minute of CPU.
# The betting lines come from the CFBD API, which needs a CFBD_API_KEY.
try(cfb4th::load_4th_pbp(2020))
}
```
