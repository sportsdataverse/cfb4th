# Load calculated 4th down probabilities from \`cfbfastR\` data

Load calculated 4th down probabilities from \`cfbfastR\` data.

## Usage

``` r
load_4th_pbp(seasons)
```

## Arguments

- seasons:

  Seasons to load. Must be 2014 and later.

## Value

\`cfbfastR\` data on 4th downs with the \`add_4th_probs()\` columns
added and also the following:

- go:

  100 if a team went for it on 4th down, 0 otherwise. It's 100 and 0 as
  a convenience for obtaining percent of times going for it.

## Examples

``` r
# \donttest{
  cfb4th::load_4th_pbp(2020)
#> Loading season 2020
#> Error in map(.x, .f, ...): ℹ In index: 1.
#> Caused by error in `dplyr::filter()`:
#> ℹ In argument: `!is.na(.data$spread)`.
#> Caused by error in `.data$spread`:
#> ! Column `spread` not found in `.data`.
# }
```
