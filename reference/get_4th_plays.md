# Get 4th down plays from a game

Get 4th down plays from a game.

## Usage

``` r
get_4th_plays(df)
```

## Arguments

- df:

  A data frame of a game to get 4th down decisions of.

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
The following columns must be present:

- game_id : ESPN game ID from ESPN or cfbfastR (e.g. '401114223')

- home_team : Name of the home team

- away_team : Name of the away team

## Examples

``` r
if (FALSE) { # \dontrun{

game <- cfbfastR::cfbd_game_info(2019, team = "Utah", week = 3)
plays <- cfb4th::get_4th_plays(game)

dplyr::glimpse(plays)
} # }
```
