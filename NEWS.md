# cfb4th 0.2.0

* First CRAN release.
* Model loading now mirrors `nfl4th`. The expected-points and field-goal
  models ship with the package (the field-goal model refreshed to the copy on
  `cfbfastR-data`), so nothing is downloaded when the package is attached. The
  two `xgboost` models (fourth-down conversion and win probability), 17 MB
  together, are no longer bundled: each is downloaded from the package's
  `model_archive` GitHub release the first time it is needed and cached under
  `tools::R_user_dir("cfb4th", "cache")`; the new `cfb4th_clear_cache()` forces
  a fresh download. The installed package shrinks from 17 MB to about 2 MB, and
  a failed download is one informative error instead of a failure inside a
  model call.
* `get_4th_plays()` requests the ESPN game summary over HTTPS.
* The model cache is kept in a folder named after the package version, and
  caching a model removes the caches of other versions, so an upgrade never
  leaves stale models behind. Caching a model now prints where it was saved,
  and the model download allows at least 300 seconds.
* The `get_4th_plays()` example now runs (under `\donttest`). The
  `load_4th_pbp()` example runs only in an interactive session with a CFBD
  API key set: scoring one season's ~10,000 fourth downs takes about a minute.
* `load_4th_pbp()` works again with 'cfbfastR' 3.x, whose play-by-play
  already has `spread` and `over_under` columns: they are replaced by the
  provider-ranked betting lines instead of being split into `.x`/`.y`
  columns, which made `add_4th_probs()` fail. A provider with a spread is
  preferred, the play-by-play's own line fills in where the chosen provider
  has none, and `provider` / `formatted_spread` describe the line used.
* `load_4th_pbp()` no longer fails with "Column `provider` not found" when
  the CFBD API returns no betting lines. `cfbfastR::cfbd_betting_lines()`
  returns an empty data frame on any API error, a rate limit (HTTP 429)
  included; `load_4th_pbp()` now warns and keeps the play-by-play's own
  spread and over/under. Lines without a `provider` column are ranked on
  spread alone, with a warning and `provider` set to `NA`. `get_4th_plays()`
  likewise tolerates games with no ESPN betting lines.
* `load_4th_pbp()` gives a clear error when a season's play-by-play is empty,
  and `add_4th_probs()` returns `NA` probabilities (with a warning, for a
  non-empty input) instead of failing when no play can be scored.
* The `cfb4th_clear_cache()` example uses a temporary cache folder, so it no
  longer deletes the user's cached models.
* `dplyr (>= 1.0.0)` is required, for `any_of()`. `cli` is a new import.
* Removed unused suggested packages (`future`, `gt`, `rmarkdown`, `tictoc`,
  `tidyselect`, `tidyverse`).

# cfb4th 0.1.2

* Tidy-select and data-masking fixes to reduce notes/warnings/errors for checks
* Load `{cfbfastR}` models in the same way that the package does (from URL)

# cfb4th 0.1.1

* Re-categorized some plays as unknown (i.e., `NA`) `go`: Penalties and Timeouts
* Fixed bug with 4th down plays inside own 10 not simulating failed go for it plays correctly
* Fixed bug with some plays having negative timeouts remaining, creating strange results 
* Improved timeout detection in `get_4th_plays()` and renamed some columns to better match cfbfastR
* Implemented 2-pt conversion handling. The model no longer assumes a touchdown is worth 7 points

# cfb4th 0.1.0

* Release as package
