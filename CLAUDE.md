# CLAUDE.md — cfb4th Development Guide

`cfb4th` estimates outcomes of NCAA-football fourth-down plays and
computes the optimal decision (go / field goal / punt) — the CFB
analogue of [`nfl4th`](https://www.nfl4th.com). It consumes `cfbfastR`
play-by-play and the shared `cfbfastR-data` EP/WP/FG models. Distributed
via r-universe (`sportsdataverse.r-universe.dev`); CRAN install is
commented out in `README.Rmd`.

- **Version**: 0.1.2 (per `DESCRIPTION`) — **License**: MIT — **R**: \>=
  3.5.0
- **Repo**: <https://github.com/sportsdataverse/cfb4th> —
  **Maintainer**: Jared Lee
- **Branch**: `main` is the default and release branch.

## Commands

R-package workflow (roxygen2 / devtools), verified against `tests/`,
`NAMESPACE`, and `.github/workflows/` (`R-CMD-check.yaml`,
`pkgdown.yaml`):

``` r

devtools::load_all()                 # interactive dev
devtools::document()                 # regenerate man/ + NAMESPACE after roxygen edits
devtools::test()                     # testthat (edition 3); tests/testthat/test-basic.R
devtools::check()                    # full R CMD check — run before a PR
devtools::build_readme()             # re-render README.md from README.Rmd (never hand-edit README.md)
pkgdown::build_site()                # build docs site locally
```

## Architecture

Public surface is **4 exported functions** (`NAMESPACE`), all driven by
[`add_4th_probs()`](https://cfb4th.sportsdataverse.org/reference/add_4th_probs.md):

| Function | File | Role |
|----|----|----|
| `add_4th_probs(df)` | `R/wrappers.R` | **Main entry.** Adds go/FG/punt win-prob columns to a 4th-down frame (raw `cfbfastR` pbp or a hand-built one-play tibble). |
| `get_4th_plays(df)` | `R/get_game_data.R` | Pulls/builds the per-play game-state frame consumable by [`add_4th_probs()`](https://cfb4th.sportsdataverse.org/reference/add_4th_probs.md). |
| `load_4th_pbp(seasons)` | `R/wrappers.R` | Season loader: [`cfbfastR::load_cfb_pbp()`](https://cfbfastR.sportsdataverse.org/reference/load_cfb_pbp.html) + `cfbd_betting_lines()` spread/OU join → [`add_4th_probs()`](https://cfb4th.sportsdataverse.org/reference/add_4th_probs.md). **Seasons must be \>= 2014** (errors otherwise). |
| `make_table_data(df)` | `R/table_functions.R` | Formats one [`add_4th_probs()`](https://cfb4th.sportsdataverse.org/reference/add_4th_probs.md) play into `gt`-ready table data. |

**Pipeline** (`add_4th_probs` → `R/helpers.R` `add_probs()`):
`prepare_cfbfastr_data()` (when `type` absent, filters `down == 4`) →
`prepare_df()` → `add_probs()`, which runs the three decision functions
in `R/decision_functions.R`: `get_go_wp()` → `get_fg_wp()` →
`get_punt_wp()`. Each calls `prep_ep()` / `prep_wp()` (`R/helpers.R`) to
score expected points and win probability per game state. `R/helpers.R`
also holds `flip_team()` / `flip_half()` / `end_game_fn()` for
possession/half/end-of-game state transitions.

## Models (two bundled, two downloaded on first use — the key gotcha)

Nothing loads at package load (`R/zzz.R` is a comment). Same shape as
`nfl4th`:

- **`ep_model`, `fg_model`** — plain objects in **`R/sysdata.rda`**,
  rebuilt by `data-raw/sysdata.R` from the `.Rdata` on the
  `cfbfastR-data` GitHub repo (the legacy cfbfastR models). `ep_model`
  is an [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)
  (predict with `type = "probs"`); `fg_model` is an `mgcv` bam
  ([`mgcv::predict.bam`](https://rdrr.io/pkg/mgcv/man/predict.bam.html)).
  Always available offline.
- **`fd_model()`, `wp_model()`** — *functions* in `R/cache.R` (mirrors
  `nfl4th::cached_model()`). Raw UBJ byte vectors saved with
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) on this repo’s
  **`model_archive` GitHub release**
  (<https://github.com/sportsdataverse/cfb4th/releases/tag/model_archive>),
  downloaded on first use, read with
  [`xgboost::xgb.load.raw()`](https://rdrr.io/pkg/xgboost/man/xgb.load.raw.html),
  parsed once per session (`.models` env) and cached under
  `tools::R_user_dir("cfb4th", "cache")`. To ship a retrained model,
  upload a new asset there; users pick it up after
  [`cfb4th_clear_cache()`](https://cfb4th.sportsdataverse.org/reference/cfb4th_clear_cache.md).

`probably_cran()` (CRAN check env vars) disables cache *writes* so
CRAN’s machines are never written to;
`options(cfb4th.force_cache = TRUE)` overrides. A failed download is one
informative [`stop()`](https://rdrr.io/r/base/stop.html) from
`download_model()`, not a `NULL` that dies inside
[`predict()`](https://rdrr.io/r/stats/predict.html). The exported
[`cfb4th_clear_cache()`](https://cfb4th.sportsdataverse.org/reference/cfb4th_clear_cache.md)
wipes the session copy and the disk copy.

`R/sysdata.rda` holds `ep_model`, `fg_model`, `punt_df`, `team_info`
(see `data-raw/sysdata.R`). `data-raw/_fg_mod.R`,
`_go_for_it_cfb_mod.R`, `_punt_mod.R` are the model-training scripts
(not run at build).

## Conventions

- **Tidy-eval / data-masking**: use `.data$col` and quoted column names
  in dplyr/tidyr to avoid R CMD check NOTEs (NEWS 0.1.2 was a
  tidy-select cleanup pass).
- **Style**: `styler::tidyverse_style()` (per file headers).
  `import(dplyr)`; `%>%` re-exported from magrittr;
  `importFrom(nnet, multinom)`, `importFrom(tidyr, pivot_wider)`,
  `importFrom(xgboost, getinfo)`.
- Never hand-edit `NAMESPACE` or `man/*.Rd` — regenerate with
  `devtools::document()`.
- `_pkgdown.yml` is the docs config (bootstrap 5, plausible analytics);
  reference groups list the 5 exports.

## Gotchas

- **The xgboost models need network on first use** (then the cache
  serves it). The tests that score plays `skip_on_cran()`; the examples
  are `\donttest{try()}`.
- **`load_4th_pbp(seasons)` rejects seasons \< 2014** with an explicit
  [`stop()`](https://rdrr.io/r/base/stop.html).
- **Betting lines drive WP inputs**:
  [`load_4th_pbp()`](https://cfb4th.sportsdataverse.org/reference/load_4th_pbp.md)
  pulls `cfbd_betting_lines()`, factor-ranks providers (`consensus`,
  `teamrankings`, …), and slices one spread/OU per `game_id` — spread/OU
  are model inputs, not passthroughs.
- **`cfbfastR (>= 1.4.0)`** is a hard `Imports` dependency; the pbp
  schema (down, yards_to_goal, timeouts, `pos_team`, 2H-kickoff flag)
  must match what `prepare_cfbfastr_data()` expects. A column-name drift
  upstream breaks prep.
- 2-pt conversion handling (NEWS 0.1.1): the model no longer assumes a
  TD = 7.

## Reference

pkgdown site: <https://cfb4th.sportsdataverse.org/>. Update `NEWS.md`
under the current version heading for user-facing changes.

## Commit Convention

[Conventional Commits](https://www.conventionalcommits.org/) — e.g.
`fix: correct go-for-it WP near own goal line`,
`docs: refresh README models note`. Use `type!:` or a `BREAKING CHANGE:`
footer for breaking changes.

**Never add AI tools (Claude, Copilot, etc.) as commit co-authors.**

## Cheat sheet

There is a printable one-page reference for this package at
<https://sportsdataverse.org/cheatsheets/cfbplotR-cfb4th-cfbseedR.pdf>,
one of [a set covering every SportsDataverse
package](https://sportsdataverse.org/cheatsheets). Keep it in mind when
adding or renaming an exported function: the sheet is a hand-built
canvas, so a surface change means the sheet needs a revision too.
