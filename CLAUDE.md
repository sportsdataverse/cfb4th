# CLAUDE.md — cfb4th Development Guide

`cfb4th` estimates outcomes of NCAA-football fourth-down plays and computes the
optimal decision (go / field goal / punt) — the CFB analogue of
[`nfl4th`](https://www.nfl4th.com). It consumes `cfbfastR` play-by-play and the
shared `cfbfastR-data` EP/WP/FG models. Distributed via r-universe
(`sportsdataverse.r-universe.dev`); CRAN install is commented out in `README.Rmd`.

- **Version**: 0.1.2 (per `DESCRIPTION`) — **License**: MIT — **R**: >= 3.5.0
- **Repo**: <https://github.com/sportsdataverse/cfb4th> — **Maintainer**: Jared Lee
- **Branch**: `main` is the default and release branch.

## Commands

R-package workflow (roxygen2 / devtools), verified against `tests/`, `NAMESPACE`,
and `.github/workflows/` (`R-CMD-check.yaml`, `pkgdown.yaml`):

```r
devtools::load_all()                 # interactive dev
devtools::document()                 # regenerate man/ + NAMESPACE after roxygen edits
devtools::test()                     # testthat (edition 3); tests/testthat/test-basic.R
devtools::check()                    # full R CMD check — run before a PR
devtools::build_readme()             # re-render README.md from README.Rmd (never hand-edit README.md)
pkgdown::build_site()                # build docs site locally
```

## Architecture

Public surface is **4 exported functions** (`NAMESPACE`), all driven by `add_4th_probs()`:

| Function | File | Role |
|---|---|---|
| `add_4th_probs(df)` | `R/wrappers.R` | **Main entry.** Adds go/FG/punt win-prob columns to a 4th-down frame (raw `cfbfastR` pbp or a hand-built one-play tibble). |
| `get_4th_plays(df)` | `R/get_game_data.R` | Pulls/builds the per-play game-state frame consumable by `add_4th_probs()`. |
| `load_4th_pbp(seasons)` | `R/wrappers.R` | Season loader: `cfbfastR::load_cfb_pbp()` + `cfbd_betting_lines()` spread/OU join → `add_4th_probs()`. **Seasons must be >= 2014** (errors otherwise). |
| `make_table_data(df)` | `R/table_functions.R` | Formats one `add_4th_probs()` play into `gt`-ready table data. |

**Pipeline** (`add_4th_probs` → `R/helpers.R` `add_probs()`):
`prepare_cfbfastr_data()` (when `type` absent, filters `down == 4`) → `prepare_df()`
→ `add_probs()`, which runs the three decision functions in `R/decision_functions.R`:
`get_go_wp()` → `get_fg_wp()` → `get_punt_wp()`. Each calls `prep_ep()` /
`prep_wp()` (`R/helpers.R`) to score expected points and win probability per
game state. `R/helpers.R` also holds `flip_team()` / `flip_half()` /
`end_game_fn()` for possession/half/end-of-game state transitions.

## Models (mixed bundling — this is the key gotcha)

Four model objects are loaded in `.onLoad` (`R/zzz.R`) and assigned into the
package namespace. They come from **two different sources**:

- **`ep_model`, `fg_model`** — downloaded at load time from the **`cfbfastR-data`
  GitHub repo** via `url(".../models/<name>.Rdata")` + `load()` inside `try()`.
  `ep_model` is an `nnet::multinom` (predicted with `type = "probs"`); `fg_model`
  is an `mgcv` bam (scored with `mgcv::predict.bam`). NEWS 0.1.2: "load `{cfbfastR}`
  models in the same way that the package does (from URL)." **No network → these
  are `NULL` and probabilities can't be computed.**
- **`fd_model` (go-for-it), `wp_model` (`wp_spread`)** — bundled **natively in
  `inst/models/*.ubj`** (`fd_model.ubj` ~15 MB, `wp_spread.ubj`), loaded via
  `xgboost::xgb.load(system.file("models", ...))`. UBJ keeps them readable across
  xgboost versions; `DESCRIPTION` pins `xgboost (>= 2.0.0)`. Shipped in the wheel.

`R/sysdata.rda` holds internal package data. `data-raw/_fg_mod.R`,
`_go_for_it_cfb_mod.R`, `_punt_mod.R` are the model-training scripts (not run at build).

## Conventions

- **Tidy-eval / data-masking**: use `.data$col` and quoted column names in
  dplyr/tidyr to avoid R CMD check NOTEs (NEWS 0.1.2 was a tidy-select cleanup pass).
- **Style**: `styler::tidyverse_style()` (per file headers). `import(dplyr)`;
  `%>%` re-exported from magrittr; `importFrom(nnet, multinom)`,
  `importFrom(tidyr, pivot_wider)`, `importFrom(xgboost, getinfo)`.
- Never hand-edit `NAMESPACE` or `man/*.Rd` — regenerate with `devtools::document()`.
- `_pkgdown.yml` is the docs config (bootstrap 5, plausible analytics); reference
  groups list the 4 exports.

## Gotchas

- **`ep_model`/`fg_model` need network at first load** (GitHub `cfbfastR-data`).
  Offline → they're `NULL` and the EP/FG legs silently degrade. `fd_model`/
  `wp_model` are local UBJ and always available.
- **`load_4th_pbp(seasons)` rejects seasons < 2014** with an explicit `stop()`.
- **Betting lines drive WP inputs**: `load_4th_pbp()` pulls `cfbd_betting_lines()`,
  factor-ranks providers (`consensus`, `teamrankings`, …), and slices one
  spread/OU per `game_id` — spread/OU are model inputs, not passthroughs.
- **`cfbfastR (>= 1.4.0)`** is a hard `Imports` dependency; the pbp schema (down,
  yards_to_goal, timeouts, `pos_team`, 2H-kickoff flag) must match what
  `prepare_cfbfastr_data()` expects. A column-name drift upstream breaks prep.
- 2-pt conversion handling (NEWS 0.1.1): the model no longer assumes a TD = 7.

## Reference

pkgdown site: <https://cfb4th.sportsdataverse.org/>. Update `NEWS.md` under the
current version heading for user-facing changes.

## Commit Convention

[Conventional Commits](https://www.conventionalcommits.org/) — e.g.
`fix: correct go-for-it WP near own goal line`, `docs: refresh README models note`.
Use `type!:` or a `BREAKING CHANGE:` footer for breaking changes.

**Never add AI tools (Claude, Copilot, etc.) as commit co-authors.**
