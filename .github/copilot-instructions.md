# GitHub Copilot Instructions -- cfb4th

These instructions tell GitHub Copilot (and other AI coding assistants) how to
write code that fits the `cfb4th` package. They mirror the fuller
[`CLAUDE.md`](../CLAUDE.md) development guide -- when the two differ, treat
`CLAUDE.md` and the current tests as authoritative.

## Package summary

`cfb4th` estimates outcomes of NCAA-football fourth-down plays and computes the
optimal decision (go / field goal / punt) -- the CFB analogue of
[`nfl4th`](https://www.nfl4th.com). It consumes `cfbfastR` play-by-play and the
shared `cfbfastR-data` EP/WP/FG models. R >= 3.5.0, MIT licensed. `main` is the
default and release branch.

## Public surface

Five exported functions, four of them driven by `add_4th_probs()`:

| Function | File | Role |
| --- | --- | --- |
| `add_4th_probs(df)` | `R/wrappers.R` | **Main entry.** Adds go/FG/punt win-prob columns to a 4th-down frame. |
| `get_4th_plays(df)` | `R/get_game_data.R` | Builds the per-play game-state frame `add_4th_probs()` consumes. |
| `load_4th_pbp(seasons)` | `R/wrappers.R` | Season loader; joins betting lines, then calls `add_4th_probs()`. |
| `make_table_data(df)` | `R/table_functions.R` | Formats one play into `gt`-ready table data. |
| `cfb4th_clear_cache(type)` | `R/cache.R` | Wipes the downloaded-model cache. |

Pipeline: `prepare_cfbfastr_data()` -> `prepare_df()` -> `add_probs()`, which
runs `get_go_wp()` / `get_fg_wp()` / `get_punt_wp()` from
`R/decision_functions.R`. Those call `prep_ep()` / `prep_wp()` in `R/helpers.R`,
which also holds the `flip_team()` / `flip_half()` / `end_game_fn()` state
transitions.

## Mandatory conventions

- **Tidy-eval / data-masking.** Use `.data$col` and quoted column names in
  dplyr/tidyr calls. Bare column names raise R CMD check NOTEs; 0.1.2 was a
  tidy-select cleanup pass and should not be regressed.
- **Style.** `styler::tidyverse_style()`. `import(dplyr)`, `%>%` re-exported
  from magrittr, plus `importFrom()` for `nnet::multinom`, `tidyr::pivot_wider`
  and `xgboost::getinfo`.
- **Never hand-edit `NAMESPACE` or `man/*.Rd`** -- regenerate with
  `devtools::document()`.
- **Never hand-edit `README.md`** -- it is knitted from `README.Rmd` via
  `devtools::build_readme()`.
- **Seasons before 2014 are rejected.** `load_4th_pbp()` `stop()`s on them
  deliberately; keep that guard.

## The model-loading gotcha (most important thing here)

Nothing loads at package load. Same shape as `nfl4th`:

- **`ep_model`, `fg_model`**: plain objects in `R/sysdata.rda` (rebuilt by
  `data-raw/sysdata.R` from the `cfbfastR-data` GitHub repo). `ep_model` is an
  `nnet::multinom` (predict with `type = "probs"`); `fg_model` is an `mgcv` bam.
- **`fd_model()`, `wp_model()`**: functions in `R/cache.R`. Raw UBJ byte
  vectors (`saveRDS`) on this repo's `model_archive` GitHub release, downloaded
  on first use, read with `xgboost::xgb.load.raw()`, parsed once per session and
  cached under `tools::R_user_dir("cfb4th", "cache")`, except during a CRAN
  check (`probably_cran()`), where nothing is written.

A failed download is an informative `stop()`; `cfb4th_clear_cache()` (exported)
forces a re-download.

## Other gotchas

- **Betting lines are model inputs, not passthroughs.** `load_4th_pbp()` pulls
  `cfbd_betting_lines()`, factor-ranks providers, and slices one spread/OU per
  `game_id`. Both feed the WP model.
- **`cfbfastR (>= 1.4.0)` is a hard dependency** and the pbp schema (down,
  yards_to_goal, timeouts, `pos_team`, the 2H-kickoff flag) must match what
  `prepare_cfbfastr_data()` expects. Upstream column drift breaks prep.
- Two-point conversions are modelled; do not reintroduce an assumption that a
  touchdown is worth exactly 7.

## Testing

`devtools::test()` -- testthat edition 3, `tests/testthat/test-basic.R`. Run
`devtools::check()` before opening a PR.

## Documentation

`_pkgdown.yml` is the docs config (bootstrap 5); its reference groups list the
four exports, so a new export needs an entry there. Update `NEWS.md` under the
current version heading for user-facing changes.

## Commits

- Use [Conventional Commits](https://www.conventionalcommits.org/)
  (`feat:`, `fix:`, `docs:`, `test:`, `refactor:`, `chore:`). Use `type!:` or a
  `BREAKING CHANGE:` footer for breaking changes.
- **Never** add AI tools as commit co-authors. Omit any `Co-Authored-By`
  trailer that references an AI assistant.

## Cheat sheet

There is a printable one-page reference for this package at
<https://sportsdataverse.org/cheatsheets/cfbplotR-cfb4th-cfbseedR.pdf>, one of
[a set covering every SportsDataverse package](https://sportsdataverse.org/cheatsheets).
Keep it in mind when adding or renaming an exported function: the sheet is a
hand-built canvas, so a surface change means the sheet needs a revision too.
