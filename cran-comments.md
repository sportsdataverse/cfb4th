## Release summary

This is the first CRAN submission of cfb4th (version 0.2.0), which estimates
the outcomes of fourth down plays in college football and compares the win
probability of going for it, attempting a field goal and punting. It is the
college-football counterpart of 'nfl4th' (CRAN) and consumes 'cfbfastR' (CRAN)
play-by-play.

## Test environments

* local: Windows 10 x64, R 4.6.0 (ucrt), `--as-cran --run-donttest`, without
  a CFBD API key in the environment (as on CRAN)
* local: Windows 10 x64, R 4.6.0, `--run-donttest --no-tests` with the
  network blocked
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  windows-latest (R release), macos-latest (R release)
* R-hub v2 (2026-11-06, commit 721e43f): linux, windows, macos and
  macos-arm64, all R-devel: all succeeded. That run was on an earlier
  commit, before the last 0.2.0 fixes (the interactive-only `load_4th_pbp()`
  example and the betting-line handling); it has not been re-run since.
* win-builder (devel and release): to be run before submission

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

With the network blocked, the `--run-donttest --no-tests` check is Status OK. Every
example that needs the network fails gracefully inside `try()`.

Example times, each run alone from an installed build with the models
cached: `add_4th_probs()` 0.61 s CPU / 0.91 s elapsed, `get_4th_plays()`
0.60 s / 0.80 s, `make_table_data()` 0.17 s / 0.19 s,
`cfb4th_clear_cache()` under 0.01 s.

## Reverse dependencies

There are no reverse dependencies (this is a new package).

## Notes for the CRAN team

* The two 'xgboost' models (17 MB together) are not bundled. Following
  'nfl4th' (CRAN), each is downloaded the first time it is needed and cached
  under tools::R_user_dir("cfb4th", "cache"), in a folder named after the
  package version. Caching a model removes the caches of other versions and
  prints a message. During R CMD check and on the CRAN check machines nothing
  is written: the check environment is detected and the model is used in
  memory only. The exported cfb4th_clear_cache() removes the cache; its
  example points the cache at a temporary folder first. A failed download
  stops with an informative message.
* The examples that need the network are wrapped in donttest and try().
  `get_4th_plays()` builds its one-game input inline and needs no API key.
* The `load_4th_pbp()` example runs only in an interactive session with a
  College Football Data API key set: it loads one whole season, the smallest
  call, and scoring its ~10,000 fourth downs is long-running (mostly the
  field-goal model's predictions).
* The tests that need a model download skip on CRAN and run on the package's
  continuous integration on every push.
* There are no published references describing the methods in this package;
  it implements the decision framework of 'nfl4th' for college football.
