## Release summary

This is the first CRAN submission of cfb4th, which estimates the outcomes of
fourth down plays in college football and compares the win probability of
going for it, attempting a field goal and punting. It is the college-football
counterpart of 'nfl4th' (CRAN) and consumes 'cfbfastR' (CRAN) play-by-play.

## Test environments

* local: Windows 10, R 4.6.0 (ucrt)
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  windows-latest (R release), macos-latest (R release)
* win-builder (devel and release): to be run before submission

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

Local runs, all with `--as-cran` and incoming checks on unless stated:

* without a CFBD API key in the environment (as on CRAN),
  `--run-donttest`: 1 NOTE (new submission).
* `_R_CHECK_DEPENDS_ONLY_=true`: 1 NOTE (new submission).
* with the network blocked, `--run-donttest`: Status OK. Every example
  that needs the network fails gracefully inside `try()`.
* with a CFBD API key set, `--run-donttest`: a second NOTE, because the
  `load_4th_pbp()` example takes about 52 s elapsed (64 s CPU) on a loaded
  machine. That example loads and scores a whole season, and runs only when
  `cfbfastR::has_cfbd_key()` is TRUE, so it is skipped on CRAN's machines.

## Notes for the CRAN team

* The two 'xgboost' models (17 MB together) are not bundled. Following
  'nfl4th' (CRAN), each is downloaded the first time it is needed and cached
  under tools::R_user_dir("cfb4th", "cache"), in a folder named after the
  package version. Caching a model removes the caches of other versions and
  prints a message. During R CMD check and on the CRAN check machines nothing
  is written: the check environment is detected and the model is used in
  memory only. The exported cfb4th_clear_cache() removes the cache. A failed
  download stops with an informative message.
* The examples that need the network are wrapped in donttest and try(). The
  `load_4th_pbp()` example also needs a College Football Data API key, so it
  runs only when one is set. The tests that need a download skip on CRAN and
  run on the package's continuous integration on every push.
* There are no published references describing the methods in this package;
  it implements the decision framework of 'nfl4th' for college football.
