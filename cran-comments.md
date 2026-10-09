## Release summary

This is the first CRAN submission of cfb4th, which estimates the outcomes of
fourth down plays in college football and compares the win probability of
going for it, attempting a field goal and punting. It is the college-football
counterpart of 'nfl4th' (CRAN) and consumes 'cfbfastR' (CRAN) play-by-play.

## Test environments

* local: Windows 10, R 4.6.1
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  windows-latest (R release), macos-latest (R release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

On Windows with R 4.6.1 the local check additionally reports an empty
directory named 'NULL' under "non-standard things in the check directory".
It is an R artifact (R CMD check runs examples and tests with
R_LIBS_USER='NULL' and R 4.6.1 on Windows creates that directory at
startup), reproduces with any package, and does not occur on Linux, macOS or
earlier R versions.

## Notes for the CRAN team

* The two 'xgboost' models (17 MB together) are not bundled. Following
  'nfl4th' (CRAN), each is downloaded the first time it is needed and cached under
  tools::R_user_dir("cfb4th", "cache"); during R CMD check and on the CRAN
  check machines nothing is written (the check environment is detected and
  the model is used in memory only). The exported cfb4th_clear_cache()
  removes the cache. A failed download stops with an informative message.
* The examples that score plays are wrapped in donttest and try(); the tests
  that need a download skip on CRAN and run on the package's continuous
  integration on every push.
* There are no published references describing the methods in this package;
  it implements the decision framework of 'nfl4th' for college football.
