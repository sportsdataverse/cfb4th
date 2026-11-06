# Reset the cfb4th model cache

The fourth-down conversion and win-probability models are downloaded the
first time they are needed and cached under
`tools::R_user_dir("cfb4th", "cache")`, in a folder named after the
package version; caches left by other versions are removed when a model
is cached. Clear the cache to force a fresh download, for example after
a model update.

## Usage

``` r
cfb4th_clear_cache(type = c("all", "fd_model", "wp_model"))
```

## Arguments

- type:

  One of `"all"` (the default), `"fd_model"` or `"wp_model"`.

## Value

Returns `TRUE` invisibly once the cache has been cleared.

## Examples

``` r
cfb4th_clear_cache()
```
