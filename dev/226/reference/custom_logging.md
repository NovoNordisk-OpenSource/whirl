# Helper function to log custom messages

Useful for e.g. read and write operations on databases etc. that are not
automatically captured.

## Usage

``` r
log_read(file, log = Sys.getenv("WHIRL_LOG_MSG"))

log_write(file, log = Sys.getenv("WHIRL_LOG_MSG"))

log_delete(file, log = Sys.getenv("WHIRL_LOG_MSG"))
```

## Arguments

- file:

  [`character()`](https://rdrr.io/r/base/character.html) description of
  the file that was read, written or deleted.

- log:

  [`character()`](https://rdrr.io/r/base/character.html) path to the log
  file.

## Details

The default environment variable `WHIRL_LOG_MSG` is set in the session
used to log scripts, and input is automatically captured in the
resulting log.

If run outside of whirl, meaning when the above environment variable is
unset, the operations are streamed to
[`stdout()`](https://rdrr.io/r/base/showConnections.html). By default
the console.

## Examples

``` r
# Stream logs to console since `WHIRL_LOG_MSG` is not set:
log_read("my/folder/input.txt")
#> {"time":"2026-01-19 08:49:49","type":"read","file":"my/folder/input.txt"}
log_write("my/folder/output.txt")
#> {"time":"2026-01-19 08:49:49","type":"write","file":"my/folder/output.txt"}
log_delete("my/folder/old_output.txt")
#> {"time":"2026-01-19 08:49:50","type":"delete","file":"my/folder/old_output.txt"}
```
