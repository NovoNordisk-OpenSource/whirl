## R CMD check results

0 errors | 0 warnings | 1 note

* Patch update improving the logs.
* Removed `verbosity_level` argument to `run()` since it is now completely controlled
by zephyr options (see `help("whirl-options")`). No reverse dependencies.
* [Check result errors](https://cran.r-project.org/web/checks/check_results_whirl.html)
should be fixed in [#197](https://github.com/NovoNordisk-OpenSource/whirl/pull/197)
