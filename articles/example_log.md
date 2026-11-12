# example.R

# example.R

Execution end time

2026-11-12T10:02:25 Coordinated Universal Time

Path to script

/tmp/RtmppnmzPh/file1c7418925508/example.R

# Summary

Tipsuccess

## Input

| Time                | File                   |
|:--------------------|:-----------------------|
| 2026-11-12 10:02:13 | /dev/urandom           |
| 2026-11-12 10:02:15 | /home/runner/.Rprofile |

## Output

| Time | File |
|:---|:---|
| 2026-11-12 10:02:13 | /dev/null |
| 2026-11-12 10:02:14 | /home/runner/.cache/deno/location_data/28cae0874d2cd1a477303d1189f80977214036c3c5b0efb610c4fbbeddbd8b3e/local_storage |
| 2026-11-12 10:02:14 | /home/runner/.cache/deno/location_data/28cae0874d2cd1a477303d1189f80977214036c3c5b0efb610c4fbbeddbd8b3e/local_storage-wal |
| 2026-11-12 10:02:14 | /home/runner/.cache/deno/location_data/28cae0874d2cd1a477303d1189f80977214036c3c5b0efb610c4fbbeddbd8b3e/local_storage-shm |

## Removed

No files

# Script

Setup

``` r
library(dplyr)
```

    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

Prepare data

``` r
x <- mtcars |>
  as_tibble(rownames = "car")

print(x)
```

    [38;5;246m# A tibble: 32 × 12[39m
       car           mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
       [3m[38;5;246m<chr>[39m[23m       [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m
    [38;5;250m 1[39m Mazda RX4    21       6  160    110  3.9   2.62  16.5     0     1     4     4
    [38;5;250m 2[39m Mazda RX4 …  21       6  160    110  3.9   2.88  17.0     0     1     4     4
    [38;5;250m 3[39m Datsun 710   22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
    [38;5;250m 4[39m Hornet 4 D…  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
    [38;5;250m 5[39m Hornet Spo…  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
    [38;5;250m 6[39m Valiant      18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
    [38;5;250m 7[39m Duster 360   14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
    [38;5;250m 8[39m Merc 240D    24.4     4  147.    62  3.69  3.19  20       1     0     4     2
    [38;5;250m 9[39m Merc 230     22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
    [38;5;250m10[39m Merc 280     19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
    [38;5;246m# ℹ 22 more rows[39m

Create and save plot

``` r
ggplot(data = x) +
  geom_point(mapping = aes(x = mpg, y = hp, size = wt, colour = as.factor(am)))
```

![](data:image/png;base64,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)

``` r
ggsave("plot1.png")
```

    [1m[22mSaving 7 x 5 in image

# Session info

NotePlatform

| Setting  | Value                                                          |
|:---------|:---------------------------------------------------------------|
| version  | R version 4.6.2 (2026-10-29)                                   |
| os       | Ubuntu 26.04.1 LTS                                             |
| system   | x86_64, linux-gnu                                              |
| ui       | X11                                                            |
| language | en                                                             |
| collate  | C.UTF-8                                                        |
| ctype    | C.UTF-8                                                        |
| tz       | UTC                                                            |
| date     | 2026-11-12                                                     |
| pandoc   | 3.8.3 @ /opt/hostedtoolcache/pandoc/3.8.3/x64/ (via rmarkdown) |
| quarto   | 1.11.9 @ /usr/local/bin/quarto                                 |

NoteR packages used directly

| Package     | Version | Source |
|:------------|:--------|:-------|
| dplyr       | 1.2.1   | RSPM   |
| ggplot2     | 4.0.3   | RSPM   |
| knitr       | 1.52    | RSPM   |
| reticulate  | 1.47.0  | RSPM   |
| rmarkdown   | 2.32    | RSPM   |
| sessioninfo | 1.2.4   | RSPM   |

NoteR packages used indirectly

| Package      | Version | Source         |
|:-------------|:--------|:---------------|
| cli          | 3.6.6   | RSPM           |
| digest       | 0.6.39  | RSPM           |
| evaluate     | 1.0.5   | RSPM           |
| farver       | 2.1.2   | RSPM           |
| fastmap      | 1.2.0   | RSPM           |
| generics     | 0.1.4   | RSPM           |
| glue         | 1.8.1   | RSPM           |
| gtable       | 0.3.6   | RSPM           |
| htmltools    | 0.5.9   | RSPM           |
| jsonlite     | 2.0.0   | RSPM           |
| labeling     | 0.4.3   | RSPM           |
| lattice      | 0.23-1  | CRAN (R 4.6.2) |
| lifecycle    | 1.0.5   | RSPM           |
| magrittr     | 2.0.5   | RSPM           |
| Matrix       | 1.7-6   | CRAN (R 4.6.2) |
| otel         | 0.3.0   | RSPM           |
| pillar       | 1.11.1  | RSPM           |
| pkgconfig    | 2.0.3   | RSPM           |
| png          | 0.1-9   | RSPM           |
| R6           | 2.6.1   | RSPM           |
| ragg         | 1.5.2   | RSPM           |
| RColorBrewer | 1.1-3   | RSPM           |
| Rcpp         | 1.1.2   | RSPM           |
| rlang        | 1.3.0   | RSPM           |
| S7           | 0.2.2   | RSPM           |
| scales       | 1.4.0   | RSPM           |
| systemfonts  | 1.3.2   | RSPM           |
| textshaping  | 1.0.5   | RSPM           |
| tibble       | 3.3.1   | RSPM           |
| tidyselect   | 1.2.1   | RSPM           |
| utf8         | 1.2.6   | RSPM           |
| vctrs        | 0.7.3   | RSPM           |
| withr        | 3.0.3   | RSPM           |
| xfun         | 0.62    | RSPM           |
| yaml         | 2.3.12  | RSPM           |

NoteEnvironment variables

| Variable | Value |
|:---|:---|
| \_R_CHECK_SYSTEM_CLOCK\_ | FALSE |
| ACCEPT_EULA | Y |
| ACTIONS_ORCHESTRATION_ID | 52dff6fe-eb7f-81cd-87b8-8416148c6872.pkgdown.pkgdown.\_\_default |
| ACTIONS_RUNNER_ACTION_ARCHIVE_CACHE | /opt/actionarchivecache |
| ACTIONS_RUNNER_RETURN_JOB_RESULT_FOR_HOSTED | 1 |
| AGENT_TOOLSDIRECTORY | /opt/hostedtoolcache |
| ANDROID_HOME | /usr/local/lib/android/sdk |
| ANDROID_NDK | /usr/local/lib/android/sdk/ndk/27.3.13750724 |
| ANDROID_NDK_HOME | /usr/local/lib/android/sdk/ndk/27.3.13750724 |
| ANDROID_NDK_LATEST_HOME | /usr/local/lib/android/sdk/ndk/30.0.16248370 |
| ANDROID_NDK_ROOT | /usr/local/lib/android/sdk/ndk/27.3.13750724 |
| ANDROID_SDK_ROOT | /usr/local/lib/android/sdk |
| ANT_HOME | /usr/share/ant |
| AZURE_EXTENSION_DIR | /opt/az/azcliextensions |
| BIBINPUTS | ::/home/runner/work/whirl/whirl/vignettes/articles:/opt/R/4.6.2/lib/R/share/texmf/tex/latex |
| BOOTSTRAP_HASKELL_NONINTERACTIVE | 1 |
| BSTINPUTS | :/home/runner/work/whirl/whirl/vignettes/articles:/opt/R/4.6.2/lib/R/share/texmf/bibtex/bst |
| BUNDLE_EXT | linux-amd64.deb |
| CALLR_IS_RUNNING | true |
| CHROME_BIN | /usr/bin/google-chrome |
| CHROMEWEBDRIVER | /usr/local/share/chromedriver-linux64 |
| CI | true |
| CYGWIN | nodosfilewarning |
| DEBIAN_FRONTEND | noninteractive |
| DENO_DOM_PLUGIN | /opt/quarto/bin/tools/x86_64/deno_dom/libplugin.so |
| DENO_NO_UPDATE_CHECK | 1 |
| DENO_TLS_CA_STORE | system,mozilla |
| DOTNET_MULTILEVEL_LOOKUP | 0 |
| DOTNET_NOLOGO | 1 |
| DOTNET_SKIP_FIRST_TIME_EXPERIENCE | 1 |
| EDGEWEBDRIVER | /usr/local/share/edge_driver |
| EDITOR | vi |
| ENABLE_RUNNER_TRACING | true |
| GECKOWEBDRIVER | /usr/local/share/gecko_driver |
| GHCUP_INSTALL_BASE_PREFIX | /usr/local |
| GITHUB_ACTION | \_\_run |
| GITHUB_ACTION_REF |  |
| GITHUB_ACTION_REPOSITORY |  |
| GITHUB_ACTIONS | true |
| GITHUB_ACTOR | akselthomsen |
| GITHUB_ACTOR_ID | 14150030 |
| GITHUB_API_URL | https://api.github.com |
| GITHUB_ARTIFACTS | /home/runner/work/\_temp/\_runner_file_commands/artifacts_154c1a49-a2ab-4988-88c1-19b28d568761 |
| GITHUB_ARTIFACTS_LIST | /home/runner/work/\_temp/\_runner_file_commands/artifacts_list_154c1a49-a2ab-4988-88c1-19b28d568761 |
| GITHUB_BASE_REF |  |
| GITHUB_ENV | /home/runner/work/\_temp/\_runner_file_commands/set_env_154c1a49-a2ab-4988-88c1-19b28d568761 |
| GITHUB_EVENT_NAME | push |
| GITHUB_GRAPHQL_URL | https://api.github.com/graphql |
| GITHUB_HEAD_REF |  |
| GITHUB_JOB | pkgdown |
| GITHUB_OUTPUT | /home/runner/work/\_temp/\_runner_file_commands/set_output_154c1a49-a2ab-4988-88c1-19b28d568761 |
| GITHUB_REF | refs/heads/main |
| GITHUB_REF_NAME | main |
| GITHUB_REF_PROTECTED | true |
| GITHUB_REF_TYPE | branch |
| GITHUB_REPOSITORY | NovoNordisk-OpenSource/whirl |
| GITHUB_REPOSITORY_ID | 865991741 |
| GITHUB_REPOSITORY_OWNER | NovoNordisk-OpenSource |
| GITHUB_REPOSITORY_OWNER_ID | 129066790 |
| GITHUB_RETENTION_DAYS | 90 |
| GITHUB_RUN_ATTEMPT | 1 |
| GITHUB_RUN_ID | 43196234154 |
| GITHUB_RUN_NUMBER | 561 |
| GITHUB_SERVER_URL | https://github.com |
| GITHUB_SHA | b74ad87513e46dea9cb2e791e4334d1e66241fbf |
| GITHUB_STATE | /home/runner/work/\_temp/\_runner_file_commands/save_state_154c1a49-a2ab-4988-88c1-19b28d568761 |
| GITHUB_STEP_SUMMARY | /home/runner/work/\_temp/\_runner_file_commands/step_summary_154c1a49-a2ab-4988-88c1-19b28d568761 |
| GITHUB_TRIGGERING_ACTOR | akselthomsen |
| GITHUB_WORKFLOW | All actions |
| GITHUB_WORKFLOW_REF | NovoNordisk-OpenSource/whirl/.github/workflows/check_and_co.yaml@refs/heads/main |
| GITHUB_WORKFLOW_SHA | b74ad87513e46dea9cb2e791e4334d1e66241fbf |
| GITHUB_WORKSPACE | /home/runner/work/whirl/whirl |
| GOROOT_1_24_X64 | /opt/hostedtoolcache/go/1.24.13/x64 |
| GOROOT_1_25_X64 | /opt/hostedtoolcache/go/1.25.14/x64 |
| GOROOT_1_26_X64 | /opt/hostedtoolcache/go/1.26.10/x64 |
| GRADLE_HOME | /usr/share/gradle-9.8.2 |
| HCA_CLOUD_PROVIDER | azure |
| HOME | /home/runner |
| HOMEBREW_CLEANUP_PERIODIC_FULL_DAYS | 3650 |
| HOMEBREW_NO_AUTO_UPDATE | 1 |
| ImageOS | ubuntu26 |
| ImageVersion | 20261101.186.1 |
| IN_PKGDOWN | true |
| INVOCATION_ID | fdca9b06bc9946118fc7046593723824 |
| JAVA_HOME | /usr/lib/jvm/temurin-25-jdk-amd64 |
| JAVA_HOME_11_X64 | /usr/lib/jvm/temurin-11-jdk-amd64 |
| JAVA_HOME_17_X64 | /usr/lib/jvm/temurin-17-jdk-amd64 |
| JAVA_HOME_21_X64 | /usr/lib/jvm/temurin-21-jdk-amd64 |
| JAVA_HOME_25_X64 | /usr/lib/jvm/temurin-25-jdk-amd64 |
| JOURNAL_STREAM | 10:15941 |
| LANG | C.UTF-8 |
| LANGUAGE | en |
| LN_S | ln -s |
| LOGNAME | runner |
| MAKE | make |
| MEMORY_PRESSURE_WATCH | /sys/fs/cgroup/system.slice/hosted-compute-agent.service/memory.pressure |
| MEMORY_PRESSURE_WRITE | c29tZSAyMDAwMDAgMjAwMDAwMAA= |
| NOT_CRAN | true |
| NVM_DIR | /home/runner/.nvm |
| PAGER | /usr/bin/pager |
| PATH | /opt/hostedtoolcache/pandoc/3.8.3/x64:/home/runner/work/\_temp/uv-python-dir:/opt/hostedtoolcache/uv/0.13.7/x86_64:/home/runner/.local/bin:/snap/bin:/home/runner/.local/bin:/opt/pipx_bin:/home/runner/.cargo/bin:/home/runner/.config/composer/vendor/bin:/usr/local/.ghcup/bin:/home/runner/.dotnet/tools:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin |
| PIPX_BIN_DIR | /opt/pipx_bin |
| PIPX_HOME | /opt/pipx |
| PKGCACHE_HTTP_VERSION | 2 |
| POWERSHELL_DISTRIBUTION_CHANNEL | GitHub-Actions-Linux |
| PROCESSX_PS1b6d5a9eed50_1794477730 | YES |
| PROCESSX_PS1c74709d5946_1794477732 | YES |
| PROCESSX_PS1c923897529b_1794477733 | YES |
| PSModulePath | /root/.local/share/powershell/Modules:/usr/local/share/powershell/Modules:/opt/microsoft/powershell/7/Modules:/usr/share/az_15.6.1 |
| PWD | /tmp/RtmppnmzPh/file1c744b4173ef |
| QUARTO_DENO | /opt/quarto/bin/tools/x86_64/deno |
| QUARTO_DOCUMENT_FILE | dummy.qmd |
| QUARTO_EXECUTE_INFO | /tmp/RtmppnmzPh/file1c744b4173ef/.quarto/quarto-session-temp7e5d6b3576125d38/2f37f6e227b64ccd |
| QUARTO_PROFILE |  |
| QUARTO_PROJECT_DIR | /tmp/RtmppnmzPh/file1c744b4173ef |
| QUARTO_PROJECT_ROOT | /tmp/RtmppnmzPh/file1c744b4173ef |
| QUARTO_ROOT | / |
| R_ARCH |  |
| R_BROWSER | false |
| R_BZIPCMD | /usr/bin/bzip2 |
| R_CLI_NUM_COLORS | 256 |
| R_DOC_DIR | /opt/R/4.6.2/lib/R/doc |
| R_GZIPCMD | /usr/bin/gzip |
| R_HOME | /opt/R/4.6.2/lib/R |
| R_INCLUDE_DIR | /opt/R/4.6.2/lib/R/include |
| R_LIB_FOR_PAK | /opt/R/4.6.2/lib/R/site-library |
| R_LIBS_SITE | /opt/R/4.6.2/lib/R/site-library |
| R_LIBS_USER | /home/runner/work/\_temp/Library |
| R_PAPERSIZE | letter |
| R_PAPERSIZE_USER | letter |
| R_PDFVIEWER | false |
| R_PLATFORM | x86_64-pc-linux-gnu |
| R_PRINTCMD | /usr/bin/lpr |
| R_RD4PDF | times,inconsolata,hyper |
| R_SESSION_TMPDIR | /tmp/RtmpV0DRGp |
| R_SHARE_DIR | /opt/R/4.6.2/lib/R/share |
| R_STRIP_SHARED_LIB | strip --strip-unneeded |
| R_STRIP_STATIC_LIB | strip --strip-debug |
| R_TESTS |  |
| R_TEXI2DVICMD | /usr/bin/texi2dvi |
| R_UNZIPCMD | /usr/bin/unzip |
| R_ZIPCMD | /usr/bin/zip |
| RENV_CONFIG_REPOS_OVERRIDE | https://packagemanager.posit.co/cran/\_\_linux\_\_/resolute/latest |
| RSPM | https://packagemanager.posit.co/cran/\_\_linux\_\_/resolute/latest |
| RUNNER_ARCH | X64 |
| RUNNER_ENVIRONMENT | github-hosted |
| RUNNER_NAME | GitHub Actions 1000039290 |
| RUNNER_OS | Linux |
| RUNNER_TEMP | /home/runner/work/\_temp |
| RUNNER_TOOL_CACHE | /opt/hostedtoolcache |
| RUNNER_TRACKING_ID | github_220f8587-54c4-4d1a-99dd-be4b14927952 |
| RUNNER_WORKSPACE | /home/runner/work/whirl |
| SED | /usr/bin/sed |
| SGX_AESM_ADDR | 1 |
| SHELL | /bin/bash |
| SHLVL | 1 |
| SYSTEMD_EXEC_PID | 2179 |
| TAR | /usr/bin/tar |
| TERM | dumb |
| TEXINPUTS | :/home/runner/work/whirl/whirl/vignettes/articles:/opt/R/4.6.2/lib/R/share/texmf/tex/latex |
| TZ | UTC |
| USE_BAZEL_FALLBACK_VERSION | silent: |
| USER | runner |
| UV_CACHE_DIR | /home/runner/work/\_temp/setup-uv-cache |
| UV_PYTHON_INSTALL_DIR | /home/runner/work/\_temp/uv-python-dir |
| VCPKG_INSTALLATION_ROOT | /usr/local/share/vcpkg |
| VIRTUAL_ENV | /home/runner/work/whirl/whirl/.venv |
| WHIRL_LOG_MSG | /tmp/RtmppnmzPh/file1c744b4173ef/log_msg.json |
| XDG_CONFIG_HOME | /home/runner/.config |
| XDG_RUNTIME_DIR | /run/user/1001 |

NoteOptions

| Option | Value |
|:---|:---|
| add.smooth | TRUE |
| ambiguousMethodSelection | function (cond) , NULL |
| bitmapType | cairo |
| browser | false |
| browserNLdisabled | FALSE |
| callr.condition_handler_cli_message | function (msg) , {, custom_handler \<- getOption("cli.default_handler"), if (is.function(custom_handler)) {, custom_handler(msg), }, else {, cli_server_default(msg), }, } |
| catch.script.errors | FALSE |
| CBoundsCheck | FALSE |
| check.bounds | FALSE |
| citation.bibtex.max | 1 |
| continue | \+ |
| contrasts | contr.treatment, contr.poly |
| defaultPackages | datasets , utils , grDevices, graphics , stats , methods |
| demo.ask | default |
| deparse.cutoff | 60 |
| device | function (width = 7, height = 7, ...) , {, grDevices::pdf(NULL, width, height, ...), } |
| device.ask.default | FALSE |
| digits | 7 |
| dvipscmd | dvips |
| echo | FALSE |
| editor | vi |
| encoding | native.enc |
| example.ask | default |
| expressions | 5000 |
| help.search.types | vignette, demo , help |
| help.try.all.packages | FALSE |
| htmltools.preserve.raw | TRUE |
| HTTPUserAgent | R/4.6.2 R (4.6.2 x86_64-pc-linux-gnu x86_64 linux-gnu) on GitHub Actions |
| internet.info | 2 |
| keep.parse.data | TRUE |
| keep.parse.data.pkgs | FALSE |
| keep.source | FALSE |
| keep.source.pkgs | FALSE |
| knitr.in.progress | TRUE |
| locatorBell | TRUE |
| mailer | mailto |
| matprod | default |
| max.contour.segments | 25000 |
| max.print | 99999 |
| menu.graphics | TRUE |
| na.action | na.omit |
| Ncpus | 1 |
| nwarnings | 50 |
| OutDec | . |
| pager | /opt/R/4.6.2/lib/R/bin/pager |
| papersize | letter |
| PCRE_limit_recursion | NA |
| PCRE_study | FALSE |
| PCRE_use_JIT | TRUE |
| pdfviewer | false |
| pkgType | source |
| printcmd | /usr/bin/lpr |
| prompt | \> |
| quiet | TRUE |
| repos | https://packagemanager.posit.co/cran/\_\_linux\_\_/resolute/latest, https://cran.rstudio.com |
| rlang_trace_top_env | \<environment: R_GlobalEnv\> |
| scipen | 0 |
| show.coef.Pvalues | TRUE |
| show.error.messages | TRUE |
| show.signif.stars | TRUE |
| showErrorCalls | TRUE |
| showNCalls | 50 |
| showWarnCalls | FALSE |
| str | no, 3, 4, 99, TRUE, function (x, ...) , format(x, trim = TRUE, drop0trailing = TRUE, ...) |
| str.dendrogram.last | \` |
| texi2dvi | /usr/bin/texi2dvi |
| tikzMetricsDictionary | dummy-tikzDictionary |
| timeout | 60 |
| try.outFile | 3 |
| ts.eps | 1e-05 |
| ts.S.compat | FALSE |
| unzip | /usr/bin/unzip |
| useFancyQuotes | FALSE |
| verbose | FALSE |
| warn | 0 |
| warning.length | 1000 |
| warnPartialMatchArgs | FALSE |
| warnPartialMatchAttr | FALSE |
| warnPartialMatchDollar | FALSE |
| width | 80 |
