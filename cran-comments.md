## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* Fixed use of `installed.packages()`. Use `packageDescription()` instead to retrieve version of each installed package in order to only read a single file (DESCRIPTION) per package. 
This funcitonality is only meant to be run using an installation folder with a limitied ammount of approved packages installed. 
If this solution is still not allowed, we will have to rethink how these packages are checked.
* Improved description of package including the use of 'Quarto'.
* To ensure users do not write to their working directory when using the `run()` examples, everything is know executed
in `tempdir()`. Before it was assumed to be sufficient the examples where not evaluated when installing.
