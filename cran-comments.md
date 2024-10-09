## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.4.1
* win-builder
* macOS builder
* ubuntu-latest on GitHub
* macos-13 on GitHub
* macos-latest on GitHub
* windows-latest on GitHub

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √


## Notes
* win-builder returns two notes: (1) "CRAN-pack does not know about .github", which is not returned otherwise (.github is needed for rhub and is in the .Rbuildignore); and (2) "This is a new release.""
