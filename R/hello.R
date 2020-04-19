# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# Reference: https://r-pkgs.org/
#
# Some useful keyboard shortcuts for package authoring:
#   Load Package:              'Cmd + Shift + L'
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "crstatreadr",
    devtools.desc.author = "Michaela Jai-Mei Kane <michaela.j.kane@gmail.com> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()

  # might be best practice to download all data when package is loaded
  # → if we plan on having this do other cool stuff,
  # → then forcing users to download discrete datasets on their own seems silly.
}
