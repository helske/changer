# changer
<!-- badges: start -->
[![R-CMD-check](https://github.com/helske/changer/workflows/R-CMD-check/badge.svg)](https://github.com/helske/changer/actions)
<!-- badges: end -->


Changing the name of an existing R package is annoying but common task in the early stages of package development (at least for me). Package `changer` tries to automate this task:

1.  Validity and availability of the new package name is checked by using `available` from [available](https://CRAN.R-project.org/package=available)). 
2.  *All* complete words matching the package name are replaced in all R scripts,
    C/C++/Fortran/Stan source codes, markdown files, and typical files in R packages,
    i.e. files with extension `.R`, `.cpp`, `.c`, `.h`, `.f`, `.f90`, `.f95`, `.stan`, `.md`, `.Rmd`, `.Rnw`, 
    `.html`, and `.bib`, as well as files `DESCRIPTION`, `NAMESPACE` `inst/CITATION`, `.Rbuildignore`,
    and `gitignore`.
    
3.  Change file names containing the package name. Only checks files with extensions listed above,
    as well as files `[oldname].Rproj`, `[oldname]-package.R`, `[oldname]-defunct.R`, and `[oldname]-deprecated.R`.
4.  If `run_roxygen` is `TRUE` (default), old `Rd` files are removed.
5.  All files with extensions `.o`, `.so`, and `.dll` in `/src` are removed to force a rebuild of the package.
6.  Name of the package directory is changed.
7.  If `change_git` is `TRUE` (default), the remote url of the git repository is changed to match the new name. Note that
    *you still need to change the name of the repository in Github/Bitbucket etc manually*. For example in Github:
    Go to the URL of your Github package, click *Settings* near the top-right, change the name under *Repository name*, and click *Rename*.
8.  If `run_roxygen` is `TRUE`, update documentation and rebuild package.



Inspired by back-and-forth naming of the package [particlefields](https://github.com/helske/particlefields)) and [Nick Tierney's blog post]( https://www.njtierney.com/post/2017/10/27/change-pkg-name/).


## Additional issues (read above as well)

*  If the package is already published in [CRAN](https://CRAN.R-project.org), then you should first consult folks at CRAN whether they will accept the name change.

* It is strongly recommended to have a backup backup before proceeding.

## Installation

You can install the developed version of changer from Github with:

``` r
devtools::install_github("helske/changer")
```

## Usage

There is just one function in the package:
``` r
changer("../mypackagewithlongandboringname", "Rbitary")
```

## News

- `changer` now tests that the suplied path actually contains an R package by checking the existence of the `DESCRIPTION` file.

