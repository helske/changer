#' Change Name of Existing R Package
#' 
#' Changing the name of an existing R package is annoying but common task 
#' especially in the early stages of package development. Function \code{changer} 
#' tries to automate this task:
#' 
#' First all complete words matching the package name are replaced in all R scripts, 
#' C/C++/Fortran/Stan source codes, markdown files (essentially files with extension 
#' \code{R}, \code{r}, \code{cpp}, \code{c}, \code{h},
#' \code{f}, \code{f90}, \code{f95}, \code{stan}, \code{md}, \code{Rmd}, \code{Rnw}, \code{html}, 
#' or \code{bib}), as well as \code{DESCRIPTION}, \code{NAMESPACE}, \code{ints/CITATION}, 
#' \code{.Rbuildignore}, and \code{.gitignore} files.
#' 
#' 
#' Note that if the package is already published in CRAN, then the name change 
#' is more problematic (you need to ask CRAN for permission first).
#' 
#' If the package is also available in Github, then you need to do the following:
#' 
#' Go to the URL of your Github package, click Settings near the top-right, change the name under "Repository name", and finally click Rename.
#' 
#' Warnings: 
#' 
#' If the current name of your package is just some commonly used word (such as "package"), then you are in trouble, as 
#' find and replace will change all of those words to \code{new_name} as well. 
#' 
#' If you have a function with same name as your package, that will change as well.
#' 
#' It is strongly recommended to have a backup backup before proceeding.
#' 
#' Inspired by Nick Tierney's blog post: https://www.njtierney.com/post/2017/10/27/change-pkg-name/
#' 
#' @param path Path of the package.
#' @param new_name Desired name of the package.
#' @param check_validity Check first if the package name is valid and available by running the 
#' function \code{available} from the package \code{available} Default is TRUE.
#' @param change_git If \code{TRUE} (default), changes the remote url of the remote. Note that
#' you still need to change the name of the GitHub repository manually as follows: 
#' Go to the URL of your Github package, click Settings, change the name under "Repository name", and click Rename.
#' @param run_roxygen Should the package documentation be updated via roxygen? If \code{TRUE}, removes all old \code{Rd} files 
#' in \code{man} directory.
#' @param remote_name Name of the remote. Defaults to \code{git2r::remotes(repo)[1]}.
changer <- function(path, new_name,  check_validity = TRUE, change_git = TRUE, run_roxygen = FALSE, remote_name = NULL) {
  
  if (!file.exists(f <- path)) 
    stop(paste0("Path '", f, "' does not exist. "))
  
  if (check_validity) {
    valid <- available::available(new_name, browse = FALSE)
    print(valid)
  }
  
  keep_going <- utils::askYesNo(paste0("Warning! This function modifies the contents and names of the files within the path '", path, "'. Do you wish to continue?"))
  
  if (isFALSE(keep_going)) return()
  
  split_path <- strsplit(path, .Platform$file.sep)[[1]]
  old_name <- split_path[length(split_path)]
  
  if (file.exists(f <- file.path(utils::head(split_path, length(split_path) - 1), new_name))) 
    stop(paste0("Path '", f, "' already exists. "))
  
  # all files and dirs:
  R_files <- list.files(path, all.files = TRUE, recursive = TRUE, include.dirs = FALSE, 
                        full.names = TRUE, pattern = "\\(.R|.r)$")
  c_or_cpp_files <- list.files(path, all.files = TRUE, recursive = TRUE, include.dirs = FALSE, 
                               full.names = TRUE, pattern = "\\.(cpp|c|h)$")
  fortran_files <- list.files(path, all.files = TRUE, recursive = TRUE, include.dirs = FALSE, 
                              full.names = TRUE, pattern = "\\.(f|f90|f95)$")
  stan_files <- list.files(path, all.files = TRUE, recursive = TRUE, include.dirs = FALSE, 
                           full.names = TRUE, pattern = "\\.(stan)$")
  md_files <- list.files(path, all.files = TRUE, recursive = TRUE, include.dirs = FALSE, 
                         full.names = TRUE, pattern = "\\.(md|Rmd|Rnw|html|bib)$")
  
  files <- c(R_files, c_or_cpp_files, fortran_files, stan_files, md_files, "DESCRIPTION", "NAMESPACE",
             if (file.exists(paste(path, ".Rbuildignore", .Platform$file.sep))) paste0(path, "/.Rbuildignore"), 
             if (file.exists(paste(path, ".gitignore", .Platform$file.sep))) paste0(path, "/.gitignore"),
             if (file.exists(paste(path, "inst/CITATION", .Platform$file.sep))) paste0(path, "/inst/CITATION"))
  
  # change contents
  pattern <- paste0('\\b', old_name, '\\b')
  for (f in files) {
    old_content <- readLines(f)
    new_content <- gsub(pattern, new_name, old_content)
    cat(new_content, file = f, sep = "\n")
  }
  
  # change file names
  if (file.exists(f <- file.path(path, paste0(old_name, ".Rproj")))) {
    file.rename(f, file.path(path, paste0(new_name, ".Rproj")))
  }
  if (file.exists(f <- file.path(path, "src", paste0(old_name, ".c")))) {
    file.rename(f, file.path(path, "src", paste0(new_name, ".c")))
  }
  if (file.exists(f <- file.path(path, "src", paste0(old_name, ".cpp")))) {
    file.rename(f, file.path(path, "src",  paste0(new_name, ".cpp")))
  }
  if (file.exists(f <- file.path(path, "src",  paste0(old_name, ".h")))) {
    file.rename(f,  file.path(path, "src",  paste0(new_name, ".h")))
  }
  if (file.exists(f <- file.path(path, "vignettes",  paste0(old_name, ".Rmd")))) {
    file.rename(f, file.path(path, "vignettes",  paste0(new_name, ".Rmd")))
  }
  if (file.exists(f <- file.path(path, "vignettes",  paste0(old_name, ".Rnw")))) {
    file.rename(f, file.path(path, "vignettes",  paste0(new_name, ".Rnw")))
  }
  if (file.exists(f <- file.path(path, "vignettes",  paste0(old_name, ".pdf")))) {
    file.rename(f, file.path(path, "vignettes",  paste0(new_name, ".pdf")))
  }
  if (file.exists(f <- file.path(path, "vignettes", paste0(old_name, ".html")))) {
    file.rename(f, file.path(path, "vignettes", paste0(new_name, ".html")))
  }
  if (file.exists(f <- file.path(path, "vignettes",  paste0(old_name, ".bib")))) {
    file.rename(f, file.path(path, "vignettes",  paste0(new_name, ".bib")))
  }
  if (file.exists(f <- file.path(path, "R", paste0(old_name, "-package.R")))) {
    file.rename(f, file.path(path, "R", paste0(new_name, "-package.R")))
  }
  if (file.exists(f <- file.path(path, "R", paste0(old_name, "-defunct.R")))) {
    file.rename(f, file.path(path, "R", paste0(new_name, "-defunct.R")))
  }
  if (file.exists(f <- file.path(path, "R", paste0(old_name, "-deprecated.R")))) {
    file.rename(f, file.path(path, "R", paste0(new_name, "-deprecated.R")))
  }
  
  if (run_roxygen) {
    # remove old Rd files
    Rd_files <- list.files(file.path(path, "man"), pattern = "\\.(Rd)$", full.names = TRUE)
    for (f in Rd_files) {
      file.remove(f)
    }
  }
  # remove to force rebuild, just to be safe...
  if (file.exists(f <- file.path(path, "src", paste0(old_name, ".o")))) {
    file.remove(f)
  }
  if (file.exists(f <- file.path(path, "/src", paste0(old_name, ".so")))) {
    file.remove(f)
  }
  if (file.exists(f <- file.path(path, "/src", paste0(old_name, ".dll")))) {
    file.remove(f)
  }
  
  # rename directory
  if (.Platform$OS.type == "windows") {
    shell(paste("rename", utils::shortPathName(normalizePath(path)), new_name))
  } else {
    system2("mv", args = c("-T", normalizePath(path), new_name))
  }
  
  if (!is.null(change_git)) {
    repo <- git2r::repository(path)
    remote <- git2r::remote_url(repo)
    if (is.null(remote_name)) remote_name <- git2r::remotes(repo)[1]
    git2r::remote_set_url(repo, name = remote_name, newname = gsub(old_name, new_name, remote))
    
  # change repo name in GitHub
  # Can you do this securely? Maybe with tokens,
  # but fetching the token from Github website takes
  # same amount of time as renaming via website...
  # calling system curl doesn't activate prompt in Windows..
  # if (grepl("github.com", remote)) {
  #   split_repo <- strsplit(remote, "/")[[1]]
  #   owner <- split_repo[length(split_repo) - 1]
  #   httr::PATCH(url = paste0("https://api.github.com/repos/", owner, "/", new_name),
  #               httr::accept_json(), httr::content_type_json(), encode = "json",
  #               body = list(name = old_name), 
  #               httr::authenticate(user = "me", password = "idontwanttogiveittoyou", type = "basic"))
  # }
  }
  
  if (run_roxygen) {
    devtools::document(roclets = 
                         c("namespace", "rd", 
                           if (any(grepl("Collate", c(readLines("DESCRIPTION"))))) "collate"))
  }
}
