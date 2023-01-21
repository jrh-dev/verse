#' Install package dependencies from CRAN
#' 
#' @description The function identifies the dependencies of all packages within
#'  the project library. If the project library doesn't contain a dependency
#'  then it is installed and added to the `verse.lock` file. 
#'  
#'  Note that dependencies in this instance refers to R packages and not system
#'  or other dependencies. Base packages are never installed in the project
#'  library.
#'  
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
._verse_install_dep = function(...) {
  
  wd = getwd()
  
  lib_path = file.path(wd, "verselib")
  
  # identify packages installed in the project library and dependencies
  pac_in_lib = as.data.frame(utils::installed.packages(lib.loc = file.path(wd, "verselib")))
  deps = unique(c(pac_in_lib$Package, unname(unlist(tools::package_dependencies(pac_in_lib$Package, recursive = TRUE)))))
  
  # identify any dependencies not installed
  req_deps = setdiff(deps, pac_in_lib$Package)
  
  # remove base packages from consideration
  req_deps = req_deps[!req_deps %in% rownames(utils::installed.packages(priority="base"))]
  
  # install
  if (length(req_deps) > 0) {
    for (pac in req_deps) {
      remotes::install_version(
        package = pac,
        version = NULL,
        upgrade = TRUE,
        dependencies = c("Depends", "Imports", "LinkingTo"),
        ...
      )
    }
  }
  return(invisible())
}

._check_verse = function(dir) {
  
  if (!(dir.exists(file.path(dir, "verselib")) &&
        file.exists(".Rprofile") &&
        file.exists("verse.lock"))) {
    
    # check verse is active
    stop(glue::glue(
      "verse does not appear to be active, please use `verse::activate`."
    ))
    
  }
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], file.path(dir, "verselib"))) {
    
    stop(glue::glue(
      "Unexpected library paths found, please use `verse::activate`."
    ))
    
  }
  
  return(invisible())
}

#' Read and parse a `verse.lock` file.
#' 
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
._read_lock = function(...) {
  
  wd = getwd()
  
  ._check_verse(wd)
  
  lock =  readLines("verse.lock", warn = FALSE)
  lock = lock[lock != ""]
  
  iter = 0
  pac = vector("list", sum(grepl("[[package]]", lock)))
  
  for (line in lock) {
    if (line == "[[package]]") {
      iter = iter + 1
    } else {
      x = strsplit(line, ":")
      pac[[iter]][x[[1]][1]] = x[[1]][2]
    }
  }
  
  return(pac)
}
