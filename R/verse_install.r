#' Install a package from CRAN
#' 
#' @description The function acts as a wrapper for `remotes::install_version()`.
#'  It allows for the installation of a specific version of a package from CRAN.
#'  The installed package is added to the `verse.lock` file. 
#'  
#' @details Archived packages on CRAN may need to be built from source. As such
#'  the development environment must contain the tools required to perform the
#'  operation. Users can check whether their system is able to build packages
#'  from source using the `has_devel()` function from the `devtools` package.
#'  
#'  The lock file always records a specific version of a package, and future
#'  restoration or rebuilding of the project ensures that those specific
#'  versions are installed.
#'  
#'  Following installation of a package the dependencies of the installed 
#'  package are checked to ensure their installation has taken place. NOTE that
#'  in this case dependencies means only other R packages and other system
#'  dependencies are not considered.
#'  
#' @param pac_name string, the name of a package to install.
#' @param vers string, specifying the version of the package to install.
#' @param verse_dep logical, default `TRUE` value checks the dependencies of the
#'  installed package are installed in the project library. Base packages are
#'  never included in the project library.
#' @param ... Arguments to be passed on to [remotes::install_version()]
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
verse_install = function(
    pac_name,
    vers = NULL,
    verse_dep = TRUE,
    ...
    ) {
  
  # check verse is active
  if (!exists(".verse")) stop("Verse does not appear to be active.")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], .verse$lib_path)) .libPaths(c(.verse$lib_path, .libPaths()))
  
  # confirm path set ok
  if (!identical(.libPaths()[1], .verse$project_lib)) {
    
    actual_lib_1 = .libPaths()[1]
    expected_lib_1 = .verse$project_lib
    
    msg = glue::glue(
      "Primary library not detected as project library.\n\n",
      "Primary library;\n",
      "{actual_lib_1}\n\n",
      "Expected library;\n",
      "{expected_lib_1}\n\n",
      "Try runnning verse::activate_verse"
    )
    
    stop(msg)
  }
  
  remotes::install_version(
    package = pac_name,
    version = vers,
    upgrade = "always"
  )
  
  # Check non base dependencies are in the system library
  if (verse_dep) ._verse_install_dep(.verse$project_lib)
  
  on.exit(write_lock)
  
  return(invisible())
}

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
  
  # check verse is active
  if (!exists(".verse")) stop("Please activate verse")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], .verse$lib_path)) .libPaths(c(.verse$lib_path, .libPaths()))
  
  # identify packages installed in the project library and dependencies
  pac_in_lib = as.data.frame(utils::installed.packages(lib.loc = .verse$project_lib))
  deps = unique(unname(unlist(tools::package_dependencies(pac_in_lib$Package))))
  
  # identify any dependencies not installed
  req_deps = setdiff(deps, pac_in_lib$Package)
  
  # remove base packages from consideration
  req_deps = req_deps[!req_deps %in% rownames(utils::installed.packages(priority="base"))]
  
  # install
  if (length(req_deps) > 0) {
    for (pac in req_deps) {
      verse_install(
        pac,
        version = NULL,
        dependencies = NA,
        upgrade = "always",
        lock_version = FALSE,
        verse_dep = FALSE
      )
    }
  }
  
  return(invisible())
}
