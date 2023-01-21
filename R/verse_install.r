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
#' @param package string, the name of a package to install.
#' @param version string, specifying the version of the package to install.
#' @param upgrade logical, default `TRUE` upgrades dependencies of the specified
#'  package where a version of the dependency is already installed.
#' @param dependencies logical, default `TRUE` installs dependencies of the
#'  specified package.
#' @param ... Arguments to be passed on to [remotes::install_version()]
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
verse_install = function(
    package,
    version = NULL,
    upgrade = TRUE,
    dependencies = c("Depends", "Imports", "LinkingTo"),
    ...
    ) {
  
  if (!is.logical(upgrade)) stop("`upgrade` must be a logical (TRUE/FALSE) value.")
  
  if (!is.logical(dependencies)) stop("`dependencies` must be a logical (TRUE/FALSE) value.")
  
  wd = getwd()
  
  ._check_verse(wd)
  
  on.exit(write_lock())
  
  remotes::install_version(
    package = package,
    version = version,
    upgrade = upgrade,
    dependencies = dependencies,
    ...
  )
  
  # Check non base dependencies are in the verse library
  ._verse_install_dep()
  
  return(invisible())
}
