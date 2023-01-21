#' Verify the project library
#' 
#' @description Used to verify that the contents of the project library adhere
#'  to the conditions set out in the lock file.
#'  
#' @details Packages within the private library are checked for compliance with 
#'  the lock file. Missing packages are installed and packages whose
#'  installations fail to meet the lock file specification are re-installed.
#'  
#'  The function does NOT remove packages that are installed in the project
#'  library and not contained within the lock file.
#'  
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
verify_lock = function(...) {
  
  wd = getwd()
  
  lib_path = file.path(wd, "verselib")
  
  ._check_verse(wd)
  
  on.exit(write_lock())
  
  lock = ._read_lock()
  
  # check verse.lock and ensure all packages in project lib
  ins_pac = as.data.frame(utils::installed.packages(lib.loc = lib_path))$Package
  
  # check presence and version of all packages in lock file
  for (pac in lock) {
    tar_pac = pac["package"]
    tar_ver = pac["version"]
    tar_met = tar_pac %in% ins_pac && tar_ver == utils::packageVersion(tar_pac, lib.loc = lib_path)
    
    # if lock file conditions aren't fulfilled; install
    if (!(tar_met)) {
      remotes::install_version(
        package = pac["package"],
        version = pac["version"],
        upgrade = FALSE,
        dependencies = TRUE
      )
    }
  }
  
  # Check non base dependencies are in the verse library
  ._verse_install_dep()
  
  return(invisible())
}
