#' Restore the project library
#' 
#' @description Used to restore the project library; all packages contained 
#'  within the lock file are installed.
#'  
#' @details The function does NOT check whether packages are already installed
#'  in the project library when it is called, rather all packages are
#'  re-installed.
#'  
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
restore_lock = function(...) {
  
  wd = getwd()
  
  lib_path = file.path(wd, "verselib")
  
  ._check_verse(wd)
  
  on.exit(write_lock())
  
  lock = ._read_lock()
  
  for (x in seq_len(length(lock))) {
    remotes::install_version(
      package = lock[[x]][["package"]],
      version = lock[[x]][["version"]],
      upgrade = FALSE,
      dependencies = TRUE
    )
  }
  
  # Check non base dependencies are in the verse library
  ._verse_install_dep()
  
  return(invisible())
}
