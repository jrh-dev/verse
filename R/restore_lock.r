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
  
  # check verse is active
  if (!exists(".verse")) stop("Verse does not appear to be active.")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], .verse$lib_path)) .libPaths(c(.verse$lib_path, .libPaths()))
  
  lock = ._read_lock()
  
  for (x in seq_len(length(lock))) {
    remotes::install_version(
      package = lock[[x]][["package"]],
      version = lock[[x]][["version"]],
      dependencies = unname(unlist(strsplit((lock[[1]][["dependencies"]]), " "))),
      upgrade = lock[[x]][["upgrade"]]
    )
  }
  return(invisible())
}
