#' Restore the project library
#' 
#' @description Used to restore the project library, that is all packages
#'  contained within the lock file are installed.
#'  
#' @details The function does NOT remove packages that are already installed in
#'  the project library when it is called.
#'  
#' @param lock R list object containing the parsed contents of the lock file.
#'  The `read_lock()` function must be used to generate the required object.
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
restore_lock = function(lock, ...) {
  
  # check verse is active
  if (!exists("verse")) stop("Please activate verse")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], verse$lib_path)) .libPaths(c(verse$lib_path, .libPaths()))
  
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
