#' Activate verse
#' 
#' @description Used to activate an existing verse setup. Activation requires
#'  the current working directory to contain a custom `.Rprofile` and 
#'  `verse.lock` file created by verse.
#'  
#' @details Once activated packages are installed in the project library using 
#' `verse::install()`.
#' 
#'  Packages within the private library are checked for compliance with 
#'  the lock file upon activation. If the private library is not found then it
#'  will be created and rebuilt with package installation taking place based on
#'  the lock file.
#'  
#'  Base packages are not recorded in the lock file and are accessed according 
#'  to the users additional library paths in the way they would if verse was not
#'  being used. 
#'  
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
activate = function(...) {
  
  wd = getwd()
  
  lib_path = file.path(wd, "verselib")
  
  # check verse.lock exists
  if (!file.exists(file.path(wd, "verse.lock"))) {
    
    # if it doesn't
    stop(
      glue::glue(
        "\'verse.lock\' file not found in working directory.\n\n",
        "Current working directory;\n\n",
        "{wd}\n\n",
        "To initialise verse use verse::init()"
      )
    )
  }
  
  # set verse lib as primary path if not already
  if (!identical(.libPaths()[1], lib_path)) {
    # add custom lib path in front of existing
    .libPaths(c(lib_path, .libPaths()))
    # confirm path set correctly
    stopifnot("Unable to set project .libPaths" = identical(.libPaths()[1], lib_path))
    
  }
  
  # read lock file
  lock_state = ._read_lock()
  
  # check whether verse lib exists and restore or verify
  if (any(grepl("verselib", list.dirs(wd, recursive = FALSE)))) {
    verify_lock(lock_state)
  } else {
    restore_lock(lock_state)
  }
  
  write("verse::activate()", ".Rprofile")
  
  return(invisible())
}
