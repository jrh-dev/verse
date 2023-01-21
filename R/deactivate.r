#' Deactivate verse
#' 
#' @description Used to deactivate verse. The `.Rprofile` file is deleted from
#'  the top level of the project directory and the project library is removed
#'  as a library path.
#'  
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
deactivate = function(...) {
  
  wd = getwd()
  
  ._check_verse(wd)
  
  # remove verselib from library paths
  .libPaths(.libPaths()[-1])
  
  # rm .Rprofile
  unlink(".Rprofile")
  
  return(invisible())
}
