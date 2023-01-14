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
  # check verse is active
  if (!exists(".verse")) stop("Verse does not appear to be active.")
  
  # check project lib is 1st in .libPaths and remove
  if (identical(.libPaths()[1], .verse$project_lib)) .libPaths(.libPaths()[2:length(.libPaths())])

  # rm .Rprofile
  unlink(".Rprofile")
  
  return(invisible())
}
