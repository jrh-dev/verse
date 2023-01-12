
deactivate = function(...) {
  # check verse is active
  stopifnot("Active verse project not found" = exists("verse"))
  
  # check project lib is 1st in .libPaths and remove
  if (identical(.libPaths()[1], verse$project_lib)) .libPaths(.libPaths()[2:length(.libPaths())])

  # rm .Rprofile
  unlink(".Rprofile")
  
  return(invisible())
}
