#' Write a `verse.lock` file.
#' 
#' @description Record the current state of the project library to the lock
#' file.
#' 
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
write_lock = function(...) {
  
  # check verse is active
  if (!exists(".verse")) stop("Please activate verse")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], .verse$lib_path)) .libPaths(c(.verse$lib_path, .libPaths()))
  
  # back up lock file
  .verse$previous_lock = ._read_lock()
  
  # identify packages installed in the project library and dependencies
  pac_in_lib = as.data.frame(utils::installed.packages(lib.loc = .verse$project_lib))
  
  if (nrow(pac_in_lib) > 0) {
    
    for (ii in seq_len(nrow(pac_in_lib))) {
      to_lock[[ii]] = c(
        pac_in_lib[["Package"]][ii],
        pac_in_lib[["Version"]][ii],
        NA,
        "always"
      )
    }
    
    # generate contents of lock file
    locked = lapply(to_lock, function(x) {
      glue::glue(
        "[[package]]\n",
        "package:{x[[1]]}\n",
        "version:{x[[2]]}\n",
        "dependencies:{x[[3]]}\n",
        "upgrade:{x[[4]]}\n\n"
      )
    })
    
    # remove old lock file and write new one
    unlink("verse.lock")
    file.create("verse.lock")
    
    for (pac in locked) write(pac, "verse.lock", append=TRUE)
   
    write("\n", "verse.lock", append=TRUE)
  }
  
  return(invisible())
}
