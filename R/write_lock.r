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
  
  wd = getwd()
  
  lib_path = file.path(wd, "verselib")
  
  ._check_verse(wd)
  
  # identify packages installed in the project library and dependencies
  pac_in_lib = as.data.frame(utils::installed.packages(lib.loc = lib_path))
  
  if (nrow(pac_in_lib) > 0) {
    
    to_lock = vector("list", nrow(pac_in_lib))
    
    for (ii in seq_len(nrow(pac_in_lib))) {
      to_lock[[ii]] = c(
        pac_in_lib[["Package"]][ii],
        pac_in_lib[["Version"]][ii]
      )
    }
    
    # generate contents of lock file
    locked = lapply(to_lock, function(x) {
      glue::glue(
        "[[package]]\n",
        "package:{x[[1]]}\n",
        "version:{x[[2]]}\n"
      )
    })
    
    lck = filelock::lock("verse.lock", timeout = 5000)
    
    on.exit(filelock::unlock(lck))
    
    write(locked[[1]], "verse.lock", append=FALSE)
    
    for (pac in locked[2:length(locked)]) write(pac, "verse.lock", append=TRUE)
    
    write("\n", "verse.lock", append=TRUE)
    
  }
  
  return(invisible())
}
