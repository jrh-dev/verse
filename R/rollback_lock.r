#' Rollback a `verse.lock` file to a previous state.
#' 
#' @description When a change is made to the lock file the prior configuration
#'  is stored whilst the session remains active. This allows a brief opportunity
#'  to test code after updating or installing a package and rollback the lock
#'  file if problems are identified.
#'
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
rollback_lock = function(...) {
  
  # check last config is available
  if (!exists("verse$previous_lock")) stop("Previous configuaration not known.")
  
  locked = lapply(verse$previous_lock, function(x) {
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
  
  return(invisible())
}
