#' Initialize verse
#' 
#' @description Used to initialize verse in the working directory which should
#'  be the top level directory of a project. Initializing creates a custom
#'  `.Rprofile` and a private library within the project directory.
#'  
#' @details Initialization also activates verse. Once activated packages are
#'  installed in the project library using `verse::install()`.
#' 
#'  To help avoid unintended consequences verse cannot be initialized if
#'  the directory already contains a `.Rprofile` or `verse.lock` file. If either
#'  of these are present they must be manually removed by the user before
#'  initialization.
#'  
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
init = function(...) {
  
  wd = getwd()
  
  # is it safe to initiate?
  ._safe_init(wd)
  
  message(glue::glue("Initializing verse project in directory; `{wd}`"))
  
  # set project lib, create it if missing
  lib_path = file.path(wd, "verselib")
  
  # create project library
  dir.create(lib_path)
  
  # add custom libpath in front of existing
  .libPaths(c(lib_path, .libPaths()))
  
  # confirm path set ok
  stopifnot("Unable to set project lib" = identical(.libPaths()[1], lib_path))
  
  on.exit(write_lock())
  
  # create .Rprofile & verse.lock file
  write("verse::activate()", ".Rprofile")
  
  return(invisible())
}

._safe_init = function(dir) {
  # check if verse.lock exists
  if (file.exists(file.path(dir, "verse.lock"))) {
    stop(glue::glue(
      "\'verse.lock\' file found in working directory.\n\n",
      "Current working directory;\n\n",
      "{getwd()}\n\n",
      "To activate verse use verse::activate().\n\n",
      "To initialise verse here please remove the\n",
      "\'verse.lock\' file."
    ))
  }
  
  # check if .Rprofile exists
  if (file.exists(file.path(dir, ".Rprofile"))) {
    stop(glue::glue(
      "\'.Rprofile\' file found in working directory.\n\n",
      "Current working directory;\n\n",
      "{getwd()}\n\n",
      "To initialise verse here please remove the\n",
      "\'.Rprofile\' file. verse creates a custom\n",
      "\'.Rprofile\' when initialized."
    ))
  }
  
  # check if verselib exists
  if (dir.exists(file.path(dir, "verselib"))) {
    stop(glue::glue(
      "\'verselib\' directory found in working directory.\n\n",
      "Current working directory;\n\n",
      "{getwd()}\n\n",
      "To initialise verse here please remove the\n",
      "\'verselib\'."
    ))
  }
  return(invisible())
}
