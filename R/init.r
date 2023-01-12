#' Initialize verse
#' 
#' @description Used to initialize verse in the working directory which should
#'  be the top level directory of a project. Initializing creates a custom
#'  `.Rprofile` and a private library within the project directory.
#'  
#'  To avoid unintended usage verse cannot be initialized if the directory
#'  already contains a `.Rprofile` or `verse.lock` file. The must be manually
#'  removed by the user before initialization.
#'  
#'  @param ... Unused arguments, for future development.
#'  
#'  @return Invisible, the function is called for it's side effects.
#'  @export
init = function(...) {
  
  wd = getwd()
  
  # check if verse.lock exists
  if (file.exists(file.path(wd, "verse.lock"))) {
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
  if (file.exists(file.path(wd, ".Rprofile"))) {
    stop(glue::glue(
      "\'.Rprofile\' file found in working directory.\n\n",
      "Current working directory;\n\n",
      "{getwd()}\n\n",
      "To initialise verse here please remove the\n",
      "\'.Rprofile\' file. verse creates a custom\n",
      "\'.Rprofile\' when initialized."
    ))
  }
  
  # check if project library exists
  if (dir.exists(file.path(wd, "lib"))) {
    stop(glue::glue(
      "\'Lib directory\' found in working directory.\n\n",
      "Current working directory;\n\n",
      "{getwd()}\n\n",
      "To initialise verse here please remove the\n",
      "\'Lib directory\' file. verse creates a custom\n",
      "\'Lib directory\' when initialized."
    ))
  }

  message(glue::glue("Initializing verse project in directory; `{wd}`"))
  
  # set project lib, create it if missing
  lib_path = file.path(wd, "lib")
  
  # verse environment is used primarily to confirm activation by other functions
  verse = new.env(parent = parent.frame())
  assign("verse_lib", lib_path, envir = verse)
  
  # add custom libpath in front of existing
  .libPaths(c(verse$lib_path, .libPaths()))
  
  # confirm path set ok
  stopifnot("Unable to set project lib" = identical(.libPaths()[1], verse$lib_path))
  
  # create .Rprofile & verse.lock
  write("verse::activate()", ".Rprofile")
  write("/n", "verse.lock")
  
  return(invisible())
}
