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
init_verse = function(...) {
  
  wd = getwd()
  
  # check if verse.lock exists
  if (file.exists(file.path(wd, "verse.lock"))) {
    # if it doesn't
    stop(VERSE_CFG)
  }
  
  # check if .Rprofile exists
  if (file.exists(file.path(wd, ".Rprofile"))) {
    # if it doesn't
    stop(RPROFILE_CFG)
  }
  
  msg = glue::glue(
    "Initializing verse project in directory;\n\n",
    "{wd}"
  )
  
  print(msg)
  
  # set project lib and create if missing
  lib_path = file.path(wd, "lib")
  
  if (!dir.exists(lib_path)) {
    dir.create(lib_path)
    restore = TRUE
  }
  
  # add custom libpath in front of existing
  .libPaths(c(lib_path, .libPaths()))
  
  # confirm path set ok
  stopifnot("Unable to set project lib" = identical(.libPaths()[1], lib_path))
  
  write("verse::activate()", ".Rprofile")
  write("/n", "verse.lock")
  
  # verse environment is used primarily to confirm activation by other functions
  verse = new.env(parent = parent.frame())
  assign("project_lib", lib_path, envir = verse)
  
  return(invisible())
}

# NOTE: make these functions
VERSE_CFG = glue::glue(
  "\'verse.lock\' file found in working directory.\n\n",
  "Current working directory;\n\n",
  "{getwd()}\n\n",
  "To activate verse use verse::activate().\n\n",
  "To initialise verse here please remove the\n",
  "\'verse.lock\' file."
)

RPROFILE_CFG = glue::glue(
  "\'.Rprofile\' file found in working directory.\n\n",
  "Current working directory;\n\n",
  "{getwd()}\n\n",
  "To initialise verse here please remove the\n",
  "\'.Rprofile\' file. verse creates a custom\n",
  "\'.Rprofile\' when initialized."
)
