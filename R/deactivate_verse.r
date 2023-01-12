
deactivate_verse = function(...) {
  stopifnot("Active verse project not found" = exists("verse"))
  unlink(".Rprofile")
  return(invisible())
}
