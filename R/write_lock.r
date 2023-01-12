write_lock = function(to_lock, ...) {
  locked = lapply(to_lock, function(x) {
    glue::glue(
      "[[package]]\n",
      "package:{x[[1]]}\n",
      "version:{x[[2]]}\n",
      "dependencies:{x[[3]]}\n",
      "upgrade:{x[[4]]}\n\n"
    )
  })
  unlink("verse.lock")
  file.create("verse.lock")
  for (pac in locked) write(pac, "verse.lock", append=TRUE)
  write("\n", "verse.lock", append=TRUE)
  return(invisible())
}
