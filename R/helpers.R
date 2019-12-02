create_if_needed <- function(path, ...) {
  if (!dir.exists(path)) {
    dir.create(path, ...)
  }
}

name2path <- function(dir, name, ext) {
  file.path(dir, paste0(name, ext))
}

remove_ext <- function(filename) {
  gsub(pattern = "(.+)\\.([^\\.]+)",
       replacement = "\\1",
       filename)
}

path2name <- function(path) {
  remove_ext(basename(path))
}
