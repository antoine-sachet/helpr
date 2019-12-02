write_list <- function(lst, path) {
  if (is.null(names(lst)) || any("" %in% names(lst))) {
    warning("List elements should all have names")
  }

  create_if_needed(path)

  purrr::imap(lst, function(el, name) {
    if ("list" %in% class(el)) {
      subpath <- file.path(path, name)
      create_if_needed(subpath)
      write_list(el, subpath)
    } else if ("data.frame" %in% class(el)) {
      fi = name2path(path, name, ".csv")
      readr::write_csv(el, path = fi)
    } else {
      fi = name2path(path, name, ".rds")
      readr::write_rds(el, path = fi)
    }
  })
}

.read_elems <- function(path, pattern, reader, ...) {
  elems <- list.files(path, pattern = pattern, full.names = TRUE)
  elems %>%
    purrr::map(reader, ...) %>%
    setNames(path2name(elems))
}

.read_sublists <- function(path, ...) {
  sublists <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  file.path(path, sublists) %>%
    purrr::map(read_list, ...) %>%
    setNames(sublists)
}


read_list <- function(path) {
  rlang::list2(
    !!! .read_elems(path, pattern = "\\.csv$", readr::read_csv),
    !!! .read_elems(path, pattern = "\\.rds$", readr::read_rds),
    !!! .read_sublists(path)
  )
}

# read_list("data")
