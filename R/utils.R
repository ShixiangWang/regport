notnull_or_na <- function(x) {
  if (is.null(x)) NA_character_ else x
}

attr_notnull_or_na <- function(x, at = "label") {
  notnull_or_na(attr(x, at, exact = TRUE))
}

get_vars <- function(text) {
  if (!is.null(text)) {
    all.vars(parse(text = text))
  } else {
    NULL
  }
}

merge_vars <- function(...) {
  vars_list <- list(...)
  rv <- NULL
  for (i in vars_list) {
    v <- unique(sapply(i, get_vars))
    if (length(v) > 0) rv <- union(rv, v)
  }
  rv
}

remove_backticks <- function(x) {
  gsub("^`|`$|\\\\(?=`)|`(?=:)|(?<=:)`", "", x, perl = TRUE)
}
