# 패키지 전용 환경 (캐시 등)
.pkg_env <- new.env(parent = emptyenv())

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}

.onLoad <- function(libname, pkgname) {
  invisible(NULL)
}
