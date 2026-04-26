#' Set or get the MOLIT public-data-portal service key
#'
#' Stores the service key in the `RTDOWN_MOLIT_KEY` environment variable for
#' the current R session. The key may be either the URL-encoded or decoded
#' form; downstream API calls handle decoding automatically.
#'
#' To make the key persist across sessions, add the line
#' `RTDOWN_MOLIT_KEY=...` to your `~/.Renviron` file
#' (use `usethis::edit_r_environ()`).
#'
#' @param key Character. MOLIT service key (Encoding 또는 Decoding 형식).
#'
#' @return Invisibly returns the key (or the empty string for the getter).
#'
#' @examples
#' \dontrun{
#' rtdown_set_molit_key("xxxxxxxx-yyyy-zzzz-...")
#' rtdown_get_molit_key()
#' }
#'
#' @export
rtdown_set_molit_key <- function(key) {
  if (is.null(key) || !is.character(key) ||
      length(key) != 1L || !nzchar(key)) {
    stop("`key` 는 비어있지 않은 문자열이어야 합니다.", call. = FALSE)
  }
  Sys.setenv(RTDOWN_MOLIT_KEY = key)
  invisible(key)
}

#' @rdname rtdown_set_molit_key
#' @export
rtdown_get_molit_key <- function() {
  v <- Sys.getenv("RTDOWN_MOLIT_KEY", unset = "")
  if (!nzchar(v)) "" else v
}
