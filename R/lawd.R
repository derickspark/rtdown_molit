#' Korean LAWD (법정동) code table
#'
#' Loads the bundled 법정동 코드 데이터 (행정안전부 공시) from
#' `inst/extdata/lawd_codes.txt`. Cached in a private package environment
#' so subsequent calls return immediately.
#'
#' Active rows (폐지여부 == "존재") are tagged with derived columns:
#' `sido_code` (2 digits), `sido_name`, `lawd_cd` (5-digit 시군구 코드,
#' or `NA` for non-시군구 rows), and boolean flags `is_sido` / `is_sigungu`.
#'
#' @return A `tibble` with one row per active 법정동 entry.
#'
#' @examples
#' \dontrun{
#' rtdown_lawd_codes()
#' }
#'
#' @export
rtdown_lawd_codes <- function() {
  if (exists(".lawd_cache", envir = .pkg_env, inherits = FALSE)) {
    return(get(".lawd_cache", envir = .pkg_env))
  }
  path <- system.file("extdata", "lawd_codes.txt",
                      package = "rtdown.molit")
  if (!nzchar(path)) {
    stop("`lawd_codes.txt` 자료 파일을 찾을 수 없습니다.", call. = FALSE)
  }
  raw <- readLines(path, encoding = "UTF-8", warn = FALSE)
  if (length(raw) >= 1L) raw <- raw[-1L]
  raw <- raw[nzchar(trimws(raw))]
  parts <- strsplit(raw, "\t", fixed = TRUE)
  ok <- vapply(parts, function(x) length(x) >= 3L, logical(1L))
  parts <- parts[ok]

  code <- vapply(parts, `[`, character(1L), 1L)
  name <- vapply(parts, `[`, character(1L), 2L)
  status <- vapply(parts, `[`, character(1L), 3L)

  active <- trimws(status) == "존재"
  code <- code[active]
  name <- name[active]
  status <- status[active]

  code <- formatC(code, width = 10L, flag = "0")

  sido_code  <- substr(code, 1L, 2L)
  is_sido    <- substr(code, 3L, 10L) == "00000000"
  is_sigungu <- (!is_sido) & (substr(code, 6L, 10L) == "00000")
  lawd_cd    <- ifelse(is_sigungu, substr(code, 1L, 5L), NA_character_)

  sido_lookup <- name[is_sido]
  names(sido_lookup) <- sido_code[is_sido]
  sido_name_full <- ifelse(
    is_sido | is_sigungu,
    unname(sido_lookup[sido_code]),
    NA_character_
  )
  sigungu_name <- ifelse(is_sigungu, name, NA_character_)

  df <- tibble::tibble(
    code         = code,
    name         = name,
    sido_code    = sido_code,
    sido_name    = sido_name_full,
    lawd_cd      = lawd_cd,
    sigungu_name = sigungu_name,
    is_sido      = is_sido,
    is_sigungu   = is_sigungu
  )
  assign(".lawd_cache", df, envir = .pkg_env)
  df
}

#' 시·도 (sido) lookup table
#'
#' @return A `tibble` of 시도 with columns `sido_code` and `sido_name`,
#'         sorted by `sido_code`.
#'
#' @examples
#' \dontrun{
#' rtdown_sido_table()
#' }
#'
#' @export
rtdown_sido_table <- function() {
  d <- rtdown_lawd_codes()
  d <- d[d$is_sido, c("sido_code", "sido_name")]
  d <- d[order(d$sido_code), , drop = FALSE]
  d <- d[!duplicated(d$sido_code), , drop = FALSE]
  tibble::as_tibble(d)
}

#' 시·군·구 (sigungu) lookup table
#'
#' @param sido_code Optional 2-digit 시도 code (character) to filter by.
#'                  `NULL` returns the full national 시군구 list.
#'
#' @return A `tibble` with `sido_code`, `sido_name`, `lawd_cd`,
#'         `sigungu_name`, sorted by `lawd_cd`.
#'
#' @examples
#' \dontrun{
#' rtdown_sigungu_table()             # 전국
#' rtdown_sigungu_table("11")         # 서울
#' }
#'
#' @export
rtdown_sigungu_table <- function(sido_code = NULL) {
  d <- rtdown_lawd_codes()
  d <- d[d$is_sigungu,
         c("sido_code", "sido_name", "lawd_cd", "sigungu_name"),
         drop = FALSE]
  if (!is.null(sido_code)) {
    d <- d[d$sido_code == as.character(sido_code), , drop = FALSE]
  }
  d <- d[order(d$lawd_cd), , drop = FALSE]
  tibble::as_tibble(d)
}
