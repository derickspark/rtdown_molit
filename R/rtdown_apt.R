#' Interactively download MOLIT 부동산 실거래 자료
#'
#' Each of these four functions walks the user through the same five
#' sequential menu / readline prompts:
#' \enumerate{
#'   \item 자료 유형 선택 (매매 / 전월세)
#'   \item 시·도 선택
#'   \item 해당 시·도의 \dQuote{전체} 또는 시·군·구 하나 선택
#'   \item 시작 연월 (YYYYMM) 입력
#'   \item 종료 연월 (YYYYMM) 입력
#' }
#' and then performs the corresponding MOLIT API calls (시군구 × 월 순회) and
#' returns a single combined `tibble`. The four functions differ only in
#' which MOLIT endpoint family they hit:
#'
#' \describe{
#'   \item{\code{rtdown_apt()}}{아파트 매매(`RTMSDataSvcAptTrade`) /
#'     전월세(`RTMSDataSvcAptRent`)}
#'   \item{\code{rtdown_RH()}}{연립다세대 매매(`RTMSDataSvcRHTrade`) /
#'     전월세(`RTMSDataSvcRHRent`)}
#'   \item{\code{rtdown_SH()}}{단독/다가구 매매(`RTMSDataSvcSHTrade`) /
#'     전월세(`RTMSDataSvcSHRent`)}
#'   \item{\code{rtdown_O()}}{오피스텔 매매(`RTMSDataSvcOffiTrade`) /
#'     전월세(`RTMSDataSvcOffiRent`)}
#' }
#'
#' All five prompt values can also be passed as arguments — when **every**
#' prompt argument is supplied (`type`, `sido_code`, `sigungu` 또는
#' `lawd_cd`, `ymd_from`, `ymd_to`), the function runs non-interactively
#' and never reads from `stdin` (useful for tests / scripting).
#'
#' @param type Optional `"trade"` (매매) or `"rent"` (전월세). If `NULL`,
#'   asks via [utils::menu()].
#' @param sido_code Optional 2-digit 시도 code (e.g. `"11"`). If `NULL`, asks.
#' @param sigungu Optional. `"all"` for the entire 시도, or a single 시군구
#'   한글명 (e.g. `"강남구"`) — must be unique within the 시도. Mutually
#'   exclusive with `lawd_cd`.
#' @param lawd_cd Optional 5-digit 시군구 코드. Bypasses both `sido_code` and
#'   `sigungu` lookups when supplied.
#' @param ymd_from,ymd_to Optional YYYYMM character (length-1).
#' @param service_key MOLIT service key. Defaults to
#'   [rtdown_get_molit_key()] (which reads `RTDOWN_MOLIT_KEY`).
#' @param api_timeout Per-request timeout in seconds (default 30).
#' @param verbose If `TRUE` (default), prints `cli` progress.
#' @param confirm If `TRUE` (default) and the resulting plan involves more
#'   than one API call, ask for `y/N` confirmation before downloading.
#'
#' @return A `tibble` of all transactions (one row per 거래/계약).
#'   Columns include `lawd_cd`, `sigungu_nm`, `deal_ymd`, `data_type`,
#'   `property_type` (`"apt"` / `"rh"` / `"sh"` / `"offi"`), schema fields
#'   (`apt_nm`, `mhouse_nm`, `house_type`, `offi_nm`, `floor`,
#'   `exclu_use_ar`, `plottage_ar`, `total_floor_ar`, ...), derived
#'   `deal_amount_won` / `price_per_m2_won` (or rent equivalents),
#'   `full_address`, `full_address_road`, `deal_date`.
#'   `attr(result, "failed")` is a list of `(property, lawd_cd, sigungu_nm,
#'   deal_ymd, error)` records for any failed (sigungu, month) pairs.
#'
#' @examples
#' \dontrun{
#' rtdown_set_molit_key("...")
#'
#' # 인터랙티브 — 5단계 메뉴
#' df_apt  <- rtdown_apt()
#' df_rh   <- rtdown_RH()
#' df_sh   <- rtdown_SH()
#' df_offi <- rtdown_O()
#'
#' # 비대화형 (스크립트용) — 모든 인자 직접 전달
#' df <- rtdown_apt(
#'   type = "trade", sido_code = "11", sigungu = "강남구",
#'   ymd_from = "202501", ymd_to = "202604"
#' )
#' df <- rtdown_O(
#'   type = "trade", lawd_cd = "11680",
#'   ymd_from = "202501", ymd_to = "202604"
#' )
#' }
#'
#' @name rtdown_property
NULL

#' @rdname rtdown_property
#' @export
rtdown_apt <- function(type = NULL, sido_code = NULL, sigungu = NULL,
                       lawd_cd = NULL, ymd_from = NULL, ymd_to = NULL,
                       service_key = NULL, api_timeout = 30L,
                       verbose = TRUE, confirm = TRUE) {
  .rtdown_property(property = "apt",
                   type = type, sido_code = sido_code, sigungu = sigungu,
                   lawd_cd = lawd_cd, ymd_from = ymd_from, ymd_to = ymd_to,
                   service_key = service_key, api_timeout = api_timeout,
                   verbose = verbose, confirm = confirm)
}

#' @rdname rtdown_property
#' @export
rtdown_RH <- function(type = NULL, sido_code = NULL, sigungu = NULL,
                      lawd_cd = NULL, ymd_from = NULL, ymd_to = NULL,
                      service_key = NULL, api_timeout = 30L,
                      verbose = TRUE, confirm = TRUE) {
  .rtdown_property(property = "rh",
                   type = type, sido_code = sido_code, sigungu = sigungu,
                   lawd_cd = lawd_cd, ymd_from = ymd_from, ymd_to = ymd_to,
                   service_key = service_key, api_timeout = api_timeout,
                   verbose = verbose, confirm = confirm)
}

#' @rdname rtdown_property
#' @export
rtdown_SH <- function(type = NULL, sido_code = NULL, sigungu = NULL,
                      lawd_cd = NULL, ymd_from = NULL, ymd_to = NULL,
                      service_key = NULL, api_timeout = 30L,
                      verbose = TRUE, confirm = TRUE) {
  .rtdown_property(property = "sh",
                   type = type, sido_code = sido_code, sigungu = sigungu,
                   lawd_cd = lawd_cd, ymd_from = ymd_from, ymd_to = ymd_to,
                   service_key = service_key, api_timeout = api_timeout,
                   verbose = verbose, confirm = confirm)
}

#' @rdname rtdown_property
#' @export
rtdown_O <- function(type = NULL, sido_code = NULL, sigungu = NULL,
                     lawd_cd = NULL, ymd_from = NULL, ymd_to = NULL,
                     service_key = NULL, api_timeout = 30L,
                     verbose = TRUE, confirm = TRUE) {
  .rtdown_property(property = "offi",
                   type = type, sido_code = sido_code, sigungu = sigungu,
                   lawd_cd = lawd_cd, ymd_from = ymd_from, ymd_to = ymd_to,
                   service_key = service_key, api_timeout = api_timeout,
                   verbose = verbose, confirm = confirm)
}

# ── 공통 본체 ─────────────────────────────────────────────────────

.rtdown_property <- function(property,
                             type, sido_code, sigungu, lawd_cd,
                             ymd_from, ymd_to,
                             service_key, api_timeout,
                             verbose, confirm) {
  if (!property %in% names(.PROPERTY_URLS)) {
    stop(sprintf("알 수 없는 부동산 유형: %s", property), call. = FALSE)
  }
  property_label <- .PROPERTY_LABELS[[property]] %||% property

  # ── Step 1: 자료 유형 ──────────────────────────────────
  if (is.null(type)) {
    .require_interactive("자료 유형(type)")
    cli::cli_h2(sprintf("[ 1 / 5 ] %s 자료 유형 선택", property_label))
    api_label <- function(suffix) {
      sprintf("RTMSDataSvc%s%s", switch(property,
                                        apt = "Apt", rh = "RH",
                                        sh = "SH", offi = "Offi"), suffix)
    }
    type_choice <- utils::menu(
      c(sprintf("%s 매매 (%s)", property_label, api_label("Trade")),
        sprintf("%s 전월세 (%s)", property_label, api_label("Rent"))),
      title = "다운로드할 자료 유형을 선택하세요:"
    )
    if (type_choice == 0L) {
      cli::cli_alert_warning("취소되었습니다.")
      return(invisible(NULL))
    }
    type <- if (type_choice == 1L) "trade" else "rent"
  }
  type <- match.arg(type, c("trade", "rent"))
  type_label <- if (type == "trade") "매매" else "전월세"

  # ── Step 2: 시·도 ──────────────────────────────────────
  sido_df <- rtdown_sido_table()
  if (is.null(sido_code) && is.null(lawd_cd)) {
    .require_interactive("시·도 (sido_code)")
    cli::cli_h2("[ 2 / 5 ] 시·도 선택")
    labels <- sprintf("%s  (%s)", sido_df$sido_name, sido_df$sido_code)
    sido_choice <- utils::menu(labels, title = "시·도를 선택하세요:")
    if (sido_choice == 0L) {
      cli::cli_alert_warning("취소되었습니다.")
      return(invisible(NULL))
    }
    sido_code <- sido_df$sido_code[sido_choice]
  }
  if (is.null(sido_code) && !is.null(lawd_cd)) {
    sido_code <- substr(as.character(lawd_cd), 1L, 2L)
  }
  sido_code <- as.character(sido_code)
  sido_name_row <- sido_df$sido_name[sido_df$sido_code == sido_code]
  if (length(sido_name_row) == 0L) {
    stop(sprintf("알 수 없는 시·도 코드: %s", sido_code), call. = FALSE)
  }
  sido_name <- sido_name_row[1L]

  # ── Step 3: 시·군·구 (전체 또는 단일) ─────────────────
  sg_df <- rtdown_sigungu_table(sido_code)
  if (nrow(sg_df) == 0L) {
    stop(sprintf("시·도 %s(%s) 산하 시군구를 찾을 수 없습니다.",
                 sido_name, sido_code), call. = FALSE)
  }
  if (is.null(lawd_cd) && is.null(sigungu)) {
    .require_interactive("시·군·구 (sigungu / lawd_cd)")
    cli::cli_h2("[ 3 / 5 ] 시·군·구 선택")
    options <- c(
      sprintf("(전체) %s 산하 %d개 시군구", sido_name, nrow(sg_df)),
      sprintf("%s  (%s)", sg_df$sigungu_name, sg_df$lawd_cd)
    )
    sg_choice <- utils::menu(
      options,
      title = sprintf("%s 의 시·군·구를 선택하세요:", sido_name)
    )
    if (sg_choice == 0L) {
      cli::cli_alert_warning("취소되었습니다.")
      return(invisible(NULL))
    }
    if (sg_choice == 1L) {
      lawd_codes    <- sg_df$lawd_cd
      sigungu_names <- sg_df$sigungu_name
      location_label <- sprintf("%s 전체 (%d개)", sido_name, nrow(sg_df))
    } else {
      idx <- sg_choice - 1L
      lawd_codes    <- sg_df$lawd_cd[idx]
      sigungu_names <- sg_df$sigungu_name[idx]
      location_label <- sg_df$sigungu_name[idx]
    }
  } else if (!is.null(lawd_cd)) {
    lawd_cd <- as.character(lawd_cd)
    matched <- sg_df[sg_df$lawd_cd == lawd_cd, , drop = FALSE]
    if (nrow(matched) == 0L) {
      stop(sprintf("`lawd_cd = '%s'` 가 시·도 %s 안에 없습니다.",
                   lawd_cd, sido_name), call. = FALSE)
    }
    lawd_codes    <- matched$lawd_cd
    sigungu_names <- matched$sigungu_name
    location_label <- matched$sigungu_name
  } else {
    sigungu <- as.character(sigungu)
    if (identical(tolower(sigungu), "all") ||
        identical(sigungu, "전체")) {
      lawd_codes    <- sg_df$lawd_cd
      sigungu_names <- sg_df$sigungu_name
      location_label <- sprintf("%s 전체 (%d개)", sido_name, nrow(sg_df))
    } else {
      matched <- sg_df[grepl(sigungu, sg_df$sigungu_name, fixed = TRUE), ,
                       drop = FALSE]
      if (nrow(matched) == 0L) {
        stop(sprintf("`sigungu = '%s'` 와 일치하는 시군구가 없습니다.",
                     sigungu), call. = FALSE)
      }
      if (nrow(matched) > 1L) {
        stop(sprintf(
          "`sigungu = '%s'` 가 %d개와 매치됩니다 — 정확한 한글명 또는 lawd_cd 로 지정하세요: %s",
          sigungu, nrow(matched),
          paste(matched$sigungu_name, collapse = ", ")
        ), call. = FALSE)
      }
      lawd_codes    <- matched$lawd_cd
      sigungu_names <- matched$sigungu_name
      location_label <- matched$sigungu_name
    }
  }

  # ── Step 4: 시작 YYYYMM ────────────────────────────────
  if (is.null(ymd_from)) {
    .require_interactive("시작 연월 (ymd_from)")
    cli::cli_h2("[ 4 / 5 ] 시작 연월 입력")
    cat("YYYYMM 6자리 숫자로 입력하세요 (예: 202501)\n")
    ymd_from <- .prompt_ymd("시작 연월: ")
    if (is.null(ymd_from)) return(invisible(NULL))
  }

  # ── Step 5: 종료 YYYYMM ────────────────────────────────
  if (is.null(ymd_to)) {
    .require_interactive("종료 연월 (ymd_to)")
    cli::cli_h2("[ 5 / 5 ] 종료 연월 입력")
    cat("YYYYMM 6자리 숫자로 입력하세요 (예: 202604)\n")
    ymd_to <- .prompt_ymd("종료 연월: ")
    if (is.null(ymd_to)) return(invisible(NULL))
  }

  ymd_from <- as.character(ymd_from)
  ymd_to <- as.character(ymd_to)
  ymd_list <- .expand_month_range(ymd_from, ymd_to)

  # ── 다운로드 계획 요약 + 확인 ──────────────────────────
  total_calls <- length(lawd_codes) * length(ymd_list)
  cli::cli_h2("다운로드 계획")
  cli::cli_ul(c(
    sprintf("부동산  : %s", property_label),
    sprintf("자료 유형: %s", type_label),
    sprintf("지역    : %s", location_label),
    sprintf("기간    : %s ~ %s (%d개월)",
            ymd_list[1L], ymd_list[length(ymd_list)], length(ymd_list)),
    sprintf("API 호출: 시군구 %d × 월 %d = 총 %d회",
            length(lawd_codes), length(ymd_list), total_calls)
  ))

  if (isTRUE(confirm) && interactive() && total_calls > 1L) {
    if (!.prompt_yn("진행할까요? (y/N): ", default_yes = FALSE)) {
      cli::cli_alert_warning("취소되었습니다.")
      return(invisible(NULL))
    }
  }

  # ── 실행 ───────────────────────────────────────────────
  .run_download(
    property      = property,
    type          = type,
    lawd_codes    = lawd_codes,
    sigungu_names = sigungu_names,
    ymd_list      = ymd_list,
    service_key   = service_key,
    timeout       = api_timeout,
    verbose       = verbose
  )
}

# 인터랙티브 환경이 아닌데 인자도 안 주어졌을 때 명확한 에러.
.require_interactive <- function(label) {
  if (!interactive()) {
    stop(sprintf(
      "비대화형 환경에서는 `%s` 를 인자로 직접 전달해야 합니다.", label
    ), call. = FALSE)
  }
  invisible(NULL)
}
