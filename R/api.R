# MOLIT (국토교통부) RTMSDataSvc API client (internal).
#
# 4가지 부동산 유형 × 매매/전월세 = 총 8개 엔드포인트를 지원한다.
# 모든 엔드포인트는 동일한 요청 변수
# (serviceKey / LAWD_CD / DEAL_YMD / pageNo / numOfRows) 를 받는다.
# XML 응답을 파싱해 list-of-list 로 모은 뒤 .rows_to_tibble() 로 변환한다.

# property_type × data_type → MOLIT 엔드포인트 URL
.PROPERTY_URLS <- list(
  apt = list(
    trade = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcAptTrade/",
                   "getRTMSDataSvcAptTrade"),
    rent  = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcAptRent/",
                   "getRTMSDataSvcAptRent")
  ),
  rh = list(
    trade = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcRHTrade/",
                   "getRTMSDataSvcRHTrade"),
    rent  = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcRHRent/",
                   "getRTMSDataSvcRHRent")
  ),
  sh = list(
    trade = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcSHTrade/",
                   "getRTMSDataSvcSHTrade"),
    rent  = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcSHRent/",
                   "getRTMSDataSvcSHRent")
  ),
  offi = list(
    trade = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcOffiTrade/",
                   "getRTMSDataSvcOffiTrade"),
    rent  = paste0("https://apis.data.go.kr/1613000/RTMSDataSvcOffiRent/",
                   "getRTMSDataSvcOffiRent")
  )
)

# 화면 표시용 한글 라벨
.PROPERTY_LABELS <- list(
  apt  = "아파트",
  rh   = "연립다세대",
  sh   = "단독/다가구",
  offi = "오피스텔"
)

.resolve_url <- function(property, type) {
  if (!property %in% names(.PROPERTY_URLS)) {
    stop(sprintf("알 수 없는 부동산 유형: %s", property), call. = FALSE)
  }
  if (!type %in% c("trade", "rent")) {
    stop(sprintf("알 수 없는 거래 유형: %s", type), call. = FALSE)
  }
  .PROPERTY_URLS[[property]][[type]]
}

# 단일 (property, type, 시군구, 계약월) 호출 → tibble.
# 페이지네이션 자동 처리.
.fetch_property <- function(property, type, lawd_cd, deal_ymd,
                            service_key, num_of_rows = 1000L,
                            timeout = 30L,
                            sigungu_nm = NA_character_) {
  url <- .resolve_url(property, type)

  if (is.null(service_key) || !nzchar(service_key)) {
    service_key <- rtdown_get_molit_key()
  }
  if (!nzchar(service_key)) {
    stop("MOLIT 서비스 키가 필요합니다. `rtdown_set_molit_key(...)` 로 설정하세요.",
         call. = FALSE)
  }
  decoded_key <- utils::URLdecode(service_key)

  lawd_cd  <- as.character(lawd_cd)
  deal_ymd <- as.character(deal_ymd)
  if (!grepl("^[0-9]{5}$", lawd_cd)) {
    stop(sprintf("LAWD_CD 는 5자리 숫자여야 합니다: %s", lawd_cd),
         call. = FALSE)
  }
  if (!grepl("^[0-9]{6}$", deal_ymd)) {
    stop(sprintf("DEAL_YMD 는 6자리 YYYYMM 이어야 합니다: %s", deal_ymd),
         call. = FALSE)
  }

  all_rows <- list()
  page <- 1L
  repeat {
    req <- httr2::req_url_query(
      httr2::request(url),
      serviceKey = decoded_key,
      LAWD_CD    = lawd_cd,
      DEAL_YMD   = deal_ymd,
      pageNo     = as.character(page),
      numOfRows  = as.character(num_of_rows)
    )
    req <- httr2::req_user_agent(req, "R-rtdown.molit/0.2")
    req <- httr2::req_timeout(req, as.numeric(timeout))
    req <- httr2::req_error(req, is_error = function(resp) FALSE)

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status >= 400L) {
      stop(sprintf("MOLIT HTTP %d", status), call. = FALSE)
    }
    body <- httr2::resp_body_raw(resp)
    doc <- tryCatch(xml2::read_xml(body),
                    error = function(e) {
                      stop(sprintf("MOLIT XML 파싱 실패: %s",
                                   conditionMessage(e)),
                           call. = FALSE)
                    })

    # OpenAPI_ServiceResponse 형식 인증/쿼터 오류
    rr <- xml2::xml_text(
      xml2::xml_find_first(doc, ".//cmmMsgHeader/returnReasonCode")
    )
    if (!is.na(rr) && nzchar(rr) && !(rr %in% c("00", "0", "000"))) {
      err <- xml2::xml_text(xml2::xml_find_first(doc, ".//cmmMsgHeader/errMsg"))
      auth <- xml2::xml_text(xml2::xml_find_first(doc, ".//cmmMsgHeader/returnAuthMsg"))
      stop(sprintf("MOLIT API 오류 (코드=%s): %s / %s",
                   rr,
                   if (is.na(err)) "" else err,
                   if (is.na(auth)) "" else auth),
           call. = FALSE)
    }
    rc <- xml2::xml_text(xml2::xml_find_first(doc, ".//resultCode"))
    if (!is.na(rc) && nzchar(rc) && !(rc %in% c("00", "0", "000"))) {
      rm <- xml2::xml_text(xml2::xml_find_first(doc, ".//resultMsg"))
      stop(sprintf("MOLIT API 오류 (코드=%s): %s",
                   rc, if (is.na(rm)) "" else rm),
           call. = FALSE)
    }

    items <- xml2::xml_find_all(doc, ".//items/item")
    rows <- lapply(items, function(it) {
      kids <- xml2::xml_children(it)
      vals <- xml2::xml_text(kids)
      names(vals) <- xml2::xml_name(kids)
      as.list(vals)
    })
    all_rows <- c(all_rows, rows)

    tc_text <- xml2::xml_text(xml2::xml_find_first(doc, ".//totalCount"))
    total_count <- suppressWarnings(as.integer(tc_text))
    if (is.na(total_count)) total_count <- length(all_rows)

    if (length(all_rows) >= total_count || length(items) == 0L) {
      break
    }
    page <- page + 1L
    if (page > 100L) break
  }

  .rows_to_tibble(all_rows, property = property, type = type,
                  lawd_cd = lawd_cd, deal_ymd = deal_ymd,
                  sigungu_nm = sigungu_nm)
}

# 시군구 × 월 순회 → 단일 tibble.
# verbose = TRUE 일 때 cli::cli_progress_bar 로 한 줄 진행 표시
# (현재 항목 / 경과 / 남은 / 총 예상 시간) 를 같이 갱신한다.
.run_download <- function(property, type,
                          lawd_codes, sigungu_names,
                          ymd_list, service_key,
                          timeout = 30L, verbose = TRUE) {
  total <- length(lawd_codes) * length(ymd_list)
  property_label <- .PROPERTY_LABELS[[property]] %||% property
  type_label <- if (type == "trade") "매매" else "전월세"

  if (isTRUE(verbose)) {
    cli::cli_alert_info(sprintf(
      "MOLIT %s %s API · 시군구 %d개 × 기간 %d개월 = 총 %d회 호출",
      property_label, type_label,
      length(lawd_codes), length(ymd_list), total
    ))
  }

  # 진행률 + 시간 표시 (한 줄에 in-place 갱신).
  # 자료 수집 도중 현재 어떤 시군구·월을 호출하는지 (status), 누적
  # 경과 시간 / 남은 예상 시간 (ETA) / 전체 예상 시간을 모두 보여준다.
  # cli 빌트인 토큰은 pb_elapsed / pb_eta 까지만 있고 "총 시간" 은 없으므로,
  # `extra = list(total = ...)` 로 직접 계산해 주입한다.
  pb <- NULL
  start_time <- NULL
  if (isTRUE(verbose) && total > 0L) {
    start_time <- Sys.time()
    pb <- cli::cli_progress_bar(
      name = sprintf("%s %s 다운로드", property_label, type_label),
      total = total,
      format = paste0(
        "[{cli::pb_current}/{cli::pb_total}] {cli::pb_status}  ",
        "\u2502 \uacbd\uacfc {cli::pb_elapsed} ",
        "\u2502 \ub0a8\uc740 ~{cli::pb_eta} ",
        "\u2502 \ucd1d ~{cli::pb_extra$total}"
      ),
      format_done = paste0(
        "{cli::pb_total}\ud68c \ud638\ucd9c \uc644\ub8cc \u00b7 ",
        "\ucd1d \uc18c\uc694 {cli::pb_elapsed}"
      ),
      extra = list(total = "?"),
      clear = FALSE,
      .auto_close = FALSE
    )
  }

  pieces <- vector("list", total)
  failed <- list()
  k <- 0L
  for (i in seq_along(lawd_codes)) {
    sg_code <- lawd_codes[i]
    sg_name <- sigungu_names[i]
    for (ymd in ymd_list) {
      k <- k + 1L
      label <- sprintf("%s (%s) \u00b7 %s",
                       sg_name %||% sg_code, sg_code, ymd)
      # 진행 표시: 현재 호출 직전에 status 를 위치명으로 갱신.
      if (!is.null(pb)) {
        cli::cli_progress_update(id = pb, set = k - 1L, status = label)
      }
      tib <- tryCatch(
        .fetch_property(property = property, type = type,
                        lawd_cd = sg_code, deal_ymd = ymd,
                        service_key = service_key, timeout = timeout,
                        sigungu_nm = sg_name),
        error = function(e) {
          msg <- conditionMessage(e)
          failed[[length(failed) + 1L]] <<- list(
            property = property, lawd_cd = sg_code,
            sigungu_nm = sg_name, deal_ymd = ymd, error = msg
          )
          if (isTRUE(verbose)) {
            cli::cli_alert_warning(sprintf("\u2197 %s \u00b7 \uc2e4\ud328: %s",
                                           label, msg))
          }
          .empty_result(property, type, sg_code, sg_name, ymd)
        }
      )
      pieces[[k]] <- tib
      # 호출 완료 후: 카운트 +1 + 총 시간 추정값 갱신.
      if (!is.null(pb)) {
        elapsed_s <- as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
        total_str <- if (k >= 1L && elapsed_s > 0) {
          .fmt_secs(elapsed_s * total / k)
        } else {
          "?"
        }
        cli::cli_progress_update(
          id = pb, set = k, status = label,
          extra = list(total = total_str)
        )
      }
    }
  }
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb)
  }

  combined <- tryCatch(
    do.call(rbind, pieces),
    error = function(e) .empty_result(property, type)
  )
  if (is.null(combined)) combined <- .empty_result(property, type)

  if (isTRUE(verbose)) {
    if (nrow(combined) > 0L) {
      cli::cli_alert_success(sprintf(
        "다운로드 완료: 총 %d건 (실패 %d건)",
        nrow(combined), length(failed)
      ))
    } else {
      cli::cli_alert_info(sprintf("수신 0건 (실패 %d건)", length(failed)))
    }
  }
  attr(combined, "failed") <- failed
  tibble::as_tibble(combined)
}
