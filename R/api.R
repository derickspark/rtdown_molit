# MOLIT (국토교통부) RTMSDataSvc API client (internal).
#
# 매매 (RTMSDataSvcAptTrade) 와 전월세 (RTMSDataSvcAptRent) 두 엔드포인트는
# 동일한 요청 변수 (serviceKey / LAWD_CD / DEAL_YMD / pageNo / numOfRows)
# 를 받는다. XML 응답을 파싱해 list-of-list 로 반환하고, .rows_to_tibble()
# 로 tibble 화한다.

.APT_TRADE_URL <- paste0(
  "https://apis.data.go.kr/1613000/RTMSDataSvcAptTrade/",
  "getRTMSDataSvcAptTrade"
)

.APT_RENT_URL <- paste0(
  "https://apis.data.go.kr/1613000/RTMSDataSvcAptRent/",
  "getRTMSDataSvcAptRent"
)

# 단일 (시군구, 계약월) 페이지네이션 호출 → tibble
.fetch_apt <- function(type, lawd_cd, deal_ymd,
                       service_key, num_of_rows = 1000L,
                       timeout = 30L,
                       sigungu_nm = NA_character_) {
  url <- if (identical(type, "rent")) .APT_RENT_URL else .APT_TRADE_URL

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
    req <- httr2::req_user_agent(req, "R-rtdown.molit/0.1")
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

  .rows_to_tibble(all_rows, type = type, lawd_cd = lawd_cd,
                  deal_ymd = deal_ymd, sigungu_nm = sigungu_nm)
}

# 시군구 × 월 순회 → 단일 tibble.
# 실패한 (sigungu, ymd) 는 attr "failed" 에 list 로 첨부.
.run_download <- function(type,
                          lawd_codes, sigungu_names,
                          ymd_list, service_key,
                          timeout = 30L, verbose = TRUE) {
  total <- length(lawd_codes) * length(ymd_list)
  if (isTRUE(verbose)) {
    cli::cli_alert_info(sprintf(
      "MOLIT %s API · 시군구 %d개 × 기간 %d개월 = 총 %d회 호출",
      if (type == "trade") "매매" else "전월세",
      length(lawd_codes), length(ymd_list), total
    ))
  }
  pieces <- vector("list", total)
  failed <- list()
  k <- 0L
  for (i in seq_along(lawd_codes)) {
    sg_code <- lawd_codes[i]
    sg_name <- sigungu_names[i]
    for (ymd in ymd_list) {
      k <- k + 1L
      if (isTRUE(verbose)) {
        cli::cli_alert(sprintf("[%d/%d] %s (%s) · %s",
                               k, total,
                               sg_name %||% sg_code, sg_code, ymd))
      }
      tib <- tryCatch(
        .fetch_apt(type = type, lawd_cd = sg_code, deal_ymd = ymd,
                   service_key = service_key, timeout = timeout,
                   sigungu_nm = sg_name),
        error = function(e) {
          msg <- conditionMessage(e)
          failed[[length(failed) + 1L]] <<- list(
            lawd_cd = sg_code, sigungu_nm = sg_name,
            deal_ymd = ymd, error = msg
          )
          if (isTRUE(verbose)) {
            cli::cli_alert_warning(sprintf("  ↳ 실패: %s", msg))
          }
          .empty_result(type, sg_code, sg_name, ymd)
        }
      )
      pieces[[k]] <- tib
    }
  }
  combined <- tryCatch(
    do.call(rbind, pieces),
    error = function(e) .empty_result(type)
  )
  if (is.null(combined)) combined <- .empty_result(type)

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
