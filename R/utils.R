# ── 월 범위 헬퍼 ────────────────────────────────────────────────────

#' Expand a YYYYMM range into the inclusive list of months
#'
#' @param from_ymd Character of length 1 (YYYYMM).
#' @param to_ymd   Character of length 1 (YYYYMM). Defaults to `from_ymd`.
#'
#' @return Character vector of YYYYMM strings, sorted ascending.
#'         Reversed input (`from > to`) is automatically swapped.
#'
#' @keywords internal
#' @noRd
.expand_month_range <- function(from_ymd, to_ymd = from_ymd) {
  if (is.null(to_ymd)) to_ymd <- from_ymd
  parse_ymd <- function(s) {
    s <- trimws(as.character(s))
    if (length(s) != 1L || !grepl("^[0-9]{6}$", s)) {
      stop(sprintf("YYYYMM(예: 202501) 형식이어야 합니다: %s", s),
           call. = FALSE)
    }
    yr <- as.integer(substr(s, 1L, 4L))
    mo <- as.integer(substr(s, 5L, 6L))
    if (mo < 1L || mo > 12L) {
      stop(sprintf("월은 01~12 이어야 합니다: %s", s), call. = FALSE)
    }
    list(year = yr, month = mo)
  }
  a <- parse_ymd(from_ymd)
  b <- parse_ymd(to_ymd)
  k1 <- a$year * 12L + (a$month - 1L)
  k2 <- b$year * 12L + (b$month - 1L)
  if (k2 < k1) {
    tmp <- k1
    k1 <- k2
    k2 <- tmp
  }
  ks <- seq.int(k1, k2)
  sprintf("%04d%02d", ks %/% 12L, (ks %% 12L) + 1L)
}

# ── 인터랙티브 입력 헬퍼 ───────────────────────────────────────────

# 6자리 YYYYMM 을 표준입력에서 받는다. 형식이 틀리면 다시 묻는다.
# 빈 입력은 NULL 반환 (취소 시그널).
.prompt_ymd <- function(prompt) {
  attempts <- 0L
  repeat {
    attempts <- attempts + 1L
    val <- trimws(readline(prompt))
    if (val == "") {
      message("취소되었습니다.")
      return(NULL)
    }
    if (grepl("^[0-9]{6}$", val)) {
      yr <- as.integer(substr(val, 1L, 4L))
      mo <- as.integer(substr(val, 5L, 6L))
      if (yr >= 2006L && yr <= 2100L && mo >= 1L && mo <= 12L) {
        return(val)
      }
    }
    message(sprintf(
      "  올바른 형식이 아닙니다. 6자리 숫자 YYYYMM (예: 202501) 으로 입력하세요. (시도 %d)",
      attempts
    ))
    if (attempts >= 5L) {
      message("  입력 횟수 초과 — 취소합니다.")
      return(NULL)
    }
  }
}

# y/N 확인 프롬프트
.prompt_yn <- function(prompt, default_yes = FALSE) {
  ans <- trimws(readline(prompt))
  if (ans == "") return(default_yes)
  tolower(ans) %in% c("y", "yes", "예", "ㅇ")
}

# ── 행(row) 변환 ───────────────────────────────────────────────────

.row_get <- function(row, key) {
  if (is.null(row)) return("")
  v <- row[[key]]
  if (is.null(v)) return("")
  if (is.na(v)) return("")
  trimws(as.character(v))
}

.build_address <- function(row) {
  umd <- .row_get(row, "umdNm")
  jib <- .row_get(row, "jibun")
  parts <- c(umd, jib)
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = " ")
}

.build_road_address <- function(row) {
  road <- .row_get(row, "roadNm")
  if (!nzchar(road)) return("")
  bonbun <- sub("^0+", "", .row_get(row, "roadNmBonbun"))
  bubun <- sub("^0+", "", .row_get(row, "roadNmBubun"))
  num <- if (nzchar(bubun)) {
    if (nzchar(bonbun)) sprintf("%s-%s", bonbun, bubun) else bubun
  } else {
    bonbun
  }
  trimws(paste(road, num))
}

.coerce_int <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  suppressWarnings(as.integer(x))
}

.coerce_long <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub(",", "", x, fixed = TRUE)
  x[!nzchar(x)] <- NA_character_
  suppressWarnings(as.numeric(x))
}

.coerce_double <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  suppressWarnings(as.numeric(x))
}

.coerce_char <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x
}

# ── 스키마 ─────────────────────────────────────────────────────────

.common_schema <- function() {
  list(
    # 공간/주소 — 4종 부동산 모두 일부 또는 전부 등장
    list(tag = "sggCd",        name = "sgg_cd",         type = "char"),
    list(tag = "umdNm",        name = "umd_nm",         type = "char"),
    list(tag = "umdCd",        name = "umd_cd",         type = "char"),
    list(tag = "jibun",        name = "jibun",          type = "char"),
    list(tag = "floor",        name = "floor",          type = "int"),
    list(tag = "excluUseAr",   name = "exclu_use_ar",   type = "double"),
    list(tag = "dealYear",     name = "deal_year",      type = "int"),
    list(tag = "dealMonth",    name = "deal_month",     type = "int"),
    list(tag = "dealDay",      name = "deal_day",       type = "int"),
    list(tag = "buildYear",    name = "build_year",     type = "int"),
    list(tag = "roadNm",       name = "road_nm",        type = "char"),
    list(tag = "roadNmBonbun", name = "road_nm_bonbun", type = "char"),
    list(tag = "roadNmBubun",  name = "road_nm_bubun",  type = "char"),
    list(tag = "bonbun",       name = "bonbun",         type = "char"),
    list(tag = "bubun",        name = "bubun",          type = "char"),
    # 부동산 유형별 단지/유형/면적 필드 — 응답에 없는 항목은 NA.
    list(tag = "aptNm",        name = "apt_nm",         type = "char"),
    list(tag = "aptDong",      name = "apt_dong",       type = "char"),
    list(tag = "mhouseNm",     name = "mhouse_nm",      type = "char"),
    list(tag = "houseType",    name = "house_type",     type = "char"),
    list(tag = "offiNm",       name = "offi_nm",        type = "char"),
    list(tag = "plottageAr",   name = "plottage_ar",    type = "double"),
    list(tag = "totalFloorAr", name = "total_floor_ar", type = "double")
  )
}

.trade_schema <- function() {
  list(
    list(tag = "dealAmount",       name = "deal_amount_manwon", type = "long"),
    list(tag = "rgstDate",         name = "rgst_date",          type = "char"),
    list(tag = "cdealType",        name = "cdeal_type",         type = "char"),
    list(tag = "cdealDay",         name = "cdeal_day",          type = "char"),
    list(tag = "dealingGbn",       name = "dealing_gbn",        type = "char"),
    list(tag = "estateAgentSggNm", name = "agent_sgg_nm",       type = "char"),
    list(tag = "slerGbn",          name = "sler_gbn",           type = "char"),
    list(tag = "buyerGbn",         name = "buyer_gbn",          type = "char")
  )
}

.rent_schema <- function() {
  list(
    list(tag = "deposit",        name = "deposit_manwon",          type = "long"),
    list(tag = "monthlyRent",    name = "monthly_rent_manwon",     type = "long"),
    list(tag = "contractTerm",   name = "contract_term",           type = "char"),
    list(tag = "contractType",   name = "contract_type",           type = "char"),
    list(tag = "useRRRight",     name = "use_rr_right",            type = "char"),
    list(tag = "preDeposit",     name = "pre_deposit_manwon",      type = "long"),
    list(tag = "preMonthlyRent", name = "pre_monthly_rent_manwon", type = "long")
  )
}

.get_schema <- function(type) {
  if (identical(type, "rent")) {
    c(.common_schema(), .rent_schema())
  } else {
    c(.common_schema(), .trade_schema())
  }
}

# 빈 결과 tibble (스키마 일치 보장)
.empty_result <- function(property, type,
                          lawd_cd = NA_character_,
                          sigungu_nm = NA_character_,
                          deal_ymd = NA_character_) {
  schema <- .get_schema(type)
  cols <- list(
    lawd_cd       = character(0),
    sigungu_nm    = character(0),
    deal_ymd      = character(0),
    data_type     = character(0),
    property_type = character(0)
  )
  for (s in schema) {
    cols[[s$name]] <- switch(
      s$type,
      int = integer(0),
      long = numeric(0),
      double = numeric(0),
      character(0)
    )
  }
  cols$full_address <- character(0)
  cols$full_address_road <- character(0)
  cols$deal_date <- character(0)
  if (identical(type, "rent")) {
    cols$deposit_won <- numeric(0)
    cols$monthly_rent_won <- numeric(0)
    cols$pre_deposit_won <- numeric(0)
    cols$pre_monthly_rent_won <- numeric(0)
  } else {
    cols$deal_amount_won <- numeric(0)
    cols$price_per_m2_won <- numeric(0)
  }
  tibble::as_tibble(cols)
}

# 응답 row 리스트 -> tibble
.rows_to_tibble <- function(rows, property, type, lawd_cd, deal_ymd,
                            sigungu_nm = NA_character_) {
  if (length(rows) == 0L) {
    return(.empty_result(property, type, lawd_cd, sigungu_nm, deal_ymd))
  }
  n <- length(rows)
  schema <- .get_schema(type)
  cols <- list(
    lawd_cd       = rep(as.character(lawd_cd), n),
    sigungu_nm    = rep(as.character(sigungu_nm %||% NA_character_), n),
    deal_ymd      = rep(as.character(deal_ymd), n),
    data_type     = rep(type, n),
    property_type = rep(as.character(property), n)
  )
  for (s in schema) {
    raw <- vapply(rows, function(r) .row_get(r, s$tag), character(1L))
    cols[[s$name]] <- switch(
      s$type,
      int = .coerce_int(raw),
      long = .coerce_long(raw),
      double = .coerce_double(raw),
      .coerce_char(raw)
    )
  }
  cols$full_address      <- vapply(rows, .build_address, character(1L))
  cols$full_address_road <- vapply(rows, .build_road_address, character(1L))

  yr <- cols$deal_year
  mo <- cols$deal_month
  dy <- cols$deal_day
  cols$deal_date <- ifelse(
    !is.na(yr) & !is.na(mo) & !is.na(dy),
    sprintf("%04d-%02d-%02d", yr, mo, dy),
    NA_character_
  )

  area <- cols$exclu_use_ar
  if (identical(type, "rent")) {
    dep   <- cols$deposit_manwon
    rent  <- cols$monthly_rent_manwon
    pdep  <- cols$pre_deposit_manwon
    prent <- cols$pre_monthly_rent_manwon
    cols$deposit_won          <- ifelse(is.na(dep), NA_real_, dep * 10000)
    cols$monthly_rent_won     <- ifelse(is.na(rent), NA_real_, rent * 10000)
    cols$pre_deposit_won      <- ifelse(is.na(pdep), NA_real_, pdep * 10000)
    cols$pre_monthly_rent_won <- ifelse(is.na(prent), NA_real_, prent * 10000)
  } else {
    manwon <- cols$deal_amount_manwon
    won <- ifelse(is.na(manwon), NA_real_, manwon * 10000)
    cols$deal_amount_won  <- won
    cols$price_per_m2_won <- ifelse(
      !is.na(won) & !is.na(area) & area > 0,
      round(won / area),
      NA_real_
    )
  }

  tibble::as_tibble(cols)
}
