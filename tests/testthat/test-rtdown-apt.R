# 비대화형 모드 — 모든 인자 직접 전달.

skip_if_not_installed("mockery")

.fake_resp <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/xml; charset=utf-8"),
    body = charToRaw(body)
  )
}

# 부동산 유형별로 적절한 단지/유형 필드를 포함한 가짜 응답 한 줄을 만든다.
.simple_body_for <- function(property, type) {
  name_field <- switch(
    property,
    apt  = c(aptNm = "테스트APT"),
    rh   = c(mhouseNm = "테스트빌라"),
    sh   = c(houseType = "단독"),
    offi = c(offiNm = "테스트오피스텔")
  )
  type_field <- if (type == "trade") {
    c(dealAmount = "150,000")
  } else {
    c(deposit = "30,000", monthlyRent = "0", contractType = "신규")
  }
  fields <- c(
    umdNm = "역삼동", jibun = "1",
    floor = "10", excluUseAr = "84.99",
    dealYear = "2025", dealMonth = "4", dealDay = "15",
    buildYear = "2010",
    name_field, type_field
  )
  inner <- paste0(
    mapply(function(k, v) sprintf("<%s>%s</%s>", k, v, k),
           names(fields), fields),
    collapse = ""
  )
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<response><header><resultCode>000</resultCode>",
    "<resultMsg>OK</resultMsg></header>",
    "<body><items><item>", inner, "</item></items>",
    "<numOfRows>1</numOfRows><pageNo>1</pageNo>",
    "<totalCount>1</totalCount></body></response>"
  )
}

test_that("rtdown_apt 비대화형 단일 시군구 단일 월", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_property, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body_for("apt", "trade")))
    df <- rtdown_apt(
      type = "trade", sido_code = "11", sigungu = "강남구",
      ymd_from = "202504", ymd_to = "202504",
      verbose = FALSE, confirm = FALSE
    )
    expect_s3_class(df, "tbl_df")
    expect_equal(nrow(df), 1L)
    expect_equal(df$lawd_cd, "11680")
    expect_equal(df$sigungu_nm, "서울특별시 강남구")
    expect_equal(df$deal_ymd, "202504")
    expect_equal(df$property_type, "apt")
    expect_equal(df$apt_nm, "테스트APT")
    expect_equal(df$deal_amount_won, 150000 * 10000)
  })
})

test_that("rtdown_RH 비대화형 — 연립다세대 매매", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_property, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body_for("rh", "trade")))
    df <- rtdown_RH(
      type = "trade", sido_code = "11", sigungu = "강남구",
      ymd_from = "202504", ymd_to = "202504",
      verbose = FALSE, confirm = FALSE
    )
    expect_equal(nrow(df), 1L)
    expect_equal(df$property_type, "rh")
    expect_equal(df$mhouse_nm, "테스트빌라")
    expect_equal(df$apt_nm, "")
  })
})

test_that("rtdown_SH 비대화형 — 단독", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_property, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body_for("sh", "trade")))
    df <- rtdown_SH(
      type = "trade", sido_code = "11", sigungu = "강남구",
      ymd_from = "202504", ymd_to = "202504",
      verbose = FALSE, confirm = FALSE
    )
    expect_equal(nrow(df), 1L)
    expect_equal(df$property_type, "sh")
    expect_equal(df$house_type, "단독")
  })
})

test_that("rtdown_O 비대화형 — 오피스텔 전월세", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_property, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body_for("offi", "rent")))
    df <- rtdown_O(
      type = "rent", lawd_cd = "11680",
      ymd_from = "202504", ymd_to = "202504",
      verbose = FALSE, confirm = FALSE
    )
    expect_equal(nrow(df), 1L)
    expect_equal(df$property_type, "offi")
    expect_equal(df$data_type, "rent")
    expect_equal(df$offi_nm, "테스트오피스텔")
    expect_equal(df$deposit_won, 30000 * 10000)
  })
})

test_that("rtdown_apt 비대화형 시도 전체 + 다중 월 (서울 25 자치구 × 3 개월)", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_property, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body_for("apt", "trade")))
    df <- rtdown_apt(
      type = "trade", sido_code = "11", sigungu = "all",
      ymd_from = "202501", ymd_to = "202503",
      verbose = FALSE, confirm = FALSE
    )
    expect_equal(nrow(df), 75L)
    expect_setequal(unique(df$deal_ymd), c("202501", "202502", "202503"))
    expect_equal(length(unique(df$lawd_cd)), 25L)
    expect_true(all(df$property_type == "apt"))
  })
})

test_that("rtdown_apt 잘못된 시도 코드 → stop()", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    expect_error(
      rtdown_apt(
        type = "trade", sido_code = "99", sigungu = "all",
        ymd_from = "202504", ymd_to = "202504",
        verbose = FALSE, confirm = FALSE
      ),
      regexp = "알 수 없는 시"
    )
  })
})

test_that("rtdown_apt 매치되는 시군구 없음 → stop()", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    expect_error(
      rtdown_apt(
        type = "trade", sido_code = "11", sigungu = "존재하지않는동",
        ymd_from = "202504", ymd_to = "202504",
        verbose = FALSE, confirm = FALSE
      ),
      regexp = "일치하는 시군구"
    )
  })
})

test_that("비대화형인데 인자 누락 → stop()", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    expect_error(
      rtdown_apt(
        sido_code = "11", sigungu = "강남구",
        ymd_from = "202504", ymd_to = "202504",
        verbose = FALSE, confirm = FALSE
      ),
      regexp = "비대화형"
    )
  })
})
