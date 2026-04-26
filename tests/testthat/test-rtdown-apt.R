# 비대화형 모드 — 모든 인자 직접 전달.

skip_if_not_installed("mockery")

.fake_resp <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/xml; charset=utf-8"),
    body = charToRaw(body)
  )
}

.simple_body <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<response><header><resultCode>000</resultCode>",
    "<resultMsg>OK</resultMsg></header>",
    "<body><items><item>",
    "<umdNm>역삼동</umdNm><jibun>1</jibun><aptNm>테스트APT</aptNm>",
    "<floor>10</floor><excluUseAr>84.99</excluUseAr>",
    "<dealYear>2025</dealYear><dealMonth>4</dealMonth><dealDay>15</dealDay>",
    "<buildYear>2010</buildYear><dealAmount>150,000</dealAmount>",
    "</item></items>",
    "<numOfRows>1</numOfRows><pageNo>1</pageNo>",
    "<totalCount>1</totalCount></body></response>"
  )
}

test_that("rtdown_apt 비대화형 단일 시군구 단일 월", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_apt, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body()))
    df <- rtdown_apt(
      type = "trade",
      sido_code = "11",
      sigungu = "강남구",
      ymd_from = "202504",
      ymd_to = "202504",
      verbose = FALSE,
      confirm = FALSE
    )
    expect_s3_class(df, "tbl_df")
    expect_equal(nrow(df), 1L)
    expect_equal(df$lawd_cd, "11680")
    expect_equal(df$sigungu_nm, "서울특별시 강남구")
    expect_equal(df$deal_ymd, "202504")
    expect_equal(df$apt_nm, "테스트APT")
    expect_equal(df$deal_amount_won, 150000 * 10000)
  })
})

test_that("rtdown_apt 비대화형 시도 전체 + 다중 월", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_apt, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body()))
    df <- rtdown_apt(
      type = "trade",
      sido_code = "11",
      sigungu = "all",
      ymd_from = "202501",
      ymd_to = "202503",
      verbose = FALSE,
      confirm = FALSE
    )
    # 서울 25 자치구 × 3개월 = 75 row (mock 이 매번 1건 반환)
    expect_equal(nrow(df), 75L)
    expect_setequal(unique(df$deal_ymd), c("202501", "202502", "202503"))
    # 시군구는 25개 (강남구, 강동구, …)
    expect_equal(length(unique(df$lawd_cd)), 25L)
  })
})

test_that("rtdown_apt 비대화형 lawd_cd 직접 지정", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    mockery::stub(.fetch_apt, "httr2::req_perform",
                  function(req) .fake_resp(.simple_body()))
    df <- rtdown_apt(
      type = "rent",
      lawd_cd = "11680",
      ymd_from = "202504",
      ymd_to = "202504",
      verbose = FALSE,
      confirm = FALSE
    )
    expect_equal(df$lawd_cd, "11680")
    expect_equal(df$data_type, "rent")
  })
})

test_that("rtdown_apt 잘못된 시도 코드 → stop()", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    expect_error(
      rtdown_apt(
        type = "trade",
        sido_code = "99",
        sigungu = "all",
        ymd_from = "202504",
        ymd_to = "202504",
        verbose = FALSE,
        confirm = FALSE
      ),
      regexp = "알 수 없는 시"
    )
  })
})

test_that("rtdown_apt 매치되는 시군구 없음 → stop()", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    expect_error(
      rtdown_apt(
        type = "trade",
        sido_code = "11",
        sigungu = "존재하지않는동",
        ymd_from = "202504",
        ymd_to = "202504",
        verbose = FALSE,
        confirm = FALSE
      ),
      regexp = "일치하는 시군구"
    )
  })
})

test_that("rtdown_apt 비대화형인데 인자 누락 → stop()", {
  # interactive() 가 FALSE 인 환경 (testthat) 에서 type 누락이면 에러.
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = "TESTKEY"), {
    expect_error(
      rtdown_apt(
        sido_code = "11",
        sigungu = "강남구",
        ymd_from = "202504",
        ymd_to = "202504",
        verbose = FALSE,
        confirm = FALSE
      ),
      regexp = "비대화형"
    )
  })
})
