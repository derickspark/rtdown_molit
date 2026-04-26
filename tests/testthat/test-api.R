skip_if_not_installed("mockery")

.fake_xml_body <- function(items, total = length(items)) {
  item_xml <- vapply(items, function(it) {
    inner <- paste0(
      mapply(
        function(k, v) sprintf("<%s>%s</%s>", k, v, k),
        names(it), it
      ),
      collapse = ""
    )
    sprintf("<item>%s</item>", inner)
  }, character(1L))
  sprintf(
    paste0(
      '<?xml version="1.0" encoding="UTF-8"?>',
      "<response><header><resultCode>000</resultCode>",
      "<resultMsg>OK</resultMsg></header>",
      "<body><items>%s</items>",
      "<numOfRows>%d</numOfRows><pageNo>1</pageNo>",
      "<totalCount>%d</totalCount></body></response>"
    ),
    paste(item_xml, collapse = ""),
    length(items), total
  )
}

.fake_xml_resp <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/xml; charset=utf-8"),
    body = charToRaw(body)
  )
}

test_that(".resolve_url 4 properties × 2 types", {
  expect_match(.resolve_url("apt", "trade"),  "RTMSDataSvcAptTrade")
  expect_match(.resolve_url("apt", "rent"),   "RTMSDataSvcAptRent")
  expect_match(.resolve_url("rh",  "trade"),  "RTMSDataSvcRHTrade")
  expect_match(.resolve_url("rh",  "rent"),   "RTMSDataSvcRHRent")
  expect_match(.resolve_url("sh",  "trade"),  "RTMSDataSvcSHTrade")
  expect_match(.resolve_url("sh",  "rent"),   "RTMSDataSvcSHRent")
  expect_match(.resolve_url("offi","trade"),  "RTMSDataSvcOffiTrade")
  expect_match(.resolve_url("offi","rent"),   "RTMSDataSvcOffiRent")
  expect_error(.resolve_url("xx", "trade"))
  expect_error(.resolve_url("apt", "buy"))
})

test_that(".fetch_property validates LAWD_CD / DEAL_YMD format", {
  expect_error(.fetch_property("apt", "trade", "1234", "202504",
                               service_key = "k"))
  expect_error(.fetch_property("apt", "trade", "11680", "20250",
                               service_key = "k"),
               regexp = "DEAL_YMD")
})

test_that(".fetch_property apt trade parses items into a tibble", {
  body <- .fake_xml_body(list(
    list(umdNm = "역삼동", jibun = "1", aptNm = "역삼아파트",
         floor = "10", excluUseAr = "84.99",
         dealYear = "2025", dealMonth = "4", dealDay = "15",
         buildYear = "2010", dealAmount = "150,000")
  ))
  mockery::stub(.fetch_property, "httr2::req_perform",
                function(req) .fake_xml_resp(body))
  df <- .fetch_property("apt", "trade", "11680", "202504",
                        service_key = "K", sigungu_nm = "강남구")
  expect_equal(nrow(df), 1L)
  expect_equal(df$apt_nm, "역삼아파트")
  expect_equal(df$deal_amount_manwon, 150000)
  expect_equal(df$lawd_cd, "11680")
  expect_equal(df$sigungu_nm, "강남구")
  expect_equal(df$property_type, "apt")
  expect_equal(df$data_type, "trade")
})

test_that(".fetch_property rh trade — mhouseNm 캡처", {
  body <- .fake_xml_body(list(
    list(umdNm = "성수동", mhouseNm = "성수빌라", houseType = "다세대",
         excluUseAr = "60.0",
         dealYear = "2025", dealMonth = "4", dealDay = "10",
         buildYear = "1995", dealAmount = "45,000")
  ))
  mockery::stub(.fetch_property, "httr2::req_perform",
                function(req) .fake_xml_resp(body))
  df <- .fetch_property("rh", "trade", "11200", "202504",
                        service_key = "K")
  expect_equal(df$property_type, "rh")
  expect_equal(df$mhouse_nm, "성수빌라")
  expect_equal(df$apt_nm, "")
})

test_that(".fetch_property offi rent — offiNm + deposit", {
  body <- .fake_xml_body(list(
    list(umdNm = "삼성동", offiNm = "삼성타워", floor = "12",
         excluUseAr = "33.5",
         dealYear = "2025", dealMonth = "4", dealDay = "5",
         buildYear = "2018",
         deposit = "10,000", monthlyRent = "85",
         contractType = "신규")
  ))
  mockery::stub(.fetch_property, "httr2::req_perform",
                function(req) .fake_xml_resp(body))
  df <- .fetch_property("offi", "rent", "11680", "202504",
                        service_key = "K")
  expect_equal(df$property_type, "offi")
  expect_equal(df$data_type, "rent")
  expect_equal(df$offi_nm, "삼성타워")
  expect_equal(df$deposit_won, 10000 * 10000)
})

test_that("OpenAPI_ServiceResponse 인증오류 → stop()", {
  err_body <- paste0(
    '<?xml version="1.0"?>',
    "<OpenAPI_ServiceResponse><cmmMsgHeader>",
    "<errMsg>SERVICE ERROR</errMsg>",
    "<returnAuthMsg>SERVICE_KEY_IS_NOT_REGISTERED_ERROR</returnAuthMsg>",
    "<returnReasonCode>30</returnReasonCode>",
    "</cmmMsgHeader></OpenAPI_ServiceResponse>"
  )
  mockery::stub(.fetch_property, "httr2::req_perform",
                function(req) .fake_xml_resp(err_body))
  expect_error(
    .fetch_property("apt", "trade", "11680", "202504",
                    service_key = "K"),
    regexp = "코드=30"
  )
})

test_that("HTTP 5xx → stop()", {
  mockery::stub(.fetch_property, "httr2::req_perform",
                function(req) .fake_xml_resp("", status = 503L))
  expect_error(.fetch_property("apt", "trade", "11680", "202504",
                               service_key = "K"),
               regexp = "HTTP 503")
})
