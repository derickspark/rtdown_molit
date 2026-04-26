test_that(".build_address joins umdNm + jibun", {
  expect_equal(.build_address(list(umdNm = "역삼동", jibun = "1")),
               "역삼동 1")
  expect_equal(.build_address(list(umdNm = "역삼동")), "역삼동")
  expect_equal(.build_address(list(jibun = "1")), "1")
  expect_equal(.build_address(list()), "")
  expect_equal(.build_address(NULL), "")
})

test_that(".build_road_address strips leading zeros", {
  row <- list(roadNm = "테헤란로", roadNmBonbun = "00152",
              roadNmBubun = "00007")
  expect_equal(.build_road_address(row), "테헤란로 152-7")

  row2 <- list(roadNm = "테헤란로", roadNmBonbun = "00152",
               roadNmBubun = "")
  expect_equal(.build_road_address(row2), "테헤란로 152")

  expect_equal(.build_road_address(list(roadNm = "")), "")
})

test_that(".coerce_long handles comma-separated 만원 amounts", {
  expect_equal(.coerce_long(c("100,000", "  ", "12345")),
               c(100000, NA, 12345))
})

test_that(".rows_to_tibble apt trade — full schema", {
  rows <- list(
    list(umdNm = "역삼동", jibun = "1", aptNm = "역삼아파트",
         floor = "10", excluUseAr = "84.99",
         dealYear = "2025", dealMonth = "4", dealDay = "15",
         buildYear = "2010", dealAmount = "150,000")
  )
  df <- .rows_to_tibble(rows, property = "apt", type = "trade",
                        lawd_cd = "11680", deal_ymd = "202504",
                        sigungu_nm = "강남구")
  expect_equal(nrow(df), 1L)
  expect_equal(df$lawd_cd, "11680")
  expect_equal(df$data_type, "trade")
  expect_equal(df$property_type, "apt")
  expect_equal(df$apt_nm, "역삼아파트")
  expect_equal(df$deal_amount_won, 150000 * 10000)
  # 다른 부동산 유형의 이름 필드는 NA / 빈 문자열
  expect_equal(df$mhouse_nm, "")
  expect_equal(df$offi_nm, "")
})

test_that(".rows_to_tibble RH trade — mhouseNm captured", {
  rows <- list(
    list(umdNm = "성수동", jibun = "10",
         mhouseNm = "성수빌라", houseType = "다세대",
         excluUseAr = "60.0",
         dealYear = "2025", dealMonth = "4", dealDay = "10",
         buildYear = "1995", dealAmount = "45,000")
  )
  df <- .rows_to_tibble(rows, property = "rh", type = "trade",
                        lawd_cd = "11200", deal_ymd = "202504")
  expect_equal(df$property_type, "rh")
  expect_equal(df$mhouse_nm, "성수빌라")
  expect_equal(df$house_type, "다세대")
  expect_equal(df$apt_nm, "")
  expect_equal(df$deal_amount_won, 45000 * 10000)
})

test_that(".rows_to_tibble SH trade — plottage / total floor area", {
  rows <- list(
    list(umdNm = "방학동", houseType = "단독",
         plottageAr = "120.5", totalFloorAr = "180.0",
         dealYear = "2025", dealMonth = "4", dealDay = "1",
         buildYear = "1990", dealAmount = "70,000")
  )
  df <- .rows_to_tibble(rows, property = "sh", type = "trade",
                        lawd_cd = "11320", deal_ymd = "202504")
  expect_equal(df$property_type, "sh")
  expect_equal(df$house_type, "단독")
  expect_equal(df$plottage_ar, 120.5)
  expect_equal(df$total_floor_ar, 180.0)
  expect_equal(df$apt_nm, "")
})

test_that(".rows_to_tibble offi rent — offiNm + deposit", {
  rows <- list(
    list(umdNm = "삼성동", jibun = "100",
         offiNm = "삼성타워", floor = "12", excluUseAr = "33.5",
         dealYear = "2025", dealMonth = "4", dealDay = "5",
         buildYear = "2018",
         deposit = "10,000", monthlyRent = "85",
         contractType = "신규")
  )
  df <- .rows_to_tibble(rows, property = "offi", type = "rent",
                        lawd_cd = "11680", deal_ymd = "202504")
  expect_equal(df$property_type, "offi")
  expect_equal(df$data_type, "rent")
  expect_equal(df$offi_nm, "삼성타워")
  expect_equal(df$deposit_won, 10000 * 10000)
  expect_equal(df$monthly_rent_won, 85 * 10000)
  expect_false("deal_amount_won" %in% names(df))
})

test_that(".empty_result has property_type column for all 4 types", {
  for (p in c("apt", "rh", "sh", "offi")) {
    e <- .empty_result(p, "trade")
    expect_equal(nrow(e), 0L)
    expect_true("property_type" %in% names(e))
    expect_true(all(c("apt_nm", "mhouse_nm", "house_type", "offi_nm",
                      "plottage_ar", "total_floor_ar") %in% names(e)))
  }
})
