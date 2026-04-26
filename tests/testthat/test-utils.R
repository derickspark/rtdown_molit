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

test_that(".rows_to_tibble produces full schema (trade)", {
  rows <- list(
    list(umdNm = "역삼동", jibun = "1", aptNm = "역삼아파트",
         floor = "10", excluUseAr = "84.99",
         dealYear = "2025", dealMonth = "4", dealDay = "15",
         buildYear = "2010", dealAmount = "150,000"),
    list(umdNm = "역삼동", jibun = "2", aptNm = "삼성아파트",
         floor = "5", excluUseAr = "59.5",
         dealYear = "2025", dealMonth = "4", dealDay = "20",
         buildYear = "2015", dealAmount = "  ")
  )
  df <- .rows_to_tibble(rows, type = "trade",
                        lawd_cd = "11680", deal_ymd = "202504",
                        sigungu_nm = "강남구")
  expect_equal(nrow(df), 2L)
  expect_equal(df$lawd_cd, c("11680", "11680"))
  expect_equal(df$deal_ymd, c("202504", "202504"))
  expect_equal(df$data_type, c("trade", "trade"))
  expect_equal(df$sigungu_nm, c("강남구", "강남구"))
  expect_equal(df$apt_nm, c("역삼아파트", "삼성아파트"))
  expect_equal(df$floor, c(10L, 5L))
  expect_equal(df$exclu_use_ar, c(84.99, 59.5))
  expect_equal(df$deal_amount_manwon, c(150000, NA))
  expect_equal(df$deal_amount_won, c(150000 * 1e4, NA))
  expect_equal(df$deal_date, c("2025-04-15", "2025-04-20"))
  expect_equal(round(df$price_per_m2_won[1L]),
               round(150000 * 10000 / 84.99))
})

test_that(".rows_to_tibble produces full schema (rent)", {
  rows <- list(list(
    umdNm = "역삼동", jibun = "1", excluUseAr = "59",
    dealYear = "2025", dealMonth = "4", dealDay = "1",
    deposit = "30,000", monthlyRent = "0",
    contractType = "신규", contractTerm = "202504~202604"
  ))
  df <- .rows_to_tibble(rows, type = "rent",
                        lawd_cd = "11680", deal_ymd = "202504")
  expect_equal(nrow(df), 1L)
  expect_equal(df$data_type, "rent")
  expect_equal(df$deposit_manwon, 30000)
  expect_equal(df$deposit_won, 30000 * 10000)
  expect_equal(df$monthly_rent_manwon, 0)
  expect_equal(df$contract_type, "신규")
  expect_false("deal_amount_won" %in% names(df))
})

test_that(".empty_result has full schema for both types", {
  e_trade <- .empty_result("trade")
  e_rent  <- .empty_result("rent")
  expect_equal(nrow(e_trade), 0L)
  expect_equal(nrow(e_rent), 0L)
  expect_true(all(c("apt_nm", "deal_amount_manwon", "deal_amount_won") %in%
                    names(e_trade)))
  expect_true(all(c("deposit_manwon", "deposit_won") %in% names(e_rent)))
})
