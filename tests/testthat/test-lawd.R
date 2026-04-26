test_that("rtdown_lawd_codes loads + classifies sido/sigungu", {
  d <- rtdown_lawd_codes()
  expect_s3_class(d, "tbl_df")
  expect_true(all(c("code", "name", "sido_code", "sido_name",
                    "lawd_cd", "sigungu_name",
                    "is_sido", "is_sigungu") %in% names(d)))

  seoul <- d[d$is_sido & d$sido_code == "11", ]
  expect_gte(nrow(seoul), 1L)

  sg <- d[d$is_sigungu, ]
  expect_true(all(nchar(sg$lawd_cd) == 5L))
  expect_true(all(!is.na(sg$sigungu_name)))
  expect_true(all(!is.na(sg$sido_name)))
})

test_that("rtdown_sido_table 17개 시·도 (또는 그 이상)", {
  s <- rtdown_sido_table()
  expect_s3_class(s, "tbl_df")
  expect_true(all(c("sido_code", "sido_name") %in% names(s)))
  # 한국에 17개 시도 (특별시/광역시/도)
  expect_gte(nrow(s), 17L)
  expect_true("11" %in% s$sido_code)   # 서울
  expect_true("48" %in% s$sido_code)   # 경상남도
  expect_true(any(grepl("서울", s$sido_name)))
})

test_that("rtdown_sigungu_table 시도별 필터", {
  all_sg <- rtdown_sigungu_table()
  expect_true(all(nchar(all_sg$lawd_cd) == 5L))

  seoul_sg <- rtdown_sigungu_table("11")
  expect_true(all(seoul_sg$sido_code == "11"))
  expect_equal(nrow(seoul_sg), 25L)  # 서울 25 자치구
})
