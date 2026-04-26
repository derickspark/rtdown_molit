test_that("MOLIT 키 set/get round trips", {
  withr::with_envvar(c(RTDOWN_MOLIT_KEY = ""), {
    expect_equal(rtdown_get_molit_key(), "")
    rtdown_set_molit_key("abc-key")
    expect_equal(rtdown_get_molit_key(), "abc-key")
  })
})

test_that("MOLIT 키 비어있는/잘못된 입력 거부", {
  expect_error(rtdown_set_molit_key(""))
  expect_error(rtdown_set_molit_key(NULL))
  expect_error(rtdown_set_molit_key(123))
  expect_error(rtdown_set_molit_key(c("a", "b")))
})
