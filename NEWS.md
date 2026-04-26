# rtdown.molit 0.1.0

* Initial release.
* `rtdown_apt()` — 5단계 인터랙티브 다운로더 (자료유형 → 시도 →
  시군구 또는 시도 전체 → 시작 YYYYMM → 종료 YYYYMM).
* MOLIT 매매(`RTMSDataSvcAptTrade`) 및 전월세(`RTMSDataSvcAptRent`)
  API 클라이언트 (httr2 + xml2, 페이지네이션, OpenAPI 인증오류 분기).
* 시도/시군구 lookup (`rtdown_lawd_codes()`, `rtdown_sido_table()`,
  `rtdown_sigungu_table()`).
* 인증키 헬퍼 (`rtdown_set_molit_key()` / `rtdown_get_molit_key()`,
  `RTDOWN_MOLIT_KEY` 환경변수).
* 비대화형 모드 — 모든 인자를 직접 전달하면 메뉴/입력 없이 바로 실행.
