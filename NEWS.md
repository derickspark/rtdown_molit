# rtdown.molit 0.2.0

* **부동산 4유형 지원** — 동일한 5단계 인터랙티브 흐름을 공유하는 4개 함수.
  - `rtdown_apt()` — 아파트 (`RTMSDataSvcAptTrade` / `RTMSDataSvcAptRent`)
  - `rtdown_RH()`  — 연립다세대 (`RTMSDataSvcRHTrade` / `RTMSDataSvcRHRent`)
  - `rtdown_SH()`  — 단독/다가구 (`RTMSDataSvcSHTrade` / `RTMSDataSvcSHRent`)
  - `rtdown_O()`   — 오피스텔 (`RTMSDataSvcOffiTrade` / `RTMSDataSvcOffiRent`)
* 결과 tibble 에 `property_type` 컬럼 추가 (`"apt"` / `"rh"` / `"sh"` / `"offi"`).
* 스키마 확장 — `apt_nm` / `mhouse_nm` / `house_type` / `offi_nm` /
  `plottage_ar` / `total_floor_ar` 가 모두 동일 스키마에 포함되며 응답에
  없는 필드는 NA / 빈값.
* 내부 리팩터: `.fetch_apt` → `.fetch_property(property, type, ...)`.

# rtdown.molit 0.1.0

* 최초 릴리스 — 아파트 매매·전월세 지원 (`rtdown_apt()`).
* MOLIT API 클라이언트 (httr2 + xml2, 페이지네이션, OpenAPI 인증오류 분기).
* 시도/시군구 lookup, 인증키 헬퍼.
