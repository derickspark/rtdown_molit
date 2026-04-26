# rtdown.molit 0.3.0

* **다운로드 진행 표시 + 시간** — `verbose = TRUE` 일 때 한 줄에 in-place
  갱신되는 `cli::cli_progress_bar` 가 표시됩니다. 형식:

  ```
  [42/400] 강남구 (11680) · 202504  │ 경과 1m 12s │ 남은 ~10m 18s │ 총 ~11m 30s
  ```

  현재 호출 중인 시군구·월, 누적 경과 시간, 남은 예상 시간 (ETA), 전체
  예상 시간이 모두 한 줄에 보입니다. 완료 시:

  ```
  ✔ 400회 호출 완료 · 총 소요 11m 25s
  ```

  실패한 (시군구, 월) 은 진행 바 위에 별도 경고로 표시됩니다.

* **선택 입력창 UX** — 안내 문구가 입력창과 같은 줄에 보이도록 개선.
  `utils::menu()` 의 고정된 "Selection:" 프롬프트 대신 새로 만든
  `.menu_prompt()` 헬퍼가 메뉴 옵션을 출력한 뒤 안내문이 포함된
  `readline()` 프롬프트 (예: `"시·도를 선택하세요 (1-17, 0=취소): "`)
  를 한 줄에 표시. YYYYMM 입력도 별도 안내 줄 없이 프롬프트 한 줄에
  형식 안내가 포함됩니다
  (예: `"시작 연월 (YYYYMM 6자리 숫자, 예: 202501, 빈 입력=취소): "`).

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
