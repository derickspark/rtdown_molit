# rtdown.molit

> 국토교통부 아파트 실거래가(매매·전월세) 인터랙티브 다운로더

`rtdown.molit` 패키지는 단 하나의 명령 `rtdown_apt()` 으로 국토교통부
공공데이터포털의 아파트 실거래가 자료를 단계별 메뉴를 따라 다운로드하는
R 패키지입니다.

> **참고**: R 패키지 이름은 언더스코어(`_`) 를 허용하지 않아 패키지명은
> `rtdown.molit` 으로 등록되었습니다. GitHub 리포지토리는 그대로
> `rtdown_molit` 입니다.

## 인터랙티브 흐름

`rtdown_apt()` 을 실행하면 다음 다섯 단계를 차례로 묻습니다:

1. **자료 유형** — 매매(`RTMSDataSvcAptTrade`) 또는 전월세(`RTMSDataSvcAptRent`)
2. **시·도** — 17개 시도 중 하나
3. **시·군·구** — 해당 시도 산하 \(전체\) 또는 하나의 시군구
4. **시작 연월** — `YYYYMM` 6자리 숫자 (예: `202501`)
5. **종료 연월** — `YYYYMM` 6자리 숫자 (예: `202604`)

다섯 입력이 끝나면 자동으로 시군구 × 월을 순차 호출하여 모든 거래를 하나의
`tibble` 로 반환합니다.

## 설치

```r
# install.packages("devtools")
devtools::install_github("derickspark/rtdown_molit")
library(rtdown.molit)
```

## 빠른 시작

```r
# 1) MOLIT 인증키 등록 (세션 단위)
rtdown_set_molit_key("MY_PUBLIC_DATA_PORTAL_KEY")

# 2) 인터랙티브 실행
df <- rtdown_apt()
#> ── [ 1 / 5 ] 자료 유형 선택 ──
#>
#> 1: 아파트 매매 (RTMSDataSvcAptTrade)
#> 2: 아파트 전월세 (RTMSDataSvcAptRent)
#>
#> 다운로드할 자료 유형을 선택하세요: 1
#>
#> ── [ 2 / 5 ] 시·도 선택 ──
#> ...
```

영속화하려면 `~/.Renviron` 에 `RTDOWN_MOLIT_KEY=...` 줄을 추가합니다
(`usethis::edit_r_environ()` 추천).

## 비대화형 모드

스크립트나 테스트에서 쓸 때는 모든 인자를 직접 전달하면 메뉴를 거치지 않고
바로 실행됩니다:

```r
df <- rtdown_apt(
  type      = "trade",        # 또는 "rent"
  sido_code = "11",           # 서울특별시
  sigungu   = "강남구",       # 또는 "all" (시도 전체)
  ymd_from  = "202501",
  ymd_to    = "202604",
  confirm   = FALSE
)

# lawd_cd 로 직접 지정도 가능
df <- rtdown_apt(
  type = "trade", lawd_cd = "11680",
  ymd_from = "202501", ymd_to = "202604"
)
```

## 반환 컬럼

| 컬럼 | 설명 |
| --- | --- |
| `lawd_cd` | 5자리 시군구 코드 |
| `sigungu_nm` | 시군구 한글명 |
| `deal_ymd` | 호출 대상 연월 (YYYYMM) |
| `data_type` | `"trade"` 또는 `"rent"` |
| `apt_nm`, `floor`, `exclu_use_ar`, … | 단지/면적/층 등 응답 그대로 |
| `deal_year/month/day`, `deal_date` | 계약일 |
| `deal_amount_manwon`, `deal_amount_won`, `price_per_m2_won` | (매매) |
| `deposit_manwon`, `monthly_rent_manwon`, `deposit_won`, … | (전월세) |
| `full_address`, `full_address_road` | 지번/도로명 주소 |

`attr(df, "failed")` 에 실패한 (시군구, 월, 에러) 기록이 list 로 첨부됩니다.

## 보조 함수

```r
rtdown_lawd_codes()           # 전체 법정동 tibble
rtdown_sido_table()           # 17개 시·도 코드/이름
rtdown_sigungu_table("11")    # 시도별 시군구 (서울 = "11")
rtdown_set_molit_key("...")   # MOLIT 키 설정
rtdown_get_molit_key()        # 현재 키 조회
```

## 의존성

`httr2`, `xml2`, `tibble`, `cli` (Imports)
`testthat`, `withr`, `mockery` (Suggests)

## 테스트

```r
devtools::test()
```

## 라이선스

MIT © 2026 Derick S. Park
