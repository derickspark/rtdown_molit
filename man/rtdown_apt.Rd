% Generated stub. Re-generate with `devtools::document()` after edits.
\name{rtdown_apt}
\alias{rtdown_apt}
\title{Interactively download MOLIT 아파트 매매 / 전월세 실거래 자료}
\usage{
rtdown_apt(
  type = NULL,
  sido_code = NULL,
  sigungu = NULL,
  lawd_cd = NULL,
  ymd_from = NULL,
  ymd_to = NULL,
  service_key = NULL,
  api_timeout = 30L,
  verbose = TRUE,
  confirm = TRUE
)
}
\arguments{
\item{type}{\code{"trade"} (매매) or \code{"rent"} (전월세). \code{NULL} = 메뉴로 묻기.}
\item{sido_code}{2-digit 시도 code. \code{NULL} = 메뉴로 묻기.}
\item{sigungu}{시군구 한글명 또는 \code{"all"}. \code{NULL} = 메뉴로 묻기.}
\item{lawd_cd}{5-digit 시군구 코드 (\code{sigungu} 대체).}
\item{ymd_from, ymd_to}{YYYYMM 시작/종료. \code{NULL} = 입력으로 묻기.}
\item{service_key}{MOLIT 서비스 키. 기본값은 \code{rtdown_get_molit_key()}.}
\item{api_timeout}{초 단위 타임아웃 (기본 30).}
\item{verbose}{\code{cli} 메시지 표시 여부.}
\item{confirm}{호출이 1회 초과 시 진행 여부 묻기.}
}
\value{
모든 거래/계약을 합친 \code{tibble}. \code{attr(., "failed")} 에 실패한
(시군구, 월) 쌍 정보가 첨부됩니다.
}
\description{
다섯 단계의 인터랙티브 메뉴/입력 창을 통해 자료 유형, 시도, 시군구
(또는 시도 전체), 시작·종료 연월을 선택하면 그 기간의 모든 거래
데이터를 한 번에 다운로드합니다. 모든 인자를 직접 전달하면
비대화형(스크립트/테스트) 모드로 동작합니다.
}
\examples{
\dontrun{
rtdown_set_molit_key("...")
df <- rtdown_apt()                 # 인터랙티브
df <- rtdown_apt(                  # 비대화형
  type = "trade", sido_code = "11",
  sigungu = "강남구",
  ymd_from = "202501", ymd_to = "202604"
)
}
}
