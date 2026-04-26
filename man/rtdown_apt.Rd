% Generated stub. Re-generate with `devtools::document()` after edits.
\name{rtdown_property}
\alias{rtdown_property}
\alias{rtdown_apt}
\alias{rtdown_RH}
\alias{rtdown_SH}
\alias{rtdown_O}
\title{Interactively download MOLIT 부동산 실거래 자료}
\usage{
rtdown_apt(type = NULL, sido_code = NULL, sigungu = NULL,
           lawd_cd = NULL, ymd_from = NULL, ymd_to = NULL,
           service_key = NULL, api_timeout = 30L,
           verbose = TRUE, confirm = TRUE)
rtdown_RH(...)
rtdown_SH(...)
rtdown_O(...)
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
\item{...}{\code{rtdown_apt()} 와 동일한 인자.}
}
\value{
모든 거래/계약을 합친 \code{tibble}. \code{property_type} 컬럼이
\code{"apt"} / \code{"rh"} / \code{"sh"} / \code{"offi"} 중 하나로 채워집니다.
\code{attr(., "failed")} 에 실패한 (시군구, 월) 쌍 정보가 첨부됩니다.
}
\description{
다섯 단계의 인터랙티브 메뉴/입력 창 (자료 유형 → 시도 → 시군구
또는 시도 전체 → 시작 연월 → 종료 연월) 을 통해 거래 데이터를
다운로드합니다. 네 함수는 다음과 같이 서로 다른 MOLIT 엔드포인트를 호출합니다:

\itemize{
  \item \code{rtdown_apt()} — 아파트 매매 (\code{RTMSDataSvcAptTrade}) /
        전월세 (\code{RTMSDataSvcAptRent})
  \item \code{rtdown_RH()} — 연립다세대 매매 (\code{RTMSDataSvcRHTrade}) /
        전월세 (\code{RTMSDataSvcRHRent})
  \item \code{rtdown_SH()} — 단독/다가구 매매 (\code{RTMSDataSvcSHTrade}) /
        전월세 (\code{RTMSDataSvcSHRent})
  \item \code{rtdown_O()} — 오피스텔 매매 (\code{RTMSDataSvcOffiTrade}) /
        전월세 (\code{RTMSDataSvcOffiRent})
}

모든 인자를 직접 전달하면 비대화형 (스크립트/테스트) 모드로 동작합니다.
}
\examples{
\dontrun{
rtdown_set_molit_key("...")

df_apt  <- rtdown_apt()                      # 인터랙티브
df_rh   <- rtdown_RH()
df_sh   <- rtdown_SH()
df_offi <- rtdown_O()

df <- rtdown_apt(                            # 비대화형
  type = "trade", sido_code = "11",
  sigungu = "강남구",
  ymd_from = "202501", ymd_to = "202604"
)
}
}
