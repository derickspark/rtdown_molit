\name{rtdown_lawd_codes}
\alias{rtdown_lawd_codes}
\alias{rtdown_sido_table}
\alias{rtdown_sigungu_table}
\title{Korean LAWD (법정동) lookup tables}
\usage{
rtdown_lawd_codes()
rtdown_sido_table()
rtdown_sigungu_table(sido_code = NULL)
}
\arguments{
\item{sido_code}{2-digit 시도 code (character). \code{NULL} = 전국.}
}
\value{
\code{rtdown_lawd_codes()} 는 전체 법정동 \code{tibble}.
\code{rtdown_sido_table()} 는 17개 시·도, \code{rtdown_sigungu_table()} 는 시·군·구.
}
\description{
패키지에 동봉된 \code{inst/extdata/lawd_codes.txt} (행정안전부 공시) 를
파싱해 시도/시군구 별 코드 표를 반환합니다. 결과는 패키지 내부에 캐시됩니다.
}
