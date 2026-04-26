\name{rtdown_set_molit_key}
\alias{rtdown_set_molit_key}
\alias{rtdown_get_molit_key}
\title{Set or get the MOLIT service key}
\usage{
rtdown_set_molit_key(key)
rtdown_get_molit_key()
}
\arguments{
\item{key}{Character. MOLIT service key.}
}
\value{Invisibly returns the key (or \code{""} for the getter).}
\description{
\code{RTDOWN_MOLIT_KEY} 환경변수에 저장 / 조회. 영속화하려면
\code{~/.Renviron} 에 \code{RTDOWN_MOLIT_KEY=...} 추가.
}
