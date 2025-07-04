% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fgcz.R
\name{.getSamples}
\alias{.getSamples}
\title{get samples of a container as data frame object}
\usage{
.getSamples(
  login = NULL,
  webservicepassword = NULL,
  posturl = NULL,
  containerid = NULL,
  updateProgress = NULL
)
}
\arguments{
\item{login}{bfabric login}

\item{webservicepassword}{bfabric webservicepassword,
visible when you check your user details in the bfabric system.}

\item{posturl}{where the flask server is working}

\item{containerid}{bfabric container id.}

\item{updateProgress}{a callback function for writing log output, e.g.,
using a \code{\link[shiny]{Progress}} object,
see also \url{https://shiny.rstudio.com/articles/progress.html}.}
}
\value{
a \code{data.frame}
}
\description{
get samples of a container as data frame object
}
\details{
2025-01 replace _id by id
}
\examples{
bfabricShiny:::.getSamples(login, webservicepassword,
   posturl = bfabricposturl,
   containerid = 30993) -> smp
smp

lapply(c(29941,30021,30041,30057), FUN = bfabricShiny:::.getSamples,
  login = login,
  webservicepassword = webservicepassword,
  posturl = bfabricposturl) -> smp
smp
}
\author{
CP 2023-03-14, 2025-01-10
}
