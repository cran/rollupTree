% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_prop.R
\name{df_get_ids}
\alias{df_get_ids}
\title{Get ids from a data frame}
\usage{
df_get_ids(df)
}
\arguments{
\item{df}{a data frame}
}
\value{
all values of the \code{id} column
}
\description{
The default name for a key column in \code{rollup} is \code{id}. \code{df_get_ids} gets all values
from the \code{id} column in a data frame.
}
\examples{
df_get_ids(wbs_table)
}
