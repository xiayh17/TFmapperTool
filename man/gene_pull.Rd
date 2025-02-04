% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_tool.R
\name{gene_pull}
\alias{gene_pull}
\title{Query possible gene for user}
\usage{
gene_pull(
  tfmapperdb = tfmapperdb,
  species_key = species_key,
  ip_key = ip_key,
  portion_key = portion_key
)
}
\arguments{
\item{tfmapperdb}{default `tfmapperdb`, SQLiteConnection name}

\item{species_key}{default `species_key`, keywords of Species}

\item{ip_key}{default `ip_key`, keywords of IP}

\item{portion_key}{default `portion_key`, keywords of Portion}
}
\value{
possible gene
}
\description{
Query possible gene for user
}
\examples{
\dontrun{
gene_pull()
}
}
