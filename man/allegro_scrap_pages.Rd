% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{allegro_scrap_pages}
\alias{allegro_scrap_pages}
\title{Scrapping auctions}
\usage{
allegro_scrap_pages(base_url, query, pageLimit = -1)
}
\arguments{
\item{base_url}{A url}

\item{query}{Query list}

\item{pageLimit}{Page limit (-1 means all)}
}
\value{
Tibble of auctions
}
\description{
Scrapping auctions
}
\examples{
allegro_scrap_pages("https://allegro.pl/kategoria/elektronika-67193", list(), 1)

}
