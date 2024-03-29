% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_external_data.R
\name{download_external_data}
\alias{download_external_data}
\title{Download external data}
\usage{
download_external_data(
  data_name,
  data_url,
  data_source,
  data_description,
  source_url,
  ...,
  data_url_function = NA_character_,
  file_pattern = NA_character_
)
}
\arguments{
\item{data_name}{Identification of the variable.}

\item{data_url}{The url link of the download.}

\item{data_source}{Who are the authors of the data, or who they belong to (used for metadata).}

\item{data_description}{Brief description of the data (used for metadata).}

\item{source_url}{The url link to the website where data can be found.}

\item{...}{Not used}

\item{data_url_function}{Function that returns a named list of more than one download url.}

\item{file_pattern}{A keyword to identify the data file in case there are many inside the download (common with compressed files).}
}
\description{
Download external data
}
