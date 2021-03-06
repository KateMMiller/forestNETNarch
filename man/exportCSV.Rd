% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportCSV.R
\name{exportCSV}
\alias{exportCSV}
\title{exportData: Export Access tables in global enviroment to .csv}
\usage{
exportCSV(path = NA)
}
\arguments{
\item{path}{Quoted path to save files to.}
}
\description{
This function exports database tables to .csv that were imported using importData.These
.csv files can then be imported via importCSV. This function is primarily for internal use to create flat
files for users to run the functions in this package without having to use an odbc driver to import and
query NETN data. This is particularly useful for Mac users and those without MS Access installed.
}
