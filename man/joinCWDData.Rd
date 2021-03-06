% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joinCWDData.R
\name{joinCWDData}
\alias{joinCWDData}
\title{joinCWDData: compile coarse woody debris data.}
\usage{
joinCWDData(
  units = c("ha", "acres"),
  park = "all",
  from = 2006,
  to = 2018,
  QAQC = FALSE,
  locType = "VS",
  panels = 1:4,
  output,
  ...
)
}
\arguments{
\item{units}{Calculates CWD Volume based on different units.
\describe{
\item{"ha"}{Default. Returns CWD volume as cubic m/hectare}
\item{"acres"}{Returns CWD volume as cubic ft/ acre}
}}
}
\value{
returns a dataframe with CWD volume for each plot, one with cubic m/ha and cubic ft/acre
}
\description{
This function combines and calculates CWD volume for each plot. Must run importData first.
}
\examples{
importData() #imports using default odbc
# Compile CWD data for MABI for most recent survey and return in ft^3/acre
cwd_data <- joinCWDData(park = 'MABI', from = 2015, to = 2018, units = 'acres')

# Compile CWD data for all parks (default) for most recent survey and return in m^3/ha (default)
cwd_data <- joinCWDData(from = 2015, to = 2018)

}
