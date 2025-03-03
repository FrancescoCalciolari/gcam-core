% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zenergy_L114.wind-energy.R
\name{module_energy_L114.wind}
\alias{module_energy_L114.wind}
\title{module_energy_L114.wind}
\usage{
module_energy_L114.wind(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L114.RsrcCurves_EJ_R_wind}. The corresponding file in the
original data system was \code{LA114.wind.R} (energy level1).
}
\description{
Calculate regional supply curves for wind using country-level supply curves.
}
\details{
Regional supply curves are generated by summing up country-level supply curves and then smoothing them out.
Country-level supply curves are generated using data from Zhou. Every country within a region shares the
the same price points for which to calculate supply. The minimum price is determined to be the lowest base price
within that region; maximum prices are calculated to be the price where supply is 95% that of the available resource,
and the highest maximum price within the region is then used. The rest of the price points are filled out in equal increments.
}
\author{
AJS June 2017
}
