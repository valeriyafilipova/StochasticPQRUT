% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simInitcond.R
\name{initconditions}
\alias{initconditions}
\title{Simulates initial conditions}
\usage{
initconditions(pathmain = NULL, incondFt, incondWt, incondSpt, incondSt, Nsim,
  durt, writeResults = TRUE, PDFplots = TRUE)
}
\arguments{
\item{pathmain}{path to files , not needed if writeResults=FALSE}

\item{incondFt, incondWt, incondSpt, incondSt}{dataframes that contain the POT for each season}

\item{Nsim}{Number of simulation}

\item{durt}{critical duration}
}
\value{
if writeResults is TRUE the results are saved in the directory pathmain if FALSE a list that  contains  dataframes of initial conditions
(SWE,soil moisture deficit and initial discharge ) for events for each season is returned
}
\description{
Simulates initial conditions (initial Q, soil moisture deficit and SWE). The procedure uses truncated mulivariate normal distribution
to simulate values between the minimum and maximum observed. The values for initial Q and soil moisture deficit are first log transformed.
}
\examples{
\dontrun{
g=initconditions(incondFt=b$ntp1F,incondWt=b$ntp1W,incondSpt=b$ntp1Sp,incondSt=b$ntp1S, Nsim=10000,durt=a$d,writeResults=FALSE,PDFplots=TRUE)
}
}
