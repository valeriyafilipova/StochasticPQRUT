% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crtduration.R
\name{criticalduration}
\alias{criticalduration}
\title{Critical duration}
\usage{
criticalduration(Q, P, qtT = 0.9, PDFplots = TRUE, intEvent = 7,
  writeResults = TRUE)
}
\arguments{
\item{Q}{discharge data;  dataframe with columns date ("\%Y-\%m-\%d") and Q, if writeResults=TRUE path to csv file with columns date and Q}

\item{P}{precipitation data; dataframe with columns date and Pr,if writeResults=TRUE path to txt file with columns date and Pr}

\item{qtT}{threshold quantile for  peak over threshold events}

\item{PDFplots}{if TRUE pdf file with plots is saved in the same directory}

\item{intEvent}{separation time between flood events in days}

\item{pathmain}{path to files, not needed if writeResults=FALSE}
}
\value{
if writeResults is TRUE the results are saved in the directory pathmain if FALSE a list that  contains duration and dataframes that lists the POT for each season is returned
}
\description{
Extract observed POT (Peak over Threshold) flood events and define the critical duration based on the correlation between a set
of flood events over a threshold and the precipitation of the preceding days
}
\examples{
\dontrun{
 criticalduration(Q="62.5QJ.csv",P="PJ1.txt",qtT=0.9,PDFplots=TRUE,intEvent=7,writeResults=TRUE)
 criticalduration(Q=Qd,PJ=P,qtT=0.9,PDFplots=TRUE,intEvent=7,writeResults=FALSE)
}
}
