% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepdata.R
\name{data.prep}
\alias{data.prep}
\title{data.prep}
\usage{
data.prep(x)
}
\arguments{
\item{data.frame}{data.frame di input importato come specificato nei dettagli.}
}
\description{
Questa funzione consente la preparazione del dataset per il calcolo degli indici STAR_ICMi e ISA.
}
\details{
Il dataset deve essere importato assicurandosi che nella funzione read.table il comando check.names sia posto uguale a F. La funzione data.prep consente di preparare
dati in cui il nome di uno o pi? taxa sia ripetuto. Nel caso non si utilizzai read.table per l'importazione ? necessario assicurarsi che il taxa ripetuto abbia lo stesso nome
nel data.frame importato (ad esempio "BAETIDAE", "BAETIDAE" e non "BAETIDAE.1", "BAETIDAE.2").\cr
La funzione data.prep fornisce una lista di valori diversa a seconda che si voglia calcolare l'idnice STAR_ICMi o l'indice ISA. Nel primo caso la funzione restituisce
un oggetto di classe STAR e nel secondo un'oggetto di classe ISA
}
\examples{
data.prep()
}
\keyword{dati}
\keyword{dei}
\keyword{preparazione}
