\name{kenya}
\alias{kenya}
\docType{data}

\title{
Kenya 2010 population raster
}
\description{
An AfriPop raster of the modelled 2010 population in Kenya.
}

\usage{data(kenya)}

\format{
A \code{RasterLayer} object in the WGS84 coordinate system at 1km resolution.
}

\source{
This is a product of the AfriPop project \url{http://www.afripop.org} provided by Professor Andy Tatem.
}

\references{
Linard C., Gilbert, M. Snow, R.W., Noor, A.M. & Tatem, A.J. (2010) Population Distribution, Settlement Patterns and Accessibility across Africa in 2010. PLoS ONE \url{http://www.plosone.org/article/info:doi/10.1371/journal.pone.0031743}
}
\examples{
data(kenya)
plot(kenya)
}
