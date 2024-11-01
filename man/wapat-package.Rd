\name{wapat-package}
\alias{wapat-package}
\alias{wapat}
\docType{package}
\title{
\packageTitle{wapat}
}
\description{
\packageDescription{wapat}
}
\details{
  This package contains all the code and data used in the study of Simon
  et al.  The vignette, available with \code{vignette("wapat")}
  describes the steps to reproduce the calculations of this paper.
}
\author{
\packageAuthor{wapat}

Maintainer: \packageMaintainer{wapat}
}
\references{
  Simon R., Calenge C., Quenette P.Y., Jean N., Drouet-Hoguet N. (2024)
  Modeling the predation sequence of wolves in sheep depredation
  hotspots. In prep. 
}
\keyword{ package }
\seealso{
  \code{\link{hotspotwolf}} for the original dataset,
  \code{\link{hotspotwolf}} for the model fit, and
  \code{\link{resultsPA}} for the results of the power analysis carried
  out in the paper.
}
\examples{
## Just a glimpse of the original dataset
data(hotspotwolf)
head(hotspotwolf)

}
