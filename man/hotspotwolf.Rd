\name{hotspotwolf}
\alias{hotspotwolf}
\docType{data}
\title{
  Data on Wolf Behaviour in Depredation Hotspots
}
\description{
  This data.frame contains the data used to model the wolf behaviour in
  depredation hotspots.
}
\usage{data("hotspotwolf")}
\format{
  A data frame with 338 observations (interventions) on the following 15 variables.
  \describe{    
    \item{\code{weather}}{a 0/1 vector indicating whether visibility was
      clear and overcast (1) or not (0) during the intervention, as
      assessed by observers}
    \item{\code{habitat}}{a 0/1 vector indicating whether habitat was
      predominantly closed (1) or open (0) during the intervention, as
      assessed by observers}
    \item{\code{flock.id}}{an integer vector indicating the ID of the
      flock protected by the intervention}
    \item{\code{shot}}{a 0/1 vector indicating whether a shot was fired
      or not during the intervention}    
    \item{\code{dog.presence}}{a 0/1 vector indicating the presence (1)
      or absence (0) of livestock guarding dogs protecting the flock}
    \item{\code{fence.presence}}{a 0/1 vector indicating the presence
      (1) or absence (0) of fences protecting the flock}    
    \item{\code{depredation.pressure}}{a 0/1 vector indicating whether
      the intervention type was simple, i.e. with a single shooter (0),
      or reinforced, i.e. with multiple shooters (1)}    
    \item{\code{approach}}{a 0/1 vector indicating whether the wolf
      approached the flock (1) or not (0) during the intervention, as
      assessed by observers}    
    \item{\code{attack}}{a 0/1 vector indicating whether the wolf
      attacked the flock (1) or not (0) during the intervention, as
      assessed by observers}    
    \item{\code{year}}{a numeric vector indicating the year of the intervention}
    \item{\code{shotwolf}}{a 0/1 vector indicating whether a shot was
      fired (1) or not (0) just after detection}    
    \item{\code{shotappr}}{a 0/1 vector indicating whether a
      shot was fired or not just after an approach was diagnosed}    
    \item{\code{shotatta}}{a 0/1 vector indicating whether a shot was
      fired or not just after an attack was diagnosed}
    \item{\code{obs.id}}{a numeric vector containing the ID of the observer}
  }
}
\source{
  Office Francais de la Biodiversite. 5 All. de Bethleem, 38610 Gieres, France.
}
\references{
  Simon R., Calenge C., Quenette P.Y., Jean N., Drouet-Hoguet N. (2024)
  Modeling the predation sequence of wolves in sheep depredation
  hotspots. In prep. 
}
\examples{
data(hotspotwolf)
head(hotspotwolf)
}
\keyword{datasets}
