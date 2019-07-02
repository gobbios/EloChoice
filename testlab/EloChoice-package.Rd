\name{EloChoice-package}
\alias{EloChoice-package}
\alias{EloChoice}
\docType{package}
\title{Preference Rating for Paired Visual Stimuli Based on Elo Ratings}
\description{This package is designed to efficiently establish global preference scores for visual stimuli based on \emph{pairwise} comparisons of stimuli instead of commonly used Likert scale approaches, which are based on rating \emph{single} stimuli.}
\details{
\tabular{ll}{
Package: \tab EloChoice\cr
Type: \tab Package\cr
Version: \tab 0.29.2\cr
Date: \tab 2016-11-13\cr
License: \tab GPL (>=2)\cr
}
}
\author{
Christof Neumann <christofneumann1@gmail.com>
}
\references{
Clark, A. P., Howard, K. L., Woods, A. T., Penton-Voak, I. S. & Neumann, C. (in prep). Why rate when you could compare? Using the ``EloChoice'' package to assess pairwise comparisons of perceived strength.

Elo, A. E. 1978. The Rating of Chess Players, Past and Present. New York: Arco.

Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495.

Neumann, C., Duboscq, J., Dubuc, C., Ginting, A., Irwan, A. M., Agil, M., Widdig, A. & Engelhardt, A. 2011. Assessing dominance hierarchies: validation and advantages of progressive evaluation with Elo-rating. Animal Behaviour, 82, 911-921.
}
\keyword{ package }
\seealso{
\code{\link[EloRating:EloRating-package]{EloRating}}
}
\examples{
data(physical)
res <- elochoice(winner = physical$Winner, loser = physical$Loser, runs = 2)
summary(res)
ratings(res, drawplot=FALSE)
}
