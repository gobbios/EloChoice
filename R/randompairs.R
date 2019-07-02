# randompairs 15_06_12


#' generate random data of pairwise preference ratings
#'
#' @param nstim numeric, number of stimuli, must be less than 2,602
#' @param nint numeric, number of paired ratings to be created
#' @param reverse numeric, proportion of ratings that go against the default preference, see below for details
#' @param skew logical, by default \code{FALSE}, see below for details
#'
#' @details The default preference for a given pair is given by their alphanumerical order. E.g. \emph{A} is preferred over \emph{M}, and \emph{kf} over \emph{kz}. The \code{reverse=} argument specifies the proportion of ratings that go against this default order.
#' 
#' The number of appearances of a given stimulus in the data set is by default determined by uniform sampling of individual stimuli, i.e. all stimuli will roughly appear equally often in a data set. If a somewhat more realistic (i.e. unbalanced) distribution is desired, the argument \code{skew=TRUE} will achieve sampling based on a negative binomial distribution.
#' @return \code{data.frame} with winner and loser column. An additional column (\code{index}) serves as an index for the sequence in which the trials occurred.
#' @author Christof Neumann
#' @export
#' @importFrom utils combn
#' @importFrom stats rnbinom
#'
#' @examples
#' # a relatively balanced data set
#' xdata <- randompairs(20, 500, skew=FALSE)
#' table(c(as.character(xdata$winner), as.character(xdata$loser)))
#' range(table(c(as.character(xdata$winner), as.character(xdata$loser))))
#' 
#' # and a less balanced data set
#' xdata <- randompairs(20, 500, skew=TRUE)
#' table(c(as.character(xdata$winner), as.character(xdata$loser)))
#' range(table(c(as.character(xdata$winner), as.character(xdata$loser))))

randompairs <- function(nstim = 10, nint = 100, reverse = 0.1, skew = FALSE) {
  if (nstim <= 26) {
    IDs <- sort(sample(letters, nstim))
  }
  if (nstim > 26 & nstim <= 325) {
    com <- combn(26, 2)[, -177]
    samplecom <- com[, sample(1:ncol(com), nstim)]
    IDs <- apply(samplecom, 2, function(x) letters[x])
    IDs <- sort(apply(IDs, 2, function(x) paste(x[1], x[2], sep = "")))
  }
  if (nstim > 325 & nstim <= 2601) {
    com <- combn(26, 3)
    samplecom <- com[, sample(1:ncol(com), nstim)]
    IDs <- apply(samplecom, 2, function(x) letters[x])
    IDs <- sort(apply(IDs, 2, function(x) paste(x[1], x[2], x[3], sep = "")))
  }


  if (!skew) {
    xdata <- cbind(sample(IDs, nint * 2, TRUE), sample(IDs, nint * 2, TRUE))
  }
  if (skew) {
    weights <- rnbinom(nstim, 10, 0.5)
    xdata <- cbind(sample(IDs, nint * 2, TRUE, weights), sample(IDs, nint * 2, TRUE, weights))
  }

  xdata <- xdata[xdata[, 1] != xdata[, 2], ]
  xdata <- xdata[1:nint, ]
  xdata <- t(apply(xdata, 1, sort))
  s <- sample(1:nrow(xdata), ceiling(nrow(xdata) * reverse))
  xdata[s, 1:2] <- xdata[s, 2:1]

  xdata <- data.frame(index = 1:nrow(xdata), winner = xdata[, 1], loser = xdata[, 2])
  return(xdata)

}
