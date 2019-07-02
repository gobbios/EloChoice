# reliability 15_10_07
# replaces 'upsetindex()' function

#' calculate reliability-index
#' 
#' calculate reliability-index of Elo-ratings
#'
#' @param x elochoice-object, the result of \code{\link{elochoice}}
#'
#' @return a data.frame with as many rows as randomizations were run in the original call to \code{elochoice()}. The first column represents the unweighted and the second the weighted reliability index (\emph{R} and \emph{R'}), which is followed by the total number of trials that contributed to the calculation of the index. Note that this number cannot reach the total number of trials in the data set because at least for the very first trial we did not have an expectation for the outcome of that trial (and such trials do not contribute to the calculation of the reliability index).
#' @export
#' @importFrom stats weighted.mean
#' 
#' @references 
#' \insertRef{clark2018}{EloChoice}
#' @author Christof Neumann
#' @examples
#' # create data set and calculate ratings (with five randomizations)
#' xdata <- randompairs(12, 500)
#' x <- elochoice(xdata$winner, xdata$loser, runs=5)
#' # extract the reliability values
#' (u <- reliability(x))
#' # calculate average reliability index
#' mean(u$upset)
#' # and in its weighted form
#' mean(u$upset.wgt)

reliability <- function(x) {
  ups <- N <- upsw <- numeric(as.numeric(x$misc["runs"]))
  for (i in 1:length(ups)) {
    ind <- which(x$decmat[i, ])
    u <- as.numeric(x$upsmat[i, ][ind])
    wg <- x$wgtmat[i, ][ind]
    ups[i] <- 1 - sum(u) / length(u)
    upsw[i] <- 1 - weighted.mean(u, w = abs(wg))
    N[i] <- length(u)
  }
  return(data.frame(upset = ups, upset.wgt = upsw, totIA = N))
}
