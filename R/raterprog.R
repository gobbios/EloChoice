#' reliability with progressive rater inclusion
#' 
#' reliability with progressive rater inclusion
#' 
#' @usage raterprog(winner, loser, raterID, runs=100, ratershuffle=1, progbar=TRUE, kval=100,
#' startvalue=0, normprob=FALSE)
#'
#' @param winner character, vector with the IDs of the winning (preferred) stimuli
#' @param loser character, vector with the IDs of the losing (not preferred) stimuli
#' @param raterID a vector (numeric, character, factor) with rater IDs
#' @param runs numeric, number of randomizations
#' @param ratershuffle numeric, number of times rater order is reshuffled/randomized
#' @param progbar logical, should a progress bar be displayed
#' @param kval numeric, k-value, which determines the maximum number of points a stimulus' rating can change after a single rating event, by default 100
#' @param startvalue numeric, start value around which ratings are centered, by default \code{0}
#' @param normprob logical, by default \code{FALSE}, which indicates a logistic approach is taken for calculating winning probabilities (see Elo 1978). Alternatively (\code{TRUE}), winning probabilities are calculated from a normal distribution
#' @param xdata results from \code{\link{raterprog}}
#' 
#' @details 
#' \code{raterprog()} calculates \code{\link{reliability}}, increasing the number of raters to be included in the rating process in a step-wise fashion. In the first (and by default only one) run, the first rater is the one that appears first in the data set, and in subsequent steps raters are added by the order in which they occur. If \code{ratershuffle=} is set to values larger than 1, the order in which raters are included is randomized.
#' 
#' \code{raterprogplot()} plots the matrix resulting from \code{raterprog()}. If \code{ratershuffle=} is larger than 1, the average reliability index is plotted alongside quartiles and results from the original rater inclusion sequence.
#' 
#' Note that the function currently only calculates the weighted version of the \code{\link{reliability}} index.
#' 
#' @return a numeric matrix. Rows correspond to number of raters in the data set, while columns reflect the number of times the rater order is reshuffled.
#' @export
#' @references 
#' \insertRef{clark2018}{EloChoice}
#' @author Christof Neumann after suggestion by TF
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @examples
#' data("physical")
#' # limit to 12 raters
#' physical <- physical[physical$raterID < 14, ]
#' 
#' x <- raterprog(physical$Winner, physical$Loser, physical$raterID, ratershuffle = 1)
#' raterprogplot(x)
#' \donttest{
#' # with multiple orders in which raters are added
#' x <- raterprog(physical$Winner, physical$Loser, physical$raterID, ratershuffle = 10)
#' raterprogplot(x)}

raterprog <- function(winner, loser, raterID, runs=100, ratershuffle=1, progbar=TRUE, kval=100, startvalue=0, normprob=FALSE) {
  # 'ratershuffle' is the number of different orders by which raters are included
  # arguments after 'progbar' are set to the default values of 'elochoice()' function

  # run some checks
  if (length(unique(c(length(winner), length(loser), length(raterID)))) > 1) stop("input vectors are not of the same length", call. = FALSE)
  if (length(winner) == 0) stop("winner is empty", call. = FALSE)
  if (length(loser) == 0) stop("loser is empty", call. = FALSE)
  if (length(raterID) == 0) stop("rater is empty", call. = FALSE)

  # get rater IDs
  rids <- unique(as.character(raterID))
  # and a numeric version
  ids <- 1:length(rids)
  # set up progress bar
  if (progbar) pb <- txtProgressBar(0, length(ids) * ratershuffle, style = 3)

  # create results table (one row per rater, one column per 'ratershuffle')
  res <- matrix(ncol = ratershuffle, nrow = length(ids))
  for (m in 1:ratershuffle) {
    for (i in ids) {
      temp <- elochoice(winner[raterID %in% rids[1:i]], loser[raterID %in% rids[1:i]], runs = runs, kval = kval, startvalue = startvalue, normprob = normprob)
      res[i, m] <- mean(reliability(temp)$upset.wgt)
      if (progbar) setTxtProgressBar(pb, (m - 1) * length(ids) + i)
    }
    # reshuffle order of raters if 'ratershuffle' > 1 is specified
    rids <- sample(rids)
  }

  ## class(res) <- "raterprog"
  return(res)

}



#' @rdname raterprog
#' @export

raterprogplot <- function(xdata) {
  shuffles <- ncol(xdata)
  ids <- 1:nrow(xdata)
  yl <- expression(reliability~index~italic("R'"))
  plot(0, 0, xlim = range(ids), ylim = range(xdata), las = 1, type = "n", xlab = "number of raters", ylab = yl)
  if (shuffles > 1) {
    x <- apply(xdata, 1, quantile, c(0.25, 0.75))
    # quartiles and mean accross shuffles
    segments(ids, x[1, ], ids, x[2, ], col = "black")
    points(ids, rowMeans(xdata), pch = 16)
    # original rater order
    points(ids, xdata[, 1], cex = 0.7, col = "grey", pch = 16)

    legend("bottom", legend = c("mean and quartiles", "original"), pch = 16, col = c("black", "grey"), pt.cex = c(1, 0.7), lty = c(1, 0), bty = "n")

  }
  if (shuffles == 1) {
    points(ids, rowMeans(xdata), pch = 16)
  }
}
