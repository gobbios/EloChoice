# ratings 15_06_21

#' indiviual stimulus ratings
#' 
#' get stimulus ratings and/or a summary plot
#'
#' @param x an object of class \code{"elochoice"}, usually the result of a call to \code{\link{elochoice}}
#' @param show character, what values should be returned, see below
#' @param drawplot logical, should a plot drawn
#' 
#' @details 
#' If \code{show="original"}, \code{show="mean"} or \code{show="var"}, a numeric vector is returned which contains either the ratings obtained from the initial/original sequence, the average ratings across all randomizations, or the total variance.
#' 
#' If \code{show="range"} or \code{show="all"}, a matrix is returned that contains either the range of ratings across all randomizations, or all ratings of all randomizations.
#' 
#' If you simply want to create the plot without any rating output being generated, use \code{show=NULL}.
#' 
#' If \code{drawplot=TRUE}, a plot is created that depicts the values of the ratings obtained from the initial sequence (red), the mean ratings across all randomizations (black) and the range of ratings across all randomizations.
#' 
#'
#' @return numeric vector or matrix, and/or a plot
#' @export
#' @author Christof Neumann
#' @importFrom graphics plot segments points legend axis box
#' @importFrom stats quantile var
#'
#' @examples
#' xdata <- randompairs(nstim = 10, nint = 100)
#' x <- elochoice(xdata$winner, xdata$loser, runs = 10)
#' 
#' # ratings from the initial sequence
#' ratings(x, "original", drawplot = FALSE)
#' 
#' # range of ratings across all randomizations
#' ratings(x, "range", drawplot = FALSE)
#' 
#' # and producing plot
#' ratings(x, NULL, drawplot = TRUE)
#' 

ratings <- function(x, show = "mean", drawplot = TRUE) {
  # 'show' can be one of the following:
  # original - initial sequence
  # mean - mean across all randomizations
  # range - range across all randomizations
  # all - all values
  # var - variance
  # plot - if true, all the info is plotted

  if (drawplot) {
    temp <- x$ratmat
    temp2 <- apply(temp, 2, range, na.rm = TRUE)
    temp2 <- rbind(temp[1, ], temp2)
    temp2 <- rbind(colMeans(temp, na.rm = TRUE), temp2)
    temp2 <- temp2[, rev(order(temp2[1, ]))]

    plot(0, 0, xlim = c(0, ncol(temp) + 1), ylim = range(temp), "n", axes = FALSE, xlab = "stimulus", ylab = "Elo-rating")
    segments(1:ncol(temp2), temp2[3, ], 1:ncol(temp2), temp2[4, ])
    points(1:ncol(temp2), temp2[1, ], pch = 16)
    points(1:ncol(temp2), temp2[2, ], pch = 16, col = "grey", cex = 0.8)
    axis(1, at = 1:ncol(temp2), labels = colnames(temp2), lwd = NA)
    axis(2, las = 1)
    box()
  }

  if (is.null(show)) show <- "donothing"
  if (show == "original") {
    res <- sort(x$ratmat[1, ], decreasing = TRUE)
    return(res)
  }

  if (show == "mean") {
    res <- sort(colMeans(x$ratmat, na.rm = TRUE), decreasing = TRUE)
    return(res)
  }

  if (show == "range") {
    res <- apply(x$ratmat, 2, range, na.rm = TRUE)
    res <- res[, rev(order(res[1, ]))]
    return(res)
  }

  if (show == "all") {
    temp <- sort(colMeans(x$ratmat, na.rm = TRUE), decreasing = TRUE)
    res <- x$ratmat[, names(temp)]
    return(res)
  }

  if (show == "var") {
    temp <- sort(colMeans(x$ratmat, na.rm = TRUE), decreasing = TRUE)
    res <- apply(x$ratmat, 2, var, na.rm = TRUE)[ names(temp)]
    return(res)
  }
}
