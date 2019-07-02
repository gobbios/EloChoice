# triplets 15_06_16


#' calculate ratings from sequence of rating events, allowing for more than two stimuli
#' 
#' calculate ratings from sequence of rating events, allowing for more than two stimuli
#'
#' @param xdata data.frame or matrix with stimulus IDs, each row representing one trial, needs to contain at least two columns
#' @param winner numeric vector of the same length as \code{nrow(xdata)}, indicating which column in \code{xdata} is the winner/preferred stimulus
#' @param runs numeric, the number of times the data set should be randomized
#' @param startvalue numeric, initial value of ratings, by default \code{0}
#' @param k numeric, value of \emph{k}-constant
#' @param progressbar logical, by default \code{TRUE}. Should a progress bar be displayed
#' @param mode character, either \code{"avg"} (default) or \code{"seq"}, see Details section
#' @details 
#' The \code{mode="avg"} option considers the losers of the trial as one individual/stimulus, whose rating is averaged. This reflects one rating step for each trial (as for \code{elochoice()}).
#' 
#' The \code{mode="seq"} option runs a sequence of interactions \emph{within} a trial, i.e. one rating step for each of the loosing stimuli. E.g. if you have three stimuli, that would be two rating steps. With four stimuli, we would have three steps, etc.
#' 
#' Because of the larger number of rating events with \code{mode="seq"}, the range of Elo-ratings will be larger as compared to \code{mode="avg"}. The average values will be the same for both though (start value). See examples...
#' 
#' Also note that this is an experimental function that has not yet been tested thoroughly! In addition, this function calculates winning probabilities in a slightly different way as compared to \code{elochoice}, i.e. based on normal probabilities (see \code{\link{elochoice}}).
#' 
#' @return a matrix with ratings
#' @author Christof Neumann
#' @export
#' @importFrom stats pnorm
#'
#' @examples
#' 
#' data(physical)
#' y <- round(triplets(physical[, 2:3], winner = rep(1,nrow(physical)), runs = 1))
#' x <- ratings(elochoice(physical$Winner, physical$Loser, runs = 1), show = "all", drawplot = FALSE)
#' x <- x[order(names(x))]
#' plot(x, y)
#' 
#' xdata <- as.matrix(t(sapply(1:500, function(x)sample(letters[1:8], 3))))
#' xdata <- t(apply(xdata, 1, sort))
#' winner <- sample(1:3, nrow(xdata), TRUE, prob = c(4, 0.8, 0.1))
#' 
#' x <- triplets(xdata, winner, runs=20, mode="avg")
#' y <- triplets(xdata, winner, runs=20, mode="seq")
#' 
#' # note different ranges along the axes
#' plot(colMeans(x), colMeans(y))
#' range(colMeans(x))
#' range(colMeans(y))

triplets <- function(xdata, winner, runs = 2, startvalue = 0, k = 100, progressbar = TRUE, mode = "avg") {
  # IDs per contest
  nid <- dim(xdata)[2]
  if (max(winner) > nid) stop(paste("winner indicates", max(winner), "individuals per contest, but only", nid, "found" ))

  xdata <- apply(xdata, 2, as.character)
  allids <- sort(unique(as.character(xdata)))

  if (mode == "seq") {
    # from wide version to long version with winner in first column and loser in second
    x <- do.call("rbind", sapply(1:nrow(xdata), function(X) {
      cbind(xdata[X, winner[X]], sample(xdata[X, -winner[X]]))
    }, simplify = FALSE))

    mat <- matrix(nrow = nrow(xdata), ncol = length(allids))
    colnames(mat) <- allids
    ups <- dec <- numeric(nrow(mat) * (nid - 1))
    curelo <- mat[1, ]
    curelo[ ] <- startvalue

    for (i in 1:nrow(x)) {
      p_win <- pnorm( (curelo[x[i, 1]] - curelo[x[i, 2]]) / (200 * sqrt(2)))
      (kp <- k * p_win)
      if (p_win == 0.5) dec[i] <- 1
      if (p_win < 0.5) ups[i] <- 1
      # winner gets 'k-kp' points
      curelo[x[i, 1]] <- curelo[x[i, 1]] - kp + k
      curelo[x[i, 2]] <- curelo[x[i, 2]] + kp - k
      curelo; mean(curelo)
      #
    }

    res <- curelo


    if (runs > 1) {
      if (progressbar) pb <- txtProgressBar(1, runs, style = 3)
      for (r in 2:runs) {
        xdata <- xdata[sample(1:nrow(xdata)), ]
        x <- do.call("rbind", sapply(1:nrow(xdata), function(X) {
          cbind(xdata[X, winner[X]], sample(xdata[X, -winner[X]]))
        }, simplify = FALSE))

        ups <- dec <- numeric(nrow(mat) * (nid - 1))
        curelo <- mat[1, ]
        curelo[ ] <- startvalue

        for (i in 1:nrow(x)) {
          p_win <- pnorm( (curelo[x[i, 1]] - curelo[x[i, 2]]) / (200 * sqrt(2)))
          kp <- k * p_win
          if (p_win == 0.5) dec[i] <- 1
          if (p_win < 0.5) ups[i] <- 1
          # winner gets 'k-kp' points
          curelo[x[i, 1]] <- curelo[x[i, 1]] - kp + k
          curelo[x[i, 2]] <- curelo[x[i, 2]] + kp - k
          curelo; mean(curelo)
          #
        }
        if (progressbar) setTxtProgressBar(pb, r)
        res <- rbind(res, curelo)
      }
      if (progressbar) close(pb)
    }

  }

  if (mode == "avg") {

    mat <- matrix(nrow = nrow(xdata), ncol = length(allids))
    colnames(mat) <- allids
    ups <- dec <- numeric(nrow(mat))
    curelo <- mat[1, ]
    curelo[ ] <- startvalue

    for (i in 1:nrow(xdata)) {
      p_win <- pnorm( (curelo[xdata[i, winner[i]]] - mean(curelo[xdata[i, -winner[i]]])) / (200 * sqrt(2)))
      kp <- k * p_win
      if (p_win == 0.5) dec[i] <- 1
      if (p_win < 0.5) ups[i] <- 1
      # winner gets 'k-kp' points
      curelo[xdata[i, winner[i]]] <- curelo[xdata[i, winner[i]]] - kp + k
      curelo[xdata[i, -winner[i]]] <- curelo[xdata[i, -winner[i]]] + (kp - k) / (nid - 1)
      curelo; mean(curelo)
      #
    }

    res <- curelo

    if (runs > 1) {
      if (progressbar) pb <- txtProgressBar(1, runs, style = 3)
      for (r in 2:runs) {
        xdata <- xdata[sample(1:nrow(xdata)), ]
        ups <- dec <- numeric(nrow(mat))
        curelo <- mat[1, ]
        curelo[ ] <- startvalue

        for (i in 1:nrow(xdata)) {
          p_win <- pnorm( (curelo[xdata[i, winner[i]]] - mean(curelo[xdata[i, -winner[i]]])) / (200 * sqrt(2)))
          (kp <- k * p_win)
          if (p_win == 0.5) dec[i] <- 1
          if (p_win < 0.5) ups[i] <- 1
          # winner gets 'k-kp' points
          curelo[xdata[i, winner[i]]] <- curelo[xdata[i, winner[i]]] - kp + k
          curelo[xdata[i, -winner[i]]] <- curelo[xdata[i, -winner[i]]] + (kp - k) / (nid - 1)
          curelo; mean(curelo)
          #
        }
        if (progressbar) setTxtProgressBar(pb, r)
        res <- rbind(res, curelo)
      }
      if (progressbar) close(pb)
    }
  }

  rownames(res) <- NULL
  return(res)
}
