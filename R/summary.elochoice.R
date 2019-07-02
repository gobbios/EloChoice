# summary.elochoice 15_08_08


#' summarize elochoice object
#'
#' @param object an object of class \code{"elochoice"}, usually the result of a call to \code{\link{elochoice}}
#' @param ... further arguments passed to or from other methods. Nothing relevant in this case.
#'
#' @export
#' @importFrom stats median
#' @author Christof Neumann
#' @examples
#' xdata <- randompairs(nstim=10, nint=500)
#' x <- elochoice(xdata$winner, xdata$loser, runs=5)
#' summary(x)

summary.elochoice <- function(object, ...) {
  cat("Elo ratings from", object$misc["n_allids"], "stimuli\n")
  cat("total (mean/median) number of rating events: ",
      object$misc["totN"],
      " (",
      round(mean(object$ov[, "total"]), 2),
      "/", median(object$ov[, "total"]),
      ")",
      "\n", sep = "")
  cat("range of rating events per stimulus:",
      min(object$ov[, "total"]),
      "-",
      max(object$ov[, "total"]),
      "\n")
  cat("startvalue:", object$misc["startval"], "\n")
  cat("k:", object$misc["kval"], "\n")
  cat("randomizations:", object$misc["runs"], "\n")
  if (as.numeric(object$misc["slf"]) > 0) message("original data contained ", object$misc["slf"], " 'self-contests' (identical winner and loser)\nthese cases were excluded from the sequence!")

}
