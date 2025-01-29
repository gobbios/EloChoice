#' Elo-ratings for pairwise comparisons of visual stimuli
#' 
#' @aliases eloint elointnorm
#' 
#' @usage elochoice(winner, loser, kval = 100, startvalue = 0, runs = 1, normprob = FALSE)
#' eloint(winner, loser, allids, kval, startvalues, runs)
#' elointnorm(winner, loser, allids, kval, startvalues, runs)
#' 
#' @param winner character, vector with the IDs of the winning (preferred) and losing (not preferred) stimuli
#' @param loser character, vector with the IDs of the winning (preferred) and losing (not preferred) stimuli
#' @param kval numeric, k-value, which determines the maximum number of points a stimulus' rating can change after a single rating event, by default 100
#' @param startvalue numeric, start value around which ratings are centered,
#'        by default 0. Can also be a named vector with start values specific
#'        for each stimulus.
#' @param runs numeric, number of randomizations
#' @param normprob logical, by default \code{FALSE}, which indicates that a logistic approach is taken for calculating winning probabilities (see Elo 1978). Alternatively (\code{TRUE}), such that winning probabilities are calculated from a normal distribution
#' @param startvalues numeric, start value around which ratings are centered, by default 0
#' @param allids internal, character of all stimulus IDs in the data set
#'
#' @details \code{elochoice()} is the workhorse function of the package, which wraps up all the calculations for obtaining Elo-ratings and the information for the reliability index
#' 
#' \code{eloint()} and \code{elointnorm()} are internal functions (which \code{elochoice()} makes use of) that do most of the calculations, but are usually not directly addressed by the user.
#' @return an object of class \code{elochoice}, i.e. a list with the following items
#'   \item{ratmat}{numeric matrix with final ratings for each stimulus, one row per randomization}
#'   \item{decmat}{logical matrix showing for each randomization (row) and each single rating event (column) whether or not there was an expectation for that trial, i.e. whether the two stimuli's ratings differed before the rating}
#'   \item{upsmat}{logical matrix showing for each randomization (row) and each single rating event (column) whether or not the outcome of a trial was in the direction of the expectation, i.e. whether or not the higher rated stimulus won}
#'   \item{wgtmat}{numeric matrix showing for each randomization (row) and each single rating event (column) the absolute difference in ratings before the rating event}
#'   \item{misc}{various information}
#'   \item{ov}{data set overview, i.e. in how many trials was a stimulus involved and how many trials did each stimulus win and lose}
#'   \item{ias}{character matrix, with the original sequence of rating events}
#' @export
#' 
#' @importFrom Rdpack reprompt
#' 
#' @references 
#' \insertRef{elo1978}{EloChoice}
#' 
#' \insertRef{clark2018}{EloChoice}
#' @author Christof Neumann
#' @examples
#' data(physical)
#' set.seed(123)
#' res <- elochoice(winner = physical$Winner, loser = physical$Loser, runs = 100)
#' summary(res)
#' ratings(res, show = NULL, drawplot = TRUE)
#' 
#' # custom start values
#' set.seed(1)
#' xdata <- randompairs(nstim = 10, nint = 20)
#' res <- elochoice(winner = xdata$winner, loser = xdata$loser,
#'                  runs = 100, startvalue = 0)
#' ratings(res)                
#' 
#' # start values as named vector with all stimuli
#' allids <- unique(c(xdata$winner, xdata$loser)) 
#' startvalue <- rep(0, length(allids))
#' names(startvalue) <- allids
#' startvalue["a"] <- 1000
#' res <- elochoice(winner = xdata$winner, loser = xdata$loser,
#'                  runs = 100, startvalue = startvalue)
#' ratings(res)                  






elochoice <- function(winner, loser, kval = 100, startvalue = 0, runs = 1, normprob = FALSE) {
  # , normprob=TRUE # additional argument to allow distinguishing between logistic and normal approach
  # for now, prefer logistic approach
  # normprob <- FALSE

  winner <- as.character(winner)
  loser <- as.character(loser)

  slfcts <- 0
  if (sum(winner == loser) > 0) {
    slfcts <- sum(winner == loser)
    message("data contained ", sum(winner == loser), " 'self-contests' (identical winner and loser)\nthese cases were excluded from the sequence!")
    ex <- which(winner == loser)
    winner <- winner[-c(ex)]
    loser <- loser[-c(ex)]
  }
  if (length(kval) != 1) warning("k has to be of length 1", call. = FALSE)
  
  allids <- sort(unique(c(winner, loser)))
  if (length(startvalue) > 1) {
    if (length(allids) != length(startvalue)) {
      stop("number of supplied start values does not match number of stimuli", call. = FALSE)
    }
    
    if (!all(allids %in% names(startvalue))) {
      stop("mismatch in ids of stimuli in data and in supplied start values", call. = FALSE)
    }
    startvalues <- startvalue[allids]
  } else {
    startvalues <- rep(startvalue[1], length(allids))
    names(startvalues) <- allids
  }
  
  
  if (normprob) {
    res <- elointnorm(winner, loser, allids, kval[1], startvalues, runs = runs)
  } else {
    res <- eloint(winner, loser, allids, kval[1], startvalues, runs = runs)
  }

  ratmat <- res[[1]]
  colnames(ratmat) <- allids

  decmat <- res[[4]]
  upsmat <- res[[3]]
  wgtmat <- res[[5]]


  ov <- matrix(nrow = length(allids), ncol = 2)
  rownames(ov) <- allids
  ov[names(table(winner)), 1] <- table(winner)
  ov[names(table(loser)), 2] <- table(loser)
  ov <- cbind(ov, rowSums(ov, na.rm = TRUE))
  ov[is.na(ov)] <- 0
  colnames(ov) <- c("winner", "loser", "total")
  misc <- c(as.character(kval), length(allids), runs, length(winner), slfcts)
  names(misc) <- c("kval", "n_allids", "runs", "totN", "slf")

  res <- list(ratmat = ratmat, 
              decmat = decmat, 
              upsmat = upsmat, 
              wgtmat = wgtmat, 
              misc = misc,
              startvalues = startvalues,
              ov = ov, 
              ias = cbind(winner, loser))
  class(res) <- "elochoice"
  return(res)
}
