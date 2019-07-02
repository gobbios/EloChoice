# based on 'makepairwise 161113.r'

# function to create a 'paircomplist' (psychotools package)
# necessary to run Bradley-Terry models via BTmodel (BradleyTerry2 package)
# we don't consider ties, i.e. forced choice
# we don't consider magnitudes of differences in winning/losing



#' transform preference data
#' 
#' transform preference data into paircomp format (\code{\link[psychotools:paircomp]{paircomp}})
#'
#' @param winner character, vector with the IDs of the winning (preferred) stimuli
#' @param loser character, vector with the IDs of the losing (not preferred) stimuli
#' @param rater character, vector of rater identity
#'
#' @return object of class \code{paircomp}
#' @export
#' @seealso \code{\link[psychotools:paircomp]{psychotools}}
#' @author Christof Neumann
#' @importFrom psychotools paircomp labels<-
#'
#' @examples
#' w <- c("B", "A", "E", "E", "D", "D", "A", "D", "E", "B", "A", "E", "D", "C", "A")
#' l <- c("C", "C", "C", "D", "B", "C", "E", "A", "B", "D", "E", "B", "E", "D", "C")
#' raters <- rep(letters[1:3], 5)
#' makepairwise(w, l, raters)

makepairwise <- function(winner, loser, rater) {
  # all stimuli/items
  allstim <- sort(unique(c(as.character(winner), as.character(loser))))
  # put together as matrix
  x <- cbind(as.character(rater), as.character(winner), as.character(loser)); colnames(x) <- c("rater", "winner", "loser")

  # all pairwise combinations of stimuli
  allc <- t(combn(allstim, 2))
  allc <- allc[order(allc[, 2]), ]

  # pairwise matrix, to be filled
  xdata <- matrix(NA, nrow = length(unique(rater)), ncol = nrow(allc))

  # go through raters
  for (ID in 1:length(unique(rater))) {
    # empty vector to be filled with choices of rater 'ID' for the choices this rater actually made
    tempres <- rep(NA, nrow(allc))
    # subset of all choices for rater 'ID'
    temp <- x[rater == unique(rater)[ID], , drop = FALSE]
    tempw <- temp[, "winner"]
    templ <- temp[, "loser"]
    # go through all possible choices and save those that actually ocurred
    for (r in 1:nrow(temp)) {
      xind <- which( (allc[, 1] == tempw[r] & allc[, 2] == templ[r]) | (allc[, 2] == tempw[r] & allc[, 1] == templ[r]))
      if (allc[xind, 1] == tempw[r]) tempres[xind] <- 1
      if (allc[xind, 1] == templ[r]) tempres[xind] <- -1
    }
    # save to pairwise matrix
    xdata[ID, ] <- tempres
    # clean up
    rm(tempres, temp, r, xind, templ, tempw)
  }


  # transform the matrix in paircomp and put stimulus IDs back
  xdata <- paircomp(xdata)
  labels(xdata) <- allstim

  return(xdata)
}
