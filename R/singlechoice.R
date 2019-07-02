# based on "singlechoice 15_09_29.R"

#' update stimulus ratings after one rating event
#' 
#' update stimulus ratings after one rating event
#'
#' @param val1 rating of the preferred stimulus \emph{before} the rating event
#' @param val2 rating of the unpreferred stimulus \emph{before} the rating event
#' @param k value of \emph{k}-constant, which determines the maximum change of ratings after a single rating event
#'
#' @return vector with two values: updated ratings \emph{after} the rating event for preferred and unpreferred stimulus
#' @export
#' @references 
#' \insertRef{elo1978}{EloChoice}
#' @author Christof Neumann
#' @seealso \code{\link[EloRating:e.single]{EloRating}}
#'
#' @examples
#' # little change because rating difference is large (positive), i.e. expectation is clear
#' singlechoice(1200, 500, 100)
#' # no change because rating difference is very large (positive), i.e. expectation is clear
#' singlechoice(1500, 500, 100)
#' # large change because rating difference is small (negative), i.e. expectation is clearly violated
#' singlechoice(500, 1500, 100)

singlechoice <- function(val1, val2, k) {

  # normal approach
  # p_win <- pnorm((val1 - val2)/(200 * sqrt(2)))

  # logistic approach
  p_win <- 1 - 1 / (1 + 10 ^ ( (val1 - val2) / 400))

  kp <- k * p_win

  val1new <- val1 - kp + k
  val2new <- val2 + kp - k

  return(round(c(val1new, val2new)))

}
