test_that("individual start values work", {
  
  xx <- randompairs(nstim = 8, nint = 33)
  allids <- unique(c(xx$winner, xx$loser))
  startvals <- rep(0, length(allids))
  names(startvals) <- allids
  
  set.seed(1)
  r1 <- ratings(elochoice(winner = xx$winner, loser = xx$loser, startvalue = startvals, runs = 20), drawplot = FALSE)
  set.seed(1)
  r2 <- ratings(elochoice(winner = xx$winner, loser = xx$loser, startvalue = 0, runs = 20), drawplot = FALSE)
  
  expect_equal(r1, r2)
  set.seed(NULL)
  
  # missing stimulus in start values
  names(startvals)[1] <- "notastimulus"
  expect_error(elochoice(winner = xx$winner, loser = xx$loser, startvalue = startvals, runs = 20))
  
  # wrong length
  startvals <- startvals[-1]
  expect_error(elochoice(winner = xx$winner, loser = xx$loser, startvalue = startvals, runs = 20))
})


test_that("individual start values work", {
  xdata <- randompairs(nstim = 10, nint = 20)
  allids <- unique(c(xdata$winner, xdata$loser))
  startvalue <- rep(0, length(allids))
  names(startvalue) <- allids
  startvalue[1] <- round(runif(1, 10, 1000))
  res <- elochoice(winner = xdata$winner, loser = xdata$loser,
                   runs = 20, startvalue = startvalue)
  # mean of start values should be the same as mean of ratings
  xtest <- round(mean(startvalue), 1) == round(rowMeans(res$ratmat), 1)
  expect_true(all(xtest))
})
