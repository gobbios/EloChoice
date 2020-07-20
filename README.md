# EloChoice

currently on CRAN:

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/EloChoice)](https://cran.r-project.org/package=EloChoice)


[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/gobbios/EloChoice?branch=master&svg=true)](https://ci.appveyor.com/project/gobbios/EloChoice)
[![Travis build status](https://travis-ci.com/gobbios/EloChoice.svg?branch=master)](https://travis-ci.com/gobbios/EloChoice)

This package allows calculating global scores for characteristics of visual stimuli as assessed by human raters. Stimuli are presented as sequence of pairwise comparisons ('contests'), during each of which a rater expresses preference for one stimulus over the other (forced choice). The algorithm for calculating global scores is based on Elo rating, which updates individual scores after each single pairwise contest. Elo rating is widely used to rank chess players according to their performance. Its core feature is that dyadic contests with expected outcomes lead to smaller changes of participants' scores than outcomes that were unexpected. As such, Elo rating is an efficient tool to rate individual stimuli when a large number of such stimuli are paired against each other in the context of experiments where the goal is to rank stimuli according to some characteristic of interest.
