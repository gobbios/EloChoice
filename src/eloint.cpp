// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp ;

// [[Rcpp::export]]
List eloint(CharacterVector winner, CharacterVector loser, CharacterVector allids, NumericVector kval, NumericVector startvalues, NumericVector runs) {
  int r = runs[0];
  int N = winner.size();

  // upset data
  LogicalMatrix umat(r, N) ;
  LogicalMatrix dmat(r, N) ;
  NumericMatrix wmat(r, N) ;

  // create startvalues
  NumericVector svals = clone<NumericVector>(startvalues) ;

  NumericMatrix indexmat(r, winner.size()) ;
  int startval = startvalues[0] ;
  NumericMatrix elomat(r, allids.size());

  IntegerVector randvec = Range(1, loser.size()) ;
  randvec = randvec - 1 ; // because Cpp indices start from zero
  IntegerVector rando(randvec.size()) ;
  CharacterMatrix winnermat(r, winner.size()) ; CharacterVector winnerrand(randvec.size()) ;
  CharacterMatrix losermat(r, winner.size()) ; CharacterVector loserrand(randvec.size()) ;

  for (int k = 0; k < winner.size(); k++ ) {
    winnerrand(k) = winner(k) ;
    loserrand(k) = loser(k) ;
  }

  int kv = kval[0];
  int loserrat = 0; int winnerrat = 0; double pscore = 0.0;
  double kp = 0.0; signed int ratingdiff = 0;
  double divi =0.0;

  for(size_t runindex=0; runindex < r; runindex++) {
    for (size_t seqindex=0; seqindex < winner.size(); seqindex++) {
      for(size_t k=0; k < allids.size(); k++) {
        if(allids[k]==loserrand[seqindex])  { loserrat = svals[k]; }
        if(allids[k]==winnerrand[seqindex]) { winnerrat = svals[k]; }
      }

      ratingdiff = winnerrat - loserrat;
      wmat(runindex, seqindex) = ratingdiff;
      if (ratingdiff > 0) {
        dmat(runindex,seqindex) = true;
      }
      if (ratingdiff < 0) {
        dmat(runindex, seqindex) = true;
        umat(runindex, seqindex) = true;
      }


      // calculate winning probabilities
      // change to logistic approach ('old' normal approach is now in 'elointnorm')
      divi = ratingdiff/400.0;
      pscore =  1 - 1 / (1 + pow(10.0, divi)) ;

      // update ratings
      kp = kv * pscore; loserrat = round(loserrat + kp - kv); winnerrat = round(winnerrat - kp + kv);
      for(size_t k=0; k < allids.size(); k++) {
        if(allids[k]==loserrand[seqindex]) { svals[k] = loserrat; }
        if(allids[k]==winnerrand[seqindex]) { svals[k] = winnerrat; }
      }

    }

    // add values to final matrix
    for(size_t k=0; k < allids.size(); k++) elomat(runindex, k) = svals(k) ;

    // reshuffle winner and loser order
    rando = RcppArmadillo::sample(randvec, randvec.size(), FALSE, NumericVector::create()) ;
    for (int k = 0; k < winner.size(); k++ ) {
      indexmat(runindex, k) = randvec(rando(k)) ;
      winnermat(runindex, k) = winner(rando(k)) ;
      winnerrand(k) = winner(rando(k)) ;
      losermat(runindex, k) = loser(rando(k)) ;
      loserrand(k) = loser(rando(k)) ;
    }
    randvec = indexmat(runindex, _) ;
    // reset startvalues
    for (int k = 0; k < allids.size(); k++) svals(k) = startval;

  }
  return Rcpp::List::create(elomat, allids, umat, dmat, wmat);

}

// [[Rcpp::export]]
List elointnorm(CharacterVector winner, CharacterVector loser, CharacterVector allids, NumericVector kval, NumericVector startvalues, NumericVector runs) {

  int r = runs[0];
  int N = winner.size();

  // upset data
  LogicalMatrix umat(r, N) ;
  LogicalMatrix dmat(r, N) ;
  NumericMatrix wmat(r, N) ;

  // create startvalues
  NumericVector svals = clone<NumericVector>(startvalues) ;


  NumericMatrix indexmat(r, winner.size()) ;
  int startval = startvalues[0] ;
  NumericMatrix elomat(r, allids.size());

  IntegerVector randvec = Range(1, loser.size()) ;
  randvec = randvec - 1 ; // because Cpp indices start from zero
  IntegerVector rando(randvec.size()) ;
  CharacterMatrix winnermat(r, winner.size()) ; CharacterVector winnerrand(randvec.size()) ;
  CharacterMatrix losermat(r, winner.size()) ; CharacterVector loserrand(randvec.size()) ;

  for (int k = 0; k < winner.size(); k++ ) {
    winnerrand(k) = winner(k) ;
    loserrand(k) = loser(k) ;
  }

  int kv = kval[0];
  int loserrat = 0; int winnerrat = 0; double pscore = 0.0;
  double kp = 0.0; signed int ratingdiff = 0;


  for(size_t runindex=0; runindex < r; runindex++) {
    for (size_t seqindex=0; seqindex < winner.size(); seqindex++) {
      for(size_t k=0; k < allids.size(); k++) {
        if(allids[k]==loserrand[seqindex])  { loserrat = svals[k]; }
        if(allids[k]==winnerrand[seqindex]) { winnerrat = svals[k]; }
      }

      ratingdiff = winnerrat - loserrat;
      wmat(runindex, seqindex) = ratingdiff;
      if (ratingdiff > 0) {
        dmat(runindex,seqindex) = true;
      }
      if (ratingdiff < 0) {
        dmat(runindex, seqindex) = true;
        umat(runindex, seqindex) = true;
      }

      // winning probabilities following 'normal' approach
      pscore = Rf_pnorm5((winnerrat - loserrat)/(200 * sqrt(2.0)), 0.0, 1.0, 1, 0);
      kp = kv * pscore; loserrat = round(loserrat + kp - kv); winnerrat = round(winnerrat - kp + kv);
      for(size_t k=0; k < allids.size(); k++) {
        if(allids[k]==loserrand[seqindex]) { svals[k] = loserrat; }
        if(allids[k]==winnerrand[seqindex]) { svals[k] = winnerrat; }
      }



    }

    // add values to final matrix
    for(size_t k=0; k < allids.size(); k++) elomat(runindex, k) = svals(k) ;

    // reshuffle winner and loser order
    rando = RcppArmadillo::sample(randvec, randvec.size(), FALSE, NumericVector::create()) ;
    for (int k = 0; k < winner.size(); k++ ) {
      indexmat(runindex, k) = randvec(rando(k)) ;
      winnermat(runindex, k) = winner(rando(k)) ;
      winnerrand(k) = winner(rando(k)) ;
      losermat(runindex, k) = loser(rando(k)) ;
      loserrand(k) = loser(rando(k)) ;
    }
    randvec = indexmat(runindex, _) ;
    // reset startvalues
    for (int k = 0; k < allids.size(); k++) svals(k) = startval;

  }
  return Rcpp::List::create(elomat, allids, umat, dmat, wmat);

}
