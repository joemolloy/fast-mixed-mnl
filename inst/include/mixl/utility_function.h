#ifndef UF_ARGS_H
#define UF_ARGS_H

#include <valarray>
#include <iterator>
#include <numeric> 

#include <Rcpp.h>
#ifdef _OPENMP
  #include <omp.h>
#endif


using Rcpp::DataFrame;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::_;
using Rcpp::stop;

//utility function arguments for simple MNL models
struct UF_args {
  const DataFrame data;
  const int Nindividuals;
  const NumericMatrix availabilities;
  const NumericVector weights;
  NumericMatrix P;

  UF_args(DataFrame data, int Nindividuals, NumericMatrix availabilities, NumericVector weights, NumericMatrix P)
    : data(data), Nindividuals(Nindividuals), availabilities(availabilities), weights(weights), P(P)
  { }
  
};

//UF_args2 includes structures needed for mixed models
struct UF_args2 {
  const DataFrame data;
  const int Nindividuals;
  const NumericMatrix availabilities;
  const NumericMatrix draws;
  const int nDraws;
  const NumericVector weights;
  NumericMatrix P;
  bool include_probability_indices;
  
  UF_args2(DataFrame data, int Nindividuals, NumericMatrix availabilities, NumericMatrix draws, int nDraws, NumericVector weights, NumericMatrix P,bool include_probability_indices)
    : data(data), Nindividuals(Nindividuals), availabilities(availabilities), draws(draws), nDraws(nDraws), weights(weights), P(P),include_probability_indices(include_probability_indices)
  { }
  
};

void utilityFunction(NumericVector betas, UF_args& v);
void utilityFunction(NumericVector betas, UF_args2& v);

#endif
