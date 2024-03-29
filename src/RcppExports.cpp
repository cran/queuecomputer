// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// qloop_numeric
List qloop_numeric(NumericVector times, NumericVector service, int n_servers);
RcppExport SEXP _queuecomputer_qloop_numeric(SEXP timesSEXP, SEXP serviceSEXP, SEXP n_serversSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type times(timesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type service(serviceSEXP);
    Rcpp::traits::input_parameter< int >::type n_servers(n_serversSEXP);
    rcpp_result_gen = Rcpp::wrap(qloop_numeric(times, service, n_servers));
    return rcpp_result_gen;
END_RCPP
}
// qloop_qq
List qloop_qq(NumericVector times, NumericVector service, NumericVector x, IntegerVector y);
RcppExport SEXP _queuecomputer_qloop_qq(SEXP timesSEXP, SEXP serviceSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type times(timesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type service(serviceSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(qloop_qq(times, service, x, y));
    return rcpp_result_gen;
END_RCPP
}
