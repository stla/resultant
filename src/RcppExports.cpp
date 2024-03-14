// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// resultantCPP
void resultantCPP(Rcpp::IntegerVector PowersF, Rcpp::IntegerVector CoeffsF, Rcpp::IntegerVector PowersG, Rcpp::IntegerVector CoeffsG);
RcppExport SEXP _resultant_resultantCPP(SEXP PowersFSEXP, SEXP CoeffsFSEXP, SEXP PowersGSEXP, SEXP CoeffsGSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type PowersF(PowersFSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type CoeffsF(CoeffsFSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type PowersG(PowersGSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type CoeffsG(CoeffsGSEXP);
    resultantCPP(PowersF, CoeffsF, PowersG, CoeffsG);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_resultant_resultantCPP", (DL_FUNC) &_resultant_resultantCPP, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_resultant(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
