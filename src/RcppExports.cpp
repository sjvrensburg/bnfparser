// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// parseBNF
Rcpp::List parseBNF(const std::string& input, bool strict_validation, bool strict_mode);
RcppExport SEXP _bnfparser_parseBNF(SEXP inputSEXP, SEXP strict_validationSEXP, SEXP strict_modeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string& >::type input(inputSEXP);
    Rcpp::traits::input_parameter< bool >::type strict_validation(strict_validationSEXP);
    Rcpp::traits::input_parameter< bool >::type strict_mode(strict_modeSEXP);
    rcpp_result_gen = Rcpp::wrap(parseBNF(input, strict_validation, strict_mode));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_bnfparser_parseBNF", (DL_FUNC) &_bnfparser_parseBNF, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_bnfparser(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
