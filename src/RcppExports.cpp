// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/genthat.h"
#include <Rcpp.h>

using namespace Rcpp;

// serialize_r
SEXP serialize_r(SEXP s);
RcppExport SEXP genthat_serialize_r(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(serialize_r(s));
    return rcpp_result_gen;
END_RCPP
}
// serialize_error_handler
SEXP serialize_error_handler(SEXP e);
RcppExport SEXP genthat_serialize_error_handler(SEXP eSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type e(eSEXP);
    rcpp_result_gen = Rcpp::wrap(serialize_error_handler(e));
    return rcpp_result_gen;
END_RCPP
}
// clearCallCache_cpp
void clearCallCache_cpp();
RcppExport SEXP genthat_clearCallCache_cpp() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    clearCallCache_cpp();
    return R_NilValue;
END_RCPP
}
// enterFunction_cpp
SEXP enterFunction_cpp(CharacterVector fname, SEXP args_list, SEXP call_id);
RcppExport SEXP genthat_enterFunction_cpp(SEXP fnameSEXP, SEXP args_listSEXP, SEXP call_idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type fname(fnameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type args_list(args_listSEXP);
    Rcpp::traits::input_parameter< SEXP >::type call_id(call_idSEXP);
    rcpp_result_gen = Rcpp::wrap(enterFunction_cpp(fname, args_list, call_id));
    return rcpp_result_gen;
END_RCPP
}
// exitFunction_cpp
SEXP exitFunction_cpp(SEXP call_id, SEXP retv_env);
RcppExport SEXP genthat_exitFunction_cpp(SEXP call_idSEXP, SEXP retv_envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type call_id(call_idSEXP);
    Rcpp::traits::input_parameter< SEXP >::type retv_env(retv_envSEXP);
    rcpp_result_gen = Rcpp::wrap(exitFunction_cpp(call_id, retv_env));
    return rcpp_result_gen;
END_RCPP
}
