#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
SEXP GetArgs(List missingArgs, SEXP dotsE){
  Environment testr = Environment::namespace_env("testr");
  Environment cache = testr.get("cache");
  LogicalVector wd = cache.get("writing.down");
  bool writingDown = as<bool>(wd);
  if (writingDown){
    return R_NilValue;
  }
  List args;
  Environment dotsEnv(dotsE);
  CharacterVector envNames = dotsEnv.ls(false);
  int nArgs = envNames.length();
  SEXP evalEnv;
  SEXP unevaluatedArg;
  SEXP evaluatedArg = R_NilValue;
  for( int i=0; i<nArgs; i++){
    evaluatedArg = R_NilValue;
    string name = as<string>(envNames[i]);
    if (name != "missingArgs" && !as<bool>(missingArgs[name])){
      SEXP nameSym = Rf_install(name.c_str());
      unevaluatedArg = Rf_findVarInFrame(dotsE, nameSym);
      if (unevaluatedArg != R_UnboundValue && TYPEOF(unevaluatedArg) == PROMSXP) {
        SEXP prcode = PRCODE(unevaluatedArg);
        if (!Rf_isNull(PRENV(unevaluatedArg))){
          evalEnv = PRENV(unevaluatedArg);
        } else {
          evalEnv = dotsE;
        }
        int err = 0;
        SEXP res = R_tryEvalSilent(unevaluatedArg, evalEnv, &err);
        if(err){
          evaluatedArg = prcode;
        } else {
          evaluatedArg = res;
        }
        args[name] = evaluatedArg; 
      }
    }
  }
  nArgs--;
  if (dotsEnv.exists("...")){
    SEXP dots = dotsEnv.get("...");
    vector<SEXP> promises;
    int dArgs = 0;
    if( dots != R_MissingArg ){ 
      while(dots != R_NilValue){
        promises.push_back(CAR(dots)) ;
        dots = CDR(dots);
        dArgs++;
      }
    }
    List dotArgs(dArgs);
    for( int i=0; i< dArgs; i++){
      unevaluatedArg = promises[i];
      if (unevaluatedArg != R_UnboundValue && TYPEOF(unevaluatedArg) == PROMSXP) {
        SEXP prcode = PRCODE(unevaluatedArg);
        if (!Rf_isNull(PRENV(unevaluatedArg))){
          evalEnv = PRENV(unevaluatedArg);
        } else {
          evalEnv = dotsE;
        }
        int err = 0;
        SEXP res = R_tryEvalSilent(unevaluatedArg, evalEnv, &err);
        if(err){
          evaluatedArg = prcode;
        } else {
          evaluatedArg = res;
        }
      }
      args.push_back(evaluatedArg);
    }
  }
  return args;
}

