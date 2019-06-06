#include <R.h>
#include <Rdefines.h>

extern Rboolean enable_memtracker;

SEXP C_enable_memtracker() {
  enable_memtracker = TRUE;
  return R_NilValue;
}


SEXP C_str_to_sexp(SEXP address_r) {
  if (TYPEOF(address_r) != STRSXP || Rf_length(address_r) != 1) {
    Rf_error("address must be a one-element character vector");
  }

  const char* address_str = CHAR(STRING_ELT(address_r, 0));
  SEXP address = (SEXP) strtoul(address_str, NULL, 0);
  return address;
}


SEXP C_sexp_to_str(SEXP obj) {
  char address_str[30];
  sprintf(address_str, "%p", obj);

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(address_str));
  UNPROTECT(1);
  return out;
}
