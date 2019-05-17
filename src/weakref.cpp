#include <R.h>
#include <Rdefines.h>

extern "C" {
  void noop_finalizer(SEXP obj) { }

  SEXP C_make_weakref(SEXP obj) {
    return R_MakeWeakRefC(obj, R_NilValue, noop_finalizer, FALSE);
  }

  SEXP C_get_weakref(SEXP obj) {
    return R_WeakRefKey(obj);
  }

  SEXP C_is_weakref(SEXP obj) {
    return Rf_ScalarLogical(TYPEOF(obj) == WEAKREFSXP);
  }
}
