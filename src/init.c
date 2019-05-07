#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

/* .Call calls */
extern SEXP C_map_create();
extern SEXP C_map_set(SEXP, SEXP, SEXP);
extern SEXP C_map_get(SEXP, SEXP);
extern SEXP C_map_remove(SEXP, SEXP);
extern SEXP C_map_size(SEXP);
extern SEXP C_map_keys(SEXP);
extern SEXP C_map_keys_idxs(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"C_map_create",                  (DL_FUNC) &C_map_create,           0},
  {"C_map_set",                     (DL_FUNC) &C_map_set,              3},
  {"C_map_get",                     (DL_FUNC) &C_map_get,              2},
  {"C_map_remove",                  (DL_FUNC) &C_map_remove,           2},
  {"C_map_size",                    (DL_FUNC) &C_map_size,             1},
  {"C_map_keys",                    (DL_FUNC) &C_map_keys,             1},
  {"C_map_keys_idxs",               (DL_FUNC) &C_map_keys_idxs,        1},
  {NULL, NULL, 0}
};

attribute_visible void R_init_fastmap(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
