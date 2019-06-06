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
extern SEXP C_map_keys(SEXP, SEXP);
extern SEXP C_map_keys_idxs(SEXP, SEXP);
extern SEXP C_char_vec_to_utf8(SEXP);
extern SEXP C_xptr_is_null(SEXP);
extern SEXP C_make_weakref(SEXP);
extern SEXP C_get_weakref(SEXP);
extern SEXP C_is_weakref(SEXP);
extern SEXP C_enable_memtracker();
extern SEXP C_str_to_sexp(SEXP);
extern SEXP C_sexp_to_str(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"C_map_create",                  (DL_FUNC) &C_map_create,           0},
  {"C_map_set",                     (DL_FUNC) &C_map_set,              3},
  {"C_map_get",                     (DL_FUNC) &C_map_get,              2},
  {"C_map_remove",                  (DL_FUNC) &C_map_remove,           2},
  {"C_map_keys",                    (DL_FUNC) &C_map_keys,             2},
  {"C_map_keys_idxs",               (DL_FUNC) &C_map_keys_idxs,        2},
  {"C_char_vec_to_utf8",            (DL_FUNC) &C_char_vec_to_utf8,     1},
  {"C_xptr_is_null",                (DL_FUNC) &C_xptr_is_null,         1},
  {"C_make_weakref",                (DL_FUNC) &C_make_weakref,         1},
  {"C_get_weakref",                 (DL_FUNC) &C_get_weakref,          1},
  {"C_is_weakref",                  (DL_FUNC) &C_is_weakref,           1},
  {"C_enable_memtracker",           (DL_FUNC) &C_enable_memtracker,    1},
  {"C_str_to_sexp",                 (DL_FUNC) &C_str_to_sexp,          1},
  {"C_sexp_to_str",                 (DL_FUNC) &C_sexp_to_str,          1},
  {NULL, NULL, 0}
};

attribute_visible void R_init_fastmap(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
