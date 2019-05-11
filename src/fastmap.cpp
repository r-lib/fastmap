#include <R.h>
#include <Rdefines.h>
#include <map>
#include <string>

typedef std::map<std::string, int> si_map;


extern "C" {
  si_map* map_from_xptr(SEXP map_xptr) {
    if (TYPEOF(map_xptr) != EXTPTRSXP) {
      error("map_xptr must be an external pointer.");
    }
    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
    if (!map) {
      error("fastmap: external pointer to string-to-index map is null.");
    }

    return map;
  }

  void map_finalizer(SEXP map_xptr) {
    si_map* map = map_from_xptr(map_xptr);
    delete map;
    R_ClearExternalPtr(map_xptr);
  }

  // Note that this returns a const char* which points to the CHARSXP's
  // memory, so its lifetime must not exceed the CHARSXP's lifetime.
  const char* key_from_sexp(SEXP key_r) {
    if (TYPEOF(key_r) != STRSXP || length(key_r) != 1) {
      error("key must be a one-element character vector");
    }
    SEXP key_c = PROTECT(STRING_ELT(key_r, 0));
    if (key_c == NA_STRING || Rf_StringBlank(key_c)) {
      error("key must be a string other than NA or \"\"");
    }
    UNPROTECT(1);
    return CHAR(key_c);
  }


  SEXP C_map_create() {
    si_map* map = new si_map;
    SEXP map_xptr = PROTECT(R_MakeExternalPtr(map, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(map_xptr, map_finalizer, TRUE);
    UNPROTECT(1);
    return map_xptr;
  }


  SEXP C_map_set(SEXP map_xptr, SEXP key_r, SEXP idx_r) {
    const char* key = key_from_sexp(key_r);

    if (TYPEOF(idx_r) != INTSXP || length(idx_r) != 1) {
      error("idx must be a one-element integer vector");
    }

    si_map* map = map_from_xptr(map_xptr);
    int idx = INTEGER(idx_r)[0];

    (*map)[key] = idx;

    return R_NilValue;
  }


  SEXP C_map_get(SEXP map_xptr, SEXP key_r) {
    const char* key = key_from_sexp(key_r);

    si_map* map = map_from_xptr(map_xptr);

    si_map::const_iterator it = map->find(key);
    if (it == map->end()) {
      return Rf_ScalarInteger(-1);
    } else {
      return Rf_ScalarInteger(it->second);
    }
  }


  SEXP C_map_remove(SEXP map_xptr, SEXP key_r) {
    const char* key = key_from_sexp(key_r);

    si_map* map = map_from_xptr(map_xptr);

    si_map::iterator it = map->find(key);
    if (it == map->end()) {
      return Rf_ScalarInteger(-1);
    } else {
      int value = it->second;
      map->erase(it);
      return Rf_ScalarInteger(value);
    }
  }

  SEXP C_map_size(SEXP map_xptr) {
    si_map* map = map_from_xptr(map_xptr);
    return(Rf_ScalarInteger(map->size()));
  }

  SEXP C_map_keys(SEXP map_xptr) {
    si_map* map = map_from_xptr(map_xptr);
    SEXP keys = PROTECT(Rf_allocVector(STRSXP, map->size()));

    int i = 0;
    for(si_map::const_iterator it = map->begin(); it != map->end(); ++it, ++i) {
      SET_STRING_ELT(keys, i, Rf_mkChar(it->first.c_str()));
    }

    UNPROTECT(1);
    return keys;
  }

  SEXP C_map_keys_idxs(SEXP map_xptr) {
    si_map* map = map_from_xptr(map_xptr);
    SEXP keys = PROTECT(Rf_allocVector(STRSXP, map->size()));
    SEXP idxs = PROTECT(Rf_allocVector(INTSXP, map->size()));

    int* idxs_ = INTEGER(idxs);
    int i = 0;
    for(si_map::const_iterator it = map->begin(); it != map->end(); ++it, ++i) {
      SET_STRING_ELT(keys, i, Rf_mkChar(it->first.c_str()));
      idxs_[i] = it->second;
    }

    setAttrib(idxs, R_NamesSymbol, keys);

    UNPROTECT(2);
    return idxs;
  }



} // extern "C"
