#include <R.h>
#include <Rdefines.h>
#include <map>
#include <string>

typedef std::map<std::string, int> si_map;


extern "C" {

  void map_finalizer(SEXP map_xptr) {
    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
    delete map;
    R_ClearExternalPtr(map_xptr);
  }


  SEXP C_map_create() {
    si_map* map = new si_map;
    SEXP map_xptr = PROTECT(R_MakeExternalPtr(map, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(map_xptr, map_finalizer, TRUE);
    UNPROTECT(1);
    return map_xptr;
  }


  SEXP C_map_set(SEXP map_xptr, SEXP key_r, SEXP idx_r) {
    if (TYPEOF(key_r) != STRSXP || length(key_r) != 1) {
      error("key must be a one-element character vector");
    }

    if (TYPEOF(idx_r) != INTSXP || length(idx_r) != 1) {
      error("idx must be a one-element integer vector");
    }

    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
    const char* key = CHAR(STRING_ELT(key_r, 0));
    int idx = INTEGER(idx_r)[0];

    (*map)[key] = idx;

    return R_NilValue;
  }


  SEXP C_map_get(SEXP map_xptr, SEXP key_r) {
    if (TYPEOF(key_r) != STRSXP || length(key_r) != 1) {
      error("key must be a one-element character vector");
    }

    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
    const char* key = CHAR(STRING_ELT(key_r, 0));

    si_map::const_iterator it = map->find(key);
    if (it == map->end()) {
      return Rf_ScalarInteger(-1);
    } else {
      return Rf_ScalarInteger(it->second);
    }
  }


  SEXP C_map_remove(SEXP map_xptr, SEXP key_r) {
    if (TYPEOF(key_r) != STRSXP || length(key_r) != 1) {
      error("key must be a one-element character vector");
    }

    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
    const char* key = CHAR(STRING_ELT(key_r, 0));

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
    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
    return(Rf_ScalarInteger(map->size()));
  }

  SEXP C_map_keys(SEXP map_xptr) {
    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
    SEXP keys =  PROTECT(Rf_allocVector(STRSXP, map->size()));

    int i = 0;
    for(si_map::const_iterator it = map->begin(); it != map->end(); ++it, ++i) {
      SET_STRING_ELT(keys, i, Rf_mkChar(it->first.c_str()));
    }

    UNPROTECT(1);
    return keys;
  }

  SEXP C_map_keys_idxs(SEXP map_xptr) {
    si_map* map = (si_map*) R_ExternalPtrAddr(map_xptr);
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
