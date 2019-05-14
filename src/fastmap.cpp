#include <R.h>
#include <Rdefines.h>
#include <string>

#if __cplusplus >= 201103L
  // std::unordered_map is faster than std::map, but requires C++11.
  #include<unordered_map>
  typedef std::unordered_map<std::string, int> si_map;
#else
  #include <map>
  typedef std::map<std::string, int> si_map;
#endif


// Note that this returns a const char* which points to the CHARSXP's
// memory, so its lifetime must not exceed the CHARSXP's lifetime.
std::string key_from_sexp(SEXP key_r) {
  if (TYPEOF(key_r) != STRSXP || length(key_r) != 1) {
    error("key must be a one-element character vector");
  }
  SEXP key_c = STRING_ELT(key_r, 0);
  if (key_c == NA_STRING || Rf_StringBlank(key_c)) {
    error("key must be not be \"\" or NA");
  }
  return std::string(Rf_translateCharUTF8(key_c));
}


extern "C" {

  bool is_ascii(const char *str) {
    while (*str) {
      if ((unsigned int)*str > 0x7F) {
        return false;
      }
      str++;
    }
    return true;
  }

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

  SEXP C_xptr_is_null(SEXP map_xptr) {
    if (TYPEOF(map_xptr) != EXTPTRSXP) {
      error("map_xptr must be an external pointer.");
    }
    return Rf_ScalarLogical(R_ExternalPtrAddr(map_xptr) == NULL);
  }


  SEXP C_map_create() {
    si_map* map = new si_map;
    SEXP map_xptr = PROTECT(R_MakeExternalPtr(map, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(map_xptr, map_finalizer, TRUE);
    UNPROTECT(1);
    return map_xptr;
  }


  SEXP C_map_set(SEXP map_xptr, SEXP key_r, SEXP idx_r) {
    std::string key = key_from_sexp(key_r);

    if (TYPEOF(idx_r) != INTSXP || length(idx_r) != 1) {
      error("idx must be a one-element integer vector");
    }

    si_map* map = map_from_xptr(map_xptr);
    int idx = INTEGER(idx_r)[0];

    (*map)[key] = idx;

    return R_NilValue;
  }


  SEXP C_map_get(SEXP map_xptr, SEXP key_r) {
    std::string key = key_from_sexp(key_r);

    si_map* map = map_from_xptr(map_xptr);

    si_map::const_iterator it = map->find(key);
    if (it == map->end()) {
      return Rf_ScalarInteger(-1);
    } else {
      return Rf_ScalarInteger(it->second);
    }
  }


  SEXP C_map_remove(SEXP map_xptr, SEXP key_r) {
    std::string key = key_from_sexp(key_r);

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

  SEXP C_map_keys(SEXP map_xptr) {
    si_map* map = map_from_xptr(map_xptr);
    SEXP keys = PROTECT(Rf_allocVector(STRSXP, map->size()));

    int i = 0;
    for(si_map::const_iterator it = map->begin(); it != map->end(); ++it, ++i) {
      SET_STRING_ELT(keys, i, Rf_mkCharCE(it->first.c_str(), CE_UTF8));
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
      SET_STRING_ELT(keys, i, Rf_mkCharCE(it->first.c_str(), CE_UTF8));
      idxs_[i] = it->second;
    }

    setAttrib(idxs, R_NamesSymbol, keys);

    UNPROTECT(2);
    return idxs;
  }

  // Convert an R character vector to UTF-8. This is necessary because iconv
  // doesn't really work for vectors where the items have mixed encoding.
  SEXP C_char_vec_to_utf8(SEXP str) {
    if (TYPEOF(str) != STRSXP) {
      error("str must be a character vector");
    }

    // Our default assumption is that all the keys are UTF-8 (or ASCII), in
    // which case we do _not_ need to re-encode the keys and copy them to a
    // new vector.
    bool need_reencode = false;
    // Fast path for the common case: check if all the strings are UTF-8. If
    // yes, just return str. If no, we need to copy and re-encode each element
    // to a new vector.
    int n_str = Rf_length(str);
    SEXP tmp;
    for (int i=0; i<n_str; i++) {
      tmp = STRING_ELT(str, i);

      if (!is_ascii(CHAR(tmp)) && Rf_getCharCE(tmp) != CE_UTF8) {
        need_reencode = true;
        break;
      }
    }

    // No need to re-encode. Just return str.
    if (!need_reencode) {
      return str;
    }

    // If we got here, we need to copy and re-encode.
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n_str));

    for (int i=0; i<n_str; i++) {
      tmp = STRING_ELT(str, i);
      SET_STRING_ELT(out, i, Rf_mkCharCE(Rf_translateCharUTF8(tmp), CE_UTF8));
    }

    UNPROTECT(1);
    return out;
  }


} // extern "C"
