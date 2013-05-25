#include "stdafx.h"

#include "escheme.h"
#include "comtypes.h"

BOOL isShortInt(Scheme_Object *o) {
  long longVal;

  if (SCHEME_INTP(o) == FALSE) {
    return FALSE;
  }

  longVal = SCHEME_INT_VAL(o);

  return ((short)longVal == longVal);
}
