// array.cxx

#ifdef MYSTERX_3M
// Created by xform.ss:
# include "xsrc/array3m.cxx"
#else

#include "mysterx_pre.h"

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "bstr.h"
#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

Scheme_Object *safeArrayElementToSchemeObject(SAFEARRAY *theArray,
                                              long *allIndices) {
  HRESULT hr;
  VARTYPE vt;
  char errBuff[128];

  hr = SafeArrayGetVartype(theArray,&vt);

  if (hr != S_OK) codedComError("Can't get array type",hr);

  switch(vt) {

  case VT_EMPTY :
  case VT_NULL :

    return scheme_void;

  case VT_UI1 :
    char cArg;
    SafeArrayGetElement(theArray,allIndices,&cArg);
    return scheme_make_char(cArg);

  case VT_UI2 :
    unsigned short usArg;
    SafeArrayGetElement(theArray,allIndices,&usArg);
    return scheme_make_integer(usArg);

  case VT_UI4 :
    unsigned long ulArg;
    SafeArrayGetElement(theArray,allIndices,&ulArg);
    return scheme_make_integer_value_from_unsigned(ulArg);

  case VT_UI8 :
    ULARGE_INTEGER uli;
    SafeArrayGetElement(theArray,allIndices, &uli.QuadPart);
    return scheme_make_integer_value_from_unsigned_long_long(uli.QuadPart);

  case VT_I1  :
    char scArg;
    SafeArrayGetElement(theArray,allIndices,&scArg);
    return scheme_make_integer(scArg);

  case VT_I2  :
    int iArg;
    SafeArrayGetElement(theArray,allIndices, &iArg);
    return scheme_make_integer(iArg);

  case VT_I4 :
    long lArg;
    SafeArrayGetElement(theArray,allIndices, &lArg);
    return scheme_make_integer_value(lArg);

  case VT_I8 :
    LARGE_INTEGER li;
    SafeArrayGetElement(theArray,allIndices, &li.QuadPart);
    return scheme_make_integer_value_from_long_long(li.QuadPart);

  case VT_R4 :
    double dArg;
#ifdef MZ_USE_SINGLE_FLOATS
    float fArg;
    SafeArrayGetElement(theArray,allIndices, &fArg);
    return scheme_make_float(fArg);
#else
    SafeArrayGetElement(theArray,allIndices, &dArg);
    return scheme_make_double((double)(dArg));
#endif

  case VT_R8 :
    SafeArrayGetElement(theArray,allIndices,&dArg);
    return scheme_make_double((double)(dArg));

  case VT_BSTR :
    BSTR bArg;
    SafeArrayGetElement(theArray,allIndices,&bArg);
    return unmarshalBSTR(bArg);

  case VT_ERROR :
    SCODE scodeArg;
    SafeArrayGetElement(theArray,allIndices,&scodeArg);
    return mx_make_scode(scodeArg);

  case VT_CY :
    CY cyArg;
    SafeArrayGetElement(theArray,allIndices,&cyArg);
    return mx_make_cy(&cyArg);

  case VT_DATE :
    DATE dateArg;
    SafeArrayGetElement(theArray,allIndices,&dateArg);
    return mx_make_date(&dateArg);

  case VT_DISPATCH :
    IDispatch * pIDispatch;
    SafeArrayGetElement(theArray,allIndices,&pIDispatch);
    return mx_make_idispatch(pIDispatch);

  case VT_UNKNOWN :
    IUnknown *pIUnknown;
    SafeArrayGetElement(theArray,allIndices,&pIUnknown);
    return mx_make_iunknown(pIUnknown);

  case VT_BOOL :
    VARIANT_BOOL boolArg;
    SafeArrayGetElement(theArray,allIndices,&boolArg);
    return boolArg ? scheme_true : scheme_false;

  case VT_VARIANT :
    VARIANT variant;
    SafeArrayGetElement(theArray,allIndices,&variant);
    return variantToSchemeObject(&variant);

  default :
    sprintf(errBuff,
            "Can't make Scheme value from array element with type 0x%X",
            vt);
    scheme_signal_error(errBuff);

  }

  return NULL;
}

Scheme_Object *buildVectorFromArray(SAFEARRAY *theArray,
                                    long currDim,
                                    long *allIndices,
                                    long *currNdx,
                                    long offset) {
  Scheme_Object *vec, *v;
  long low,high,vecSize;
  long i,j;

  SafeArrayGetLBound(theArray,currDim,&low);
  SafeArrayGetUBound(theArray,currDim,&high);
  vecSize = high - low + 1;

  vec = scheme_make_vector(vecSize,scheme_void);

  if (currDim > 1) {
    for (i = 0,j = low; i < vecSize; i++,j++) {
      currNdx[offset] = j;
      v = buildVectorFromArray(theArray, currDim - 1,
                               allIndices, currNdx, offset - 1);
      SCHEME_VEC_ELS(vec)[i] = v;
    }
  }
  else {
    for (i = 0,j = low; i < vecSize; i++,j++) {
      currNdx[offset] = j;
      v = safeArrayElementToSchemeObject(theArray,allIndices);
      SCHEME_VEC_ELS(vec)[i] = v;
    }
  }

  return vec;
}

Scheme_Object *safeArrayToSchemeVector(SAFEARRAY *theArray) {
  long numDims;
  long *indices;
  Scheme_Object *retval;

  numDims = SafeArrayGetDim(theArray);
  indices = (long *)scheme_malloc_atomic(numDims * sizeof(long));
  retval = buildVectorFromArray(theArray,numDims,
                                indices, indices, numDims - 1);
  return retval;
}

int getSchemeVectorDims(Scheme_Object *vec) {
  Scheme_Object *currObj;
  int numDims;

  numDims = 0;
  currObj = vec;

  do {
    numDims++;
    currObj = SCHEME_VEC_ELS(currObj)[0];
  } while (SCHEME_VECTORP(currObj));

  return numDims;
}

void setArrayEltCounts(Scheme_Object *vec,
                       SAFEARRAYBOUND *rayBounds,long numDims) {
  Scheme_Object *currObj;
  long i;

  currObj = vec;
  i = numDims - 1;

  do {
    rayBounds[i--].cElements = SCHEME_VEC_SIZE(currObj);
    currObj = SCHEME_VEC_ELS(currObj)[0];
  } while (SCHEME_VECTORP(currObj));
}

BOOL isRegularVector(Scheme_Object *vec) {
  Scheme_Object **elts,*elt;
  BOOL isVec,zeroIsVec;
  int len,currLen,zeroLen;
  int i;

  if (SCHEME_VECTORP(vec) == FALSE) return TRUE;

  len = SCHEME_VEC_SIZE(vec);
  elts = SCHEME_VEC_ELS(vec);

  // use zeroth elt as standard

  elt = elts[0];

  zeroIsVec = SCHEME_VECTORP(elt);
  if (zeroIsVec) zeroLen = SCHEME_VEC_SIZE(elt);

  if (isRegularVector(elt) == FALSE) return FALSE;

  for (i = 1; i < len; i++) {
    elt = elts[i];
    isVec = SCHEME_VECTORP(elt);
    if (isVec != zeroIsVec) return FALSE;
    if (isVec) {
      currLen = SCHEME_VEC_SIZE(elt);
      if (currLen != zeroLen) return FALSE;
      if (isRegularVector(elt) == FALSE) return FALSE;
    }
  }

  return TRUE;
}


void* variantDataPointer(VARTYPE vt,VARIANTARG *pVariantArg)
{
  char errBuff[256];

  switch (vt) {
  case VT_NULL                   : return NULL;
  case VT_I1                     : return &pVariantArg->cVal;
  case VT_I1 | VT_BYREF          : return &pVariantArg->pcVal;
  case VT_UI1                    : return &pVariantArg->bVal;
  case VT_UI1 | VT_BYREF         : return &pVariantArg->pbVal;
  case VT_I2                     : return &pVariantArg->iVal;
  case VT_I2 | VT_BYREF          : return &pVariantArg->piVal;
  case VT_UI2                    : return &pVariantArg->uiVal;
  case VT_UI2 | VT_BYREF         : return &pVariantArg->puiVal;
  case VT_I4                     : return &pVariantArg->lVal;
  case VT_I4 | VT_BYREF          : return &pVariantArg->plVal;
  case VT_UI4                    : return &pVariantArg->ulVal;
  case VT_UI4 | VT_BYREF         : return &pVariantArg->pulVal;
  case VT_INT                    : return &pVariantArg->intVal;
  case VT_INT | VT_BYREF         : return &pVariantArg->pintVal;
  case VT_UINT                   : return &pVariantArg->uintVal;
  case VT_UINT | VT_BYREF        : return &pVariantArg->puintVal;
  // VT_USERDEFINED in the typeDesc indicates an ENUM, but
  // VT_USERDEFINED is illegal to use in the DISPPARAMS.  The right
  // thing to do is pass it as an INT.  Note that we have to bash out
  // the variant tag.
  //  ** NOTE THAT VT_USERDEFINED | VT_BYREF IS NOT
  //  ** A REFERENCE TO AN INT
  case VT_USERDEFINED            : return &pVariantArg->vt;
  case VT_R4                     : return &pVariantArg->fltVal;
  case VT_R4 | VT_BYREF          : return &pVariantArg->pfltVal;
  case VT_R8                     : return &pVariantArg->dblVal;
  case VT_R8 | VT_BYREF          : return &pVariantArg->pdblVal;
  case VT_BSTR                   : return pVariantArg->bstrVal;
  case VT_BSTR | VT_BYREF        : return &pVariantArg->pbstrVal;
  case VT_CY                     : return &pVariantArg->cyVal;
  case VT_CY | VT_BYREF          : return &pVariantArg->pcyVal;
  case VT_DATE                   : return &pVariantArg->date;
  case VT_DATE | VT_BYREF        : return &pVariantArg->pdate;
  case VT_BOOL                   : return &pVariantArg->boolVal;
  case VT_BOOL | VT_BYREF        : return &pVariantArg->pboolVal;
  case VT_ERROR                  : return &pVariantArg->scode;
  case VT_ERROR | VT_BYREF       : return &pVariantArg->pscode;
  case VT_DISPATCH               : return pVariantArg->pdispVal;
  case VT_DISPATCH | VT_BYREF    : return &pVariantArg->ppdispVal;
  // VT_USERDEFINED | VT_BYREF indicates that we should pass the
  // IUnknown pointer of a COM object.
  // VT_USERDEFINED | VT_BYREF is illegal in the DISPPARAMS, so we
  // bash it out to VT_UNKNOWN.
  case VT_USERDEFINED | VT_BYREF : return &pVariantArg->punkVal;
  case VT_VARIANT | VT_BYREF     : return &pVariantArg->pvarVal;
  case VT_UNKNOWN                : return pVariantArg->punkVal;
  case VT_UNKNOWN | VT_BYREF     : return &pVariantArg->ppunkVal;
  case VT_VARIANT                : return pVariantArg;
  case VT_PTR :
    scheme_signal_error("unable to marshal VT_PTR");
    break;
  default :
    sprintf(errBuff, "Unable to marshal Scheme value into VARIANT: 0x%X",
            pVariantArg->vt);
    scheme_signal_error(errBuff);
  }

  // Make the compiler happy
  return pVariantArg;
}

VARTYPE schemeValueToCOMType(Scheme_Object* val)
{
  if (SCHEME_CHARP(val))               return VT_UI1;
  else if (SCHEME_EXACT_INTEGERP(val)) return VT_I4;
#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP(val))           return VT_R4;
#endif
  else if (SCHEME_DBLP(val))           return VT_R8;
  else if (SCHEME_STRSYMP(val))        return VT_BSTR;
  else if (MX_CYP(val))                return VT_CY;
  else if (MX_DATEP(val))              return VT_DATE;
  else if (val == scheme_false)        return VT_BOOL;
  else if (val == scheme_true)         return VT_BOOL;
  else if (MX_SCODEP(val))             return VT_ERROR;
  else if (MX_COM_OBJP(val))           return VT_DISPATCH;
  else if (MX_IUNKNOWNP(val))          return VT_UNKNOWN;
  else if (SCHEME_VECTORP(val)) getSchemeVectorType(val);
  else if (scheme_apply(mx_marshal_raw_scheme_objects, 0, NULL) == scheme_false)
    scheme_signal_error("Unable to inject Scheme value %V into VARIANT", val);
  else return VT_INT;
  return VT_VARIANT; // If all else fails. (Eli: Looks like this is redundant)
}


void doSetArrayElts(Scheme_Object *vec, VARTYPE elementType, SAFEARRAY *theArray,
                    long *allIndices, long *currNdx, long offset) {
  VARIANT variant;
  Scheme_Object *elt;
  int len;
  int i;

  len = SCHEME_VEC_SIZE(vec);

  if (offset) {
    for (i = 0; i < len; i++) {
      elt = SCHEME_VEC_ELS(vec)[i];
      currNdx[offset] = i;
      doSetArrayElts(elt, elementType, theArray, allIndices,
                     currNdx, offset - 1);
    }
  } else {
    for (i = 0; i < len; i++) {
      elt = SCHEME_VEC_ELS(vec)[i];
      currNdx[offset] = i;
      marshalSchemeValueToVariant(elt,&variant);
      if (variant.vt != elementType && elementType != VT_VARIANT) {
        char errBuff[100];
        sprintf(errBuff,
                "Unable to put an element of COM type 0x%x into an array of COM type 0x%x",
                variant.vt, elementType);
        scheme_signal_error(errBuff);
      }
      SafeArrayPutElement(theArray, allIndices,
                          variantDataPointer(elementType,&variant));
    }
  }
}

void setArrayElts(Scheme_Object *vec, VARTYPE elementType, SAFEARRAY *theArray,
                  long numDims) {
  long indices[MAXARRAYDIMS];

  memset(indices,0,sizeof(indices));
  doSetArrayElts(vec,elementType,theArray,indices,indices, numDims - 1);
}

// This doesn't work if we have an integer in a double array (or want
// a double array but have an integer vector).  But it should work if
// we have doubles and integers (and return a VT_R8 array). Try to
// subtype it.
VARTYPE getSchemeVectorType(Scheme_Object *vec) {
  VARTYPE type;
  int i, size = SCHEME_VEC_SIZE(vec);

  type = schemeValueToCOMType(SCHEME_VEC_ELS(vec)[0]);
  if (VT_VARIANT == type) return type;
  for (i = 1; i < size; ++i) {
    if (type != schemeValueToCOMType(SCHEME_VEC_ELS(vec)[i]))
      return VT_VARIANT;
  }
  return type;
}

SAFEARRAY *schemeVectorToSafeArray(Scheme_Object *vec, VARTYPE *vt) {
  SAFEARRAY *theArray;
  SAFEARRAYBOUND *rayBounds;
  int numDims;
  int i;
  VARTYPE _vt;

  if (SCHEME_VECTORP(vec) == FALSE)
    scheme_signal_error("Can't convert non-vector to SAFEARRAY");
  if (isRegularVector(vec) == FALSE)
    scheme_signal_error("Can't convert irregular vector to SAFEARRAY");
  numDims = getSchemeVectorDims(vec);
  if (numDims > MAXARRAYDIMS)
    scheme_signal_error("Too many array dimensions");
  rayBounds = (SAFEARRAYBOUND *)malloc(numDims * sizeof(SAFEARRAYBOUND));
  for (i = 0; i < numDims; i++) { rayBounds[i].lLbound = 0L; }
  setArrayEltCounts(vec,rayBounds,numDims);
  _vt = getSchemeVectorType(vec);
  *vt = _vt;
  theArray = SafeArrayCreate(*vt,numDims,rayBounds);
  setArrayElts(vec,*vt,theArray,numDims);
  return theArray;
}

#endif // MYSTERX_3M
