// array.cxx

#include "stdafx.h"

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

  if (hr != S_OK) {
    codedComError("Can't get array type",hr);
  }

  switch(vt) {

  case VT_EMPTY :
  case VT_NULL :

    return scheme_void;

  case VT_UI1 :
    char cArg;
    SafeArrayGetElement(theArray,allIndices,&cArg);
    return scheme_make_character(cArg);

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
    return unmarshalBSTR((unsigned short *)bArg);

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
	    "Can't make Scheme value from array element with type 0x%X",vt);
    scheme_signal_error(errBuff);

  }

  return NULL;
}

Scheme_Object *buildVectorFromArray(SAFEARRAY *theArray,long currDim,
				    long *allIndices,long *currNdx) {
  Scheme_Object *vec;
  long low,high,vecSize;
  long i,j;

  SafeArrayGetLBound(theArray,currDim,&low);
  SafeArrayGetUBound(theArray,currDim,&high);
  vecSize = high - low + 1;

  vec = scheme_make_vector(vecSize,scheme_void);

  if (currDim > 1) {
    for (i = 0,j = low; i < vecSize; i++,j++) {
      currNdx[0] = j;
      SCHEME_VEC_ELS(vec)[i] =
	buildVectorFromArray(theArray,currDim - 1,
			     allIndices,currNdx - 1);
    }
  }
  else {
    for (i = 0,j = low; i < vecSize; i++,j++) {
      currNdx[0] = j;
      SCHEME_VEC_ELS(vec)[i] =
	safeArrayElementToSchemeObject(theArray,allIndices);
    }
  }

  return vec;
}

Scheme_Object *safeArrayToSchemeVector(SAFEARRAY *theArray) {
  long numDims;
  long *indices;
  Scheme_Object *retval;

  numDims = SafeArrayGetDim(theArray);

  indices = (long *)scheme_malloc(numDims * sizeof(long));

  retval = buildVectorFromArray(theArray,numDims,
				indices,indices + numDims - 1);

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

  if (SCHEME_VECTORP(vec) == FALSE) {
    return TRUE;
  }

  len = SCHEME_VEC_SIZE(vec);
  elts = SCHEME_VEC_ELS(vec);

  // use zeroth elt as standard

  elt = elts[0];

  zeroIsVec = SCHEME_VECTORP(elt);
  if (zeroIsVec) {
    zeroLen = SCHEME_VEC_SIZE(elt);
  }

  if (isRegularVector(elt) == FALSE) {
    return FALSE;
  }

  for (i = 1; i < len; i++) {
    elt = elts[i];

    isVec = SCHEME_VECTORP(elt);

    if (isVec != zeroIsVec) {
      return FALSE;
    }

    if (isVec) {
      currLen = SCHEME_VEC_SIZE(elt);

      if (currLen != zeroLen) {
	return FALSE;
      }

      if (isRegularVector(elt) == FALSE) {
	return FALSE;
      }
    }
  }

  return TRUE;
}

void doSetArrayElts(Scheme_Object *vec,SAFEARRAY *theArray,
		    long *allIndices,long *currNdx) {
  VARIANT variant;
  Scheme_Object *elt;
  int len;
  int i;

  len = SCHEME_VEC_SIZE(vec);

  if (currNdx > allIndices) {
    for (i = 0; i < len; i++) {
      elt = SCHEME_VEC_ELS(vec)[i];
      currNdx[0] = i;
      doSetArrayElts(elt,theArray,allIndices,currNdx - 1);
    }
  }
  else {
    for (i = 0; i < len; i++) {
      elt = SCHEME_VEC_ELS(vec)[i];
      currNdx[0] = i;
      marshalSchemeValueToVariant(elt,&variant);
      SafeArrayPutElement(theArray,allIndices,&variant);
    }
  }
}

void setArrayElts(Scheme_Object *vec,SAFEARRAY *theArray,long numDims) {
  long indices[MAXARRAYDIMS];

  memset(indices,0,sizeof(indices));

  doSetArrayElts(vec,theArray,indices,indices + numDims - 1);
}

SAFEARRAY *schemeVectorToSafeArray(Scheme_Object *vec) {
  SAFEARRAY *theArray;
  SAFEARRAYBOUND *rayBounds;
  int numDims;
  int i;

  if (SCHEME_VECTORP(vec) == FALSE) {
    scheme_signal_error("Can't convert non-vector to SAFEARRAY");
  }

  if (isRegularVector(vec) == FALSE) {
    scheme_signal_error("Can't convert irregular vector to SAFEARRAY");
  }

  numDims = getSchemeVectorDims(vec);

  if (numDims > MAXARRAYDIMS) {
    scheme_signal_error("Too many array dimensions");
  }

  rayBounds = (SAFEARRAYBOUND *)scheme_malloc(numDims * sizeof(SAFEARRAYBOUND));

  for (i = 0; i < numDims; i++) {
    rayBounds[i].lLbound = 0L;
  }

  setArrayEltCounts(vec,rayBounds,numDims);

  theArray = SafeArrayCreate(VT_VARIANT,numDims,rayBounds);

  setArrayElts(vec,theArray,numDims);

  return theArray;

}
