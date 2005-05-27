// TestControl.cpp : Implementation of CTestControl

#include "stdafx.h"
#include "Testobject.h"
#include "TestControl.h"

/////////////////////////////////////////////////////////////////////////////
// CTestControl


STDMETHODIMP CTestControl::AddTest(long n1, long *n2, long *n3)
{
  // note side effect
  *n3 = n1 + *n2;
  *n2 = n1;
  return S_OK;
}

STDMETHODIMP CTestControl::StringTest (BSTR s1, BSTR s2, BSTR *s3)
{
  int len1,len2;

  len1 = SysStringLen(s1);
  len2 = SysStringLen(s2);

  *s3 = SysAllocStringByteLen (NULL, (len1 + len2)*2);
  wcsncpy (*s3, s1, len1);
  wcsncpy (*s3 + len1, s2, len2);

  return S_OK;
}

STDMETHODIMP CTestControl::ShortTest (short n1, short n2, short *n3)
{
  *n3 = n1 * n2;
  return S_OK;
}

STDMETHODIMP CTestControl::FloatTest (double n1, double n2, double *n3)
{
  *n3 = n2 - n1;
  return S_OK;
}

STDMETHODIMP CTestControl::UnsignedTest (unsigned n1, unsigned n2, unsigned *n3)
{
  *n3 = n2 - n1;
  return S_OK;
}

STDMETHODIMP CTestControl::get_Numprop (long ndx,long *pVal)
{
  *pVal = the_value;
  return S_OK;
}

STDMETHODIMP CTestControl::put_Numprop (long ndx,long newVal)
{
  the_value = (ndx > 21) ? 42 : 99;
  return S_OK;
}
