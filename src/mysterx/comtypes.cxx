// comtypes.cxx

#include "stdafx.h"

#include <assert.h>

#include <stdio.h>
#include <malloc.h>
#include <float.h>

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

Scheme_Type mx_com_object_type;
Scheme_Type mx_com_type_type;
Scheme_Type mx_browser_type;
Scheme_Type mx_document_type;
Scheme_Type mx_element_type;
Scheme_Type mx_event_type;

Scheme_Type mx_com_cy_type;
Scheme_Type mx_com_date_type;
Scheme_Type mx_com_scode_type;
Scheme_Type mx_com_iunknown_type;
Scheme_Type mx_com_omit_type;
Scheme_Type mx_com_typedesc_type;

Scheme_Object *mx_document_pred(int argc,Scheme_Object **argv)
{
  return MX_DOCUMENTP (argv[0]) ? scheme_true : scheme_false;
}

Scheme_Object *mx_make_cy (CY *pCy)
{
  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc_atomic (sizeof (MX_COM_Data_Object));

  retval->type = mx_com_cy_type;
  retval->cy = *pCy;

  return (Scheme_Object *)retval;
}

Scheme_Object *mx_make_date (DATE *pDate)
{
  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc_atomic (sizeof (MX_COM_Data_Object));

  retval->type = mx_com_date_type;
  retval->date = *pDate;

  return (Scheme_Object *)retval;
}

Scheme_Object *mx_make_bool(unsigned boolVal)
{
  return (boolVal == 0) ? scheme_false : scheme_true;
}

Scheme_Object *mx_make_scode(SCODE scode)
{
  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc_atomic (sizeof (MX_COM_Data_Object));

  retval->type = mx_com_scode_type;
  retval->scode = scode;

  return (Scheme_Object *)retval;
}

Scheme_Object *mx_make_idispatch(IDispatch *pIDispatch)
{
  MX_COM_Object *retval;

  if (pIDispatch == NULL) return scheme_false;

  retval = (MX_COM_Object *)scheme_malloc(sizeof(MX_COM_Object));

  retval->type = mx_com_object_type;
  retval->pIDispatch = pIDispatch;
  retval->clsId = emptyClsId;
  retval->pITypeInfo = NULL;
  retval->pEventTypeInfo = NULL;
  retval->pIConnectionPoint = NULL;
  retval->pISink = NULL;
  retval->connectionCookie = (DWORD)0;
  retval->released = FALSE;

  mx_register_com_object((Scheme_Object *)retval,pIDispatch);

  return (Scheme_Object *)retval;
}


Scheme_Object *mx_make_iunknown(IUnknown *pIUnknown) {
  IDispatch * pIDispatch = NULL;
  IUnknown * pUnk = NULL;
  HRESULT hr;

  // Ensure we have the canonical iunknown!
  pIUnknown->QueryInterface (IID_IUnknown, (void **)&pUnk);
  pIUnknown->Release();

  // Try to get Dispatch pointer
  hr = pUnk->QueryInterface (IID_IDispatch, (void **)&pIDispatch);

  if (SUCCEEDED (hr)) {
      pUnk->Release();
      return mx_make_idispatch (pIDispatch);
      }

  // DebugBreak();

  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc(sizeof(MX_COM_Data_Object));

  retval->type = mx_com_iunknown_type;
  retval->released = FALSE;
  retval->pIUnknown = pUnk;

  mx_register_simple_com_object ((Scheme_Object *)retval, pUnk);

  return (Scheme_Object *)retval;
}

BOOL mx_cy_pred(Scheme_Object *obj) {
  return MX_CYP(obj);
}

Scheme_Object *mx_cy_pred_ex(int argc,Scheme_Object **argv) {
  return mx_cy_pred(argv[0]) ? scheme_true : scheme_false;
}

BOOL mx_date_pred(Scheme_Object *obj) {
  return MX_DATEP(obj);
}

Scheme_Object *mx_date_pred_ex(int argc,Scheme_Object **argv) {
  return mx_date_pred(argv[0]) ? scheme_true : scheme_false;
}

BOOL mx_scode_pred(Scheme_Object *obj) {
  return MX_SCODEP(obj);
}

Scheme_Object *mx_scode_pred_ex (int argc,Scheme_Object **argv)
{
  return mx_scode_pred(argv[0]) ? scheme_true : scheme_false;
}

BOOL mx_comobj_pred (Scheme_Object *obj)
{
  return MX_COM_OBJP(obj);
}

Scheme_Object *mx_comobj_pred_ex(int argc,Scheme_Object **argv)
{
  return mx_comobj_pred (argv[0]) ? scheme_true : scheme_false;
}

BOOL mx_iunknown_pred(Scheme_Object *obj)
{
  return MX_IUNKNOWNP(obj);
}

Scheme_Object *mx_iunknown_pred_ex(int argc,Scheme_Object **argv)
{
  return mx_iunknown_pred (argv[0]) ? scheme_true : scheme_false;
}

CY mx_cy_val (Scheme_Object *obj)
{
  return MX_CY_VAL(obj);
}

Scheme_Object *mx_currency_to_scheme_number(int argc,Scheme_Object **argv)
{
  CY cy;
  char buff[40];
  int len;
  Scheme_Object *port;

  cy = MX_CY_VAL (GUARANTEE_CY ("com-currency->number", 0));

  sprintf(buff,"%I64d",cy);

  len = (int)strlen(buff);

  // divide by 10,000 by shifting digits

   if (len > 4) {
    memmove(buff + len - 3,buff + len - 4,4);
    buff[len - 4] = '.';
    buff[len + 1] = '\0';
  }
  else if (len > 0) {
    int i;

    memmove(buff + 5 - len,buff,len);
    buff[0] = '.';
    for (i = 1; i < 5 - len; i++) {
      buff[i] = '0';
    }
    buff[6-len] = '\0';
  }
  else {
    buff[0] = '0';
    buff[1] = '\0';
  }

  port = scheme_make_byte_string_input_port(buff);

  return scheme_read(port);
}

BOOL lt64 (_int64 n1,_int64 n2)
{
  return n1 < n2;
}

BOOL gt64 (_int64 n1,_int64 n2)
{
  return n1 > n2;
}

_int64 add64(_int64 n,int m)
{
  return n + m;
}

_int64 sub64(_int64 n,int m)
{
  return n - m;
}

_int64 scanNum64(char *s,_int64 (*combine)(_int64,int),
		 BOOL (*cmp)(_int64,_int64),Scheme_Object *obj) {
  _int64 cy,last;

  last = cy = 0;
  while (*s) {
    cy *= 10;
    cy = combine(cy,*s - '0');
    if (cmp(cy,last))
      scheme_signal_error("number->com-currency: "
			  "number %V too big to fit in com-currency",
			  obj);

    last = cy;
    s++;
  }
  return cy;
}

Scheme_Object *scheme_number_to_mx_currency(int argc,Scheme_Object **argv) {
  char *p,*q,*r,*s;
  char buff[40];
  _int64 cy;
  int neededZeroes;
  int len;
  int i;

  if (SCHEME_EXACT_INTEGERP(argv[0]) == FALSE &&
      SCHEME_FLOATP(argv[0]) == FALSE)
    scheme_wrong_type("number->com-currency","exact or inexact number",0,argc,argv);

  s = scheme_display_to_string(argv[0],NULL);
  strncpy(buff,s,sizeof(buff)-1);
  buff[min(strlen(s),sizeof(buff))] = '\0';

  // multiply by 10,000

  len = (int)strlen(buff);
  p = strchr(buff,'.');

  if (p) {
    int numDecimals;

    numDecimals = (int)(buff - p) + (len - 1);
    neededZeroes = max(4 - numDecimals,0);

    memmove(p,p+1,min(numDecimals,4));
    q = p + numDecimals;
  }
  else {
    q = buff + len;
    neededZeroes = 4;
  }

  for (i = 0; i < neededZeroes; i++)
    *q++ = '0';

  *q = '\0';

  r = buff;

  cy = (*r == '-')
      ? scanNum64 (r+1, sub64, gt64, argv[0])
      : scanNum64 (r,   add64, lt64, argv[0]);

  return mx_make_cy((CY *)&cy);
}

DATE mx_date_val (Scheme_Object *obj)
{
  return MX_DATE_VAL (obj);
}

BOOL isLeapYear(int year)
{
  return
      (year % 4) ? FALSE
      : (year % 400) ? TRUE
      : (year % 100) ? FALSE
      : TRUE;
}

Scheme_Object *mx_date_to_scheme_date(int argc,Scheme_Object **argv) {
  SYSTEMTIME sysTime;
  Scheme_Object *p[10];
  int yearDay;
  static int offsets[12] =
  { 0,   // Jan
    31,  // Feb
    59,  // Mar
    90,  // Apr
    120, // May
    151, // Jun
    181, // Jul
    212, // Aug
    243, // Sept
    273, // Oct
    304, // Nov
    334, // Dec
  };

  GUARANTEE_DATE ("date->com-date", 0);

  if (VariantTimeToSystemTime(MX_DATE_VAL(argv[0]),&sysTime) == FALSE)
    scheme_signal_error("com-date->date: error in conversion");


  yearDay = offsets[sysTime.wMonth - 1] + sysTime.wDay;

  yearDay--; /* because 0-based */

  if (sysTime.wMonth > 2 && isLeapYear(sysTime.wYear))
    yearDay++;

  p[0] = scheme_make_integer(sysTime.wSecond);
  p[1] = scheme_make_integer(sysTime.wMinute);
  p[2] = scheme_make_integer(sysTime.wHour);
  p[3] = scheme_make_integer(sysTime.wDay);
  p[4] = scheme_make_integer(sysTime.wMonth);
  p[5] = scheme_make_integer(sysTime.wYear);
  p[6] = scheme_make_integer(sysTime.wDayOfWeek);
  p[7] = scheme_make_integer(yearDay);
  p[8] = scheme_false;
  p[9] = scheme_make_integer(0); // time zone offset

  return scheme_make_struct_instance(scheme_date_type,sizeray(p),p);
}

Scheme_Object *scheme_date_to_mx_date(int argc,Scheme_Object **argv) {
  SYSTEMTIME sysTime;
  DATE vDate;
  Scheme_Object *date;
  static char *fieldNames[] = {
    "second","minute","hour","day","month","year","week-day",
    "year-day","dst?","time-zone-offset"
  };
  int i;

  if (scheme_is_struct_instance(scheme_date_type,argv[0]) == FALSE)
    scheme_wrong_type("date->com-date","struct:date",0,argc,argv);

  date = argv[0];

  for (i = 0; i < 10; i++) {
    // ignore DST boolean field
    if (i != 8 && SCHEME_INTP(scheme_struct_ref(date,i)) == FALSE)
      scheme_signal_error("date->com-date: date structure contains "
			  "non-fixnum in %s field",fieldNames[i]);
  }

  sysTime.wMilliseconds = 0;
  sysTime.wSecond = (WORD)SCHEME_INT_VAL(scheme_struct_ref(date,0));
  sysTime.wMinute = (WORD)SCHEME_INT_VAL(scheme_struct_ref(date,1));
  sysTime.wHour = (WORD)SCHEME_INT_VAL(scheme_struct_ref(date,2));
  sysTime.wDay = (WORD)SCHEME_INT_VAL(scheme_struct_ref(date,3));
  sysTime.wMonth = (WORD)SCHEME_INT_VAL(scheme_struct_ref(date,4));
  sysTime.wYear = (WORD)SCHEME_INT_VAL(scheme_struct_ref(date,5));
  sysTime.wDayOfWeek = (WORD)SCHEME_INT_VAL(scheme_struct_ref(date,6));

  if (SystemTimeToVariantTime(&sysTime,&vDate) == 0)
    scheme_signal_error("date->com-date: unable to perform conversion");

  return mx_make_date(&vDate);
}

SCODE mx_scode_val (Scheme_Object *obj)
{
  return MX_SCODE_VAL (obj);
}

Scheme_Object * mx_scode_to_scheme_number (int argc, Scheme_Object **argv)
{
  return scheme_make_integer_value (MX_SCODE_VAL (GUARANTEE_SCODE ("com-scode->number", 0)));
}

Scheme_Object * scheme_number_to_mx_scode(int argc, Scheme_Object **argv)
{
  SCODE scode;

  GUARANTEE_TYPE ("number->com-scode", 0, SCHEME_REALP, "number");

  if (scheme_get_int_val (argv[0], &scode) == 0)
      scheme_signal_error("number->com-scode: "
			  "number %V too big to fit in com-scode", argv[0]);

  return mx_make_scode (scode);
}

IDispatch * mx_comobj_val (Scheme_Object * obj)
{
  return MX_COM_OBJ_VAL (obj);
}

IUnknown * mx_iunknown_val (Scheme_Object * obj)
{
  return MX_IUNKNOWN_VAL (obj);
}
