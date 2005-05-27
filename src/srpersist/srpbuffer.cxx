/* srpbuffer.c */

#ifndef ODBCVER
#error Must define ODBCVER when compiling
#endif

#include <ctype.h>

#ifdef WIN32
#include <windows.h>
#else
#define FALSE (0)
#define TRUE  (1)
typedef int BOOL;
typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned long DWORD;
/* dummy typedefs -- only used in trace API, not ODBC as such */
typedef void VOID; 
typedef int GUID; 
#ifndef HAVE_CHAR
typedef int CHAR; 
#endif
#ifndef HAVE_WCHAR
typedef int WCHAR; 
#endif
#ifndef HAVE_LPWSTR
typedef void *LPWSTR; 
#endif
#endif /* end not WIN32 */

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#include "escheme.h"

#include "srptypes.h"
#include "srpbuffer.h"
#include "srpersist.h"

#if HASINT64
#ifndef WIN32
SRPINT64 _atoi64(char *);
SRPUINT64 _atoui64(char *);
#endif
#endif

Scheme_Object *readCharBuffer(char *buffer,long width,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i,j;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1, j = width * (arrayLength-1); i >= 0; i--, j -= width) {
      retval = scheme_make_pair(scheme_make_string(buffer + j),retval);
    }
  }
  else {
    retval = scheme_make_string(buffer + (ndx * width));
  }

  return retval;
}

void writeCharBuffer(char *buffer,Scheme_Object *obj,long width,long ndx) {
  char *b;

  b = buffer + width * ndx;
  memset(b,'\0',width);
  strcpy(b,SCHEME_STR_VAL(obj));
}

#if (ODBCVER >= 0x0300)
Scheme_Object *readWideString(long sz,wchar_t *buffer,unsigned long n) {
  char *s;
  long i,j;

  s = (char *)scheme_malloc(sz + 1);

  /* truncate wide chars */
      
  for (i = n,j = 0; j < sz; i++,j++) {
    if (buffer[i] & 0xFF00) {
      scheme_signal_error("SQL_C_WCHAR buffer contains wide character, "
			  "value %s",intToHexString(buffer[i]));
    }

    s[i] = (char)(buffer[i] & 0xFF);

    if (s[i] == '\0') {
      break;
    }
  }

  return scheme_make_string(s);
}


Scheme_Object *readWideCharBuffer(wchar_t *buffer,long width,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;
    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readWideString(width,buffer,i*width),retval);
    }
  }
  else {
    retval = readWideString(width,buffer,ndx*width);
  }

  return retval;
}

void writeWideCharBuffer(wchar_t *buffer,Scheme_Object *obj,
			 long width,long ndx) {
  char *s;
  wchar_t *b;

  s = SCHEME_STR_VAL(obj);

  b = buffer + width * ndx;
  memset(b,'\0',width * sizeof(wchar_t));
  while (*s) {
    *b++ = (wchar_t)(*s++);
  }
}
#endif

Scheme_Object *readLongBuffer(long *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_integer_value(buffer[i]),retval);
    }

  }
  else {
    retval = scheme_make_integer_value(buffer[ndx]);
  }

  return retval;
}

void writeLongBuffer(long *buffer,Scheme_Object *obj,long ndx) {
  long longVal;

  if (scheme_get_int_val(obj,&longVal) == 0) {
    scheme_signal_error("write-buffer: number too big");
  }

  buffer[ndx] = longVal;
}

Scheme_Object *readULongBuffer(unsigned long *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(buffer[i]),retval);
    }
  }
  else {
    retval = scheme_make_integer_value_from_unsigned(buffer[ndx]);
  }

  return retval;
}

void writeULongBuffer(unsigned long *buffer,Scheme_Object *obj,long ndx) {
  unsigned long ulongVal;

  if (scheme_get_unsigned_int_val(obj,&ulongVal) == 0) {
      scheme_signal_error("write-buffer: number too big");
    } 

  buffer[ndx] = ulongVal;
}

Scheme_Object *readShortBuffer(short *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_integer_value((long)(buffer[i])),retval);
    }
  }
  else {
    retval = scheme_make_integer_value((long)(buffer[ndx]));
  }

  return retval;
}

void writeShortBuffer(short *buffer,Scheme_Object *obj,long ndx) {
  if (isSmallInt(obj) == FALSE) {
    scheme_signal_error("write-buffer: number too big");
  } 

  buffer[ndx] = (short)SCHEME_INT_VAL(obj);
}

Scheme_Object *readUShortBuffer(unsigned short *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_integer_value_from_unsigned((unsigned long)(buffer[i])),retval);
    }
  }
  else {
    retval = scheme_make_integer_value_from_unsigned((unsigned long)(buffer[ndx]));
  }

  return retval;
}

void writeUShortBuffer(unsigned short *buffer,Scheme_Object *obj,long ndx) {
  if (isUnsignedSmallInt(obj) == FALSE) {
    scheme_signal_error("write-buffer: number too big");
  } 

  buffer[ndx] = (unsigned short)SCHEME_INT_VAL(obj);
}

Scheme_Object *readFloatBuffer(float *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_double((double)(buffer[i])),retval);
    }
  }
  else {
    retval = scheme_make_double((double)(buffer[ndx]));
  }

  return retval;
}

void writeFloatBuffer(float *buffer,Scheme_Object *obj,long ndx) {
  Scheme_Object *currVal;

  currVal = obj;

  buffer[ndx] = (float)SCHEME_FLOAT_VAL(currVal);
}

Scheme_Object *readDoubleBuffer(double *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_double(buffer[i]),retval);
    }
  }
  else {
    retval = scheme_make_double(buffer[ndx]);
  }

  return retval;
}

void writeDoubleBuffer(double *buffer,Scheme_Object *obj,long ndx) {
  buffer[ndx] = SCHEME_DBL_VAL(obj);
}

#if ODBCVER >= 0x0300

Scheme_Object *readNumericVal(SQL_NUMERIC_STRUCT *buffer,long offset) {
  Scheme_Object *digits;
  Scheme_Object *argv[4];
  SQL_NUMERIC_STRUCT *currVal;
  long j,k;

  currVal = buffer + offset;
  argv[0] = scheme_make_integer(currVal->precision);
  argv[1] = scheme_make_integer(currVal->scale);
  argv[2] = scheme_make_integer_value_from_unsigned(currVal->sign);
      
  /* in Scheme structure, store hex digits with MSBytes leftmost
     in MS structure, MSBytes are rightmost */

  digits = scheme_null;

  /* rightmost 0's in MS structure can be stripped off */

  k = sizeray(currVal->val) - 1;
  while (k >= 0) {
    if (currVal->val[k] != 0) {
      break;
    }
    k--;
  }

  for (j = 0; j <= k; j++) {
    digits = scheme_make_pair(scheme_make_integer(currVal->val[j]),digits);
  }
  argv[3] = scheme_list_to_vector(digits);  
  
  return scheme_make_struct_instance(NUMERIC_STRUCT_TYPE,sizeray(argv),argv);
}

Scheme_Object *readNumericBuffer(SQL_NUMERIC_STRUCT *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readNumericVal(buffer,i),retval);
    }
  }
  else {
    retval = readNumericVal(buffer,ndx);
  }

  return retval;
}
#endif

#if ODBCVER >= 0x0300 
void writeNumericBuffer(SQL_NUMERIC_STRUCT *buffer,Scheme_Object *obj,
			long ndx) {
  Scheme_Object *precision,*scale,*sign,*val;
  SQL_NUMERIC_STRUCT *currBuff;
  char *signStr;
  int i,j;

  currBuff = buffer + ndx;

  precision = scheme_apply(NUMERIC_PRECISION,1,&obj);
  scale = scheme_apply(NUMERIC_SCALE,1,&obj);  
  sign = scheme_apply(NUMERIC_SIGN,1,&obj);    
  val = scheme_apply(NUMERIC_VAL,1,&obj);    
  
  if (isUnsignedCharInt(precision) == FALSE) {
    scheme_signal_error("Precision in numeric structure not exact integer or too large");
  }

  if (isCharInt(scale) == FALSE) {
    scheme_signal_error("Scale in numeric structure not exact integer or too large");
  }

  if (SCHEME_SYMBOLP(sign) == FALSE) {
    scheme_signal_error("Sign in numeric structure neither \'+ nor \'-");
  }

  signStr = SCHEME_SYM_VAL(sign);

  if (strcmp(signStr,"+") && strcmp(signStr,"-")) {
    scheme_signal_error("Sign in numeric structure neither \'+ nor \'-");
  }

  if (SCHEME_VECTORP(val) == FALSE) {
    scheme_signal_error("Value in numeric structure not a vector of exact integers");
  }

  if (SCHEME_VEC_SIZE(val) > SQL_MAX_NUMERIC_LEN) {
    scheme_signal_error("Length of value vector in numeric structure too long");
  }

  for (i = 0; i < SQL_MAX_NUMERIC_LEN; i++) {
    if (isUnsignedCharInt(SCHEME_VEC_ELS(val)[i]) == FALSE) {
      scheme_signal_error("Value in numeric structure not a vector of exact integers");
    }
  }

  currBuff->precision = (SQLCHAR)SCHEME_INT_VAL(precision);
  currBuff->scale = (SQLSCHAR)SCHEME_INT_VAL(scale);
  currBuff->sign = (*signStr == '+') ?  1 : 0;

  i = SQL_MAX_NUMERIC_LEN - 1;
  while (i >= 0) {
    if (SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[i]) != 0) {
      break;
    }
    i--;
  }

  for (j = 0; i >= 0; i--,j++) {
    currBuff->val[j] = (SQLCHAR)SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[i]);
  }
}
#endif

#if (ODBCVER >= 0x0300)
Scheme_Object *readDateVal(SQL_DATE_STRUCT *buffer,long offset) {
  SQL_DATE_STRUCT *currVal;
#else
Scheme_Object *readDateVal(DATE_STRUCT *buffer,long offset) {
  DATE_STRUCT *currVal;
#endif
  Scheme_Object *argv[3];

  currVal = buffer + offset;
  argv[0] = scheme_make_integer(currVal->year);
  argv[1] = scheme_make_integer_value_from_unsigned(currVal->month);
  argv[2] = scheme_make_integer_value_from_unsigned(currVal->day);

  return scheme_make_struct_instance(DATE_STRUCT_TYPE,sizeray(argv),argv);
}

#if (ODBCVER >= 0x0300)
Scheme_Object *readDateBuffer(SQL_DATE_STRUCT *buffer,long arrayLength,long ndx) {
#else
Scheme_Object *readDateBuffer(DATE_STRUCT *buffer,long arrayLength,long ndx) {
#endif
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = arrayLength - 1; i >= 0; i--) {
    retval = scheme_make_pair(readDateVal(buffer,i),retval);
  }

  return retval;
}

void writeDateBuffer(DATE_STRUCT *buffer,Scheme_Object *obj,long ndx) {
  Scheme_Object *year,*month,*day;
  DATE_STRUCT *currBuff;

  currBuff = buffer + ndx;

  year = scheme_apply(DATE_YEAR,1,&obj);
  month = scheme_apply(DATE_MONTH,1,&obj);  
  day = scheme_apply(DATE_DAY,1,&obj);    

  if (isSmallInt(year) == FALSE) {
      scheme_signal_error("Year in date structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(month) == FALSE) {
    scheme_signal_error("Month in date structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(day) == FALSE) {
    scheme_signal_error("Day in date structure not exact integer or too large");
  }

  currBuff->year = (SQLSMALLINT)SCHEME_INT_VAL(year);
  currBuff->month = (SQLUSMALLINT)SCHEME_INT_VAL(month);
  currBuff->day = (SQLUSMALLINT)SCHEME_INT_VAL(day);
}

#if (ODBCVER >= 0x0300)
Scheme_Object *readTimeVal(SQL_TIME_STRUCT *buffer,long offset) { 
  SQL_TIME_STRUCT *currVal;
#else
Scheme_Object *readTimeVal(TIME_STRUCT *buffer,long offset) {
  TIME_STRUCT *currVal;
#endif
  Scheme_Object *argv[3];

  currVal = buffer + offset;
  argv[0] = scheme_make_integer_value_from_unsigned(currVal->hour);
  argv[1] = scheme_make_integer_value_from_unsigned(currVal->minute);
  argv[2] = scheme_make_integer_value_from_unsigned(currVal->second);
  return scheme_make_struct_instance(TIME_STRUCT_TYPE,sizeray(argv),argv);
}

#if (ODBCVER >= 0x0300)
Scheme_Object *readTimeBuffer(SQL_TIME_STRUCT *buffer,long arrayLength,long ndx) {
#else
Scheme_Object *readTimeBuffer(TIME_STRUCT *buffer,long arrayLength,long ndx) {
#endif
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;
    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readTimeVal(buffer,i),retval);
    }
  }
  else {
    retval = readTimeVal(buffer,ndx);
  }

  return retval;
}

#if (ODBCVER >= 0x0300)
void writeTimeBuffer(SQL_TIME_STRUCT *buffer,Scheme_Object *obj,long ndx) {
  SQL_TIME_STRUCT *currBuff;
#else
void writeTimeBuffer(TIME_STRUCT *buffer,Scheme_Object *obj,long ndx) {
  TIME_STRUCT *currBuff;
#endif
  Scheme_Object *currVal;
  Scheme_Object *hour,*minute,*second;

  currVal = obj;

  currBuff = buffer + ndx;

  hour = scheme_apply(TIME_HOUR,1,&currVal);
  minute = scheme_apply(TIME_MINUTE,1,&currVal);  
  second = scheme_apply(TIME_SECOND,1,&currVal);    

  if (isUnsignedSmallInt(hour) == FALSE) {
    scheme_signal_error("Hour in time structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(minute) == FALSE) {
    scheme_signal_error("Minute in time structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(second) == FALSE) {
    scheme_signal_error("Second in time structure not exact integer or too large");
  }

  currBuff->hour = (SQLUSMALLINT)SCHEME_INT_VAL(hour);
  currBuff->minute = (SQLUSMALLINT)SCHEME_INT_VAL(minute);
  currBuff->second = (SQLUSMALLINT)SCHEME_INT_VAL(second);
}

#if (ODBCVER >= 0x0300)
Scheme_Object *readTimeStampVal(SQL_TIMESTAMP_STRUCT *buffer,long offset) {
  SQL_TIMESTAMP_STRUCT *currVal;
#else
Scheme_Object *readTimeStampVal(TIMESTAMP_STRUCT *buffer,long offset) {
  TIMESTAMP_STRUCT *currVal;
#endif
  Scheme_Object *argv[7];

  currVal = buffer + offset;
  argv[0] = scheme_make_integer(currVal->year);
  argv[1] = scheme_make_integer_value_from_unsigned(currVal->month);
  argv[2] = scheme_make_integer_value_from_unsigned(currVal->day);
  argv[3] = scheme_make_integer_value_from_unsigned(currVal->hour);
  argv[4] = scheme_make_integer_value_from_unsigned(currVal->minute);
  argv[5] = scheme_make_integer_value_from_unsigned(currVal->second);
  argv[6] = scheme_make_integer_value_from_unsigned(currVal->fraction);
  return scheme_make_struct_instance(TIMESTAMP_STRUCT_TYPE,sizeray(argv),argv);
}

#if (ODBCVER >= 0x0300)
Scheme_Object *readTimeStampBuffer(SQL_TIMESTAMP_STRUCT *buffer,long arrayLength,long ndx) {
#else
Scheme_Object *readTimeStampBuffer(TIMESTAMP_STRUCT *buffer,long arrayLength,long ndx) {
#endif
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;
    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readTimeStampVal(buffer,i),retval);
    }
  }
  else {
    retval = readTimeStampVal(buffer,ndx);
  }

  return retval;
}


#if ODBCVER >= 0x0300 
void writeTimeStampBuffer(SQL_TIMESTAMP_STRUCT *buffer,Scheme_Object *obj,
			  long ndx) {
  SQL_TIMESTAMP_STRUCT *currBuff;
#else
void writeTimeStampBuffer(TIMESTAMP_STRUCT *buffer,Scheme_Object *obj,
			  long ndx) {
  TIMESTAMP_STRUCT *currBuff;
#endif
  Scheme_Object *currVal;
  Scheme_Object *year,*month,*day,*hour,*minute,*second,*fraction;
  SQLUINTEGER fractionVal;

  currVal = obj;
  currBuff = buffer + ndx;

  year = scheme_apply(TIMESTAMP_YEAR,1,&currVal);
  month = scheme_apply(TIMESTAMP_MONTH,1,&currVal);  
  day = scheme_apply(TIMESTAMP_DAY,1,&currVal);    
  hour = scheme_apply(TIMESTAMP_HOUR,1,&currVal);
  minute = scheme_apply(TIMESTAMP_MINUTE,1,&currVal);  
  second = scheme_apply(TIMESTAMP_SECOND,1,&currVal);    
  fraction = scheme_apply(TIMESTAMP_FRACTION,1,&currVal);    

  if (isSmallInt(year) == FALSE) {
    scheme_signal_error("Year in timestamp structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(month) == FALSE) {
    scheme_signal_error("Month in timestamp structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(day) == FALSE) {
    scheme_signal_error("Day in timestamp structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(hour) == FALSE) {
    scheme_signal_error("Hour in timestamp structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(minute) == FALSE) {
    scheme_signal_error("Minute in timestamp structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(second) == FALSE) {
    scheme_signal_error("Second in timestamp structure not exact integer or too large");
  }

  if (SCHEME_EXACT_INTEGERP(fraction) == FALSE) {
    scheme_signal_error("Fraction in timestamp structure not exact integer");
  }

  if (scheme_get_unsigned_int_val(fraction,&fractionVal) == 0) {
    scheme_signal_error("Fraction in timestamp structure too large");
  }

  currBuff->year = (SQLSMALLINT)SCHEME_INT_VAL(year);
  currBuff->month = (SQLUSMALLINT)SCHEME_INT_VAL(month);
  currBuff->day = (SQLUSMALLINT)SCHEME_INT_VAL(day);
  currBuff->hour = (SQLUSMALLINT)SCHEME_INT_VAL(hour);
  currBuff->minute = (SQLUSMALLINT)SCHEME_INT_VAL(minute);
  currBuff->second = (SQLUSMALLINT)SCHEME_INT_VAL(second);
  currBuff->fraction = fractionVal;
}

#if ODBCVER >= 0x0350 
Scheme_Object *readGuidVal(SQLGUID *buffer,long offset) {
  Scheme_Object *argv[4];
  SQLGUID *currVal;
  short j;

  currVal = buffer + offset;
  argv[0] = scheme_make_integer_value_from_unsigned(currVal->Data1);
  argv[1] = scheme_make_integer_value_from_unsigned(currVal->Data2);
  argv[2] = scheme_make_integer_value_from_unsigned(currVal->Data3);
  argv[3] = scheme_make_vector(8,scheme_void);
  for (j = 0; j < 8; j++) {
    SCHEME_VEC_ELS(argv[3])[j] = 
      scheme_make_integer_value_from_unsigned(currVal->Data4[j]);
  }
  return scheme_make_struct_instance(GUID_STRUCT_TYPE,sizeray(argv),argv);
}

Scheme_Object *readGuidBuffer(SQLGUID *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readGuidVal(buffer,i),retval);
    }
  }
  else {
    retval = readGuidVal(buffer,ndx);
  }

  return retval;
}
#endif

#if ODBCVER >= 0x0350 
void writeGuidBuffer(SQLGUID *buffer,Scheme_Object *obj,long ndx) {
  Scheme_Object *currVal;
  Scheme_Object *Data1,*Data2,*Data3,*Data4;
  unsigned long Data1Val;
  SQLGUID *currBuff;
  short i;

  currVal = obj;
  currBuff = buffer + ndx;

  Data1 = scheme_apply(GUID_DATA1,1,&currVal);
  Data2 = scheme_apply(GUID_DATA2,1,&currVal);
  Data3 = scheme_apply(GUID_DATA3,1,&currVal);
  Data4 = scheme_apply(GUID_DATA4,1,&currVal);
    
  if (SCHEME_EXACT_INTEGERP(Data1) == FALSE ||
      scheme_get_unsigned_int_val(Data1,&Data1Val) == 0) {
    scheme_signal_error("Data2 in GUID structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(Data2) == FALSE) {
    scheme_signal_error("Data2 in GUID structure not exact integer or too large");
  }

  if (isUnsignedSmallInt(Data3) == FALSE) {
    scheme_signal_error("Data3 in GUID structure not exact integer or too large");
  }

  if (SCHEME_VECTORP(Data4) == FALSE) {
    scheme_signal_error("Data4 in GUID structure not a vector of exact integers");
  }

  for (i = 0; i < 8; i++) {
    if (isUnsignedCharInt(SCHEME_VEC_ELS(Data4)[i]) == FALSE) {
      scheme_signal_error("vector element in Data4 in GUID structure not exact integer or too large");
    }
  }

  currBuff->Data1 = Data1Val;
  currBuff->Data2 = (SQLUSMALLINT)SCHEME_INT_VAL(Data2);
  currBuff->Data3 = (SQLUSMALLINT)SCHEME_INT_VAL(Data3);

  for (i = 0; i < 8; i++) {
    currBuff->Data4[i] = 
      (BYTE)SCHEME_INT_VAL(SCHEME_VEC_ELS(Data4)[i]);
  }
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalVal(SQL_INTERVAL_STRUCT *buffer,long offset,
			       Scheme_Object *structType,
			       INTERVAL_FIELD_ACCESSOR *fs,
			       size_t numAcc) {
  SQL_INTERVAL_STRUCT *currVal;
  Scheme_Object *argv[10];
  size_t j;
  
  currVal = buffer + offset;
  argv[0] = scheme_make_integer(currVal->interval_sign);
  for (j = 0; j < numAcc; j++) {
    argv[j+1] = scheme_make_integer_value_from_unsigned(*(fs[j](currVal)));
  }

  return scheme_make_struct_instance(structType,numAcc+1,argv);
}

Scheme_Object *readIntervalBuffer(SQL_INTERVAL_STRUCT *buffer,
				  long arrayLength,
				  long ndx,
				  Scheme_Object *structType,
				  INTERVAL_FIELD_ACCESSOR *fs,
				  size_t numAcc) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readIntervalVal(buffer,i,structType,fs,numAcc),
				retval);
    }
  }
  else {
    retval = readIntervalVal(buffer,ndx,structType,fs,numAcc);
  }

  return retval;
}
#endif

#if ODBCVER >= 0x0300
SQLUINTEGER *getIntervalYear(SQL_INTERVAL_STRUCT *p) {
  return &p->intval.year_month.year;
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalYearBuffer(SQL_INTERVAL_STRUCT *buffer,
				      long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalYear };
  return readIntervalBuffer(buffer,arrayLength,ndx,
			    YEAR_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
SQLUINTEGER *getIntervalMonth(SQL_INTERVAL_STRUCT *p) {
  return &p->intval.year_month.month;
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalMonthBuffer(SQL_INTERVAL_STRUCT *buffer,
				       long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalMonth };
  return readIntervalBuffer(buffer,arrayLength,ndx,
			    MONTH_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
SQLUINTEGER *getIntervalDay(SQL_INTERVAL_STRUCT *p) {
  return &p->intval.day_second.day;
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalDayBuffer(SQL_INTERVAL_STRUCT *buffer,
				       long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalDay };
  return readIntervalBuffer(buffer,arrayLength,ndx,
			    DAY_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
SQLUINTEGER *getIntervalHour(SQL_INTERVAL_STRUCT *p) {
  return &p->intval.day_second.hour;
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalHourBuffer(SQL_INTERVAL_STRUCT *buffer,
				      long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalHour };
  return readIntervalBuffer(buffer,arrayLength,ndx,
			    HOUR_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
SQLUINTEGER *getIntervalMinute(SQL_INTERVAL_STRUCT *p) {
  return &p->intval.day_second.minute;
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalMinute };
  return readIntervalBuffer(buffer,arrayLength,ndx,
			    MINUTE_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
SQLUINTEGER *getIntervalSecond(SQL_INTERVAL_STRUCT *p) {
  return &p->intval.day_second.second;
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalSecond };
  return readIntervalBuffer(buffer,arrayLength,ndx,
			    SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalYearMonthBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalYear,getIntervalMonth };

  return readIntervalBuffer(buffer,arrayLength,ndx,
			    YEAR_TO_MONTH_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalDayHourBuffer(SQL_INTERVAL_STRUCT *buffer,
					 long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalDay,getIntervalHour }; 

  return readIntervalBuffer(buffer,arrayLength,ndx,
			    DAY_TO_HOUR_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalDayMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalDay,getIntervalHour,
				    getIntervalMinute }; 

  return readIntervalBuffer(buffer,arrayLength,ndx,
			    DAY_TO_MINUTE_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalDaySecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalDay,getIntervalHour, 
			      getIntervalMinute,getIntervalSecond }; 

  return readIntervalBuffer(buffer,arrayLength,ndx,
			    DAY_TO_SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalHourMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					    long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalHour,getIntervalMinute };

  return readIntervalBuffer(buffer,arrayLength,ndx,
			    HOUR_TO_MINUTE_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalHourSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					    long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalHour,getIntervalMinute, getIntervalSecond };

  return readIntervalBuffer(buffer,arrayLength,ndx,
			    HOUR_TO_SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

#if ODBCVER >= 0x0300
Scheme_Object *readIntervalMinuteSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					      long arrayLength,long ndx) {
  INTERVAL_FIELD_ACCESSOR acc[] = { getIntervalMinute,getIntervalSecond };
  return readIntervalBuffer(buffer,arrayLength,ndx,
			    MINUTE_TO_SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}
#endif

Scheme_Object *readBinaryBuffer(char *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  char *s;
  char *fmt = "%02X";
  int i,j;

  /* convert each byte to hex char pairs */

  if (ndx == WHOLE_BUFFER) {

    retval = scheme_alloc_string(arrayLength * 2 + 1,'\0');
    s = SCHEME_STR_VAL(retval);

    for (i = 0,j = 0; i < arrayLength; i++,j+=2) {
      sprintf(s + j,fmt,(int)buffer[i]);
    }
  }
  else {
    retval = scheme_alloc_string(3,'\0');
    s = SCHEME_STR_VAL(retval);

    sprintf(s,fmt,(int)buffer[ndx]);
  }

  return retval;
}

int hexCharToValue(int c) {
  if (c >= '0' && c <= '9') {
    return (c - '0');
  }

  if (c >= 'A' && c <= 'F') {
    return (c - 'A' + 10);
  }

  if (c >= 'a' && c <= 'f') {
    return (c - 'a' + 10);
  }

  return 0;
}

void writeBinaryBuffer(char *buffer,Scheme_Object *obj,long ndx) {
  char *s;
  int len;
  int i;
  long j;

  s = SCHEME_STR_VAL(obj);
  len = SCHEME_STRLEN_VAL(obj);
  
  if (len != 2) {
    scheme_signal_error("Binary buffer not of length 2");
  }

  for (i = 0,j = ndx; i < len; i++,j++) {
    if (isxdigit(*s) == FALSE) {
      scheme_signal_error("Non-hex value in binary buffer");
    }

    buffer[j] = hexCharToValue(*s);
    buffer[j] *= 16;
    s++;
    buffer[j] += hexCharToValue(*s);
    s++;
  }
}

Scheme_Object *readBitBuffer(unsigned char *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  char *s;
  int i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_alloc_string(arrayLength + 1,'\0');
    s = SCHEME_STR_VAL(retval);

    for (i = 0; i < arrayLength; i++) {
      sprintf(s + i,buffer[i] ? "1" : "0");
    }
  }
  else {
    retval = scheme_alloc_string(2,'\0');
    s = SCHEME_STR_VAL(retval);
    sprintf(s,buffer[ndx] ? "1" : "0");
  }

  return retval;
}

void writeBitBuffer(char *buffer,Scheme_Object *obj,long ndx) {
  char *s;
  int len;

  s = SCHEME_STR_VAL(obj);
  len = SCHEME_STRLEN_VAL(obj);
  
  if (len != 1) {
    scheme_signal_error("Bit buffer not of length 1");
  }

  if (s[0] == '0') {
    buffer[ndx] = 0;
  }
  else if (s[0] == '1') {
    buffer[ndx] = 1;
  }
  else {
    scheme_signal_error("write-buffer: character other than 0 or 1 in bit string");
  }
}

#if HASINT64
Scheme_Object *readBigIntVal(SRPINT64 *buffer,long offset) {
  int lo,hi;
  char bigBuff[25];
  Scheme_Object *bigLo,*bigHi;

  lo = (int)(buffer[offset] & 0xFFFFFFFF);
  hi = (int)((buffer[offset] >> 32) & 0xFFFFFFFF);
  bigLo = scheme_make_bignum(lo);
  bigHi = scheme_make_bignum(hi);
  sprintf(bigBuff,"%s%s",
	  scheme_bignum_to_string(bigHi,16),
	  scheme_bignum_to_string(bigLo,16));
  return scheme_read_bignum(bigBuff,0,16);
}

Scheme_Object *readBigIntBuffer(SRPINT64 *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readBigIntVal(buffer,i),retval);
    }
  }
  else {
    retval = readBigIntVal(buffer,ndx);
  }

  return retval;
}
#endif

#if HASINT64
void writeBigIntBuffer(SRPINT64 *buffer,Scheme_Object *obj,long ndx) {
  Scheme_Object *currVal;
  static Scheme_Object *argv[2];
  static Scheme_Object *reallyBigNum;
  static Scheme_Object *reallySmallNum;
  static Scheme_Object *greaterThan;
  static Scheme_Object *lessThan;
  static BOOL init;

  if (init == FALSE) {
    greaterThan = scheme_lookup_global(scheme_intern_symbol("#%>"),
				       scheme_get_env(scheme_config));
    lessThan = scheme_lookup_global(scheme_intern_symbol("#%<"),
				    scheme_get_env(scheme_config));
    reallyBigNum = scheme_read_bignum("9223372036854775807",0,10);
    reallySmallNum = scheme_read_bignum("-9223372036854775808",0,10);
    init = TRUE;
  }

  currVal = obj;

  if (SCHEME_INTP(currVal)) {
    buffer[ndx] = SCHEME_INT_VAL(currVal);
  }
  else {

    argv[0] = currVal;
    argv[1] = reallyBigNum;

    if (scheme_apply(greaterThan,2,argv) == scheme_true) {
      scheme_signal_error("write-buffer: number too big");
    }

    argv[1] = reallySmallNum;

    if (scheme_apply(lessThan,2,argv) == scheme_true) {
      scheme_signal_error("write-buffer: number too small");
    }

    buffer[ndx] = _atoi64(scheme_bignum_to_string(currVal,10));
  }
}
#endif

#if HASINT64
Scheme_Object *readUBigIntVal(SRPUINT64 *buffer,long offset) {
  unsigned lo,hi;
  char bigBuff[25];
  Scheme_Object *bigLo,*bigHi;

  lo = (unsigned)(buffer[offset] & 0xFFFFFFFF);
  hi = (unsigned)((buffer[offset] >> 32) & 0xFFFFFFFF);
  bigLo = scheme_make_bignum_from_unsigned(lo);
  bigHi = scheme_make_bignum_from_unsigned(hi);
  sprintf(bigBuff,"%s%s",
	  scheme_bignum_to_string(bigHi,16),
	  scheme_bignum_to_string(bigLo,16));
  return scheme_read_bignum(bigBuff,0,16);
}

Scheme_Object *readUBigIntBuffer(SRPUINT64 *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(readUBigIntVal(buffer,i),retval);
    }
  }
  else {
    retval = readUBigIntVal(buffer,ndx);
  }

  return retval;
}
#endif

#if HASINT64
SRPUINT64 _atoui64(char *s) {
  SRPUINT64 retval;

  retval = 0;

  while(*s && isdigit(*s)) {
    retval *= 10;
    retval += *s - '0';
    s++;
  }

  return retval;
}

#ifndef WIN32
// Windows has native _atoi64()
SRPINT64 add64(SRPINT64 n1, SRPINT64 n2) { 
  return n1 + n2;
}

SRPINT64 sub64(SRPINT64 n1, SRPINT64 n2) { 
  return n1 - n2;
}

SRPINT64 _atoi64(char *s) {
  SRPINT64 retval;
  SRPINT64 (*f)(SRPINT64,SRPINT64);
  BOOL isNeg;  

  isNeg = *s == '-';

  if (isNeg || *s == '+') {
    f = (isNeg ? sub64 : add64);
    s++;
  }
  else {
    f = add64;
  }

  retval = 0;

  while(*s && isdigit(*s)) {
    retval *= 10;
    retval = (*f)(retval,*s - '0');
    s++;
  }

  return retval;
}
#endif
#endif

#if HASINT64
void writeUBigIntBuffer(SRPUINT64 *buffer,Scheme_Object *obj,long ndx) {
  Scheme_Object *currVal;
  static Scheme_Object *argv[2];
  static Scheme_Object *reallyBigNum;
  static Scheme_Object *greaterThan;
  static Scheme_Object *lessThan;
  static Scheme_Object *zero;
  static BOOL init;

  if (init == FALSE) {
    reallyBigNum = scheme_read_bignum("FFFFFFFFFFFFFFFF",0,16);
    greaterThan = scheme_lookup_global(scheme_intern_symbol("#%>"),
				       scheme_get_env(scheme_config));
    lessThan = scheme_lookup_global(scheme_intern_symbol("#%<"),
				    scheme_get_env(scheme_config));
    zero = scheme_make_integer(0);

    init = TRUE;
  }

  currVal = obj;

  if (SCHEME_INTP(currVal)) {
    buffer[ndx] = SCHEME_INT_VAL(currVal);
    }
  else {

    argv[0] = currVal;
    argv[1] = reallyBigNum;

    if (scheme_apply(greaterThan,2,argv) == scheme_true) {
      scheme_signal_error("write-buffer: number too big");
    }
      
    argv[1] = zero;

    if (scheme_apply(lessThan,2,argv) == scheme_true) {
      scheme_signal_error("write-buffer: number too small");
    }

    buffer[ndx] = _atoui64(scheme_bignum_to_string(currVal,10));
  }
}
#endif

Scheme_Object *readTinyBuffer(char *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_integer(buffer[i]),retval);
    }
  }
  else {
    retval = scheme_make_integer(buffer[ndx]);
  }

  return retval;
}

void writeTinyBuffer(char *buffer,Scheme_Object *obj,long ndx) {
  Scheme_Object *currVal;

  currVal = obj;

  if (isCharInt(currVal) == FALSE) {
    scheme_signal_error("write-buffer: number too big");
  } 
  
  buffer[ndx] = (char)SCHEME_INT_VAL(currVal);
}

Scheme_Object *readUTinyBuffer(unsigned char *buffer,long arrayLength,long ndx) {
  Scheme_Object *retval;
  long i;

  if (ndx == WHOLE_BUFFER) {
    retval = scheme_null;

    for (i = arrayLength - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(buffer[i]),retval);
    }
  }
  else {
    retval = scheme_make_integer_value_from_unsigned(buffer[ndx]);
  }

  return retval;
}

void writeUTinyBuffer(unsigned char *buffer,Scheme_Object *obj,long ndx) {
  Scheme_Object *currVal;

  currVal = obj;

  if (isUnsignedCharInt(currVal) == FALSE) {
    scheme_signal_error("write-buffer: number too big");
  } 

  buffer[ndx] = (unsigned char)SCHEME_INT_VAL(currVal);
}
