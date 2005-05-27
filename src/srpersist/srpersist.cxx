/* srpersist.cxx */

#ifndef ODBCVER
#error Must define ODBCVER when compiling
#endif

#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#if !defined(WIN32) && !defined(__MAC_OS__)
/* must be some UNIX flavor */
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
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
typedef short unsigned int * LPWSTR;
typedef char CHAR;
typedef char * GUID;
typedef short unsigned int WCHAR;
#endif

/* 
   Microsoft ODBC SDK include files 
   obtainable as part of Data Access SDK
   at http://www.microsoft.com/data/
*/

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#ifndef NO_SQLUCODE
#include <sqlucode.h>
#endif

#include "escheme.h"
#include "schvers.h"

#include "srptypes.h"
#include "srpbuffer.h"
#include "srpersist.h"

#include "srpprims.tbl"
#include "srpstructs.tbl"
#include "srpexns.tbl"
#include "srpconsts.tbl"
#include "srpinfo.tbl"
#include "srpbitmask.tbl"

static SRP_BUFFER_TBL_ENTRY *bufferTable[BUFFER_TBL_SIZE];
static Scheme_Object *srp_name;
static char *srp_name_string = "srpmain";

/* NOTE

   When we wish to return a Scheme string, and a string length
   is available from ODBC, we use scheme_make_sized_string.
   Drivers need not return NULL-terminated strings.

*/

#ifndef WIN32
char *strlwr(char *s) {
  char *p;

  p = s;

  while (*p) {
    *p++ = tolower(*p);
  }

  return p;
}

int stricmp(char *s1,char *s2) {
  char *buff1 = (char *)scheme_malloc(strlen(s1) + 1);
  char *buff2 = (char *)scheme_malloc(strlen(s2) + 1);

  strcpy(buff1,s1);
  strcpy(buff2,s2);

  strlwr(buff1);
  strlwr(buff2);

  return strcmp(buff1,buff2);
}  
#endif

char *intToHexString(int n) {
  static char buff[20];

  sprintf(buff,"0x%X",n);

  return buff;
}

int keyConstCmp(char *s,SRP_NAMED_CONSTANT *p) {
  return stricmp(s,p->scheme_name);
}

int keySmallConstCmp(char *s,SRP_NAMED_SMALL_CONSTANT *p) {
  return stricmp(s,p->scheme_name);
} 

int keyTypedConstCmp(char *s,SRP_NAMED_TYPED_CONSTANT *p) {
  return stricmp(s,p->scheme_name);
}

int keyBitsDictCmp(char *s,SRP_NAMED_BITS_DICT *p) {
  return stricmp(s,p->scheme_name);
}

int namedBitsDictCmp(SRP_NAMED_BITS_DICT *p1,SRP_NAMED_BITS_DICT *p2) {
  return stricmp(p1->scheme_name,p2->scheme_name);
}

char *findBitByValueInDict(SQLUINTEGER value,SRP_NAMED_CONSTANT *entry,size_t numBits) {
  size_t i;

  for (i = 0; i < numBits; i++,entry++) {
    if ((SQLUINTEGER)(entry->val) == value) {
      return entry->scheme_name;
    }
  } 

  return NO_BIT_NAME;
}

SQLUINTEGER findBitByNameInDict(char *intName,SRP_NAMED_CONSTANT *entry,
				size_t numBits) {
  size_t i;
  
  for (i = 0; i < numBits; i++,entry++) {
    if (stricmp(entry->scheme_name,intName) == 0) {
      return entry->val;
    }
  } 

  return (SQLUINTEGER)(-1);  /* appears to be unused in ODBC header files */
}

char *findSmallIntName(char *name,SQLUSMALLINT value,
		       SRP_NAMED_BITS_DICT *dict,size_t dictsize) {
  SRP_NAMED_BITS_DICT *p;

  p = namedBitsDictSearch(name,dict,dictsize);

  if (p == NULL) {
    scheme_signal_error("Unknown constant dictionary entry: %s",name);
  }

  return findBitByValueInDict(value,p->bits,p->numBits);
}

char *findIntegerName(char *name,SQLUINTEGER value,
		      SRP_NAMED_BITS_DICT *dict,size_t dictsize) {
  SRP_NAMED_BITS_DICT *p;

  p = namedBitsDictSearch(name,dict,dictsize);

  if (p == NULL) {
    scheme_signal_error("Unknown constant dictionary entry: %s",name);
  }

  return findBitByValueInDict(value,p->bits,p->numBits);
}

SQLUINTEGER findNamedInteger(char *name,char *intName,
			     SRP_NAMED_BITS_DICT *dict,size_t dictsize) {
  SRP_NAMED_BITS_DICT *p;

  p = namedBitsDictSearch(name,dict,dictsize);

  if (p == NULL) {
    scheme_signal_error("Unknown constant dictionary entry: %s",name);
  }

  return findBitByNameInDict(intName,p->bits,p->numBits);
}

Scheme_Object *bitsListFromBitMask(char *name,SQLUINTEGER bits) {
  Scheme_Object *retval;
  SRP_NAMED_BITS_DICT *p;
  size_t numBits;
  SRP_NAMED_CONSTANT *q;
  size_t i;

  p = namedBitsDictSearch(name,bitMaskTable,sizeray(bitMaskTable));

  if (p == NULL) {
    scheme_signal_error("Unknown constant dictionary entry: %s",name);
  }

  numBits = p->numBits;

  retval = scheme_null;

  for (i = 0, q = p->bits; i < numBits; i++,q++) {
    if (q->val & bits) {
      retval = scheme_make_pair(scheme_intern_symbol(q->scheme_name),
				retval);
    }
  }

  return retval;
}

#if (ODBCVER >= 0x0300)
void SchemeObjectToHandle(Scheme_Object *obj,
			  SQLHANDLE *handle,SQLSMALLINT *handleType) {

  if (SQL_HENVP(obj)) {
    *handle = SQL_HENV_VAL(obj);
    *handleType = SQL_HANDLE_ENV;
  }
  else if (SQL_HDBCP(obj)) {
    *handle = SQL_HDBC_VAL(obj);
    *handleType = SQL_HANDLE_DBC;
  }
  else if (SQL_HSTMTP(obj)) {
    *handle = SQL_HSTMT_VAL(obj);
    *handleType = SQL_HANDLE_STMT;
  }
  else if (SQL_HDESCP(obj)) {
    *handle = SQL_HDESC_VAL(obj);
    *handleType = SQL_HANDLE_DESC;
  }
}
#endif

int sizeofCDataType(SQLSMALLINT type) {
  switch (type) {
  case SQL_C_CHAR :
    return sizeof(unsigned char);
#if (ODBCVER >= 0x0300)
  case SQL_C_WCHAR :
	   return sizeof(wchar_t);
#endif
  case SQL_C_SHORT :
  case SQL_C_SSHORT :
    return sizeof(short int);
  case SQL_C_USHORT :
    return sizeof(unsigned short int);
  case SQL_C_LONG :
  case SQL_C_SLONG :
    return sizeof(long int);
  case SQL_C_ULONG :
    /* SQL_C_BOOKMARK is same value */
    return sizeof(unsigned long int);
  case SQL_C_FLOAT :
    return sizeof(float);
  case SQL_C_DOUBLE :
    return sizeof(double);
  case SQL_C_BIT :
    return sizeof(unsigned char);
  case SQL_C_TINYINT :
  case SQL_C_STINYINT :
    return sizeof(signed char);
  case SQL_C_UTINYINT :
    return sizeof(unsigned char);
  case SQL_C_DATE :
    return sizeof(DATE_STRUCT);
  case SQL_C_TIME :
    return sizeof(TIME_STRUCT);
  case SQL_C_TIMESTAMP :
    return sizeof(TIMESTAMP_STRUCT);

#if HASINT64
#if (ODBCVER >= 0x0300)
  case SQL_C_SBIGINT :
    return sizeof(SRPINT64);
  case SQL_C_UBIGINT :
    return sizeof(SRPUINT64);
#endif
#endif

  case SQL_C_BINARY :
    /* SQL_C_VARBOOKMARK has same value */
    return sizeof(unsigned char *);

#if (ODBCVER >= 0x0300)
  case SQL_C_TYPE_DATE :
    return sizeof(DATE_STRUCT);
  case SQL_C_TYPE_TIME :
    return sizeof(TIME_STRUCT);
  case SQL_C_TYPE_TIMESTAMP :
    return sizeof(TIMESTAMP_STRUCT);
  case SQL_C_NUMERIC :
    return sizeof(SQL_NUMERIC_STRUCT);
  case SQL_C_INTERVAL_YEAR :
  case SQL_C_INTERVAL_MONTH :
  case SQL_C_INTERVAL_DAY :
  case SQL_C_INTERVAL_HOUR :
  case SQL_C_INTERVAL_MINUTE :
  case SQL_C_INTERVAL_SECOND :
  case SQL_C_INTERVAL_YEAR_TO_MONTH :
  case SQL_C_INTERVAL_DAY_TO_HOUR :
  case SQL_C_INTERVAL_DAY_TO_MINUTE :
  case SQL_C_INTERVAL_DAY_TO_SECOND :
  case SQL_C_INTERVAL_HOUR_TO_MINUTE :
  case SQL_C_INTERVAL_HOUR_TO_SECOND :
  case SQL_C_INTERVAL_MINUTE_TO_SECOND :
    return sizeof(SQL_INTERVAL_STRUCT);
#endif
#if ODBCVER >= 0x0350
  case SQL_C_GUID :
    return sizeof(SQLGUID);
#endif
  }
  
  scheme_signal_error("Unknown C data type constant: %s",intToHexString((int)type));

  return 0;  /* unreachable */
}

unsigned short getHashValue(void *address) {
  return ((unsigned short)(unsigned int)address >> 4) % BUFFER_TBL_SIZE;
}

void addToBufferTable(void *address,SRP_SQL_BUFFER *buffer) {
  unsigned short hashVal;
  SRP_BUFFER_TBL_ENTRY *pEntry,*p;

  pEntry = (SRP_BUFFER_TBL_ENTRY *)scheme_malloc(sizeof(SRP_BUFFER_TBL_ENTRY));
  scheme_dont_gc_ptr(pEntry);
  pEntry->address = address;
  pEntry->buffer = buffer;
  
  hashVal = getHashValue(address);
  
  p = bufferTable[hashVal];
  
  if (p == NULL) {
    bufferTable[hashVal] = pEntry;
  }
  else {
    while (p->next != NULL) {
      p = p->next;
    }
    p->next = pEntry; 
  }
}

void removeFromBufferTable(SRP_SQL_BUFFER *buffer) {
  unsigned short hashVal;
  SRP_BUFFER_TBL_ENTRY *p,*q;
  
  hashVal = getHashValue(buffer->storage);
  
  p = bufferTable[hashVal];

  if (p == NULL) {
    return;
  }

  if (p->buffer == buffer) {
    bufferTable[hashVal] = p->next;
    scheme_gc_ptr_ok(p); 
  }

  q = p;
  p = p->next;

  while (p) {
    if (p->buffer == buffer) {
      q->next = p->next;
      scheme_gc_ptr_ok(p);
      return;
    }  
    q = p;
    p = p->next; 
  }
}

SRP_SQL_BUFFER *lookupBufferFromAddress(void *address) {
  unsigned short hashVal;
  SRP_BUFFER_TBL_ENTRY *p;

  hashVal = getHashValue(address);
  
  p = bufferTable[hashVal];
  
  while (p) {
    if (p->address == address) {
      return p->buffer;
    }
    p = p->next;
  }

  return NULL;
}

char *rowStatusToString(SQLUSMALLINT rowStatus) {
  switch (rowStatus) {

  case SQL_ROW_DELETED :
    return "sql-row-deleted";

  case SQL_ROW_ERROR :
    return "sql-row-error";

  case SQL_ROW_SUCCESS :
    return "sql-row-success";

  case SQL_ROW_UPDATED :
    return "sql-row-updated";
  }

  return "?";
}

/* utilities */

Scheme_Object *srp_make_length(int argc,Scheme_Object **argv) {
  SQLINTEGER len;
  SRP_SQL_LENGTH *retval;

  if (argc == 1) {
    if (SCHEME_EXACT_INTEGERP(argv[0]) == FALSE) {
      scheme_wrong_type("make-length","exact integer",0,argc,argv);
    }
    else if (scheme_get_int_val(argv[0],&len) == 0) {
      scheme_signal_error("Too-large length");
    }
  }
  else {
    len = 0;
  }

  retval = (SRP_SQL_LENGTH *)scheme_malloc(sizeof(SRP_SQL_LENGTH));
  scheme_dont_gc_ptr(retval);

  retval->type = sql_length_type; 
  retval->value = len;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_free_length(int argc,Scheme_Object **argv) {
  if (SQL_LENGTHP(argv[0]) == FALSE) {
    scheme_wrong_type("free-length","<sql-length>",0,argc,argv);
  } 

  scheme_gc_ptr_ok(argv[0]);

  return scheme_void;
}


Scheme_Object *srp_make_indicator(int argc,Scheme_Object **argv) {
  SRP_SQL_INDICATOR *retval;
  long size;

  if (argc == 1) {
    if (SCHEME_EXACT_INTEGERP(argv[0]) == FALSE) {
      scheme_wrong_type("make-indicator","positive exact integer",0,argc,argv);
    }

    if (scheme_get_int_val(argv[0],&size) == 0) {
      scheme_signal_error("make-indicator: size argument too big = %V",argv[0]);
    }

    if (size <= 0) {
      scheme_signal_error("make-indicator: size argument is 0, expected positive exact number");
    }
  }
  else {
    size = 1;
  }

  retval = (SRP_SQL_INDICATOR *)scheme_malloc(sizeof(SRP_SQL_INDICATOR));
  scheme_dont_gc_ptr(retval);

  retval->type = sql_indicator_type; 
  retval->arrayLength = size;
  retval->storage = (SQLINTEGER *)scheme_malloc(sizeof(SQLINTEGER) * size);
  memset(retval->storage,0,sizeof(SQLINTEGER) * size); /* redundant? */

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_free_indicator(int argc,Scheme_Object **argv) {
  if (SQL_INDICATORP(argv[0]) == FALSE) {
    scheme_wrong_type("free-indicator","<sql-indicator>",0,argc,argv);
  } 

  scheme_gc_ptr_ok(argv[0]);

  return scheme_void;
}

Scheme_Object *srp_read_op_parms(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("read-op-parms");
#else
  SQLUINTEGER i;
  SQLUSMALLINT *values;
  Scheme_Object *retval,*symbol;
  
  if (SQL_OP_PARMSP(argv[0]) == FALSE) {
    scheme_wrong_type("read-op-parms","sql-op-parms",0,argc,argv);
  }

  values = SQL_OP_PARMS_VAL(argv[0]);

  if (values == NULL) {
    return scheme_null;
  }

  retval = scheme_null;

  for (i = SQL_OP_PARMS_LEN(argv[0]) - 1; i >= 0; i--) {
    switch(values[i]) {
    case SQL_PARAM_PROCEED :
      symbol = scheme_intern_symbol("sql-param-proceed");
      break;
    case SQL_PARAM_IGNORE :
      symbol = scheme_intern_symbol("sql-param-ignore");
      break;
    default :
      scheme_signal_error("read-op-parms: unknown operation parameter: %s",
			  intToHexString((unsigned int)(values[i])));
    }
    retval = scheme_make_pair(symbol,retval);
  }

  return retval;
#endif
}

Scheme_Object *srp_make_boxed_uint(int argc,Scheme_Object **argv) {
  SQLUINTEGER *pint;
  SRP_SQL_BOXED_UINT *retval;

  if (SCHEME_EXACT_INTEGERP(argv[0]) == FALSE) {
    scheme_wrong_type("make-boxed-uint","exact nonnegative number",0,argc,argv);
  }

  pint = (SQLUINTEGER *)scheme_malloc(sizeof(SQLUINTEGER));

  if (scheme_get_unsigned_int_val(argv[0],pint) == 0) {
    scheme_signal_error("make-boxed-int: number too large");
  }

  retval = (SRP_SQL_BOXED_UINT *)scheme_malloc(sizeof(SRP_SQL_BOXED_UINT));
  scheme_dont_gc_ptr((Scheme_Object *)pint);

  retval->type = sql_boxed_uint_type;
  retval->pointer = pint;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_free_boxed_uint(int argc,Scheme_Object **argv) {
  if (SQL_BOXED_UINTP(argv[0]) == FALSE) {
    scheme_wrong_type("free-boxed-uint!","sql-boxed-uint",0,argc,argv);
  }

  scheme_gc_ptr_ok(argv[0]);

  return scheme_void;
}

Scheme_Object *srp_read_boxed_uint(int argc,Scheme_Object **argv) {
  if (SQL_BOXED_UINTP(argv[0]) == FALSE) {
    scheme_wrong_type("read-boxed-uint","sql-boxed-uint",0,argc,argv);
  }

  return scheme_make_integer_value_from_unsigned(*(SQL_BOXED_UINT_VAL(argv[0])));
}

Scheme_Object *make_one_indicator(SQLINTEGER value) {
  Scheme_Object *retval;

  switch(value) {
  case SQL_NO_TOTAL :
    return scheme_intern_symbol("sql-no-total");
  case SQL_NULL_DATA :
    return scheme_intern_symbol("sql-null-data");
  case SQL_NTS :
    return scheme_intern_symbol("sql-nts");
#if (ODBCVER >= 0x0300)
  case SQL_COLUMN_IGNORE :
    return scheme_intern_symbol("sql-column-ignore");
#endif
  case SQL_DATA_AT_EXEC :
    return scheme_intern_symbol("sql-data-at-exec");
  }

  retval = scheme_make_integer_value(value);

  if (value <= SQL_LEN_DATA_AT_EXEC_OFFSET) {
    return scheme_make_pair(scheme_intern_symbol("sql-len-data-at-exec"),
			    scheme_make_pair(retval,scheme_null));
  }

  return retval;
}

Scheme_Object *srp_read_indicator(int argc,Scheme_Object **argv) {
  SQLINTEGER *storage;
  Scheme_Object *retval;
  long len;
  long offset;
  long i;

  if (SQL_INDICATORP(argv[0]) == FALSE) {
    scheme_wrong_type("read-indicator","sql-indicator",0,argc,argv);
  }

  storage = SQL_INDICATOR_VAL(argv[0]);

  if (argc == 2) { /* return one indicator */
    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
      scheme_wrong_type("read-indicator","nonnegative exact integer",1,
			argc,argv);
    }

    if (scheme_get_int_val(argv[1],&offset) == 0) {
      scheme_signal_error("read-indicator: offset too big = %V",argv[1]);
    }

    if (offset < 0 || offset >= SQL_INDICATOR_LEN(argv[0])) {
      scheme_signal_error("read-indicator: offset %d out of range [0..%d]",
			  offset,SQL_INDICATOR_LEN(argv[0]) - 1);
    }

    return make_one_indicator(storage[offset]);
  }

  /* return list of indicators */

  retval = scheme_null;

  len = SQL_INDICATOR_LEN(argv[0]);

  for (i = len - 1; i >= 0; i--) {
    retval = scheme_make_pair(make_one_indicator(storage[i]),retval);
  } 

  return retval;
}

Scheme_Object *srp_read_length(int argc,Scheme_Object **argv) {
  if (SQL_LENGTHP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-read-length","sql-length",0,argc,argv);
  }

  return scheme_make_integer_value(SQL_LENGTH_VAL(argv[0]));
}

Scheme_Object *srp_set_indicator(int argc,Scheme_Object **argv) {
  char *lenString;
  SQLINTEGER *storage;
  long offset;
  int execVal;

  if (SQL_INDICATORP(argv[0]) == FALSE) {
    scheme_wrong_type("set-indicator!","sql-indicator",0,argc,argv);
  }

  storage = SQL_INDICATOR_VAL(argv[0]);

  if (SCHEME_INTP(argv[1]) == FALSE && 
      SCHEME_SYMBOLP(argv[1]) == FALSE &&  SCHEME_PAIRP(argv[1]) == FALSE) {
    scheme_wrong_type("set-indicator!","integer or symbol or pair",1,argc,argv);
  }

  if (argc == 3) { /* explicit offset */

    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) {
      scheme_wrong_type("set-indicator!","nonnegative exact integer",2,
			argc,argv);
    }

    if (scheme_get_int_val(argv[2],&offset) == 0) {
      scheme_signal_error("set-indicator!: offset too big = %V",argv[2]);
    }

    if (offset < 0 || offset >= SQL_INDICATOR_LEN(argv[0])) {
      scheme_signal_error("set-indicator!: offset %d out of range [0..%d]",
			  offset,SQL_INDICATOR_LEN(argv[0]) - 1);
    }
  }
  else {
    offset = 0;
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    lenString = SCHEME_SYM_VAL(argv[1]);

    if (stricmp(lenString,"sql-nts") == 0) {
      storage[offset] = SQL_NTS;
    }
    else if (stricmp(lenString,"sql-null-data") == 0) {
      storage[offset] = SQL_NULL_DATA;
    }
#if (ODBCVER >= 0x0300)
    else if (stricmp(lenString,"sql-column-ignore") == 0) {
      storage[offset] = SQL_COLUMN_IGNORE;
    }
#endif
    else if (stricmp(lenString,"sql-data-at-exec") == 0) {
      storage[offset] = SQL_DATA_AT_EXEC;
    }
    else {
      scheme_signal_error("set-indicator!: "
			  "unknown indicator value %s",
			  lenString);
    }

  } /* should be len-data-at-exec pair */
  else if (SCHEME_PAIRP(argv[1])) {
    Scheme_Object *car,*cadr;

    car = SCHEME_CAR(argv[1]);
    cadr = SCHEME_CADR(argv[1]);

    if (SCHEME_SYMBOLP(car) == FALSE ||
	stricmp(SCHEME_SYM_VAL(car),"sql-len-data-at-exec")) {
      scheme_signal_error("set-indicator!: first element of pair argument must be 'sql-len-data-at-exec, got: %V",car);
    }
    
    if (SCHEME_INTP(cadr) == FALSE) {
      scheme_signal_error("set-indicator!: second element of pair argument must be integer, got: %V",cadr);
    }

    /* argument to len-data-at-exec */
    execVal = SCHEME_INT_VAL(cadr);

    storage[offset] = SQL_LEN_DATA_AT_EXEC(execVal);
  }
  else { /* must be integer */
    storage[offset] = SCHEME_INT_VAL(argv[1]);
  }

  return scheme_void;
}


Scheme_Object *srp_make_row_status(int argc,Scheme_Object **argv) {
  SQLUINTEGER numRows;
  SRP_SQL_ROW_STATUS *retval;

  if (argc == 1) {
    if (SCHEME_EXACT_INTEGERP(argv[0]) == FALSE ||
	scheme_get_unsigned_int_val(argv[0],&numRows) == 0 ||
	numRows == 0) {
      scheme_wrong_type("make-row-status","positive exact integer",0,argc,argv);
    }
  }
  else {
    numRows = 1;
  }

  retval = (SRP_SQL_ROW_STATUS *)scheme_malloc(sizeof(SRP_SQL_ROW_STATUS));
  retval->type = sql_row_status_type;
  retval->values = (SQLUSMALLINT *)scheme_malloc(numRows * sizeof(SQLUSMALLINT));
  scheme_dont_gc_ptr(retval->values);
  retval->usesSchemeStorage = TRUE;
  retval->numRows = numRows;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_read_row_status(int argc,Scheme_Object **argv) {
  SQLUSMALLINT *values;
  SQLUINTEGER numRows;
  Scheme_Object *retval;
  Scheme_Object *symbol;
  long i;

  if (SQL_ROW_STATUSP(argv[0]) == FALSE) {
    scheme_wrong_type("read-row-status","sql-row-status",0,argc,argv);
  }

  values = SQL_ROW_STATUS_VAL(argv[0]);
  numRows = SQL_ROW_STATUS_LEN(argv[0]);

  if (argc == 1) { /* whole array */  
    retval = scheme_null;
    for (i = numRows-1; i >= 0; i--) {
      symbol = scheme_intern_symbol(rowStatusToString(values[i]));
      retval = scheme_make_pair(symbol,retval);
    }
  }
  else {
    long ndx;

    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
      scheme_wrong_type("read-row-status","nonnegative exact integer",1,argc,argv);
    }

    if (scheme_get_int_val(argv[1],&ndx) == 0 || ndx < 0 || ndx >= (long)numRows) {
      scheme_signal_error("read-row-status: index argument (%V) outside range [0..%d]",
			  argv[1],numRows-1);
    }
    
    retval = scheme_intern_symbol(rowStatusToString(values[ndx]));
  }

  return retval;
}

Scheme_Object *srp_free_row_status(int argc,Scheme_Object **argv) {
  SRP_SQL_ROW_STATUS *p;

  if (SQL_ROW_STATUSP(argv[0]) == FALSE) {
    scheme_wrong_type("free-row-status","sql-row-status",0,argc,argv);
  }

  p = (SRP_SQL_ROW_STATUS *)(argv[0]);

  if (p->usesSchemeStorage) {
    scheme_gc_ptr_ok(SQL_ROW_STATUS_VAL(p));
  }
  scheme_gc_ptr_ok(argv[0]);

  return scheme_void;
}

#if (ODBCVER >= 0x0300)
char *APDArrayStatusToString(SQLUSMALLINT status) {
  switch(status) {
  case SQL_PARAM_PROCEED :
    return "sql-param-proceed";
  case SQL_PARAM_IGNORE :
    return "sql-param-ignore";
  default :
    scheme_signal_error("Unknown array status (%s) for APD descriptor",intToHexString(status));
  }
  return NULL;
}
#endif

#if (ODBCVER >= 0x0300)
char *IPDArrayStatusToString(SQLUSMALLINT status) {
  switch(status) {
  case SQL_PARAM_SUCCESS :
    return "sql-param-success";
  case SQL_PARAM_SUCCESS_WITH_INFO :
    return "sql-param-success-with-info";
  case SQL_PARAM_ERROR :
    return "sql-param-error";
  case SQL_PARAM_UNUSED :
    return "sql-param-unused";
  case SQL_PARAM_DIAG_UNAVAILABLE :
    return "sql-param-unavailable";
  default :
    scheme_signal_error("Unknown IPD array status (%s)",intToHexString(status));
  }
  return NULL;
}
#endif

#if (ODBCVER >= 0x0300)
char *ARDArrayStatusToString(SQLUSMALLINT status) {
  switch(status) {
  case SQL_ROW_PROCEED :
    return "sql-row-proceed";
  case SQL_ROW_IGNORE :
    return "sql-row-ignore";
  default :
    scheme_signal_error("Unknown ARD array status (%s)",intToHexString(status));
  }
  return NULL;
}
#endif

#if (ODBCVER >= 0x0300)
char *IRDArrayStatusToString(SQLUSMALLINT status) {
  switch(status) {
  case SQL_ROW_SUCCESS :
    return "sql-row-success";
  case SQL_ROW_SUCCESS_WITH_INFO :
    return "sql-row-success-with-info";
  case SQL_ROW_ERROR :
    return "sql-row-error";
  case SQL_ROW_UPDATED :
    return "sql-row-updated";
  case SQL_ROW_DELETED :
    return "sql-row-deleted";
  case SQL_ROW_ADDED :
    return "sql-row-added";
  case SQL_ROW_NOROW :
    return "sql-row-norow";
  default :
    scheme_signal_error("Unknown IRD array status (%s)",intToHexString(status));
  }
  return NULL;
}
#endif

#if (ODBCVER >= 0x0300)
Scheme_Object *srp_read_array_status(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLUSMALLINT *values;
  DESCTYPE descType;
  SQLHDESC hdesc;
  SQLUINTEGER arraySize;
  SQLSMALLINT descCount;
  SQLUINTEGER *pRowsCount;
  SQLINTEGER actualLen;
  Scheme_Object *retval;
  long i;

  if (SQL_ARRAY_STATUSP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-read-array-status","sql-array-status",0,argc,argv);
  }

  values = SQL_ARRAY_STATUS_VAL(argv[0]);
  descType = SQL_ARRAY_STATUS_DESCTYPE(argv[0]);
  hdesc = SQL_ARRAY_STATUS_HDESC(argv[0]);

  if (values == NULL) {
    return scheme_null;
  }

  retval = scheme_null;

  switch(descType) {
  case APD :

    sr = SQLGetDescField(hdesc,1,SQL_DESC_COUNT,
			 &descCount,sizeof(descCount),&actualLen);

    if (sr != SQL_SUCCESS) {
      scheme_signal_error("Can't get array size for APD descriptor");
    }

    retval = scheme_null;

    for (i = descCount - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_intern_symbol(APDArrayStatusToString(values[i])),
				retval);
    }

    return retval;

  case ARD :

    sr = SQLGetDescField(hdesc,1,SQL_DESC_COUNT,
			 &arraySize,sizeof(arraySize),&actualLen);

    if (sr != SQL_SUCCESS) {
      scheme_signal_error("Can't get rowset size for ARD descriptor");
    }

    retval = scheme_null;

    for (i = arraySize - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_intern_symbol(ARDArrayStatusToString(values[i])),
				retval);
    }

    return retval;

  case IPD :

    sr = SQLGetDescField(hdesc,1,SQL_DESC_COUNT,
			 &descCount,sizeof(descCount),&actualLen);

    if (sr != SQL_SUCCESS) {
      scheme_signal_error("Can't get parameter count for IPD descriptor");
    }

    retval = scheme_null;

    for (i = descCount - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_intern_symbol(IPDArrayStatusToString(values[i])),
				retval);
    }

    return retval;

  case IRD :

    sr = SQLGetDescField(hdesc,1,SQL_DESC_ROWS_PROCESSED_PTR,
			 &pRowsCount,sizeof(pRowsCount),&actualLen);

    if (sr != SQL_SUCCESS) {
      scheme_signal_error("Can't get rowset size for IRD descriptor");
    }

    retval = scheme_null;

    for (i = *pRowsCount - 1; i >= 0; i--) {
      retval = scheme_make_pair(scheme_intern_symbol(IRDArrayStatusToString(values[i])),
				retval);
    }

    return retval;

  case EXPLICIT :

    scheme_signal_error("Can't get array status for explicitly-allocated descriptor");
  } 

  return retval;

}
#endif

Scheme_Object *srp_make_buffer(int argc,Scheme_Object **argv) {
  SRP_SQL_BUFFER *retval;
  char *typeName;
  SRP_NAMED_SMALL_CONSTANT *p;
  long width;
  long arrayLength;

  if (SCHEME_SYMBOLP(argv[0]) == FALSE && 
      SCHEME_PAIRP(argv[0]) == FALSE) {
    scheme_wrong_type("make-buffer","symbol or pair",0,argc,argv);
  }

  if (argc == 2) {
    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
      scheme_wrong_type("make-buffer","positive exact integer",1,argc,argv);
    }
    if (scheme_get_int_val(argv[1],&arrayLength) == 0) {
      scheme_signal_error("make-buffer: number of elements too large");
    }
  }
  else {
    arrayLength = 1;
  }

  if (SCHEME_SYMBOLP(argv[0])) {
    typeName = SCHEME_SYM_VAL(argv[0]);
    width = 1;
  }
  else { /* should be pair of char-type, width */
    Scheme_Object *car,*cadr;

    car = SCHEME_CAR(argv[0]);
    cadr = SCHEME_CADR(argv[0]);

    if (SCHEME_SYMBOLP(car) == FALSE ||
	SCHEME_EXACT_INTEGERP(cadr) == FALSE) {
      scheme_wrong_type("make-buffer","symbol, exact integer pair",0,argc,argv);
    }
    
    typeName = SCHEME_SYM_VAL(car);

    if (stricmp(typeName,"sql-c-char") && 
	stricmp(typeName,"sql-c-wchar")) {
      scheme_signal_error("make-buffer: invalid character C data type \"%s\"",
			  typeName);
    }

    if (scheme_get_int_val(cadr,&width) == 0) {
      scheme_signal_error("make-buffer: requested width of character buffer too big");
    }

    if (width <= 0) {
      scheme_signal_error("make-buffer: requested width of character buffer too small");
    }
  }    

  p = namedSmallConstSearch(typeName,CDataTypes);
  
  if (p == NULL) {
    scheme_signal_error("make-buffer: invalid C type: %s",typeName);
  }

  retval = (SRP_SQL_BUFFER *)scheme_malloc(sizeof(SRP_SQL_BUFFER));
  scheme_dont_gc_ptr(retval);  

  retval->type = sql_buffer_type;

  retval->width = width;
  retval->arrayLength = arrayLength;

  retval->CDataType = (SQLSMALLINT)(p->val);

  retval->eltSize = sizeofCDataType(retval->CDataType);

  /* buffers might be relinquished by Scheme, 
     but still bound to OBDC columns
     so make actual storage uncollectable for now */

  retval->storage = scheme_malloc(retval->width * retval->arrayLength * sizeof(retval->eltSize));
  scheme_dont_gc_ptr(retval->storage);  

  /* need to be able to recover <sql-buffer> from storage address
     for use by SQLParamData() */

  addToBufferTable(retval->storage,retval);

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_free_buffer(int argc,Scheme_Object **argv) {
  SRP_SQL_BUFFER *buff;

  if (SQL_BUFFERP(argv[0]) == FALSE) {
    scheme_wrong_type("free-buffer","<sql-buffer>",0,argc,argv);
  } 

  buff = (SRP_SQL_BUFFER *)(argv[0]);

  removeFromBufferTable(buff);

  scheme_gc_ptr_ok(buff->storage);
  scheme_gc_ptr_ok(buff);

  return scheme_void;
}

Scheme_Object *srp_read_buffer(int argc,Scheme_Object **argv) {
  SQLSMALLINT CDataType;
  void *buffer;
  long width;
  long arrayLength;
  BOOL isCharType;
  long ndx;

  if (SQL_BUFFERP(argv[0]) == FALSE) {
    scheme_wrong_type("read-buffer","<sql-buffer>",0,argc,argv);
  } 

  if (argc == 2) {
    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE ||
	SCHEME_INT_VAL(argv[1]) < 0) {
      scheme_wrong_type("read-buffer","nonnegative exact integer",1,argc,argv);
    }
    ndx = SCHEME_INT_VAL(argv[1]);
  }
  else {
    ndx = WHOLE_BUFFER;
  }

  CDataType = SQL_BUFFER_CTYPE(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[0]);

#if (ODBCVER >= 0x0300)
  isCharType = (CDataType == SQL_C_CHAR || CDataType == SQL_C_WCHAR);
#else
  isCharType = (CDataType == SQL_C_CHAR);
#endif 

  width = SQL_BUFFER_WIDTH(argv[0]);
  arrayLength = SQL_BUFFER_ARRAYLEN(argv[0]);

  if (ndx != WHOLE_BUFFER && ndx >= arrayLength) {
    scheme_signal_error("read-buffer: index = %d outside buffer range = [0..%d]",
			ndx,arrayLength-1);
  }

  switch(CDataType) {
  case SQL_C_CHAR :
    return readCharBuffer((char *)buffer,width,arrayLength,ndx);
#if (ODBCVER >= 0x0300)
  case SQL_C_WCHAR :
    return readWideCharBuffer((wchar_t *)buffer,width,arrayLength,ndx);
#endif
  case SQL_C_SLONG :
  case SQL_C_LONG :
    return readLongBuffer((long *)buffer,arrayLength,ndx);
  case SQL_C_ULONG :
    /* SQL_C_BOOKMARK is the same */
    return readULongBuffer((unsigned long *)buffer,arrayLength,ndx);
  case SQL_C_SSHORT :
  case SQL_C_SHORT :
    return readShortBuffer((short *)buffer,arrayLength,ndx);
  case SQL_C_USHORT :
    return readUShortBuffer((unsigned short *)buffer,arrayLength,ndx);
  case SQL_C_FLOAT :
    return readFloatBuffer((float *)buffer,arrayLength,ndx);
  case SQL_C_DOUBLE :
    return readDoubleBuffer((double *)buffer,arrayLength,ndx);
#if (ODBCVER >= 0x0300)
  case SQL_C_NUMERIC :
    return readNumericBuffer((SQL_NUMERIC_STRUCT *)buffer,arrayLength,ndx);
#endif
  case SQL_C_DATE :
#if (ODBCVER >= 0x0300)
  case SQL_C_TYPE_DATE :
    return readDateBuffer((SQL_DATE_STRUCT *)buffer,arrayLength,ndx);
#else
    return readDateBuffer((DATE_STRUCT *)buffer,arrayLength,ndx);
#endif
  case SQL_C_TIME :
#if (ODBCVER >= 0x0300)
  case SQL_C_TYPE_TIME :
    return readTimeBuffer((SQL_TIME_STRUCT *)buffer,arrayLength,ndx);
#else
    return readTimeBuffer((TIME_STRUCT *)buffer,arrayLength,ndx);
#endif
  case SQL_C_TIMESTAMP :
#if (ODBCVER >= 0x0300)
  case SQL_C_TYPE_TIMESTAMP :
    return readTimeStampBuffer((SQL_TIMESTAMP_STRUCT *)buffer,arrayLength,ndx);
#else
    return readTimeStampBuffer((TIMESTAMP_STRUCT *)buffer,arrayLength,ndx);
#endif
#if (ODBCVER >= 0x0300)
  case SQL_C_INTERVAL_YEAR :
    return readIntervalYearBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_MONTH :
    return readIntervalMonthBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_DAY :
    return readIntervalDayBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_HOUR :
    return readIntervalHourBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_MINUTE :
    return readIntervalMinuteBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_SECOND :
    return readIntervalSecondBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_YEAR_TO_MONTH :
    return readIntervalYearMonthBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_DAY_TO_HOUR :
    return readIntervalDayHourBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_DAY_TO_MINUTE :
    return readIntervalDayMinuteBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_DAY_TO_SECOND :
    return readIntervalDaySecondBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_HOUR_TO_MINUTE :
    return readIntervalHourMinuteBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_HOUR_TO_SECOND :
    return readIntervalHourSecondBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
  case SQL_C_INTERVAL_MINUTE_TO_SECOND :
    return readIntervalMinuteSecondBuffer((SQL_INTERVAL_STRUCT *)buffer,arrayLength,ndx);
#endif
  case SQL_C_BINARY :
    /* SQL_C_VARBOOKMARK is the same */
    return readBinaryBuffer((char *)buffer,arrayLength,ndx);
  case SQL_C_BIT :
    return readBitBuffer((unsigned char *)buffer,arrayLength,ndx);

#if HASINT64
#if (ODBCVER >= 0x0300)
  case SQL_C_SBIGINT :
    return readBigIntBuffer((SRPINT64 *)buffer,arrayLength,ndx);
  case SQL_C_UBIGINT :
    return readUBigIntBuffer((SRPUINT64 *)buffer,arrayLength,ndx);
#endif
#endif

  case SQL_C_STINYINT :
  case SQL_C_TINYINT :
    return readTinyBuffer((char *)buffer,arrayLength,ndx);
  case SQL_C_UTINYINT :
    return readUTinyBuffer((unsigned char *)buffer,arrayLength,ndx);
#if (ODBCVER >= 0x0350)
  case SQL_C_GUID :
    return readGuidBuffer((SQLGUID *)buffer,arrayLength,ndx);
#endif
  }

  scheme_signal_error("Unknown buffer C data type: %s",intToHexString(CDataType));

  return scheme_void; /* unreachable */

}

BOOL schemeIntP(Scheme_Object *o) {
  return SCHEME_INTP(o);
}

BOOL schemeExactIntegerP(Scheme_Object *o) {
  return SCHEME_EXACT_INTEGERP(o);
}

BOOL schemeFloatP(Scheme_Object *o) {
  return SCHEME_FLOATP(o);
}

BOOL schemeDoubleP(Scheme_Object *o) {
  return SCHEME_DBLP(o);
}

BOOL schemeNumericP(Scheme_Object *o) {
  return (scheme_is_struct_instance(NUMERIC_STRUCT_TYPE,o));
}

BOOL schemeDateP(Scheme_Object *o) {
  return (scheme_is_struct_instance(DATE_STRUCT_TYPE,o));
}

BOOL schemeTimeP(Scheme_Object *o) {
  return (scheme_is_struct_instance(TIME_STRUCT_TYPE,o));
}

BOOL schemeTimeStampP(Scheme_Object *o) {
  return (scheme_is_struct_instance(TIMESTAMP_STRUCT_TYPE,o));
}

BOOL schemeGuidP(Scheme_Object *o) {
  return (scheme_is_struct_instance(GUID_STRUCT_TYPE,o));
}

#if (ODBCVER >= 0x0300)
void writeIntervalToBuff(void *buffer,Scheme_Object *obj,
			 long arrayLength,long ndx,
			 SQLINTERVAL intervalType,
			 short numFields,
			 Scheme_Object *signProc,
			 Scheme_Object **intProc,
			 INTERVAL_FIELD_ACCESSOR *fieldFromInterval) {
  Scheme_Object *currVal;
  Scheme_Object *currSign,*currInt;
  char *signStr;
  SQL_INTERVAL_STRUCT *pInterval;
  long i;

  currVal = obj;

  pInterval = (SQL_INTERVAL_STRUCT *)buffer + ndx;

  currSign = scheme_apply(signProc,1,&currVal);

  pInterval->interval_type = intervalType;

  signStr = SCHEME_SYM_VAL(currSign);
  pInterval->interval_sign = 
    (*signStr == '+') ? SQL_FALSE : SQL_TRUE;

  for (i = 0; i < numFields; i++) {

    currInt = scheme_apply(intProc[i],1,&currVal);

    /* this depends on sizeof(long) == sizeof(int) */

    if (scheme_get_unsigned_int_val(currInt,fieldFromInterval[i](pInterval)) == 0) {
      scheme_signal_error("write-buffer: interval too big");
    }
  }
}
#endif

Scheme_Object *srp_write_buffer(int argc,Scheme_Object **argv) {
  SQLSMALLINT CDataType;
  void *buffer;
  long arrayLength,width,ndx;
#if (ODBCVER >= 0x0300)
  Scheme_Object *accessors[5];
  INTERVAL_FIELD_ACCESSOR fields[5];
#endif  

  if (SQL_BUFFERP(argv[0]) == FALSE) {
    scheme_wrong_type("write-buffer","sql-buffer",0,argc,argv);
  } 

  if (argc == 3) {
    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE ||
	SCHEME_INT_VAL(argv[2]) < 0)
      scheme_wrong_type("write-buffer","nonnegative exact integer",2,
			argc,argv);
    ndx = SCHEME_INT_VAL(argv[2]);
  }
  else {
    ndx = 0;
  }

  CDataType = SQL_BUFFER_CTYPE(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[0]);
  arrayLength = SQL_BUFFER_ARRAYLEN(argv[0]);

  if (ndx >= arrayLength) {
    scheme_signal_error("write-buffer: index = %d outside buffer range = [0..%d]",
			ndx,arrayLength - 1);

  }

  /* check that data to be written is of appropriate type, 
     then call specialized write routine */

  switch(CDataType) {
  case SQL_C_CHAR :

    if (SCHEME_STRINGP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","string",1,argc,argv);
    }

    width = SQL_BUFFER_WIDTH(argv[0]);
    if (SCHEME_STRLEN_VAL(argv[1]) >= width) {
      scheme_signal_error("write-buffer: string too wide for buffer");
    }

    writeCharBuffer((char *)buffer,argv[1],width,ndx);
    break;

#if (ODBCVER >= 0x0300)
  case SQL_C_WCHAR :

    if (SCHEME_STRINGP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","string",1,argc,argv);
    }

    width = SQL_BUFFER_WIDTH(argv[0]);
    if (SCHEME_STRLEN_VAL(argv[1]) >= width) {
      scheme_signal_error("write-buffer: string too wide for buffer");
    }

    writeWideCharBuffer((wchar_t *)buffer,argv[1],width,ndx);
    break;
#endif

  case SQL_C_SLONG :
  case SQL_C_LONG :

    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","exact integer",1,argc,argv);
    }

    writeLongBuffer((long *)buffer,argv[1],ndx); 

    break;

  case SQL_C_ULONG :

    /* SQL_C_BOOKMARK is the same */

    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","exact integer",1,argc,argv);
    }

    writeULongBuffer((unsigned long *)buffer,argv[1],ndx); 

    break;

  case SQL_C_SSHORT :
  case SQL_C_SHORT :

    if (SCHEME_INTP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","integer",1,argc,argv);
    }

    writeShortBuffer((short *)buffer,argv[1],ndx); 

    break;


  case SQL_C_USHORT :

    if (SCHEME_INTP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","integer",1,argc,argv);
    }

    writeUShortBuffer((unsigned short *)buffer,argv[1],ndx); 

    break;

  case SQL_C_STINYINT :
  case SQL_C_TINYINT :

    if (SCHEME_INTP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","integer",1,argc,argv);
    }

    writeTinyBuffer((char *)buffer,argv[1],ndx); 

  case SQL_C_UTINYINT :

    if (SCHEME_INTP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","integer",1,argc,argv);
    }

    writeUTinyBuffer((unsigned char *)buffer,argv[1],ndx); 

#if HASINT64
#if (ODBCVER >= 0x0300)
  case SQL_C_SBIGINT :

    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","integer",1,argc,argv);
    }

    writeBigIntBuffer((SRPINT64 *)buffer,argv[1],ndx); 

  case SQL_C_UBIGINT :

    if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","integer",1,argc,argv);
    }

    writeUBigIntBuffer((SRPUINT64 *)buffer,argv[1],ndx); 
#endif
#endif

  case SQL_C_FLOAT :

    if (SCHEME_FLOATP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","single-precision flonum",1,argc,argv);
    }

    writeFloatBuffer((float *)buffer,argv[1],ndx); 

    break;

  case SQL_C_DOUBLE :

    if (SCHEME_DBLP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","double-precision flonum",1,argc,argv);
    }

    writeDoubleBuffer((double *)buffer,argv[1],ndx); 

    break;

#if (ODBCVER >= 0x0300)
  case SQL_C_NUMERIC :

    if (scheme_is_struct_instance(NUMERIC_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-numeric",1,argc,argv);
    }

    writeNumericBuffer((SQL_NUMERIC_STRUCT *)buffer,argv[1],ndx); 

    break;
#endif

  case SQL_C_DATE :
#if (ODBCVER >= 0x0300)
  case SQL_C_TYPE_DATE :

    if (scheme_is_struct_instance(DATE_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-date",1,argc,argv);
    }

    writeDateBuffer((SQL_DATE_STRUCT *)buffer,argv[1],ndx); 
#else
    if (scheme_is_struct_instance(DATE_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-date",1,argc,argv);
    }

    writeDateBuffer((DATE_STRUCT *)buffer,argv[1],ndx); 
#endif

    break;

  case SQL_C_TIME :
#if (ODBCVER >= 0x0300)
  case SQL_C_TYPE_TIME :
    if (scheme_is_struct_instance(TIME_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-time",1,argc,argv);
    }
    writeTimeBuffer((SQL_TIME_STRUCT *)buffer,argv[1],ndx); 
#else
    if (scheme_is_struct_instance(TIME_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-time",1,argc,argv);
    }
    writeTimeBuffer((TIME_STRUCT *)buffer,argv[1],ndx); 
#endif

    break;

  case SQL_C_TIMESTAMP :
#if (ODBCVER >= 0x0300)
  case SQL_C_TYPE_TIMESTAMP :
    if (scheme_is_struct_instance(TIMESTAMP_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-timestamp",1,argc,argv);
    }
    writeTimeStampBuffer((SQL_TIMESTAMP_STRUCT *)buffer,argv[1],ndx); 
#else
    if (scheme_is_struct_instance(TIMESTAMP_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-timestamp",1,argc,argv);
    }
    writeTimeStampBuffer((TIMESTAMP_STRUCT *)buffer,argv[1],ndx); 
#endif

    break;

#if (ODBCVER >= 0x0350)
  case SQL_C_GUID :

    if (scheme_is_struct_instance(GUID_STRUCT_TYPE,argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","sql-guid",1,argc,argv);
    }
    writeGuidBuffer((SQLGUID *)buffer,argv[1],ndx); 

    break;
#endif

  case SQL_C_BIT :

    if (SCHEME_STRINGP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","string",1,argc,argv);
    }

    writeBitBuffer((char *)buffer,argv[1],ndx); 

    break;

  case SQL_C_BINARY :
    /* SQL_C_VARBOOKMARK is the same */

    if (SCHEME_STRINGP(argv[1]) == FALSE) {
      scheme_wrong_type("write-buffer","string",1,argc,argv);
    }

    writeBinaryBuffer((char *)buffer,argv[1],ndx); 

    break;


#if (ODBCVER >= 0x0300)
  case SQL_C_INTERVAL_YEAR :

    if (scheme_is_struct_instance(YEAR_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-year",1,argc,argv);
    }

    accessors[0] = YEAR_INTERVAL_YEAR;
    fields[0] = getIntervalYear;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_YEAR,
			1,YEAR_INTERVAL_SIGN,
			accessors,fields);

    break;

  case SQL_C_INTERVAL_MONTH :

    if (scheme_is_struct_instance(MONTH_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-month",1,argc,argv);
    }

    accessors[0] = MONTH_INTERVAL_MONTH;
    fields[0] = getIntervalMonth;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_MONTH,
			1,MONTH_INTERVAL_SIGN,
			accessors,fields);

    break;

  case SQL_C_INTERVAL_DAY :

    if (scheme_is_struct_instance(DAY_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-day",1,argc,argv);
    }

    accessors[0] = DAY_INTERVAL_DAY;
    fields[0] = getIntervalDay;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_DAY,
			1,DAY_INTERVAL_SIGN,
			accessors,fields);

    break;

  case SQL_C_INTERVAL_HOUR :

    if (scheme_is_struct_instance(HOUR_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-hour",1,argc,argv);
    }

    accessors[0] = HOUR_INTERVAL_HOUR;
    fields[0] = getIntervalHour;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_HOUR,
			1,HOUR_INTERVAL_SIGN,
			accessors,fields);

    break;

  case SQL_C_INTERVAL_MINUTE :

    if (scheme_is_struct_instance(MINUTE_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-minute",1,argc,argv);
    }

    accessors[0] = MINUTE_INTERVAL_MINUTE;
    fields[0] = getIntervalMinute;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_MINUTE,
			1,MINUTE_INTERVAL_SIGN,
			accessors,fields);

    break;

  case SQL_C_INTERVAL_SECOND :

    if (scheme_is_struct_instance(SECOND_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-second",1,argc,argv);
    }

    accessors[0] = SECOND_INTERVAL_SECOND;
    fields[0] = getIntervalSecond;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_SECOND,
			1,SECOND_INTERVAL_SIGN,
			accessors,fields);

    break;

  case SQL_C_INTERVAL_YEAR_TO_MONTH :

    if (scheme_is_struct_instance(YEAR_TO_MONTH_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-year-to-month",1,argc,argv);
    }

    accessors[0] = YEAR_TO_MONTH_INTERVAL_YEAR;
    accessors[1] = YEAR_TO_MONTH_INTERVAL_MONTH;

    fields[0] = getIntervalYear;
    fields[1] = getIntervalMonth;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_YEAR_TO_MONTH,
			2,YEAR_TO_MONTH_INTERVAL_SIGN,
			accessors,fields);

    break;


  case SQL_C_INTERVAL_DAY_TO_HOUR :

    if (scheme_is_struct_instance(DAY_TO_HOUR_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-day-to-hour",1,argc,argv);
    }

    accessors[0] = DAY_TO_HOUR_INTERVAL_DAY;
    accessors[1] = DAY_TO_HOUR_INTERVAL_HOUR;

    fields[0] = getIntervalDay;
    fields[1] = getIntervalHour;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_DAY_TO_HOUR,
			2,DAY_TO_HOUR_INTERVAL_SIGN,
			accessors,fields);

    break;


  case SQL_C_INTERVAL_DAY_TO_MINUTE :

    if (scheme_is_struct_instance(DAY_TO_MINUTE_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-day-to-minute",1,argc,argv);
    }

    accessors[0] = DAY_TO_MINUTE_INTERVAL_DAY;
    accessors[1] = DAY_TO_MINUTE_INTERVAL_HOUR;
    accessors[2] = DAY_TO_MINUTE_INTERVAL_MINUTE;

    fields[0] = getIntervalDay;
    fields[1] = getIntervalHour;
    fields[2] = getIntervalMinute;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_DAY_TO_MINUTE,
			3,DAY_TO_MINUTE_INTERVAL_SIGN,
			accessors,fields);
			
    break;

  case SQL_C_INTERVAL_DAY_TO_SECOND :

    if (scheme_is_struct_instance(DAY_TO_SECOND_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-day-to-second",1,argc,argv);
    }

    accessors[0] = DAY_TO_SECOND_INTERVAL_DAY;
    accessors[1] = DAY_TO_SECOND_INTERVAL_HOUR;
    accessors[2] = DAY_TO_SECOND_INTERVAL_MINUTE;
    accessors[3] = DAY_TO_SECOND_INTERVAL_SECOND;

    fields[0] = getIntervalDay;
    fields[1] = getIntervalHour;
    fields[2] = getIntervalMinute;
    fields[3] = getIntervalSecond;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_DAY_TO_SECOND,
			4,DAY_TO_SECOND_INTERVAL_SIGN,
			accessors,fields);

			
  case SQL_C_INTERVAL_HOUR_TO_MINUTE :

    if (scheme_is_struct_instance(HOUR_TO_MINUTE_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-hour-to-minute",1,argc,argv);
    }

    accessors[0] = HOUR_TO_MINUTE_INTERVAL_HOUR;
    accessors[1] = HOUR_TO_MINUTE_INTERVAL_MINUTE;

    fields[0] = getIntervalHour;
    fields[1] = getIntervalMinute;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_HOUR_TO_MINUTE,
			2,HOUR_TO_MINUTE_INTERVAL_SIGN,
			accessors,fields);

    break;


  case SQL_C_INTERVAL_HOUR_TO_SECOND :

    if (scheme_is_struct_instance(HOUR_TO_SECOND_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-hour-to-second",1,argc,argv);
    }

    accessors[0] = HOUR_TO_SECOND_INTERVAL_HOUR;
    accessors[1] = HOUR_TO_SECOND_INTERVAL_MINUTE;
    accessors[2] = HOUR_TO_SECOND_INTERVAL_SECOND;

    fields[0] = getIntervalHour;
    fields[1] = getIntervalMinute;
    fields[2] = getIntervalSecond;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_HOUR_TO_SECOND,
			3,HOUR_TO_SECOND_INTERVAL_SIGN,
			accessors,fields);

    break;

  case SQL_C_INTERVAL_MINUTE_TO_SECOND :

    if (scheme_is_struct_instance(MINUTE_TO_SECOND_INTERVAL_STRUCT_TYPE,argv[1]) == 0) {
      scheme_wrong_type("write-buffer","sql-interval-minute-to-second",1,argc,argv);
    }

    accessors[0] = MINUTE_TO_SECOND_INTERVAL_MINUTE;
    accessors[1] = MINUTE_TO_SECOND_INTERVAL_SECOND;

    fields[0] = getIntervalMinute;
    fields[1] = getIntervalSecond;

    writeIntervalToBuff(buffer,argv[1],arrayLength,ndx,
			SQL_IS_MINUTE_TO_SECOND,
			2,MINUTE_TO_SECOND_INTERVAL_SIGN,
			accessors,fields);

    break;
#endif /* ODBCVER >= 0x0300 */

  }

  return scheme_void;
}

Scheme_Object *srp_SQLLenBinaryAttr(int argc,Scheme_Object **argv) {
  long intVal;

  if (SCHEME_INTP(argv[0]) == FALSE) {
    scheme_wrong_type("len-binary-attr","integer",0,argc,argv);
  }

  if (scheme_get_int_val(argv[0],&intVal) == 0) {
    scheme_signal_error("len-binary-attr: number too big");
  }

  /* Scheme equivalent of SQL_LEN_BINARY_ATTR macro in SQLEXT.H */

  return scheme_make_integer_value(-100L - intVal);
}

void scheme_add_prim_to_env(Scheme_Env *env,
			    Scheme_Object *(*f)(int,Scheme_Object **),
			    char *name,
			    short minArgs,short maxArgs) {
  Scheme_Object *pobj;

  pobj = scheme_make_prim_w_arity(f,name,minArgs,maxArgs);
    
  scheme_add_global(name,pobj,env);
}

char *nullableToString(SQLSMALLINT nullable) {
  
  switch(nullable) {
  case SQL_NO_NULLS :
    return "sql-no-nulls";
    
  case SQL_NULLABLE :
    return "sql-nullable";

  default :
    return "sql-nullable-unknown";
  }
}

int namedConstCmp(SRP_NAMED_CONSTANT *p1,SRP_NAMED_CONSTANT *p2) {
  return stricmp(p1->scheme_name,p2->scheme_name);
}

int namedTypedConstCmp(SRP_NAMED_TYPED_CONSTANT *p1,SRP_NAMED_TYPED_CONSTANT *p2) {
  return stricmp(p1->scheme_name,p2->scheme_name);
}

BOOL isSmallInt(Scheme_Object *s) {
  long val;
  short smallVal;

  if (SCHEME_INTP(s) == FALSE) {
    return FALSE;
  }

  val = SCHEME_INT_VAL(s);

  smallVal = (short)val;

  if (smallVal != val) {
    return FALSE;
  }
    
  return TRUE;
}

BOOL isUnsignedInt(Scheme_Object *obj) {
  unsigned long val;

  if (SCHEME_EXACT_INTEGERP(obj) == FALSE ||
      scheme_get_unsigned_int_val(obj,&val) == 0) {
    return FALSE;
  }

  return TRUE;
}

BOOL isUnsignedSmallInt(Scheme_Object *obj) {
  long val;
  unsigned short smallVal;

  if (SCHEME_INTP(obj) == FALSE) {
    return FALSE;
  }

  val = SCHEME_INT_VAL(obj);

  smallVal = (unsigned short)val;

  if (smallVal != val) {
    return FALSE;
  }
    
  return TRUE;
}

BOOL isCharInt(Scheme_Object *s) {
  long val;
  char charVal;

  if (SCHEME_INTP(s) == FALSE) {
    return FALSE;
  }

  val = SCHEME_INT_VAL(s);

  charVal = (char)val;

  if (charVal != val) {
    return FALSE;
  }
    
  return TRUE;
}

BOOL isUnsignedCharInt(Scheme_Object *s) {
  unsigned long val;
  unsigned char charVal;

  if (SCHEME_INTP(s) == FALSE) {
    return FALSE;
  }

  val = SCHEME_INT_VAL(s);

  charVal = (unsigned char)val;

  if (charVal != val) {
    return FALSE;
  }
    
  return TRUE;
}

Scheme_Object *raise_valued_exn(Scheme_Object *val,char *f,
				Scheme_Object *type,char *name) { 
  Scheme_Object *exn;
  Scheme_Object *argv[3];
  char buff[128];

  sprintf(buff,"%s occurred in %s",name,f);

  argv[0] = scheme_make_string(buff);
  argv[1] = scheme_current_continuation_marks();
  argv[2] = val;

  exn = scheme_make_struct_instance(type,3,argv);

  scheme_raise(exn);

  return scheme_void;
}

Scheme_Object *raise_info_exn(Scheme_Object *val,char *f) {
  return raise_valued_exn(val,f,WITH_INFO_EXN_TYPE,"SQL_SUCCESS_WITH_INFO"); 
}

Scheme_Object *raise_need_data_exn(Scheme_Object *val,char *f) {
  return raise_valued_exn(val,f,NEED_DATA_EXN_TYPE,"SQL_NEED_DATA"); 
}

Scheme_Object *raise_not_implemented(char *proc) {
  Scheme_Object *exn_object;
  Scheme_Object *argv[2];
  char buff[256];

  sprintf(buff,
	  "Procedure %s is not implemented in ODBC version %s",
	  proc,odbc_version());

  argv[0] = scheme_make_string(buff);
  argv[1] = scheme_current_continuation_marks();
  
  exn_object = scheme_make_struct_instance(NOT_IMPLEMENTED_EXN_TYPE,2,argv);

  scheme_raise(exn_object);

  return scheme_void;
}

RETURN_CODE checkSQLReturn(SQLRETURN sr,char *f) {
  char buff[128];
  Scheme_Object *exn_object;
  Scheme_Object *argv[2];

  switch (sr) {

  case SQL_SUCCESS :
    return success;

  case SQL_SUCCESS_WITH_INFO :
    return with_info;

  case SQL_NEED_DATA :
    return need_data;

#if (ODBCVER < 0x0300)
  case SQL_NO_DATA_FOUND :
    sprintf(buff,"SQL_NO_DATA_FOUND error in %s",f);
#else 
  case SQL_NO_DATA :
    sprintf(buff,"SQL_NO_DATA error in %s",f);
#endif

    argv[0] = scheme_make_string(buff);
    argv[1] = scheme_current_continuation_marks();

    exn_object = scheme_make_struct_instance(NO_DATA_EXN_TYPE,2,argv);

    scheme_raise(exn_object);

    break;

  case SQL_INVALID_HANDLE :

    sprintf(buff,"SQL_INVALID_HANDLE error in %s",f);

    argv[0] = scheme_make_string(buff);
    argv[1] = scheme_current_continuation_marks();

    exn_object = scheme_make_struct_instance(INVALID_HANDLE_EXN_TYPE,2,argv);

    scheme_raise(exn_object);

    break;

  case SQL_ERROR :

    sprintf(buff,"Unspecified error in %s",f);

    argv[0] = scheme_make_string(buff);
    argv[1] = scheme_current_continuation_marks();

    exn_object = scheme_make_struct_instance(ERROR_EXN_TYPE,2,argv);

    scheme_raise(exn_object);

    break;

  case SQL_STILL_EXECUTING :


    sprintf(buff,"SQL_STILL_EXECUTING error in %s",f);

    argv[0] = scheme_make_string(buff);
    argv[1] = scheme_current_continuation_marks();

    exn_object = scheme_make_struct_instance(STILL_EXECUTING_EXN_TYPE,2,argv);

    scheme_raise(exn_object);

    break;

  default :

    scheme_signal_error("Unknown ODBC status code: %s",intToHexString(sr));

  }

  return success; /* unreachable */
}

char *sqlReturnToString(SQLRETURN sr) {

 switch (sr) {

  case SQL_SUCCESS :
    return "sql-success";

#if (ODBCVER >= 0x0300)
  case SQL_NO_DATA :
    return "sql-no-data";
#endif

  case SQL_INVALID_HANDLE :
    return "sql-invalid-handle";

  case SQL_ERROR :
    return "sql-error";

 case SQL_NEED_DATA :
   return "sql-need-data";
 
 case SQL_SUCCESS_WITH_INFO :
   return "sql-success-with-info";

  default :
    return "?";
 }
}

/* actual ODBC procedures */

/*

  Note on structuring of ODBC procedures:

  after each native ODBC procedure is called, we have

   retcode = checkSQLReturn(status_code,"scheme-name-of-procedure")
   ...
   sql_return(return_value,retcode,"scheme-name-of-procedure"); 

  checkSQLReturn either
   return success, meaning the procedure succeeded, or 
   returns has_info , meaning the procedure succeeded with info, or
   returns need_data, meaning the procedure failed and needs information, or
   raises an exception

  if retcode is with_info, sql_return raises exn-with-info, which
   contains the return value
  else if retcode is need_data, sql_return raises exn-need-data, which
   contains the return value
  otherwise, sql_return returns the return value 

*/

/* version info */

Scheme_Object *srp_version(int argc,Scheme_Object **argv) {
  return scheme_make_string(SRP_VERSION);
}

char *odbc_version(void) {
  static char buff[15];
  static BOOL init;

  if (!init) {
    int version;

    sprintf(buff,"%X",ODBCVER);
    version = atoi(buff);
    sprintf(buff,"%.2f",version/100.0);
    init = TRUE;
  }

  return buff;
}

Scheme_Object *srp_odbc_version(int argc,Scheme_Object **argv) {
  return scheme_make_string(odbc_version());
}

/* Functions in SQL.H */

Scheme_Object *srp_SQLAllocConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLHDBC connectHandle;
  SRP_SQL_HDBC *retval;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("alloc-connect","sql-henv",0,argc,argv);
  }

  envHandle = SQL_HENV_VAL(argv[0]);

  sr = SQLAllocConnect(envHandle,&connectHandle);

  retcode = checkSQLReturn(sr,"alloc-connect");

  retval = (SRP_SQL_HDBC *)scheme_malloc(sizeof(SRP_SQL_HDBC));
  retval->type = sql_hdbc_type;
  retval->hdbc = connectHandle;

  sql_return((Scheme_Object *)retval,retcode,"alloc-connect");
}

Scheme_Object *srp_SQLAllocEnv(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SRP_SQL_HENV *retval;
  RETURN_CODE retcode;

  sr = SQLAllocEnv(&envHandle);

  retcode = checkSQLReturn(sr,"alloc-env");

  retval = (SRP_SQL_HENV *)scheme_malloc(sizeof(SRP_SQL_HENV));
  retval->type = sql_henv_type;
  retval->henv = envHandle;

  sql_return((Scheme_Object *)retval,retcode,"alloc-env");
}

Scheme_Object *srp_SQLAllocHandle(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("alloc-handle");
#else
  SQLRETURN sr;
  char *handleTypeString;
  RETURN_CODE retcode;

  if (SCHEME_SYMBOLP(argv[0]) == FALSE) {
    scheme_wrong_type("alloc-handle","symbol",0,argc,argv);
  }

  handleTypeString = SCHEME_SYM_VAL(argv[0]);

  if (stricmp(handleTypeString,"sql-handle-env") == 0) {
    SQLHENV envHandle;
    SRP_SQL_HENV *retval;
    
    if (argc > 1) {
      scheme_signal_error("In sql-alloc-handle, when first argument is "
			  "'sql-handle-env, no other argument is allowed"); 
    }

    sr = SQLAllocHandle(SQL_HANDLE_ENV,SQL_NULL_HANDLE,&envHandle);

    retcode = checkSQLReturn(sr,"alloc-handle");

    retval = (SRP_SQL_HENV *)scheme_malloc(sizeof(SRP_SQL_HENV));
    retval->type = sql_henv_type;
    retval->henv = envHandle;

    sql_return((Scheme_Object *)retval,retcode,"alloc-handle");
  }

  if (argc < 2) {
    scheme_signal_error("sql-alloc-handle: unless first argument "
			"is 'sql-handle-env, second argument required"); 
  }

  if (stricmp(handleTypeString,"sql-handle-dbc") == 0) {
    SQLHDBC dbcHandle;
    SRP_SQL_HDBC *retval;
    
    if (SQL_HENVP(argv[1]) == FALSE) {
      scheme_wrong_type("alloc-handle","sql-henv",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_DBC,SQL_HENV_VAL(argv[1]),&dbcHandle);

    retcode = checkSQLReturn(sr,"alloc-handle");

    retval = (SRP_SQL_HDBC *)scheme_malloc(sizeof(SRP_SQL_HDBC));
    retval->type = sql_hdbc_type;
    retval->hdbc = dbcHandle;

    sql_return((Scheme_Object *)retval,retcode,"alloc-handle");
  }

  if (stricmp(handleTypeString,"sql-handle-stmt") == 0) {
    SQLHSTMT stmtHandle;
    SRP_SQL_HSTMT *retval;
    
    if (SQL_HDBCP(argv[1]) == FALSE) {
      scheme_wrong_type("alloc-handle","sql-hdbc",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_STMT,SQL_HDBC_VAL(argv[1]),&stmtHandle);

    retcode = checkSQLReturn(sr,"alloc-handle");

    retval = (SRP_SQL_HSTMT *)scheme_malloc(sizeof(SRP_SQL_HSTMT));
    retval->type = sql_hstmt_type;
    retval->hstmt = stmtHandle;

    sql_return((Scheme_Object *)retval,retcode,"alloc-handle");

  }

  if (stricmp(handleTypeString,"sql-handle-desc") == 0) {
    SQLHDESC descHandle;
    SRP_SQL_HDESC *retval;
    
    if (SQL_HDBCP(argv[1]) == FALSE) {
      scheme_wrong_type("alloc-handle","sql-hdbc",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_DESC,SQL_HDBC_VAL(argv[1]),&descHandle);

    retcode = checkSQLReturn(sr,"alloc-handle");

    retval = (SRP_SQL_HDESC *)scheme_malloc(sizeof(SRP_SQL_HDESC));
    retval->type = sql_hdesc_type;
    retval->descType = EXPLICIT;
    retval->hdesc = descHandle;

    sql_return((Scheme_Object *)retval,retcode,"alloc-handle");
  }

  scheme_signal_error("Handle type must be one of "
		      "'sql-handle-env, " 
		      "'sql-handle-dbc, "
		      "'sql-handle-stmt, or "
		      "'sql-handle-desc");

  return scheme_void; /* unreachable */
#endif
}

Scheme_Object *srp_SQLAllocStmt(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectHandle;
  SQLHSTMT stmtHandle;
  SRP_SQL_HSTMT *retval;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("alloc-stmt","sql-hdbc",0,argc,argv);
  }

  connectHandle = SQL_HDBC_VAL(argv[0]);

  sr = SQLAllocStmt(connectHandle,&stmtHandle);

  retcode = checkSQLReturn(sr,"alloc-stmt");

  retval = (SRP_SQL_HSTMT *)scheme_malloc(sizeof(SRP_SQL_HSTMT));
  retval->type = sql_hstmt_type;
  retval->hstmt = stmtHandle;

  sql_return((Scheme_Object *)retval,retcode,"alloc-stmt");
}

Scheme_Object *srp_SQLBindCol(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  int colNumber;
  void *buffer;
  long buflen;
  int buftype;
  SQLINTEGER *indicator;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("bind-col","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("bind-col","unsigned-small-int",1,argc,argv);
  }
   
  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("bind-col","sql-buffer",2,argc,argv);
  }

  if (SQL_INDICATORP(argv[3]) == FALSE) {
    scheme_wrong_type("bind-col","sql-indicator",3,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  colNumber = SCHEME_INT_VAL(argv[1]);

  buffer = SQL_BUFFER_VAL(argv[2]);
  buflen = SQL_BUFFER_WIDTH(argv[2]);
  buftype = SQL_BUFFER_CTYPE(argv[2]);

  indicator = SQL_INDICATOR_VAL(argv[3]);

  sr = SQLBindCol(stmtHandle,colNumber,buftype,buffer,buflen,indicator);

  retcode = checkSQLReturn(sr,"bind-col");

  sql_return(argv[0],retcode,"bind-col");
}

Scheme_Object *srp_SQLBindParam(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("bind-param"); 
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SRP_NAMED_SMALL_CONSTANT *p;
  char *SQLTypeName;
  SQLSMALLINT CTypeVal,SQLTypeVal;
  short paramNum;
  short decimalDigits;
  unsigned long lengthPrecision;
  SQLPOINTER buffer;
  SQLINTEGER *indicator;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("bind-param","sql-hstmt",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("bind-param","small-int",1,argc,argv);
  }    

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("bind-param","symbol",2,argc,argv);
  }
   
  if (SCHEME_EXACT_INTEGERP(argv[3]) == FALSE) {
    scheme_wrong_type("bind-param","exact integer",3,argc,argv);
  }
   
  if (SQL_BUFFERP(argv[4]) == FALSE) {
    scheme_wrong_type("bind-param","sql-buffer",4,argc,argv);
  }
   
  if (SQL_INDICATORP(argv[5]) == FALSE) {
    scheme_wrong_type("bind-param","sql-indicator",5,argc,argv);
  }
   
  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  paramNum = (short)(SCHEME_INT_VAL(argv[1]));

  SQLTypeName = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(SQLTypeName,SQLDataTypes);
 
  if (p == NULL) {
    scheme_signal_error("sql-bind-col: invalid SQL data type name %s",SQLTypeName);
  }

  SQLTypeVal = (SQLSMALLINT)(p->val);

  scheme_get_unsigned_int_val(argv[3],&lengthPrecision);

  CTypeVal = SQL_BUFFER_CTYPE(argv[4]);

  buffer = SQL_BUFFER_VAL(argv[4]);

  indicator = SQL_INDICATOR_VAL(argv[5]);

  switch(SQLTypeVal) {

  case SQL_DECIMAL :
  case SQL_NUMERIC :
  case SQL_TIME :
  case SQL_TIMESTAMP :
  case SQL_TYPE_TIME :
  case SQL_INTERVAL_SECOND :
  case SQL_INTERVAL_DAY_TO_SECOND :
  case SQL_INTERVAL_HOUR_TO_SECOND :
  case SQL_INTERVAL_MINUTE_TO_SECOND :

    /* need Decimals */

      if (argc != 7) {
	scheme_wrong_count("bind-param",7,7,argc,argv);
      }

      if (isSmallInt(argv[6]) == FALSE) {
	scheme_wrong_type("bind-param","small-int",6,argc,argv);
      }

      decimalDigits = (SQLSMALLINT)SCHEME_INT_VAL(argv[6]);

      break;


  default :

    decimalDigits = 0;
    break;
  }

  sr = SQLBindParam(stmtHandle,paramNum,CTypeVal,SQLTypeVal,
		    lengthPrecision,decimalDigits,buffer,
		    indicator);

  retcode = checkSQLReturn(sr,"bind-param");

  sql_return(argv[0],retcode,"bind-param");

#endif
}

Scheme_Object *srp_SQLCancel(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("cancel","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLCancel(stmtHandle);

  retcode = checkSQLReturn(sr,"cancel");
  
  sql_return(argv[0],retcode,"cancel");
}

Scheme_Object *srp_SQLCloseCursor(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("close-cursor"); 
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("close-cursor","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLCloseCursor(stmtHandle);

  retcode = checkSQLReturn(sr,"close-cancel");
  
  sql_return(argv[0],retcode,"close-cancel");

#endif
}


Scheme_Object *srp_SQLColAttribute(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("col-attribute");
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT colNumber;
  SQLUSMALLINT fieldId;
  char *fieldIdString;
  char buff[2048];
  SQLSMALLINT bufflen;
  SQLINTEGER numBuffer;
  SQLSMALLINT actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("col-attribute","sql-hstmt",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("col-attribute","small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("col-attribute","symbol",2,argc,argv);
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedTypedConstSearch(fieldIdString,colAttributes);

  if (p == NULL) {
    scheme_signal_error("Invalid column attribute: %s",fieldIdString);
  }
    
  fieldId = (SQLUSMALLINT)(p->val);
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  switch(p->type) {

  case sqlbool :

    bufflen = 0;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attribute");		       
    retval = (numBuffer == SQL_FALSE) ? scheme_false : scheme_true;
    sql_return(retval,retcode,"col-attribute");

  case sqlinteger :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attribute");		       
    retval = scheme_make_integer_value((long)numBuffer);
    sql_return(retval,retcode,"col-attribute");

  case namedinteger :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attribute");		       
    retval = scheme_intern_symbol(findIntegerName(fieldIdString,numBuffer,
						namedColAttrIntegers,
						sizeray(namedColAttrIntegers)));
    sql_return(retval,retcode,"col-attribute");

  case string :

    bufflen = sizeof(buff);
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attribute");		       
    retval = scheme_make_sized_string(buff,actualLen,TRUE);
    sql_return(retval,retcode,"col-attribute");

  default :
    scheme_signal_error("sql-col-attribute: invalid attribute type");

  }

  return scheme_void; /* unreachable */

#endif
}

Scheme_Object *srp_SQLColumns(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalog;
  SQLSMALLINT catalogLen;
  SQLCHAR *schema;
  SQLSMALLINT schemaLen;
  SQLCHAR *table;
  SQLSMALLINT tableLen;
  SQLCHAR *column;
  SQLSMALLINT columnLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("columns","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("columns","string",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  catalog = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogLen = SCHEME_STRLEN_VAL(argv[1]);
  schema = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaLen = SCHEME_STRLEN_VAL(argv[2]);
  table = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableLen = SCHEME_STRLEN_VAL(argv[3]);
  column = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  columnLen = SCHEME_STRLEN_VAL(argv[4]);

  sr = SQLColumns(stmtHandle,
		  catalog,catalogLen,
		  schema,schemaLen,
		  table,tableLen,
		  column,columnLen);

  retcode = checkSQLReturn(sr,"columns");		       

  sql_return(argv[0],retcode,"columns");
}

Scheme_Object *srp_SQLConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC dbcHandle;
  SQLCHAR *server;
  SQLSMALLINT serverLen;
  SQLCHAR *user;
  SQLSMALLINT userLen;
  SQLCHAR *password;
  SQLSMALLINT passwordLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("columns","sql-hdbc",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("columns","string",i,argc,argv);
    }
  }

  dbcHandle = SQL_HDBC_VAL(argv[0]);
  server = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  serverLen = SCHEME_STRLEN_VAL(argv[1]);
  user = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  userLen = SCHEME_STRLEN_VAL(argv[2]);
  password = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  passwordLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLConnect(dbcHandle,
		  server,serverLen,
		  user,userLen,
		  password,passwordLen);
		  
  retcode = checkSQLReturn(sr,"connect");		       

  sql_return(argv[0],retcode,"connect");
}


Scheme_Object *srp_SQLCopyDesc(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("copy-desc"); 
#else
  SQLRETURN sr;
  SQLHDESC srcDescHandle,targetDescHandle;
  RETURN_CODE retcode;
  int i;

  for (i = 0; i <= 1; i++) {
    if (SQL_HDESCP(argv[i]) == FALSE) {
      scheme_wrong_type("copy-desc","sql-hdesc",i,argc,argv);
    }
  }

  srcDescHandle = SQL_HDESC_VAL(argv[0]);
  targetDescHandle = SQL_HDESC_VAL(argv[1]);

  sr = SQLCopyDesc(srcDescHandle,targetDescHandle);

  retcode = checkSQLReturn(sr,"copy-desc");		       

  sql_return(argv[0],retcode,"copy-desc");

#endif
}


Scheme_Object *srp_SQLDataSources(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLUSMALLINT direction;
  char *directionString;
  SQLCHAR server[SQL_MAX_DSN_LENGTH + 1];
  SQLCHAR description[SQL_MAX_DSN_LENGTH + 1];
  SQLSMALLINT serverLen,descriptionLen;
  SRP_NAMED_SMALL_CONSTANT *p;
  RETURN_CODE retcode;
  Scheme_Object *retval;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("data-sources","sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("data-sources","symbol",0,argc,argv);
  }

  directionString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(directionString,fetchDirections);

  if (p == NULL) {
    scheme_signal_error("sql-data-sources: invalid direction: %s",
			directionString);
  }

  direction = p->val;

  envHandle = SQL_HENV_VAL(argv[0]);

  sr = SQLDataSources(envHandle,direction,
		      server,sizeof(server),&serverLen,
		      description,sizeof(description),&descriptionLen);

  retcode = checkSQLReturn(sr,"data-sources");		       

  retval = scheme_make_pair(scheme_make_sized_string((char *)description,
						     descriptionLen,TRUE),
			    scheme_null);
  retval = scheme_make_pair(scheme_make_sized_string((char *)server,
						     serverLen,TRUE),
			    retval);

  sql_return(retval,retcode,"data-sources");
}

Scheme_Object *srp_SQLDescribeCol(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT colNumber;
  SQLCHAR columnName[256];
  SQLSMALLINT colNameLen;
  SQLSMALLINT dataType;
  SQLUINTEGER colSize;
  SQLSMALLINT decimalDigits;
  SQLSMALLINT nullable;
  char *nullableString;
  char *dataTypeString;
  RETURN_CODE retcode;
  int i;
  Scheme_Object *retval;
  
  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("describe-col","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("describe-col","unsigned-small-int",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);

  sr = SQLDescribeCol(stmtHandle,colNumber,
		      columnName,sizeof(columnName),&colNameLen,
		      &dataType,&colSize,&decimalDigits,
		      &nullable);

  retcode = checkSQLReturn(sr,"describe-col");		       

  dataTypeString = "?";

  for (i = 0; i < (int)sizeray(SQLDataTypes); i++) {
    if (dataType == SQLDataTypes[i].val) {
      dataTypeString = SQLDataTypes[i].scheme_name;
      break;
    }
  }

  nullableString = nullableToString(nullable);

  retval = scheme_null;
  retval = scheme_make_pair(scheme_intern_symbol(nullableString),retval);
  retval = scheme_make_pair(scheme_make_integer_value(decimalDigits),retval);
  retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(colSize),retval);
  retval = scheme_make_pair(scheme_intern_symbol(dataTypeString),retval);
  retval = scheme_make_pair(scheme_make_sized_string((char *)columnName,
						     colNameLen,TRUE),retval);
  
  sql_return(retval,retcode,"describe-col");
}

Scheme_Object *srp_SQLDisconnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC dbcHandle;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("disconnect","sql-hdbc",0,argc,argv);
  }

  dbcHandle = SQL_HDBC_VAL(argv[0]);

  sr = SQLDisconnect(dbcHandle);

  retcode = checkSQLReturn(sr,"disconnect");
  
  sql_return(argv[0],retcode,"disconnect");
}

Scheme_Object *srp_SQLEndTran(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("end-tran");
#else
  SQLRETURN sr;
  SQLSMALLINT actionType;
  RETURN_CODE retcode;
  char *action;

  if (SQL_HDBCP(argv[0]) == FALSE && SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("end-tran","sql-hdbc> or <sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("end-tran","symbol",1,argc,argv);
  }

  action = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(action,"sql-commit") == 0) {
    actionType = SQL_COMMIT;
  }
  else if (stricmp(action,"sql-rollback") == 0) {
    actionType = SQL_ROLLBACK;
  }
  else {
    scheme_signal_error("sql-end-tran: invalid completion type: %s",
			action);
  }

  if (SQL_HDBCP(argv[0])) {
    sr = SQLEndTran(SQL_HANDLE_DBC,SQL_HDBC_VAL(argv[0]),actionType);
  }
  else if (SQL_HENVP(argv[0])) {
    sr = SQLEndTran(SQL_HANDLE_ENV,SQL_HENV_VAL(argv[0]),actionType);
  }

  retcode = checkSQLReturn(sr,"end-tran");
  
  sql_return(argv[0],retcode,"end-tran");

#endif
}


Scheme_Object *srp_SQLError(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLHDBC connectionHandle;
  SQLHSTMT stmtHandle;
  SQLCHAR state[6];
  SQLINTEGER nativeError;
  SQLCHAR text[2048];
  SQLSMALLINT textLen;
  RETURN_CODE retcode;
  Scheme_Object *retval;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-error","sql-henv",0,argc,argv);
  }

  if (SQL_HDBCP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-error","sql-hdbc or symbol",1,argc,argv);
  }

  if (SQL_HSTMTP(argv[2]) == FALSE && SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-error","sql-hstmt or symbol",2,argc,argv);
  }

  envHandle = SQL_HENV_VAL(argv[0]);

  if (SCHEME_SYMBOLP(argv[1])) {
    if (stricmp(SCHEME_SYM_VAL(argv[1]),"sql-null-hdbc")) {
      scheme_signal_error("sql-error: 'sql-null-hdbc is only valid symbol for 2nd argument");
    }
    connectionHandle = SQL_NULL_HDBC;
  }
  else {
    connectionHandle = SQL_HDBC_VAL(argv[1]);
  }

  if (SCHEME_SYMBOLP(argv[2])) {
    if (stricmp(SCHEME_SYM_VAL(argv[2]),"sql-null-hstmt")) {
      scheme_signal_error("sql-error: 'sql-null-hstmt is only valid symbol for 3rd argument");
    }
    stmtHandle = SQL_NULL_HSTMT;
  }
  else {
    stmtHandle = SQL_HSTMT_VAL(argv[2]);
  }

  sr = SQLError(envHandle,connectionHandle,stmtHandle,
		state,&nativeError,
		text,sizeof(text),&textLen);
		
  retcode = checkSQLReturn(sr,"sql-error");

  retval = scheme_null;
  retval = scheme_make_pair(scheme_make_sized_string((char *)text,textLen,TRUE),
			    retval);
  retval = scheme_make_pair(scheme_make_integer_value(nativeError),
			    retval);
  retval = scheme_make_pair(scheme_make_string((const char *)state),
			    retval);

  sql_return(retval,retcode,"sql-error");
}

Scheme_Object *srp_SQLExecDirect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *stmt;
  SQLINTEGER stmtLen;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("exec-direct","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("exec-direct","string",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  stmt = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  stmtLen = SCHEME_STRLEN_VAL(argv[1]);

  sr = SQLExecDirect(stmtHandle,stmt,stmtLen);

  retcode = checkSQLReturn(sr,"exec-direct");  

  sql_return(argv[0],retcode,"exec-direct");
}

Scheme_Object *srp_SQLExecute(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-execute","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLExecute(stmtHandle);

  retcode = checkSQLReturn(sr,"sql-execute");
  
  sql_return(argv[0],retcode,"sql-execute");
}

Scheme_Object *srp_SQLFetch(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("fetch","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLFetch(stmtHandle);

  retcode = checkSQLReturn(sr,"fetch");
  
  sql_return(argv[0],retcode,"fetch");
}

Scheme_Object *srp_SQLFetchScroll(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("fetch-scroll");
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER offset;
  char *orientationString;
  SRP_NAMED_SMALL_CONSTANT *p;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("fetch-scroll","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("fetch-scroll","symbol",1,argc,argv);
  }

  orientationString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(orientationString,fetchScrolls);

  if (p == NULL) {
    scheme_signal_error("sql-fetch-scroll: invalid orientation: %s",
			orientationString);
  } 

  if (p->val == SQL_FETCH_ABSOLUTE || p->val == SQL_FETCH_RELATIVE || 
      p->val == SQL_FETCH_BOOKMARK) {
    if (argc != 3) {
      scheme_signal_error("sql-fetch-scroll: given orientation %s "
			  "requires offset",
			  orientationString);
    }
    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) {
      scheme_wrong_type("fetch-scroll","exact integer",2,argc,argv);
      if (scheme_get_int_val(argv[2],&offset) == 0) {
	scheme_signal_error("sql-fetch-scroll: offset too large");
      }
    }
  }
  else {
    offset = 0;
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  offset = SCHEME_INT_VAL(argv[1]);

  sr = SQLFetchScroll(stmtHandle,p->val,offset);

  retcode = checkSQLReturn(sr,"fetch-scroll");  

  sql_return(argv[0],retcode,"fetch-scroll");

#endif
}

Scheme_Object *srp_SQLFreeConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("free-connect","sql-hdbc",0,argc,argv);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  sr = SQLFreeConnect(connectionHandle);

  retcode = checkSQLReturn(sr,"free-connect");  

  sql_return(argv[0],retcode,"free-connect");
}

Scheme_Object *srp_SQLFreeEnv(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("free-env","sql-henv",0,argc,argv);
  }

  envHandle = SQL_HENV_VAL(argv[0]);

  sr = SQLFreeEnv(envHandle);

  retcode = checkSQLReturn(sr,"free-env");  

  sql_return(argv[0],retcode,"free-env");
}

Scheme_Object *srp_SQLFreeHandle(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("free-handle");
#else
  SQLRETURN sr;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_ENV,SQL_HENV_VAL(argv[0]));
  }
  else if (SQL_HDBCP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_DBC,SQL_HDBC_VAL(argv[0]));
  }
  else if (SQL_HSTMTP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_STMT,SQL_HSTMT_VAL(argv[0]));
  }
  else if (SQL_HDESCP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_DESC,SQL_HDESC_VAL(argv[0]));
  }
  else {
    scheme_wrong_type("free-handle",
		      "sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc",
		      0,argc,argv);
  }
    
  retcode = checkSQLReturn(sr,"free-handle");  

  sql_return(argv[0],retcode,"free-handle");
#endif
}

Scheme_Object *srp_SQLFreeStmt(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT hstmt;
  SQLUSMALLINT option;
  char *optionString;
  SRP_NAMED_SMALL_CONSTANT *p;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("free-stmt","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("free-stmt","symbol",1,argc,argv);
  }

  optionString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(optionString,stmtFreeOptions);

  if (p == NULL) {
    scheme_signal_error("sql-free-stmt: invalid option: %s",optionString);
  }

  option = p->val;

  hstmt = SQL_HSTMT_VAL(argv[0]);

  sr = SQLFreeStmt(hstmt,option);

  retcode = checkSQLReturn(sr,"free-stmt");  

  sql_return(argv[0],retcode,"free-stmt");
}

Scheme_Object *srp_SQLGetConnectAttr(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("get-connect-attr"); 
#else
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLINTEGER attribute;
  char *attributeString;
  SRP_CONST_TYPE attributeType;
  SQLUINTEGER number;
  char buff[2048];
  SQLINTEGER actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-connect-attr","sql-hdbc",0,argc,argv);
  }
  
  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("get-connect-attr","symbol",1,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  /* need to check settable and read-only attributes */

  p = namedTypedConstSearch(attributeString,settableConnectionAttributes);
  
  if (p == NULL) {

    p = namedTypedConstSearch(attributeString,readOnlyConnectionAttributes);

    if (p == NULL) {
      scheme_signal_error("sql-get-connect-attr: invalid attribute: %s",
			  attributeString);
    }
  }

  attribute = p->val;
  attributeType = p->type;

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  switch(attributeType) {

  case sqlbool :

    sr = SQLGetConnectAttr(connectionHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-connect-attr");  			 
    retval = (number == SQL_FALSE) ? scheme_false : scheme_true;
    sql_return(retval,retcode,"get-connect-attr");

  case string :

    sr = SQLGetConnectAttr(connectionHandle,attribute,buff,sizeray(buff),&actualLen);
    retcode = checkSQLReturn(sr,"get-connect-attr");  			 
    retval = scheme_make_sized_string(buff,actualLen,TRUE);
    sql_return(retval,retcode,"get-connect-attr");

  case sqluinteger :

    sr = SQLGetConnectAttr(connectionHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-connect-attr");  			 
    retval = scheme_make_integer_value_from_unsigned(number);
    sql_return(retval,retcode,"get-connect-attr");

  case nameduinteger :

    sr = SQLGetConnectAttr(connectionHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-connect-attr");  			 

    retval = scheme_intern_symbol(findIntegerName(attributeString,number,
						    namedConnectAttrIntegers,
						    sizeray(namedConnectAttrIntegers)));
    sql_return(retval,retcode,"get-connect-attr");

  default :

    scheme_signal_error("sql-connect-attribute: invalid attribute type");

  }
  
  return scheme_void; /* for compiler */
#endif
}

Scheme_Object *srp_SQLGetConnectOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT option;
  char *optionString;
  SRP_CONST_TYPE optionType;
  SQLUINTEGER number;
  char buff[2048];
  SRP_NAMED_TYPED_CONSTANT *p;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-connect-option","sql-hdbc",0,argc,argv);
  }
  
  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("get-connect-option","symbol",1,argc,argv);
  }

  optionString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(optionString,connectionOptions);
  
  if (p == NULL) {
    scheme_signal_error("sql-get-connect-option: invalid option: %s",
			optionString);
  }

  option = (SQLUSMALLINT)(p->val);
  optionType = p->type;

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  switch(optionType) {

  case string :

    sr = SQLGetConnectOption(connectionHandle,option,buff);
    retcode = checkSQLReturn(sr,"get-connect-option");  			 
    retval = scheme_make_string(buff);
    sql_return(retval,retcode,"get-connect-option");

  case sqluinteger :

    sr = SQLGetConnectOption(connectionHandle,option,&number);
    retcode = checkSQLReturn(sr,"get-connect-option");  			 
    retval = scheme_make_integer_value_from_unsigned(number);
    sql_return(retval,retcode,"get-connect-option");

  case nameduinteger :

    sr = SQLGetConnectOption(connectionHandle,option,&number);
    retcode = checkSQLReturn(sr,"get-connect-option");  			 
    retval = scheme_intern_symbol(findIntegerName(optionString,number,
						  namedConnectOptionIntegers,
						  sizeray(namedConnectOptionIntegers)));
    sql_return(retval,retcode,"get-connect-option");

  case bitmask :

    sr = SQLGetConnectOption(connectionHandle,option,&number);
    retcode = checkSQLReturn(sr,"get-connect-option");  			 
    retval = bitsListFromBitMask(optionString,number);
    sql_return(retval,retcode,"get-connect-option");

  default : 

    scheme_signal_error("sql-connect-option: invalid attribute type");

  }
  
  return scheme_void; /* for compiler */
}

Scheme_Object *srp_SQLGetCursorName(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR name[1024];
  SQLSMALLINT actualLen;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("get-cursor-name","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLGetCursorName(stmtHandle,name,sizeray(name),&actualLen);

  retcode = checkSQLReturn(sr,"get-cursor-name");

  retval = scheme_make_sized_string((char *)name,actualLen,TRUE);
  sql_return(retval,retcode,"get-cursor-name");
}

Scheme_Object *srp_SQLGetData(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT colNumber;
  SQLPOINTER buffer;
  SQLINTEGER bufferlen;
  SQLSMALLINT buffertype;
  SQLINTEGER *indicator;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("get-data","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("get-data","unsigned-small-int",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("get-data","sql-buffer",2,argc,argv);
  }

  if (SQL_INDICATORP(argv[3]) == FALSE) {
    scheme_wrong_type("get-data","sql-indicator",3,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferlen = SQL_BUFFER_LEN(argv[2]);
  buffertype = SQL_BUFFER_CTYPE(argv[2]);

  indicator = SQL_INDICATOR_VAL(argv[3]);

  sr = SQLGetData(stmtHandle,colNumber,buffertype,buffer,bufferlen,indicator);

  retcode = checkSQLReturn(sr,"get-data");

  sql_return(argv[0],retcode,"get-data");
}

Scheme_Object *srp_SQLGetDescField(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x300)
  return raise_not_implemented("get-desc-field");
#else
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLSMALLINT fieldId;
  SRP_CONST_TYPE fieldType;
  char *fieldIdString;
  SQLINTEGER actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-desc-field","sql-hdesc",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("get-desc-field","small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("get-desc-field","symbol",2,argc,argv);
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedTypedConstSearch(fieldIdString,fieldDescriptors);

  if (p == NULL) {
    scheme_signal_error("sql-get-desc-field: invalid field identifier: %s",
			fieldIdString);
  }

  fieldId = (SQLSMALLINT)(p->val);
  fieldType = p->type;

  descHandle = SQL_HDESC_VAL(argv[0]);
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  switch(fieldType) {

  case sqlsmallint :

    SQLSMALLINT smallIntVal;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &smallIntVal,sizeof(SQLSMALLINT),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");
 
    retval = scheme_make_integer(smallIntVal);
    sql_return(retval,retcode,"get-desc-field");

  case sqlinteger :

    SQLINTEGER intVal;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &intVal,sizeof(SQLINTEGER),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    retval = scheme_make_integer_value(intVal);
    sql_return(retval,retcode,"get-desc-field");
    
  case sqluinteger :

    SQLUINTEGER uintVal;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &uintVal,sizeof(SQLUINTEGER),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    retval = scheme_make_integer_value_from_unsigned(uintVal);
    sql_return(retval,retcode,"get-desc-field");
    
  case string :

    char buff[2048];

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 buff,sizeof(buff),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    retval = scheme_make_string(buff);
    sql_return(retval,retcode,"get-desc-field");
    
  case sqlbool :

    SQLINTEGER boolVal;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &boolVal,sizeof(SQLINTEGER),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    retval = (boolVal == SQL_FALSE) ? scheme_false : scheme_true;
    sql_return(retval,retcode,"get-desc-field");

  case namedinteger :
    SQLINTEGER namedIntVal;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &namedIntVal,sizeof(SQLINTEGER),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    retval = scheme_intern_symbol(findIntegerName(fieldIdString,namedIntVal,
						namedFieldDescriptors,
						sizeray(namedFieldDescriptors)));
    sql_return(retval,retcode,"get-desc-field");

  case sqlbuffer :
    SQLPOINTER p;
    SRP_SQL_BUFFER *pBuffer;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &p,sizeof(SQLPOINTER),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    if (p == NULL) {
      scheme_signal_error("NULL data pointer");
    }

    pBuffer = lookupBufferFromAddress(p);

    if (pBuffer == NULL) {
      scheme_signal_error("Internal buffer, can't be made into Scheme value");
    }

    sql_return((Scheme_Object *)pBuffer,retcode,"get-desc-field");

  case sqlindicator :
    SQLINTEGER *pIntVal;
    SRP_SQL_INDICATOR *pIndicator;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &pIntVal,sizeof(SQLINTEGER *),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    pIndicator = (SRP_SQL_INDICATOR *)scheme_malloc(sizeof(SRP_SQL_INDICATOR));
    scheme_dont_gc_ptr(pIndicator);
    pIndicator->type = sql_indicator_type;
    pIndicator->arrayLength = 1;
    pIndicator->storage = pIntVal;

    sql_return((Scheme_Object *)pIndicator,retcode,"get-desc-field");

  case arraystatus :
    SQLUSMALLINT *pSmallUintVal;
    SRP_SQL_ARRAY_STATUS *pArrayStatus;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &pSmallUintVal,sizeof(SQLUSMALLINT *),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    pArrayStatus = (SRP_SQL_ARRAY_STATUS *)scheme_malloc(sizeof(SRP_SQL_ARRAY_STATUS));
    scheme_dont_gc_ptr(pArrayStatus);
    pArrayStatus->type = sql_array_status_type;
    pArrayStatus->hdesc = descHandle;
    pArrayStatus->descType = SQL_HDESC_DESCTYPE(argv[0]);
    pArrayStatus->values = pSmallUintVal;

    sql_return((Scheme_Object *)pArrayStatus,retcode,"get-desc-field");

  case bindingoffset :
    /* SQLINTEGER *pIntVal; */
    SRP_SQL_BINDING_OFFSET *pBindingOffset;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &pIntVal,sizeof(SQLINTEGER *),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    pBindingOffset = (SRP_SQL_BINDING_OFFSET *)scheme_malloc(sizeof(SRP_SQL_BINDING_OFFSET));
    scheme_dont_gc_ptr(pBindingOffset);
    pBindingOffset->type = sql_binding_offset_type;
    pBindingOffset->val = pIntVal;

    sql_return((Scheme_Object *)pBindingOffset,retcode,"get-desc-field");

  case rowsprocessed :
    SQLUINTEGER *pUintVal;
    SRP_SQL_ROWS_PROCESSED *pRowsProcessed;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &pUintVal,sizeof(SQLUINTEGER *),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    pRowsProcessed = (SRP_SQL_ROWS_PROCESSED *)scheme_malloc(sizeof(SRP_SQL_ROWS_PROCESSED));
    scheme_dont_gc_ptr(pRowsProcessed);
    pRowsProcessed->type = sql_rows_processed_type;
    pRowsProcessed->val = pUintVal;

    sql_return((Scheme_Object *)pRowsProcessed,retcode,"get-desc-field");

  case octetlength :
    /* SQLINTEGER *pIntVal; */
    SRP_SQL_OCTET_LENGTH *pOctetLength;

    sr = SQLGetDescField(descHandle,recNumber,fieldId,
			 &pIntVal,sizeof(SQLINTEGER *),&actualLen);

    retcode = checkSQLReturn(sr,"get-desc-field");

    pOctetLength = (SRP_SQL_OCTET_LENGTH *)scheme_malloc(sizeof(SRP_SQL_OCTET_LENGTH));
    scheme_dont_gc_ptr(pOctetLength);
    pOctetLength->type = sql_octet_length_type;
    pOctetLength->val = pIntVal;

    sql_return((Scheme_Object *)pOctetLength,retcode,"get-desc-field");

  default :
    scheme_signal_error("sql-get-desc-field: unknown field type %s",intToHexString(fieldType));
  }

  return scheme_void;

#endif
} 


Scheme_Object *srp_SQLGetDescRec(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("get-desc-rec");
#else
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLCHAR name[1024];
  SQLSMALLINT actualLen;
  SQLSMALLINT type;
  char *typeString;
  SQLSMALLINT subtype;
  char *subtypeString;
  SQLINTEGER length;
  SQLSMALLINT precision;
  SQLSMALLINT scale;
  SQLSMALLINT nullable; 
  char *nullableString;
  Scheme_Object *retval;
  RETURN_CODE retcode;
  int i;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-desc-rec","sql-hdesc",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("get-desc-rec","small-int",1,argc,argv);
  }
  
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  descHandle = SQL_HDESC_VAL(argv[0]);

  sr = SQLGetDescRec(descHandle,recNumber,name,sizeray(name),&actualLen,
		     &type,&subtype,&length,&precision,
		     &scale,&nullable);

  retcode = checkSQLReturn(sr,"get-desc-rec");

  typeString = "?";

  for (i = 0; i < (int)sizeray(SQLDataTypes); i++) {
    if (SQLDataTypes[i].val == type) {
      typeString = SQLDataTypes[i].scheme_name;
      break;
    }
  }

  subtypeString = "?";

  for (i = 0; i < (int)sizeray(datetimeIntervalCodes); i++) {
    if ((SQLINTEGER)(datetimeIntervalCodes[i].val) == subtype) {
      subtypeString = datetimeIntervalCodes[i].scheme_name;
      break;
    }
  }

  nullableString = nullableToString(nullable);

  retval = scheme_null;
  retval = scheme_make_pair(scheme_intern_symbol(nullableString),retval);
  retval = scheme_make_pair(scheme_make_integer_value(scale),retval);
  retval = scheme_make_pair(scheme_make_integer_value(precision),retval);
  retval = scheme_make_pair(scheme_make_integer_value(length),retval);
  retval = scheme_make_pair(scheme_intern_symbol(subtypeString),retval);
  retval = scheme_make_pair(scheme_intern_symbol(typeString),retval);
  retval = scheme_make_pair(scheme_make_sized_string((char *)name,
						     actualLen,TRUE),retval);
  
  sql_return(retval,retcode,"get-desc-rec");

#endif
}

Scheme_Object *srp_SQLGetDiagField(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("get-diag-field");
#else
  SQLRETURN sr;
  SQLSMALLINT handleType;
  SQLHANDLE handle;
  SQLSMALLINT recNumber;
  SQLSMALLINT diagId;
  char *diagIdString;
  SQLSMALLINT actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE && 
      SQL_HDBCP(argv[0]) == FALSE &&
      SQL_HSTMTP(argv[0]) == FALSE &&
      SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-diag-field",
		      "sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc",
		      0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("get-diag-field",
		      "small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("get-diag-field","symbol",2,argc,argv);
  }

  diagIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedTypedConstSearch(diagIdString,diagFields);

  if (p == NULL) {
    scheme_signal_error("Invalid diagnostic: %s",diagIdString);
  }
    
  diagId = (SQLUSMALLINT)(p->val);

  SchemeObjectToHandle(argv[0],&handle,&handleType);

  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  switch(p->type) {

  case sqlinteger :

    SQLINTEGER intVal;

    sr = SQLGetDiagField(handleType,handle,recNumber,diagId,
			 &intVal,0,&actualLen);
    
    retcode = checkSQLReturn(sr,"get-diag-field");		       

    sql_return(scheme_make_integer_value(intVal),retcode,"get-diag-field");

  case possiblynamedinteger :

    /* SQLINTEGER intVal; */
    char *diagString;

    sr = SQLGetDiagField(handleType,handle,recNumber,diagId,
			 &intVal,0,&actualLen);
    
    retcode = checkSQLReturn(sr,"get-diag-field");		       

    diagString = findIntegerName(diagIdString,intVal,
			       namedDiagFieldIntegers,
			       sizeray(namedDiagFieldIntegers));

    if (strcmp(diagString,NO_BIT_NAME) == 0) {
      sql_return(scheme_make_integer_value(intVal),retcode,
		 "get-diag-field");
    }

    sql_return (scheme_intern_symbol(diagString),retcode,"get-diag-field");

  case string :

    char buffer[2048];

    sr = SQLGetDiagField(handleType,handle,recNumber,diagId,
			 buffer,sizeof(buffer),&actualLen);
    
    retcode = checkSQLReturn(sr,"get-diag-field");		       

    sql_return(scheme_make_sized_string(buffer,actualLen,TRUE),
	       retcode,"get-diag-field");

  case sqlreturn :

    SQLRETURN returnVal;

    sr = SQLGetDiagField(handleType,handle,recNumber,diagId,
			 &returnVal,sizeof(returnVal),&actualLen);

    retcode = checkSQLReturn(sr,"get-diag-field");		       

    sql_return(scheme_intern_symbol(sqlReturnToString(returnVal)),
	       retcode,"get-diag-field");

  default :

    scheme_signal_error("Unknown diagnostic type: %s",intToHexString((int)p->type));
  }

  return scheme_void;

#endif
}

Scheme_Object *srp_SQLGetDiagRec(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("get-diag-rec");
#else
  SQLRETURN sr;
  SQLSMALLINT handleType;
  SQLHANDLE handle;
  SQLSMALLINT recNumber;
  SQLCHAR sqlState[6];
  SQLINTEGER nativeError;
  SQLCHAR messageText[1024];
  SQLSMALLINT actualLen;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE && 
      SQL_HDBCP(argv[0]) == FALSE &&
      SQL_HSTMTP(argv[0]) == FALSE &&
      SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-diag-rec",
		      "sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc",
		      0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("get-diag-rec",
		      "small-int",1,argc,argv);
  }

  SchemeObjectToHandle(argv[0],&handle,&handleType);

  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  sqlState[5] = '\0';

  sr = SQLGetDiagRec(handleType,handle,recNumber,
		     sqlState,&nativeError,
		     messageText,sizeray(messageText),
		     &actualLen);

  retcode = checkSQLReturn(sr,"get-diag-rec");		       

  retval = scheme_null;
  retval = scheme_make_pair(scheme_make_sized_string((char *)messageText,
						     actualLen,TRUE),retval);
  retval = scheme_make_pair(scheme_make_integer(nativeError),retval);
  retval = scheme_make_pair(scheme_make_string((const char *)sqlState),retval);

  sql_return(retval,retcode,"get-diag-rec");

#endif
}


Scheme_Object *srp_SQLGetEnvAttr(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("get-env-attr");
#else
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLINTEGER attribute;
  SRP_CONST_TYPE attributeType;
  char *attributeString;
  SRP_NAMED_TYPED_CONSTANT *p;
  SQLINTEGER actualLen;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE) { 
    scheme_wrong_type("get-env-attr","sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) { 
    scheme_wrong_type("get-env-attr","symbol",1,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(attributeString,envAttributes);

  if (p == NULL) {
    scheme_signal_error("sql-get-env-attr: invalid environment attribute: %s",
			attributeString);
  }

  attribute = p->val;
  attributeType = p->type;

  envHandle = SQL_HENV_VAL(argv[0]);

  /* interpret attribute according to type */

  switch(attributeType) {

  case nameduinteger :

    SQLUINTEGER number;

    sr = SQLGetEnvAttr(envHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-env-attr");		       

    retval = scheme_intern_symbol(findIntegerName(attributeString,number,
						namedEnvAttrIntegers,
						sizeray(namedEnvAttrIntegers)));
    sql_return(retval,retcode,"get-env-attr");

  case sqlbool :

    SQLUINTEGER boolval;

    sr = SQLGetEnvAttr(envHandle,attribute,&boolval,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-env-attr");		       

    retval = (boolval == SQL_FALSE) ? scheme_false : scheme_true;
    sql_return(retval,retcode,"get-env-attr");

  default :

    scheme_signal_error("Unknown environment attribute type: %s",
			intToHexString((int)attributeType));
  }

  return scheme_void; /* unreachable */

#endif
}

Scheme_Object *srp_SQLGetFunctions(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT function;
  char *functionString;
#if (ODBCVER >= 0x0300)
  SQLUSMALLINT supported[SQL_API_ODBC3_ALL_FUNCTIONS_SIZE];
#else
  SQLUSMALLINT supported[sizeray(sqlFunctions)];
#endif
  SRP_NAMED_SMALL_CONSTANT *p;  
  RETURN_CODE retcode;
  int i;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-functions","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("get-functions","symbol",1,argc,argv);
  }

  functionString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(functionString,sqlFunctions);
  
  if (p == NULL) {
    scheme_signal_error("sql-get-functions: invalid function name: %s",
			functionString);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  function = p->val;
  
  sr = SQLGetFunctions(connectionHandle,function,supported);

  retcode = checkSQLReturn(sr,"get-functions");		       

  if (function == SQL_API_ALL_FUNCTIONS) {
    Scheme_Object *value;
    Scheme_Object *retval;
    int ndx;

    retval = scheme_null;

    for (i = 0; i < (int)sizeray(sqlFunctions); i++) {
      ndx = sqlFunctions[i].val;
      if (ndx < 100) { /* valid ODBC 2 function */
	if (supported[ndx]) { /* rely on SQL_FALSE == 0 */
	  value = scheme_true;
	}
	else {
	  value = scheme_false;
	}
	retval = scheme_make_pair(scheme_make_pair(scheme_intern_symbol(sqlFunctions[i].scheme_name),
						   value),
				  retval);
      }
    }

    sql_return(retval,retcode,"get-functions");
  }
#if (ODBCVER >= 0x0300)
  else if (function == SQL_API_ODBC3_ALL_FUNCTIONS) {
    Scheme_Object *value;
    Scheme_Object *retval;

    retval = scheme_null;

    for (i = 0; i < sizeray(sqlFunctions); i++) {
      if (SQL_FUNC_EXISTS(supported,sqlFunctions[i].val) == SQL_TRUE) {
	value = scheme_true;
      }
      else {
	value = scheme_false;
      }
      retval = scheme_make_pair(scheme_make_pair(scheme_intern_symbol(sqlFunctions[i].scheme_name),
						 value),
				retval);
    }

    sql_return(retval,retcode,"get-functions");

  }
#endif
  else if (*supported) { /* rely on SQL_FALSE == 0 */
    sql_return(scheme_true,retcode,"get-functions");
  }
  
  sql_return(scheme_false,retcode,"get-functions");
}

Scheme_Object *srp_SQLGetInfo(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT infoName;
  char *infoNameString;
  SRP_CONST_TYPE infoType;
  char buffer[2048];
  SQLSMALLINT actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;    
  SQLUSMALLINT usmallint_value;
  SQLUINTEGER uinteger_value;
#if (ODBCVER >= 0x0300)
  SQLHDESC retDescHandle;
#endif
  SQLHDBC retConnectHandle;
  SQLHENV retEnvHandle;
  SQLHSTMT retStmtHandle;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("get-info","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("get-info","symbol",1,argc,argv);
  }

  infoNameString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(infoNameString,sqlInfo);

  if (p == NULL) {
    scheme_signal_error("sql-get-info: invalid info type: %s",
			infoNameString);
  }

  infoName = (SQLUSMALLINT)p->val;
  infoType = p->type;

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  if (argc == 3) {
#if (ODBCVER >= 0x0300)
    if (stricmp(infoNameString,"sql-driver-hdesc") == 0) {
      if (SQL_HDESCP(argv[2]) == FALSE) {
	scheme_wrong_type("get-info","sql-hdesc",2,argc,argv);
      }
      
      retDescHandle = SQL_HDBC_VAL(argv[2]);

    }
    else 
#endif
    if (stricmp(infoNameString,"sql-driver-hstmt") == 0) {
      if (SQL_HSTMTP(argv[2]) == FALSE) {
	scheme_wrong_type("get-info","sql-hstmt",2,argc,argv);
      }
      
      retStmtHandle = SQL_HSTMT_VAL(argv[2]);

    }
    else {
      scheme_signal_error("sql-get-info: too many arguments "
			  "for information type %s",infoNameString);
    }
  }

  switch(infoType) {

  case sqlusmallint :

    sr = SQLGetInfo(connectionHandle,infoName,&usmallint_value,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-info");    

    sql_return(scheme_make_integer((long)usmallint_value),
	       retcode,"get-info");

  case sqluinteger :

    sr = SQLGetInfo(connectionHandle,infoName,&uinteger_value,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-info");    

    sql_return(scheme_make_integer_value_from_unsigned(uinteger_value),
	       retcode,"get-info");

  case namedusmallint :

    sr = SQLGetInfo(connectionHandle,infoName,&usmallint_value,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-info");    

    retval = 
      scheme_intern_symbol(findSmallIntName(infoNameString,usmallint_value,
					    namedInfoSmallInts,
					    sizeray(namedInfoSmallInts)));
    sql_return(retval,retcode,"get-info");

  case nameduinteger :

    sr = SQLGetInfo(connectionHandle,infoName,&uinteger_value,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-info");    

    retval = scheme_intern_symbol(findIntegerName(infoNameString,
						  uinteger_value,
						  namedInfoIntegers,
						  sizeray(namedInfoIntegers)));
    sql_return(retval,retcode,"get-info");

  case boolstring :

    sr = SQLGetInfo(connectionHandle,infoName,buffer,sizeray(buffer),&actualLen);
    retcode = checkSQLReturn(sr,"get-info");    

    if (buffer[0] == 'Y' || buffer[0] == 'y') {
      sql_return(scheme_true,retcode,"get-info");
    }
    else if (buffer[0] == 'N' || buffer[0] == 'n') {
      sql_return(scheme_false,retcode,"get-info");
    }
    else {
      scheme_signal_error("sql-get-info: expected 'Y' or 'N', got %s",buffer);
    }

  case string :

    sr = SQLGetInfo(connectionHandle,infoName,buffer,sizeray(buffer),&actualLen);
    retcode = checkSQLReturn(sr,"get-info");    

    sql_return(scheme_make_sized_string(buffer,actualLen,TRUE),
	       retcode,"get-info");

  case bitmask :

    sr = SQLGetInfo(connectionHandle,infoName,&uinteger_value,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-info");    

    sql_return(bitsListFromBitMask(infoNameString,uinteger_value),
	       retcode,"get-info");

  case henv :

    {
    
      SRP_SQL_HENV *retval;

      sr = SQLGetInfo(connectionHandle,infoName,&retEnvHandle,0,&actualLen);

      retcode = checkSQLReturn(sr,"get-info");    

      retval = (SRP_SQL_HENV *)scheme_malloc(sizeof(SRP_SQL_HENV));
      retval->type = sql_henv_type;
      retval->henv = retEnvHandle;

      sql_return((Scheme_Object *)retval,retcode,"get-info");

    }

  case hdbc : 

    { 

      SRP_SQL_HDBC *retval;

      sr = SQLGetInfo(connectionHandle,infoName,&retConnectHandle,0,&actualLen);

      retcode = checkSQLReturn(sr,"get-info");    

      retval = (SRP_SQL_HDBC *)scheme_malloc(sizeof(SRP_SQL_HDBC));
      retval->type = sql_hdbc_type;
      retval->hdbc = retConnectHandle;

      sql_return((Scheme_Object *)retval,retcode,"get-info");

    }

  case hstmt :

    {

      SRP_SQL_HSTMT *retval;

      sr = SQLGetInfo(connectionHandle,infoName,&retStmtHandle,0,&actualLen);

      retcode = checkSQLReturn(sr,"get-info");    

      retval = (SRP_SQL_HSTMT *)scheme_malloc(sizeof(SRP_SQL_HSTMT));
      retval->type = sql_hstmt_type;
      retval->hstmt = retStmtHandle;
      
      sql_return((Scheme_Object *)retval,retcode,"get-info");

    }

#if (ODBCVER >= 0x0300)
  case hdesc :
    
    {

      SRP_SQL_HDESC *retval;

      sr = SQLGetInfo(connectionHandle,infoName,&retDescHandle,0,&actualLen);

      retcode = checkSQLReturn(sr,"get-info");    

      retval = (SRP_SQL_HDESC *)scheme_malloc(sizeof(SRP_SQL_HDESC));
      retval->type = sql_hdesc_type;
      retval->hdesc = retDescHandle;

      sql_return((Scheme_Object *)retval,retcode,"get-info");

    }
#endif

  default :

    scheme_signal_error("get-info-type: invalid info type: %s",intToHexString(infoType));

  }

  return scheme_void;  /* unreachable */
}

Scheme_Object *srp_SQLGetStmtAttr(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("get-stmt-attr");
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER actualLen;
  char *attributeString;
  SQLINTEGER attribute;
  SRP_CONST_TYPE attributeType;
  SQLUINTEGER number;
  SQLUINTEGER *numpointer;
  SQLUSMALLINT *smallnumpointer;
  SRP_NAMED_TYPED_CONSTANT *p;    
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("get-stmt-attr","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("get-stmt-attr","symbol",1,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(attributeString,stmtAttributes);

  if (p == NULL) {
    scheme_signal_error("sql-get-stmt-attr: invalid attribute: %s",
			attributeString);
  }

  attribute = p->val;
  attributeType = p->type;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  /* interpret attribute according to type */

  switch(attributeType) {

  case sqluinteger :

    sr = SQLGetStmtAttr(stmtHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-stmt-attr");

    sql_return(scheme_make_integer_value_from_unsigned(number),
	       retcode,"get-stmt-attr");

  case sqlbool :

    sr = SQLGetStmtAttr(stmtHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-stmt-attr");
    sql_return((number == SQL_FALSE) ? scheme_false : scheme_true,
	       retcode,"get-stmt-attr");

  case nameduinteger :

    sr = SQLGetStmtAttr(stmtHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-stmt-attr");

    retval = scheme_intern_symbol(findIntegerName(attributeString,number,
						  namedStmtAttributes,
						  sizeray(namedStmtAttributes)));

    sql_return(retval,retcode,"get-stmt-attr");
    
  case possiblynameduinteger :

    char *attrName;

    sr = SQLGetStmtAttr(stmtHandle,attribute,&number,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-stmt-attr");

    attrName = findIntegerName(attributeString,number,
			       namedStmtAttributes,
			       sizeray(namedStmtAttributes));

    if (strcmp(attrName,NO_BIT_NAME) == 0) {
      return scheme_make_integer_value_from_unsigned(number);
    }

    sql_return(scheme_intern_symbol(attrName),
	       retcode,"get-stmt-attr");

  case rowstatus :
    
    SRP_SQL_ROW_STATUS *rowStatus;

    sr = SQLGetStmtAttr(stmtHandle,attribute,&smallnumpointer,0,&actualLen);
    retcode = checkSQLReturn(sr,"get-stmt-attr");

    rowStatus = (SRP_SQL_ROW_STATUS *)scheme_malloc(sizeof(SRP_SQL_ROW_STATUS));
    scheme_dont_gc_ptr(rowStatus);
    rowStatus->type = sql_row_status_type;
    rowStatus->numRows = 1; /* can we do better? */ 
    rowStatus->values = smallnumpointer;
    rowStatus->usesSchemeStorage = FALSE;

    sql_return((Scheme_Object *)rowStatus,retcode,"get-stmt-attr");

  case sqlboxeduint :

    { SRP_SQL_BOXED_UINT *retval;

      sr = SQLGetStmtAttr(stmtHandle,attribute,&numpointer,0,&actualLen);
      retcode = checkSQLReturn(sr,"get-stmt-attr");

      retval = (SRP_SQL_BOXED_UINT *)scheme_malloc(sizeof(SRP_SQL_BOXED_UINT));
      retval->type = sql_boxed_uint_type;
      retval->pointer = numpointer;

      sql_return((Scheme_Object *)retval,retcode,"get-stmt-attr");
    }

  case hdesc :

    { SRP_SQL_HDESC *retval;

      sr = SQLGetStmtAttr(stmtHandle,attribute,&number,0,&actualLen);
      retcode = checkSQLReturn(sr,"get-stmt-attr");

      retval = (SRP_SQL_HDESC *)scheme_malloc(sizeof(SRP_SQL_HDESC));
      retval->type = sql_hdesc_type;

      switch(attribute) {

      case SQL_ATTR_APP_PARAM_DESC :
	retval->descType = APD;
	break;
      case SQL_ATTR_APP_ROW_DESC :
	retval->descType = ARD;
	break;
      case SQL_ATTR_IMP_PARAM_DESC :
	retval->descType = IPD;
	break;
      case SQL_ATTR_IMP_ROW_DESC :
	retval->descType = IRD;
	break;
      default :
	scheme_signal_error("sql-get-stmt-attribute: unknown hdesc type");
      }
      
      retval->hdesc = (SQLHDESC)number;

      sql_return((Scheme_Object *)retval,retcode,"get-stmt-attr");
    }

  case opparms :  

    { SRP_SQL_OP_PARMS *retval;
      SQLUINTEGER paramSetSize;

      sr = SQLGetStmtAttr(stmtHandle,SQL_ATTR_PARAMSET_SIZE,&paramSetSize,0,&actualLen);
      checkSQLReturn(sr,"get-stmt-attr");
      
      sr = SQLGetStmtAttr(stmtHandle,attribute,&smallnumpointer,0,&actualLen);
      retcode = checkSQLReturn(sr,"get-stmt-attr");
    
      retval = (SRP_SQL_OP_PARMS *)scheme_malloc(sizeof(SRP_SQL_OP_PARMS));
      retval->type = sql_op_parms_type;
      retval->paramSetSize = paramSetSize;
      retval->values = smallnumpointer;

      sql_return((Scheme_Object *)retval,retcode,"get-stmt-attr");
    }

  default :

    scheme_signal_error("sql-get-stmt-attr: invalid attribute type: %s",
			intToHexString(attributeType));

  }

  return scheme_void; /* unreachable */

#endif
}


Scheme_Object *srp_SQLGetStmtOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  char *optionString;
  SQLUSMALLINT option;
  SRP_CONST_TYPE optionType;
  SQLUINTEGER number;
  SRP_NAMED_TYPED_CONSTANT *p;    
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("get-stmt-option","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("get-stmt-option","symbol",1,argc,argv);
  }

  optionString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(optionString,stmtOptions);

  if (p == NULL) {
    scheme_signal_error("sql-get-stmt-option: invalid option: %s",
			optionString);
  }

  option = (SQLUSMALLINT)(p->val);
  optionType = p->type;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);


  /* interpret option according to type */

  switch(optionType) {

  case sqluinteger :

    sr = SQLGetStmtOption(stmtHandle,option,&number);
    retcode = checkSQLReturn(sr,"get-stmt-option");

    sql_return(scheme_make_integer_value_from_unsigned(number),
	       retcode,"get-stmt-option");

  case nameduinteger :

    sr = SQLGetStmtOption(stmtHandle,option,&number);
    retcode = checkSQLReturn(sr,"get-stmt-option");

    retval = 
      scheme_intern_symbol(findIntegerName(optionString,number,
					   namedStmtOptions,
					   sizeray(namedStmtOptions)));
    sql_return(retval,retcode,"get-stmt-option");
    
  case possiblynameduinteger :

    char *attrName;

    sr = SQLGetStmtOption(stmtHandle,option,&number);
    retcode = checkSQLReturn(sr,"get-stmt-option");

    attrName = findIntegerName(optionString,number,
			       namedStmtOptions,
			       sizeray(namedStmtOptions));

    if (strcmp(attrName,NO_BIT_NAME) == 0) {
      return scheme_make_integer_value_from_unsigned(number);
    }

    sql_return(scheme_intern_symbol(attrName),retcode,"get-stmt-option");

  default :

    scheme_signal_error("get-stmt-option: invalid option type: %s",
			intToHexString(optionType));

  }

  return scheme_void; /* unreachable */

}

Scheme_Object *srp_SQLGetTypeInfo(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT dataType;
  char *dataTypeString;
  SRP_NAMED_SMALL_CONSTANT *p;    
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("get-type-info","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("get-type-info","symbol",1,argc,argv);
  }

  dataTypeString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(dataTypeString,SQLDataTypes);

  if (p == NULL) {
    scheme_signal_error("sql-get-type-info: invalid data type: %s",
			dataTypeString);
  }

  dataType = p->val;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLGetTypeInfo(stmtHandle,dataType);

  retcode = checkSQLReturn(sr,"get-type-info");

  sql_return(argv[0],retcode,"get-type-info");
}

Scheme_Object *srp_SQLNumResultCols(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT colCount;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("num-result-cols","sql-hstmt",0,argc,argv);
  }
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLNumResultCols(stmtHandle,&colCount);

  retcode = checkSQLReturn(sr,"num-result-cols");

  sql_return(scheme_make_integer(colCount),retcode,"num-result-cols");
}

Scheme_Object *srp_SQLParamData(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLPOINTER buffer;
  RETURN_CODE retcode;
  SRP_SQL_BUFFER *retval;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("param-data","sql-hstmt",0,argc,argv);
  }
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLParamData(stmtHandle,&buffer);

  retcode = checkSQLReturn(sr,"param-data");

  retval = lookupBufferFromAddress(buffer);

  if (retval == NULL) {
    scheme_signal_error("sql-param-data: no bound sql-buffer");
  }

  sql_return((Scheme_Object *)retval,retcode,"param-data");
}

Scheme_Object *srp_SQLPrepare(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *text;
  SQLINTEGER textLen;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("prepare","sql-hstmt",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("prepare","string",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  text = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  textLen = SCHEME_STRLEN_VAL(argv[1]);

  sr = SQLPrepare(stmtHandle,text,textLen);

  retcode = checkSQLReturn(sr,"prepare");

  sql_return(argv[0],retcode,"prepare");
}

Scheme_Object *srp_SQLPutData(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("put-data","sql-hstmt",0,argc,argv);
  }
  
  if (SQL_BUFFERP(argv[1]) == FALSE) {
    scheme_wrong_type("put-data","sql-buffer",1,argc,argv);
  }
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[1]);
  bufferLen = SQL_BUFFER_LEN(argv[1]);
  
  sr = SQLPutData(stmtHandle,buffer,bufferLen);

  retcode = checkSQLReturn(sr,"put-data");

  sql_return(argv[0],retcode,"put-data");
}

Scheme_Object *srp_SQLRowCount(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER rowCount;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("row-count","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  sr = SQLRowCount(stmtHandle,&rowCount);

  retcode = checkSQLReturn(sr,"row-count");

  if (rowCount >= 0) {
    sql_return(scheme_make_integer_value((long)rowCount),
	       retcode,"row-count");
  }

  sql_return(scheme_intern_symbol("sql-row-count-unavailable"),
	     retcode,"row-count");
}

Scheme_Object *srp_SQLSetConnectAttr(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("set-connect-attr");
#else
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLINTEGER attribute;
  char *attributeString;
  SRP_CONST_TYPE attributeType;
  SQLUINTEGER val;
  char *attributeValString;
  SRP_NAMED_TYPED_CONSTANT *p;    
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("set-connect-attr","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE && SCHEME_INTP(argv[1]) == FALSE) {
    scheme_wrong_type("set-connect-attr","sym or int",1,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(attributeString,settableConnectionAttributes);

  if (p == NULL) {
    scheme_signal_error("sql-set-connect-attr: invalid connection attribute: %s",
			attributeString);
  }

  attribute = p->val;
  attributeType = p->type;

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  switch(attributeType) {

  case sqlbool :

    SQLUINTEGER boolVal;

    /* treat non-#f as true */

    boolVal = (argv[2] == scheme_false) ? SQL_FALSE : SQL_TRUE;

    sr = SQLSetConnectAttr(connectionHandle,attribute,(SQLPOINTER)boolVal,0);
    break;

  case sqluinteger :

    SQLUINTEGER number;

    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) { 
      scheme_wrong_type("set-connect-attr","exact integer",2,argc,argv);
    }

    if (scheme_get_unsigned_int_val(argv[2],&number) == 0) {
      scheme_signal_error("sql-set-connect-attr: numeric value too big");
    }

    sr = SQLSetConnectAttr(connectionHandle,attribute,(SQLPOINTER)number,0);
    break;

  case nameduinteger :

    if (SCHEME_SYMBOLP(argv[2]) == FALSE) { 
      scheme_wrong_type("set-connect-attr","symbol",2,argc,argv);
    }

    attributeValString = SCHEME_SYM_VAL(argv[2]);

    val = findNamedInteger(attributeString,attributeValString,
			   namedConnectAttrIntegers,
			   sizeray(namedConnectAttrIntegers));

    if (val == (SQLUINTEGER)-1) {
      scheme_signal_error("sql-set-connect-attr: unknown attribute value: %s",
			  attributeValString);
    }

    sr = SQLSetConnectAttr(connectionHandle,attribute,(SQLPOINTER)val,0);
    break;

  case string :

    SQLCHAR *s;
    SQLINTEGER len;

    if (SCHEME_STRINGP(argv[2]) == FALSE) { 
      scheme_wrong_type("set-connect-attr","string",2,argc,argv);
    }

    s = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
    len = SCHEME_STRLEN_VAL(argv[2]);

    sr = SQLSetConnectAttr(connectionHandle,attribute,s,len);
    break;

  default :

    sr = 0; 
    scheme_signal_error("sql-set-connect-attr: unknown attribute type: %s",
			intToHexString(attributeType));

  }

  retcode = checkSQLReturn(sr,"set-connect-attr");
  sql_return(argv[0],retcode,"set-connect-attr");

#endif
}


Scheme_Object *srp_SQLSetConnectOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT option;
  char *optionString;
  SRP_CONST_TYPE optionType;
  SQLUINTEGER val;
  char *optionValString;
  SRP_NAMED_TYPED_CONSTANT *p;    
  RETURN_CODE retcode;
  
  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("set-connect-option","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("set-connect-option","symbol",1,argc,argv);
  }

  optionString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(optionString,connectionOptions);

  if (p == NULL) {
    scheme_signal_error("set-connect-option: invalid connection option: %s",
			optionString);
  }

  /* p->val is an SQLINTEGER, but all the connection options
     are between 101 and 112, so this cast is OK */

  option = (SQLUSMALLINT)(p->val);
  optionType = p->type;

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  switch(optionType) {

  case sqluinteger :

    SQLUINTEGER number;

    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) { 
      scheme_wrong_type("set-connect-option","exact integer",2,argc,argv);
    }

    if (scheme_get_unsigned_int_val(argv[2],&number) == 0) {
      scheme_signal_error("sql-set-connect-option: numeric value too big");
    }

    sr = SQLSetConnectOption(connectionHandle,option,number);
    break;

  case nameduinteger :

    if (SCHEME_SYMBOLP(argv[2]) == FALSE) { 
      scheme_wrong_type("set-connect-option","symbol",2,argc,argv);
    }

    optionValString = SCHEME_SYM_VAL(argv[2]);

    val = findNamedInteger(optionString,optionValString,
			   namedConnectOptionIntegers,
			   sizeray(namedConnectOptionIntegers));

    if (val == (SQLUINTEGER)-1) {
      scheme_signal_error("sql-set-connect-option: unknown option value: %s",
			  optionValString);
    }

    sr = SQLSetConnectOption(connectionHandle,option,val);
    break;

  case string :

    SQLCHAR *s;

    if (SCHEME_STRINGP(argv[2]) == FALSE) { 
      scheme_wrong_type("set-connect-option","string",2,argc,argv);
    }

    s = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);

    sr = SQLSetConnectOption(connectionHandle,option,(SQLUINTEGER)s);
    break;

  default :

    sr = 0;
    scheme_signal_error("sql-set-connect-option: unknown option type: %s",
			intToHexString(optionType));

  }

  retcode = checkSQLReturn(sr,"set-connect-option");

  sql_return(scheme_void,retcode,"set-connect-option");

}

Scheme_Object *srp_SQLSetCursorName(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *name;
  SQLSMALLINT nameLen;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("set-cursor-name","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("set-cursor-name","string",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  name = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  nameLen = SCHEME_STRLEN_VAL(argv[1]);

  sr = SQLSetCursorName(stmtHandle,name,nameLen);

  retcode = checkSQLReturn(sr,"set-cursor-name");

  sql_return(argv[0],retcode,"set-cursor-name");
}

Scheme_Object *srp_SQLSetDescField(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("set-desc-field");
#else
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLSMALLINT fieldId;
  SQLINTEGER intVal;
  SQLUINTEGER uintVal;
  SRP_CONST_TYPE fieldType;
  char *fieldIdString;
  SRP_NAMED_TYPED_CONSTANT *p;    
  RETURN_CODE retcode;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("set-desc-field","sql-hdesc",0,argc,argv);    
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("set-desc-field","small-int",1,argc,argv);    
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("set-desc-field","symbol",2,argc,argv);    
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedTypedConstSearch(fieldIdString,fieldDescriptors);

  if (p == NULL) {
      scheme_signal_error("sql-set-desc-field: invalid field id: %s",
			  fieldIdString);

  }

  fieldId = (SQLSMALLINT)(p->val);
  fieldType = p->type;

  descHandle = SQL_HDESC_VAL(argv[0]);
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  switch(fieldType) {

  case sqlsmallint :

    if (isSmallInt(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","small-int",3,argc,argv);    
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)SCHEME_INT_VAL(argv[3]),0);

    break;

  case sqlinteger :

    if (SCHEME_EXACT_INTEGERP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","exact integer",3,argc,argv);    
    }

    if (scheme_get_int_val(argv[3],&intVal) == 0) {
      scheme_signal_error("sql-set-desc-field: exact integer value too large");
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)intVal,0);

    break;
 
  case sqluinteger :

    if (SCHEME_EXACT_INTEGERP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","unsigned int",3,argc,argv);    
    }

    if (scheme_get_unsigned_int_val(argv[3],&uintVal) == 0) {
      scheme_signal_error("sql-set-desc-field: exact integer value too large");
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,(SQLPOINTER)uintVal,0);
    
    break;

  case string :

    if (SCHEME_STRINGP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","string",3,argc,argv);    
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)SCHEME_STR_VAL(argv[3]),
			 SCHEME_STRLEN_VAL(argv[3]));
    
    break;

  case sqlbool :

    intVal = (argv[3] == scheme_false) ? SQL_FALSE : SQL_TRUE;

    sr = SQLSetDescField(descHandle,recNumber,fieldId,(SQLPOINTER)intVal,0);

    break;

  case namedinteger :

    if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","symbol",3,argc,argv);    
    }

    uintVal = findNamedInteger(fieldIdString,SCHEME_SYM_VAL(argv[3]),
			       namedFieldDescriptors,sizeray(namedFieldDescriptors));

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)uintVal,0);

    break;

  case sqlbuffer :

    if (SQL_BUFFERP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","sql-buffer",3,argc,argv);    
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)SQL_BUFFER_VAL(argv[3]),sizeof(SQLPOINTER));

    break;

  case sqlindicator :

    if (SQL_INDICATORP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","sql-indicator",3,argc,argv);    
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)&SQL_INDICATOR_VAL(argv[3]),
			 sizeof(SQLPOINTER));

    break;

  case arraystatus :

    if (SQL_ARRAY_STATUSP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","sql-array-status",3,argc,argv);
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)SQL_ARRAY_STATUS_VAL(argv[3]),
			 sizeof(SQLPOINTER));


  case bindingoffset :

    if (SQL_BINDING_OFFSETP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","sql-binding-offset",3,argc,argv);
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)SQL_BINDING_OFFSET_VAL(argv[3]),
			 sizeof(SQLPOINTER));

    break;

  case rowsprocessed :

    if (SQL_ROWS_PROCESSEDP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","sql-rows-processed",3,argc,argv);
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)SQL_ROWS_PROCESSED_VAL(argv[3]),
			 sizeof(SQLPOINTER));

    break;

  case octetlength :

    if (SQL_OCTET_LENGTHP(argv[3]) == FALSE) {
      scheme_wrong_type("set-desc-field","sql-octet-length",3,argc,argv);
    }

    sr = SQLSetDescField(descHandle,recNumber,fieldId,
			 (SQLPOINTER)SQL_OCTET_LENGTH_VAL(argv[3]),
			 sizeof(SQLPOINTER));

  default :

    sr = 0;
    scheme_signal_error("sql-set-desc-field: unknown field type %s",intToHexString(fieldType));
  }

  retcode = checkSQLReturn(sr,"set-desc-field");

  sql_return(argv[0],retcode,"set-desc-field");

#endif
}

Scheme_Object *srp_SQLSetDescRec(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("set-desc-rec");
#else
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLSMALLINT type;
  char *typeString;
  SQLSMALLINT subType;
  char *subTypeString;
  SQLINTEGER octetLen;
  SQLSMALLINT precision;
  SQLSMALLINT scale;
  SQLPOINTER buffer;
  SRP_NAMED_SMALL_CONSTANT *p;    
  SQLINTEGER *length;
  SQLINTEGER *indicator;
  RETURN_CODE retcode;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("set-desc-rec","sql-hdesc",0,argc,argv);    
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("set-desc-rec","small-int",1,argc,argv);    
  }
  
  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("set-desc-rec","symbol",1,argc,argv);    
  }

  if (isSmallInt(argv[3]) == FALSE) {
    scheme_wrong_type("set-desc-rec","small-int",3,argc,argv);    
  }

  if (isSmallInt(argv[4]) == FALSE) {
    scheme_wrong_type("set-desc-rec","small-int",4,argc,argv);    
  }

  if (SQL_BUFFERP(argv[5]) == FALSE) {
    scheme_wrong_type("set-desc-rec","sql-buffer",5,argc,argv);    
  }

  if (SQL_LENGTHP(argv[6]) == FALSE) {
    scheme_wrong_type("set-desc-rec","sql-length",6,argc,argv);    
  }

  if (SQL_INDICATORP(argv[7]) == FALSE) {
    scheme_wrong_type("set-desc-rec","sql-indicator",7,argc,argv);    
  }

  typeString = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(typeString,SQLDataTypes);

  if (p == NULL) {
      scheme_signal_error("sql-set-desc-rec: invalid data type: %s",
			  typeString);
  }

  type = p->val;

  if (type == SQL_DATETIME || type == SQL_INTERVAL) {

    if (SCHEME_INTP(argv[8]) == FALSE) {
      scheme_wrong_type("set-desc-rec","integer",3,argc,argv);    
    }

    subTypeString = SCHEME_SYM_VAL(argv[8]);

    p = namedSmallConstSearch(subTypeString,datetimeIntervalCodes);
 
    if (p == NULL) {
      scheme_signal_error("sql-set-desc-rec: invalid date/time interval code: %s",
			  subTypeString);
      
    }

    subType = p->val;
  }
  else {
    subType = 0;
  }

  descHandle = SQL_HDESC_VAL(argv[0]);
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);
  precision = (SQLSMALLINT)SCHEME_INT_VAL(argv[3]);
  scale = (SQLSMALLINT)SCHEME_INT_VAL(argv[4]);
  octetLen = SQL_BUFFER_LEN(argv[5]);
  buffer = SQL_BUFFER_VAL(argv[5]);
  length = &SQL_LENGTH_VAL(argv[6]);
  indicator = SQL_INDICATOR_VAL(argv[7]);
  
  sr = SQLSetDescRec(descHandle,recNumber,type,subType,
		     octetLen,precision,scale,buffer,
		     length,indicator);
  
  retcode = checkSQLReturn(sr,"set-desc-rec");

  sql_return(argv[0],retcode,"set-desc-rec");

#endif
}


Scheme_Object *srp_SQLSetEnvAttr(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("set-env-attr");
#else
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLINTEGER attribute;
  SRP_CONST_TYPE attributeType;
  char *attributeString;
  SRP_NAMED_TYPED_CONSTANT *p;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("set-env-attr","sql-henv",0,argc,argv);    
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) { 
    scheme_wrong_type("set-env-attr","symbol",1,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(attributeString,envAttributes);

  if (p == NULL) {
    scheme_signal_error("sql-get-env-attr: invalid environment attribute: %s",
			attributeString);
  }

  attribute = p->val;
  attributeType = p->type;

  envHandle = SQL_HENV_VAL(argv[0]);

  switch(attributeType) {

  case nameduinteger :

    SQLUINTEGER val;
    char *attributeValString;

    if (SCHEME_SYMBOLP(argv[2]) == FALSE) { 
      scheme_wrong_type("set-env-attr","symbol",2,argc,argv);
    }

    attributeValString = SCHEME_SYM_VAL(argv[2]);

    val = findNamedInteger(attributeString,attributeValString,
			   namedEnvAttrIntegers,
			   sizeray(namedEnvAttrIntegers));

    if (val == (SQLUINTEGER)-1) {
      scheme_signal_error("sql-set-env-attr: unknown attribute value: %s",
			  attributeValString);
    }

    sr = SQLSetEnvAttr(envHandle,attribute,(SQLPOINTER)val,0);
    break;

  case sqlbool :

    SQLUINTEGER boolVal;

    /* treat non-#f as true */

    boolVal = (argv[2] == scheme_false) ? SQL_FALSE : SQL_TRUE;

    sr = SQLSetEnvAttr(envHandle,attribute,(SQLPOINTER)boolVal,0);
    break;

  default :

    sr = 0;
    scheme_signal_error("sql-set-env-attr: unknown attribute type: %s",
		      intToHexString(attributeType));

  }

  retcode = checkSQLReturn(sr,"set-env-attr");
  sql_return(argv[0],retcode,"set-env-attr");
#endif
}

Scheme_Object *srp_SQLSetParam(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT paramNumber;
  SQLSMALLINT CDataType;
  SQLSMALLINT paramType;
  char *paramTypeString;
  SQLUINTEGER precision;
  SQLSMALLINT scale;
  SQLPOINTER buffer;
  SQLINTEGER *indicator;
  SRP_NAMED_SMALL_CONSTANT *p;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("set-param","sql-hstmt",0,argc,argv);
  } 

  if (SCHEME_INTP(argv[1]) == FALSE) {
    scheme_wrong_type("set-param","integer",1,argc,argv);
  } 

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("set-param","symbol",2,argc,argv);
  } 

  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("set-param","sql-buffer",3,argc,argv);
  }

  if (SQL_INDICATORP(argv[4]) == FALSE) {
    scheme_wrong_type("set-param","sql-indicator",4,argc,argv);
  }

  paramTypeString = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(paramTypeString,SQLDataTypes);

  if (p == NULL) {
      scheme_signal_error("sql-set-param: invalid parameter type: %s",
			  paramTypeString);
  }

  paramType = p->val;

  CDataType = SQL_BUFFER_CTYPE(argv[3]);

  switch(paramType) {

  case SQL_DECIMAL:
  case SQL_NUMERIC:
  case SQL_TIMESTAMP:

    if (argc != 7) {
      scheme_wrong_count("sql-set-param",6,6,argc,argv);
    }

    if (isUnsignedInt(argv[5]) == FALSE) {
      scheme_wrong_type("set-param","integer",5,argc,argv);
    }

    if (isSmallInt(argv[6]) == FALSE) {
      scheme_wrong_type("set-param","small-int",6,argc,argv);
    }

    scheme_get_unsigned_int_val(argv[5],&precision);

    if (paramType == SQL_TIMESTAMP && (precision < 16 || precision > 26)) {
      scheme_signal_error("Timestamp precision must be between 16 and 26");
    }

    scale = (SQLSMALLINT)SCHEME_INT_VAL(argv[6]);

    break;

  default :
    
    precision = SQL_BUFFER_LEN(argv[3]);
    scale = 0L;

    break;
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[3]);
  indicator = SQL_INDICATOR_VAL(argv[4]);

  sr = SQLSetParam(stmtHandle,paramNumber,CDataType,paramType,
		   precision,scale,buffer,indicator);

  retcode = checkSQLReturn(sr,"set-param");

  sql_return(argv[0],retcode,"set-param");
}


Scheme_Object *srp_SQLSetStmtAttr(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("set-stmt-attr");
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER attribute;
  char *attributeString;
  SQLINTEGER actualLen;
  SQLUINTEGER val;
  char *attributeValString;
  SRP_NAMED_TYPED_CONSTANT *p;    
  SRP_CONST_TYPE attributeType;
  SQLUINTEGER number;
  SQLUINTEGER boolVal;
  SQLUINTEGER paramSetSize;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("set-stmt-attr","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("set-stmt-attr","symbol",1,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(attributeString,stmtAttributes);

  if (p == NULL) {
    scheme_signal_error("sql-set-stmt-attr: invalid statement attribute: %s",
			attributeString);
  }

  attribute = p->val;
  attributeType = p->type;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  switch(attributeType) {

  case sqluinteger :

    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-attr","exact integer",2,argc,argv);
    }

    if (scheme_get_unsigned_int_val(argv[2],&number) == 0) {
      scheme_signal_error("Numeric argument too large");
    }

    sr = SQLSetStmtAttr(stmtHandle,attribute,(SQLPOINTER)number,0);
    break;

  case sqlbool :

    if (argv[2] == scheme_false) {
      boolVal = SQL_FALSE;
    }
    else {
      boolVal = SQL_TRUE;
    }

    sr = SQLSetStmtAttr(stmtHandle,attribute,(SQLPOINTER)boolVal,0);
    break;

  case nameduinteger :

    if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-attr","symbol",2,argc,argv);
    }

    attributeValString = SCHEME_SYM_VAL(argv[2]);

    val = findNamedInteger(attributeString,attributeValString,
			   namedStmtAttributes,sizeray(namedStmtAttributes));

    if (val == (SQLUINTEGER)-1) {
      scheme_signal_error("sql-set-stmt-attr: unknown attribute value: %s",
			  attributeValString);
    }

    sr = SQLSetStmtAttr(stmtHandle,attribute,(SQLPOINTER)val,0);
    break;

  case possiblynameduinteger :

    if (SCHEME_SYMBOLP(argv[2])) {

      attributeValString = SCHEME_SYM_VAL(argv[2]);

      val = findNamedInteger(attributeString,attributeValString,
			     namedStmtAttributes,sizeray(namedStmtAttributes));

      if (val == (SQLUINTEGER)-1) {
	scheme_signal_error("sql-set-stmt-attr: unknown attribute value: %s",
			    attributeValString);
      }

      sr = SQLSetStmtAttr(stmtHandle,attribute,(SQLPOINTER)val,0);
    }
    else if (SCHEME_EXACT_INTEGERP(argv[2])) {
      if (scheme_get_unsigned_int_val(argv[2],&number) == 0) {
	scheme_signal_error("Numeric argument too large");
      }

      sr = SQLSetStmtAttr(stmtHandle,attribute,(SQLPOINTER)number,0);
    }
    else {
      scheme_wrong_type("set-stmt-attr","int or symbol",2,argc,argv);
    }

    break;

  case rowstatus :
    
    if (SQL_ROW_STATUSP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-attr","sql-row-status",2,argc,argv);
    }

    sr = SQLSetStmtAttr(stmtHandle,attribute,SQL_ROW_STATUS_VAL(argv[2]),0);
    break;

  case sqlboxeduint :

    if (SQL_BOXED_UINTP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-attr","sql-boxed-uint",2,argc,argv);
    }

    sr = SQLSetStmtAttr(stmtHandle,attribute,SQL_BOXED_UINT_VAL(argv[2]),0);
    break;

  case hdesc :

    if (SQL_HDESCP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-attr","sql-hdesc",2,argc,argv);
    }

    sr = SQLSetStmtAttr(stmtHandle,attribute,(SQLPOINTER)SQL_HDESC_VAL(argv[2]),0);
    break;

  case opparms :  

    if (SQL_OP_PARMSP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-attr","sql-op-parms",2,argc,argv);
    }

    sr = SQLGetStmtAttr(stmtHandle,SQL_ATTR_PARAMSET_SIZE,&paramSetSize,
			0,&actualLen);
    checkSQLReturn(sr,"get-stmt-attr");

    if (paramSetSize != SQL_OP_PARMS_LEN(argv[2])) {
      scheme_signal_error("Lengnth of operational parameters does not "
			  "match current number of parameters");

    } 
      
    sr = SQLSetStmtAttr(stmtHandle,attribute,SQL_OP_PARMS_VAL(argv[2]),0);
    break;

  default :

    sr = 0;
    scheme_signal_error("sql-set-stmt-attr: invalid attribute type: %s",
			intToHexString(attributeType));
  }

  retcode = checkSQLReturn(sr,"set-stmt-attr");

  sql_return(argv[0],retcode,"set-stmt-attr");

#endif
}


Scheme_Object *srp_SQLSetStmtOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT option;
  char *optionString;
  SQLUINTEGER val;
  char *optionValString;
  SRP_NAMED_TYPED_CONSTANT *p;    
  SRP_CONST_TYPE optionType;
  SQLUINTEGER number;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("set-stmt-option","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("set-stmt-option","symbol",1,argc,argv);
  }

  optionString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(optionString,stmtOptions);

  if (p == NULL) {
    scheme_signal_error("sql-set-stmt-option: invalid statement option: %s",
			optionString);
  }

  option = (SQLUSMALLINT)(p->val);
  optionType = p->type;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  switch(optionType) {

  case sqluinteger :

    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-option","exact integer",2,argc,argv);
    }

    if (scheme_get_unsigned_int_val(argv[2],&number) == 0) {
      scheme_signal_error("Numeric argument too large");
    }

    sr = SQLSetStmtOption(stmtHandle,option,number);
    break;

  case nameduinteger :

    if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
      scheme_wrong_type("set-stmt-option","symbol",2,argc,argv);
    }

    optionValString = SCHEME_SYM_VAL(argv[2]);

    val = findNamedInteger(optionString,optionValString,
			   namedStmtOptions,sizeray(namedStmtOptions));

    if (val == (SQLUINTEGER)-1) {
      scheme_signal_error("set-stmt-option: unknown option value: %s",
			  optionValString);
    }

    sr = SQLSetStmtOption(stmtHandle,option,val);
    break;

  case possiblynameduinteger :

    if (SCHEME_SYMBOLP(argv[2])) {

      optionValString = SCHEME_SYM_VAL(argv[2]);

      val = findNamedInteger(optionString,optionValString,
			     namedStmtOptions,sizeray(namedStmtOptions));

      if (val == (SQLUINTEGER)-1) {
	scheme_signal_error("sql-set-stmt-option: unknown option value: %s",
			    optionValString);
      }

      sr = SQLSetStmtOption(stmtHandle,option,val);
    }
    else if (SCHEME_EXACT_INTEGERP(argv[2])) {
      if (scheme_get_unsigned_int_val(argv[2],&number) == 0) {
	scheme_signal_error("Numeric argument too large");
      }

      sr = SQLSetStmtOption(stmtHandle,option,number);
    }
    else {
      scheme_wrong_type("set-stmt-option","int or symbol",2,argc,argv);
    }

    break;


  default :

    sr = 0;
    scheme_signal_error("sql-set-stmt-option: invalid option type: %s",
			intToHexString(optionType));

  }

  retcode = checkSQLReturn(sr,"set-stmt-option");

  sql_return(scheme_void,retcode,"set-stmt-option");

}

Scheme_Object *srp_SQLSpecialColumns(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT idType;
  char *idTypeString;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLSMALLINT scope;
  char *scopeString;
  SQLSMALLINT nullable;
  char *nullableString;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("special-columns","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("special-columns","symbol",1,argc,argv);
  }

  for (i = 2; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("special-columns","string",i,argc,argv);
    }
  }

  for (i = 5; i <= 6; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("special-columns","symbol",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  idTypeString = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(idTypeString,"sql-best-rowid") == 0) {
    idType = SQL_BEST_ROWID;
  }
  else if (stricmp(idTypeString,"sql-rowver") == 0) {
    idType = SQL_ROWVER;
  }
  else {
    scheme_signal_error("sql-special-columns: invalid identifier type: %s",
			idTypeString);
  }

  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[2]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[3]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[4]);

  scopeString = SCHEME_SYM_VAL(argv[5]);

  if (stricmp(scopeString,"sql-scope-currow") == 0) {
    scope = SQL_SCOPE_CURROW;
  }
  else if (stricmp(scopeString,"sql-scope-transaction") == 0) {
    scope = SQL_SCOPE_TRANSACTION;
  }
  else if (stricmp(scopeString,"sql-scope-session") == 0) {
    scope = SQL_SCOPE_SESSION;
  }
  else {
    scheme_signal_error("sql-special-columns: invalid scope: %s",
			scopeString);
  }

  nullableString = SCHEME_SYM_VAL(argv[6]);

  if (stricmp(nullableString,"sql-no-nulls") == 0) {
    nullable = SQL_NO_NULLS;
  }
  else if (stricmp(nullableString,"sql-nullable") == 0) {
    nullable = SQL_NULLABLE;
  }
  else {
    scheme_signal_error("sql-special-columns: invalid nullable: %s",
			nullableString);
  }

  sr = SQLSpecialColumns(stmtHandle,idType,
			 catalogName,catalogNameLen,
			 schemaName,schemaNameLen,
			 tableName,tableNameLen,
			 scope,nullable);

  retcode = checkSQLReturn(sr,"special-columns");

  sql_return(argv[0],retcode,"special-columns");
}

Scheme_Object *srp_SQLStatistics(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLSMALLINT unique;
  char *uniqueString;
  SQLSMALLINT reserved;
  char *reservedString;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("statistics","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("statistics","string",i,argc,argv);
    }
  }

  for (i = 4; i <= 5; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("statistics","symbol",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  uniqueString = SCHEME_SYM_VAL(argv[4]);

  if (stricmp(uniqueString,"sql-index-unique") == 0) {
    unique = SQL_INDEX_UNIQUE;
  }
  else if (stricmp(uniqueString,"sql-index-all") == 0) {
    unique = SQL_INDEX_ALL;
  }
  else {
    scheme_signal_error("sql-statistics: invalid uniqueness specification: %s",
			uniqueString);
  }

  reservedString = SCHEME_SYM_VAL(argv[5]);

  if (stricmp(reservedString,"sql-ensure") == 0) {
    reserved = SQL_ENSURE;
  }
  else if (stricmp(reservedString,"sql-quick") == 0) {
    reserved = SQL_QUICK;
  }
  else {
    scheme_signal_error("sql-statistics: invalid reserved specification: %s",
			reservedString);
  }

  sr = SQLStatistics(stmtHandle,
		     catalogName,catalogNameLen,
		     schemaName,schemaNameLen,
		     tableName,tableNameLen,
		     unique,reserved);

  retcode = checkSQLReturn(sr,"statistics");

  sql_return(argv[0],retcode,"statistics");
}

Scheme_Object *srp_SQLTables(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLCHAR *tableType;
  SQLSMALLINT tableTypeLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("tables","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 2; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE
#if ODBCVER >= 0x0300
	&& SCHEME_SYMBOLP(argv[i]) == FALSE
#endif
	) {
      scheme_wrong_type("tables","string or symbol",i,argc,argv);
    }
  }

  if (SCHEME_STRINGP(argv[3]) == FALSE) {
    scheme_wrong_type("tables","string or symbol",i,argc,argv);
  }

  if (SCHEME_STRINGP(argv[4]) == FALSE 
#if ODBVER >= 0x0300
      && SCHEME_SYMBOLP(argv[4]) == FALSE
#endif
      ) {
    scheme_wrong_type("tables","string or symbol",4,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

#if (ODBCVER >= 0x0300)
  if (SCHEME_SYMBOLP(argv[1])) {
    char *catalogNameString;

    catalogNameString = SCHEME_SYM_VAL(argv[1]);
    if (stricmp(catalogNameString,"sql-all-catalogs") == 0) {
      catalogName = (SQLCHAR *)SQL_ALL_CATALOGS;
      catalogNameLen = strlen((const char *)catalogName);
    }
    else {
      scheme_signal_error("sql-tables: invalid catalog name: %s",catalogNameString);
    }
  }
  else {
    catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
    catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);
  }
#else
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);
#endif

#if (ODBCVER >= 0x0300)
  if (SCHEME_SYMBOLP(argv[2])) {
    char *schemaNameString;

    schemaNameString = SCHEME_SYM_VAL(argv[2]);
    if (stricmp(schemaNameString,"sql-all-schemas") == 0) {
      schemaName = (SQLCHAR *)SQL_ALL_SCHEMAS;
      schemaNameLen = strlen((const char *)schemaName);
    }
    else {
      scheme_signal_error("sql-tables: invalid schema name: %s",schemaNameString);
    }
  }
  else {
    schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
    schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);
  }
#else 
  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);
#endif

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

#if (ODBCVER >= 0x0300)
  if (SCHEME_SYMBOLP(argv[4])) {
    char *tableTypeString;

    tableTypeString = SCHEME_SYM_VAL(argv[4]);
    if (stricmp(tableTypeString,"sql-all-table-types") == 0) {
      tableType = (SQLCHAR *)SQL_ALL_TABLE_TYPES;
      tableTypeLen = strlen((const char *)tableType);
    }
    else {
      scheme_signal_error("sql-tables: invalid schema name: %s",tableTypeString);
    }
  }
  else {
    tableType = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
    tableTypeLen = SCHEME_STRLEN_VAL(argv[4]);
  }
#else
  tableType = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  tableTypeLen = SCHEME_STRLEN_VAL(argv[4]);
#endif

  sr = SQLTables(stmtHandle,
		 catalogName,catalogNameLen,
		 schemaName,schemaNameLen,
		 tableName,tableNameLen,
		 tableType,tableTypeLen);

  retcode = checkSQLReturn(sr,"tables");

  sql_return(argv[0],retcode,"tables");

}

Scheme_Object *srp_SQLTransact(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLHDBC connectionHandle;
  SQLUSMALLINT action;
  char *actionString;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("transact","sql-henv",0,argc,argv);
  }

  if (SQL_HDBCP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("transact","sql-hdbc or symbol",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("transact","symbol",2,argc,argv);
  }

  actionString = SCHEME_SYM_VAL(argv[2]);

  if (stricmp(actionString,"sql-commit") == 0) {
    action = SQL_COMMIT;
  }
  else if (stricmp(actionString,"sql-rollback") == 0) {
    action = SQL_ROLLBACK;
  }
  else {
    scheme_signal_error("sql-transact: invalid completion type: %s",
			actionString);
  }

  envHandle = SQL_HENV_VAL(argv[0]);

  if (SCHEME_SYMBOLP(argv[1])) {
    if (stricmp(SCHEME_SYM_VAL(argv[1]),"sql-null-hdbc")) {
      scheme_signal_error("transact: 'sql-null-hdbc is only allowed symbol for 2nd argument");
    }
    connectionHandle = SQL_NULL_HDBC;
  }
  else {
    connectionHandle = SQL_HDBC_VAL(argv[1]);
  }

  sr = SQLTransact(envHandle,connectionHandle,action);

  retcode = checkSQLReturn(sr,"transaction");

  sql_return(scheme_void,retcode,"transaction");
}

/* Functions in SQLEXT.H */

Scheme_Object *srp_SQLDriverConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLCHAR *inConnectString;
  SQLSMALLINT inConnectStringLen;
  SQLCHAR outConnectString[2048];
  SQLSMALLINT actualLen;
  char *completionString;
  SQLUSMALLINT completion; 
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("driver-connect","sql-hdbc",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("driver-connect","string",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("driver-connect","symbol",2,argc,argv);
  }

  completionString = SCHEME_SYM_VAL(argv[2]);

  if (stricmp(completionString,"sql-driver-prompt") == 0) {
    completion = SQL_DRIVER_PROMPT;
  }
  else if (stricmp(completionString,"sql-driver-complete") == 0) {
    completion = SQL_DRIVER_COMPLETE;
  }
  else if (stricmp(completionString,"sql-driver-complete-required") == 0) {
    completion = SQL_DRIVER_COMPLETE_REQUIRED;
  }
  else if (stricmp(completionString,"sql-driver-no-prompt") == 0) {
    completion = SQL_DRIVER_NOPROMPT;
  }
  else {
    scheme_signal_error("sql-driver-connect: invalid completion: %s",
			completionString);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);
  inConnectString = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  inConnectStringLen = SCHEME_STRLEN_VAL(argv[1]);
  
  sr = SQLDriverConnect(connectionHandle,
#ifdef WIN32			
			GetDesktopWindow(),
#else
			NULL,
#endif
			inConnectString,inConnectStringLen,
			outConnectString,sizeray(outConnectString),
			&actualLen,completion);

  retcode = checkSQLReturn(sr,"driver-connect");

  retval = scheme_make_sized_string((char *)outConnectString,actualLen,TRUE);
  sql_return(retval,retcode,"driver-connect"); 
}

Scheme_Object *srp_SQLBrowseConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLCHAR *inConnectString;
  SQLSMALLINT inConnectStringLen;
  SQLCHAR outConnectString[2048];
  SQLSMALLINT actualLen;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("browse-connect","sql-hdbc",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("browse-connect","string",1,argc,argv);
  }
  
  connectionHandle = SQL_HDBC_VAL(argv[0]);
  inConnectString = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  inConnectStringLen = SCHEME_STRLEN_VAL(argv[1]);
  
  sr = SQLBrowseConnect(connectionHandle,
			inConnectString,inConnectStringLen,
			outConnectString,sizeray(outConnectString),
			&actualLen);

  retcode = checkSQLReturn(sr,"browse-connect");

  retval = scheme_make_sized_string((char *)outConnectString,actualLen,TRUE);
  sql_return(retval,retcode,"browse-connect");
}

Scheme_Object *srp_SQLBulkOperations(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0300)
  return raise_not_implemented("bulk-operations");
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT operation;
  char *operationString;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("bulk-operations","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("bulk-operations","symbol",1,argc,argv);
  }

  operationString = SCHEME_SYM_VAL(argv[1]);
  
  if (stricmp(operationString,"sql-add") == 0) {
    operation = SQL_ADD;
  }
  else if (stricmp(operationString,"sql-update-by-bookmark") == 0) {
    operation = SQL_UPDATE_BY_BOOKMARK;
  }
  else if (stricmp(operationString,"sql-delete-by-bookmark") == 0) {
    operation = SQL_DELETE_BY_BOOKMARK;
  }
  else if (stricmp(operationString,"sql-fetch-by-bookmark") == 0) {
    operation = SQL_FETCH_BY_BOOKMARK;
  }
  else {
    scheme_signal_error("sql-bulk-operations: invalid operation: %s",
			operationString);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLBulkOperations(stmtHandle,operation);

  retcode = checkSQLReturn(sr,"bulk-operations");

  sql_return(argv[0],retcode,"bulk-operations");

#endif
}

Scheme_Object *srp_SQLColAttributes(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT colNumber;
  SQLUSMALLINT fieldId;
  char *fieldIdString;
  char buff[2048];
  SQLSMALLINT bufflen;
  SQLINTEGER numBuffer;
  SQLSMALLINT actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("col-attributes","sql-hstmt",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("col-attributes","small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("col-attributes","symbol",2,argc,argv);
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedTypedConstSearch(fieldIdString,colAttributesOld);

  if (p == NULL) {
    scheme_signal_error("Invalid column attribute: %s",fieldIdString);
  }
    
  fieldId = (SQLUSMALLINT)(p->val);
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  switch(p->type) {

  case sqlbool :

    sr = SQLColAttributes(stmtHandle,colNumber,fieldId,
			  buff,0,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attributes");		       
    sql_return(numBuffer ? scheme_true : scheme_false,
	       retcode,"col-attributes");

  case sqlinteger :

    sr = SQLColAttributes(stmtHandle,colNumber,fieldId,
			  buff,0,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attributes");		       
    sql_return(scheme_make_integer_value((long)numBuffer),
	       retcode,"col-attributes");		       

  case namedinteger :

    sr = SQLColAttributes(stmtHandle,colNumber,fieldId,
			  buff,0,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attributes");		       

    retval = 
      scheme_intern_symbol(findIntegerName(fieldIdString,numBuffer,
					   namedColAttrsIntegers,
					   sizeray(namedColAttrsIntegers)));
    sql_return(retval,retcode,"col-attributes");

  case string :

    bufflen = sizeof(buff);
    sr = SQLColAttributes(stmtHandle,colNumber,fieldId,
			  buff,bufflen,&actualLen,&numBuffer);
    retcode = checkSQLReturn(sr,"col-attributes");		       
    sql_return(scheme_make_sized_string(buff,actualLen,TRUE),
	       retcode,"col-attributes"); 

  default :

    scheme_signal_error("sql-col-attributes: invalid attribute type");

  }

  return scheme_void; /* unreachable */
} 

Scheme_Object *srp_SQLColumnPrivileges(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLCHAR *columnName;
  SQLSMALLINT columnNameLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("column-privileges","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("column-privileges","string",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);
  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);
  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);
  columnName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  columnNameLen = SCHEME_STRLEN_VAL(argv[4]);

  sr = SQLColumnPrivileges(stmtHandle,
			   catalogName,catalogNameLen,
			   schemaName,schemaNameLen,
			   tableName,tableNameLen,
			   columnName,columnNameLen);

  retcode = checkSQLReturn(sr,"column-privileges");

  sql_return(argv[0],retcode,"column-privileges");
}

Scheme_Object *srp_SQLDescribeParam(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT paramNumber;
  SQLSMALLINT dataType;
  SQLUINTEGER paramSize;
  SQLSMALLINT decimalDigits;
  SQLSMALLINT nullable;
  char *nullableString;
  char *dataTypeString;
  RETURN_CODE retcode;
  int i;
  Scheme_Object *retval;
  
  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("describe-param","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("describe-param","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);

  sr = SQLDescribeParam(stmtHandle,paramNumber,
			&dataType,&paramSize,&decimalDigits,
			&nullable);

  retcode = checkSQLReturn(sr,"describe-param");		       

  dataTypeString = "?";

  for (i = 0; i < (int)sizeray(SQLDataTypes); i++) {
    if (dataType == SQLDataTypes[i].val) {
      dataTypeString = SQLDataTypes[i].scheme_name;
      break;
    }
  }

  nullableString = nullableToString(nullable);

  retval = scheme_null;
  retval = scheme_make_pair(scheme_intern_symbol(nullableString),retval);
  retval = scheme_make_pair(scheme_make_integer_value(decimalDigits),retval);
  retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(paramSize),retval);
  retval = scheme_make_pair(scheme_intern_symbol(dataTypeString),retval);
  
  sql_return(retval,retcode,"describe-param");
}

Scheme_Object *srp_SQLExtendedFetch(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT fetchType;
  char *fetchTypeString;
  SQLINTEGER rowNumber;
  SRP_SQL_ROW_STATUS *rowStatus;
  SRP_NAMED_SMALL_CONSTANT *p;      
#if (ODBCVER >= 0x0300)
  SQLINTEGER actualLen;
#endif
  SQLINTEGER numRows;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("extended-fetch","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("extended-fetch","symbol",1,argc,argv);
  }
  
  fetchTypeString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(fetchTypeString,fetchOrientation);

  if (p == NULL) {
    scheme_signal_error("sql-extended-fetch: invalid fetch orientation: %s",
			fetchTypeString);
  }

  fetchType = p->val;

  switch(fetchType) {
  case SQL_FETCH_NEXT :
  case SQL_FETCH_PRIOR :
  case SQL_FETCH_FIRST :
  case SQL_FETCH_LAST :

    if (argc > 2) {
      scheme_signal_error("extended-fetch: Only two arguments allowed "
			  "when given '%s",fetchTypeString);
    }
    rowNumber = 0;
    break;

  case SQL_FETCH_ABSOLUTE :
  case SQL_FETCH_RELATIVE :
  case SQL_FETCH_BOOKMARK :

    if (SCHEME_INTP(argv[2]) == FALSE) {
      scheme_wrong_type("extended-fetch","integer",2,argc,argv);
    }
    rowNumber = SCHEME_INT_VAL(argv[1]);
    break;

  default :
    ;
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

#if (ODBCVER >= 0x0300)
  sr = SQLGetStmtAttr(stmtHandle,SQL_ATTR_ROW_ARRAY_SIZE,&numRows,0,&actualLen);
  checkSQLReturn(sr,"get-stmt-attr");
#else
  sr = SQLGetStmtOption(stmtHandle,SQL_ROWSET_SIZE,&numRows);
  checkSQLReturn(sr,"get-stmt-option");
#endif

  /* need to keep rowStatus around until cursor closed
     conservatively, make it uncollectable */

  rowStatus = (SRP_SQL_ROW_STATUS *)scheme_malloc(sizeof(SRP_SQL_ROW_STATUS));
  scheme_dont_gc_ptr(rowStatus);
  rowStatus->type = sql_row_status_type;
  rowStatus->numRows = 1; 
  rowStatus->values = (SQLUSMALLINT *)scheme_malloc(numRows * sizeof(SQLUSMALLINT));
  rowStatus->usesSchemeStorage = TRUE;
  scheme_dont_gc_ptr(rowStatus->values);

  sr = SQLExtendedFetch(stmtHandle,fetchType,rowNumber,
			&rowStatus->numRows,rowStatus->values);

  retcode = checkSQLReturn(sr,"extended-fetch");

  sql_return((Scheme_Object *)rowStatus,retcode,"extended-fetch");
}

Scheme_Object *srp_SQLForeignKeys(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLCHAR *foreignCatalogName;
  SQLSMALLINT foreignCatalogNameLen;
  SQLCHAR *foreignSchemaName;
  SQLSMALLINT foreignSchemaNameLen;
  SQLCHAR *foreignTableName;
  SQLSMALLINT foreignTableNameLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("foreign-keys","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 6; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("foreign-keys","string",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);
  
  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);
  
  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  foreignCatalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  foreignCatalogNameLen = SCHEME_STRLEN_VAL(argv[4]);
  
  foreignSchemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[5]);
  foreignSchemaNameLen = SCHEME_STRLEN_VAL(argv[5]);
  
  foreignTableName = (SQLCHAR *)SCHEME_STR_VAL(argv[6]);
  foreignTableNameLen = SCHEME_STRLEN_VAL(argv[6]);

  sr = SQLForeignKeys(stmtHandle,
		      catalogName,catalogNameLen,
		      schemaName,schemaNameLen,
		      tableName,tableNameLen,
		      foreignCatalogName,foreignCatalogNameLen,
		      foreignSchemaName,foreignSchemaNameLen,
		      foreignTableName,foreignTableNameLen);

  retcode = checkSQLReturn(sr,"foreign-keys");
  
  sql_return(argv[0],retcode,"foreign-keys");
}

Scheme_Object *srp_SQLMoreResults(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("more-results","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLMoreResults(stmtHandle);

  retcode = checkSQLReturn(sr,"more-results");  

  sql_return(argv[0],retcode,"more-results");  
}

Scheme_Object *srp_SQLNativeSql(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLCHAR *inSql;
  SQLINTEGER inSqlLen;
  SQLCHAR *outSql;
  SQLINTEGER outSqlLen;
  SQLINTEGER actualLen;
  RETURN_CODE retcode;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("native-sql","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("native-sql","string",1,argc,argv);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  inSql = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  inSqlLen = SCHEME_STRLEN_VAL(argv[1]);

  outSqlLen = 2 * inSqlLen;
  outSql = (SQLCHAR *)scheme_malloc(outSqlLen * sizeof(*outSql));

  sr = SQLNativeSql(connectionHandle,
		    inSql,inSqlLen,
		    outSql,outSqlLen,
		    &actualLen);

  retcode = checkSQLReturn(sr,"native-sql");    

  sql_return(scheme_make_sized_string((char *)outSql,actualLen,TRUE),
	     retcode,"native-sql");    
}

Scheme_Object *srp_SQLNumParams(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT numParams;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("num-params","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);  

  sr = SQLNumParams(stmtHandle,&numParams);

  retcode = checkSQLReturn(sr,"num-params");    

  sql_return(scheme_make_integer(numParams),retcode,"num-params");
}

Scheme_Object *srp_SQLParamOptions(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUINTEGER cRow;
  SQLUINTEGER piRow;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("param-options","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedInt(argv[1]) == FALSE) {
    scheme_wrong_type("param-options","unsigned-int",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  scheme_get_unsigned_int_val(argv[1],&cRow);

  sr = SQLParamOptions(stmtHandle,cRow,&piRow);
 
  retcode = checkSQLReturn(sr,"param-options");    

  sql_return(scheme_make_integer_value_from_unsigned(piRow),
	     retcode,"param-options");    
}

Scheme_Object *srp_SQLPrimaryKeys(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("primary-keys","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("primary-keys","string",i,argc,argv);
    }
  } 
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLPrimaryKeys(stmtHandle,
		      catalogName,catalogNameLen,
		      schemaName,schemaNameLen,
		      tableName,tableNameLen);

  retcode = checkSQLReturn(sr,"primary-keys");    

  sql_return(argv[0],retcode,"primary-keys");    
}

Scheme_Object *srp_SQLProcedureColumns(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *procName;
  SQLSMALLINT procNameLen;
  SQLCHAR *columnName;
  SQLSMALLINT columnNameLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("procedure-columns","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("procedure-columns","string",i,argc,argv);
    }
  } 
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  procName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  procNameLen = SCHEME_STRLEN_VAL(argv[3]);

  columnName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  columnNameLen = SCHEME_STRLEN_VAL(argv[4]);

  sr = SQLProcedureColumns(stmtHandle,
			   catalogName,catalogNameLen,
			   schemaName,schemaNameLen,
			   procName,procNameLen,
			   columnName,columnNameLen);

  retcode = checkSQLReturn(sr,"procedure-columns");    

  sql_return(argv[0],retcode,"procedure-columns");    

}

Scheme_Object *srp_SQLProcedures(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *procName;
  SQLSMALLINT procNameLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("procedures","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("procedures","string",i,argc,argv);
    }
  } 

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  procName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  procNameLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLProcedures(stmtHandle,
		     catalogName,catalogNameLen,
		     schemaName,schemaNameLen,
		     procName,procNameLen);

  retcode = checkSQLReturn(sr,"procedures");    

  sql_return(argv[0],retcode,"procedures");    
}

Scheme_Object *srp_SQLSetPos(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT rowNumber;
  SQLUSMALLINT operation;
  char *operationString;
  SQLUSMALLINT lock;
  char *lockString;
  SRP_NAMED_SMALL_CONSTANT *p;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("set-pos","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("set-pos","unsigned-small-int",1,argc,argv);
  }

  for (i = 2; i <= 3; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("set-pos","symbol",i,argc,argv);
    }
  }

  operationString = SCHEME_SYM_VAL(argv[2]);
  lockString = SCHEME_SYM_VAL(argv[3]);

  p = namedSmallConstSearch(operationString,posOperations);

  if (p == NULL) {
    scheme_signal_error("set-pos: invalid operation: %s",operationString);
  }

  operation = p->val;
  
  p = namedSmallConstSearch(lockString,lockTypes);

  if (p == NULL) {
    scheme_signal_error("set-pos: invalid lock type: %s",lockString);
  }

  lock = p->val;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  rowNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  
  sr = SQLSetPos(stmtHandle,rowNumber,operation,lock);

  retcode = checkSQLReturn(sr,"set-pos");    

  sql_return(argv[0],retcode,"set-pos");    
}

Scheme_Object *srp_SQLTablePrivileges(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("table-privileges","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("table-privileges","string",i,argc,argv);
    }
  } 
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLTablePrivileges(stmtHandle,
			  catalogName,catalogNameLen,
			  schemaName,schemaNameLen,
			  tableName,tableNameLen);

  retcode = checkSQLReturn(sr,"table-privileges");    

  sql_return(argv[0],retcode,"table-privileges");    
}

Scheme_Object *srp_SQLDrivers(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0200)
  return raise_not_implemented("drivers");
#else
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLUSMALLINT selection;
  char *selectionString;
  SQLCHAR description[2048];
  SQLCHAR attributes[2048];
  SQLSMALLINT actualLen1,actualLen2;
  Scheme_Object *retval;
  RETURN_CODE retcode;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("drivers","sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("drivers","symbol",1,argc,argv);
  }

  selectionString = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(selectionString,"sql-fetch-first") == 0) {
    selection = SQL_FETCH_FIRST;
  }
  else if (stricmp(selectionString,"sql-fetch-next") == 0) {
    selection = SQL_FETCH_NEXT;
  }
  else {
    scheme_signal_error("sql-drivers: invalid selection: %s",
			selectionString);
  }

  envHandle = SQL_HENV_VAL(argv[0]);
  
  sr = SQLDrivers(envHandle,selection,
		  description,sizeray(description),&actualLen1,
		  attributes,sizeray(attributes),&actualLen2);

  retcode = checkSQLReturn(sr,"drivers");    

  retval = scheme_null;
  retval = scheme_make_pair(scheme_make_sized_string((char *)attributes,
						     actualLen2,TRUE),
			    retval);
  retval = scheme_make_pair(scheme_make_sized_string((char *)description,
						     actualLen1,TRUE),
			    retval);
					       
  sql_return(retval,retcode,"drivers");    
#endif
}

Scheme_Object *srp_SQLBindParameter(int argc,Scheme_Object **argv) {
#if (ODBCVER < 0x0200)
  return raise_not_implemented("bind-parameter");
#else
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT paramNumber;
  SQLSMALLINT ioType;
  char *ioTypeString;
  SQLSMALLINT valueType;
  SQLSMALLINT paramType;
  char *paramTypeString;
  SQLUINTEGER valueSize;
  SQLSMALLINT decimalDigits;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SQLINTEGER *indicator;
  SRP_NAMED_SMALL_CONSTANT *p;      
  RETURN_CODE retcode;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("bind-parameter","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("bind-parameter","unsigned-small-int",1,argc,argv);
  }

  for (i = 2; i <= 3; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("bind-parameter","symbol",i,argc,argv);
    }
  }

  if (isSmallInt(argv[4]) == FALSE) {
    scheme_wrong_type("bind-parameter","small-int",4,argc,argv);
  }
  
  if (SQL_BUFFERP(argv[5]) == FALSE) {
    scheme_wrong_type("bind-parameter","sql-buffer",5,argc,argv);
  }

  if (SQL_INDICATORP(argv[6]) == FALSE) {
    scheme_wrong_type("bind-parameter","sql-indicator",6,argc,argv);
  }

  ioTypeString = SCHEME_SYM_VAL(argv[2]);

  if (stricmp(ioTypeString,"sql-param-input") == 0) {
    ioType = SQL_PARAM_INPUT;
  }
  else if (stricmp(ioTypeString,"sql-param-input-output") == 0) {
    ioType = SQL_PARAM_INPUT_OUTPUT;
  }
  else if (stricmp(ioTypeString,"sql-param-output") == 0) {
    ioType = SQL_PARAM_OUTPUT;
  }
  else {
    scheme_signal_error("sql-bind-parameter: invalid I/O type: %s",
			ioTypeString);
  }

  paramTypeString = SCHEME_SYM_VAL(argv[3]);

  p = namedSmallConstSearch(paramTypeString,SQLDataTypes);

  if (p == NULL) {
    scheme_signal_error("sql-bind-parameter: invalid parameter type: %s",
			paramTypeString);
  }

  paramType = p->val;

  switch(paramType) {

  case SQL_DECIMAL :
  case SQL_NUMERIC :
  case SQL_TIME :
  case SQL_TIMESTAMP :
#if ODBCVER >= 0x0300
  case SQL_TYPE_TIME :
#endif
  case SQL_INTERVAL_SECOND :
  case SQL_INTERVAL_DAY_TO_SECOND :
  case SQL_INTERVAL_HOUR_TO_SECOND :
  case SQL_INTERVAL_MINUTE_TO_SECOND :

      /* need Decimals */

      if (argc != 8) {
	scheme_wrong_count("sql-bind-parameter",8,8,argc,argv);
      }

      if (isSmallInt(argv[7]) == FALSE) {
	scheme_wrong_type("bind-parameter","small-int",7,argc,argv);
      }

      decimalDigits = (SQLSMALLINT)SCHEME_INT_VAL(argv[6]);

      break;


  default :

    decimalDigits = 0;
    break;
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[5]);
  valueType = SQL_BUFFER_CTYPE(argv[5]);
  bufferLen = SQL_BUFFER_LEN(argv[5]);
  scheme_get_unsigned_int_val(argv[4],&valueSize);
  indicator = SQL_INDICATOR_VAL(argv[6]);

  sr = SQLBindParameter(stmtHandle,paramNumber,ioType,
			valueType,paramType,
			valueSize,decimalDigits,
			buffer,bufferLen,indicator);

  retcode = checkSQLReturn(sr,"bind-parameter");    

  sql_return(argv[0],retcode,"bind-parameter");

#endif
}


Scheme_Object *srp_SQLSetScrollOptions(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT concur;
  char *concurString;
  SQLINTEGER keyset;
  SQLUSMALLINT rowset;
  SRP_NAMED_SMALL_CONSTANT *p;
  RETURN_CODE retcode;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("set-scroll-options","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("set-scroll-options","symbol",1,argc,argv);
  }

  /* deal with argv[2] below */

  if (isUnsignedSmallInt(argv[3]) == FALSE) {
    scheme_wrong_type("set-scroll-options","unsigned-small-int",3,argc,argv);
  }

  concurString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(concurString,scrollConcurrency);

  if (p == NULL) {
    scheme_signal_error("sql-set-scroll-options: invalid concurrency: %s",
			concurString);
  }

  concur = p->val;

  rowset = (SQLUSMALLINT)SCHEME_INT_VAL(argv[3]);  

  if (SCHEME_SYMBOLP(argv[2])) {
    char *keysetString;
    SRP_NAMED_CONSTANT *q;

    keysetString = SCHEME_SYM_VAL(argv[2]);
    
    q = namedConstSearch(keysetString,scrollCursor);

    if (q == NULL) {
      scheme_signal_error("sql-set-scroll-options: invalid keyset: %s",
			  keysetString);
    }

    keyset = q->val;
  }
  else if (SCHEME_EXACT_INTEGERP(argv[2])) {
    if (scheme_get_int_val(argv[1],&keyset) == 0) {
      scheme_signal_error("sql-set-scroll-options: keyset value too large");
    }

    if (keyset < rowset) {
      scheme_signal_error("sql-set-scroll-options: keyset smaller than rowset");
    }
  }
  else {
    scheme_wrong_type("set-scroll-options","symbol or int",2,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLSetScrollOptions(stmtHandle,concur,keyset,rowset);

  retcode = checkSQLReturn(sr,"set-scroll-options");    

  sql_return(argv[0],retcode,"set-scroll-options");    
}

Scheme_Object *stringArrayToSchemeSymbolList(char **strs,int count) {
  Scheme_Object *retval;
  int i;

  retval = scheme_null;

  for (i = count-1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_intern_symbol(strs[i]),retval);
  }

  return retval;
}

char **schemeSymbolListToStringArray(Scheme_Object **objs,int n) {
  char **strs;
  int i;

  strs = (char **)scheme_malloc(n * sizeof(char *));

  for (i = 0; i < n; i++) {
    strs[i] = SCHEME_SYM_VAL(objs[i]);
  }

  return strs;
}

void initTypes(void) {
  sql_date_type = scheme_make_type("<sql-date>");
  sql_decimal_type = scheme_make_type("<sql-decimal>");
  sql_pointer_type = scheme_make_type("<sql-pointer>");
  sql_time_type = scheme_make_type("<sql-time>");
  sql_timestamp_type = scheme_make_type("<sql-timestamp>");
  sql_return_type = scheme_make_type("<sql-return>");
  sql_henv_type = scheme_make_type("<sql-henv>");
  sql_hdbc_type = scheme_make_type("<sql-hdbc>");
  sql_hstmt_type = scheme_make_type("<sql-hstmt>");
  sql_hdesc_type = scheme_make_type("<sql-hdesc>");
  sql_boxed_uint_type = scheme_make_type("<sql-boxed-uint>");
  sql_buffer_type = scheme_make_type("<sql-buffer>");
  sql_length_type = scheme_make_type("<sql-length>");
  sql_indicator_type = scheme_make_type("<sql-indicator>");
  sql_row_status_type = scheme_make_type("<sql-row-status>");
  sql_array_status_type = scheme_make_type("<sql-array-status>");
  sql_binding_offset_type = scheme_make_type("<sql-binding-offset>");
  sql_rows_processed_type = scheme_make_type("<sql-rows-processed>");
  sql_octet_length_type = scheme_make_type("<sql-octet-length>");
  sql_op_parms_type = scheme_make_type("<sql-op-parms>");
  sql_guid_type = scheme_make_type("<sql-guid>");
  sql_paramlength_type = scheme_make_type("<sql-paramlength>");
}

void initExns(void) {
  Scheme_Object *exn_type;
  Scheme_Object *new_exn_name;
  Scheme_Object *new_exn_type;
  Scheme_Object **new_exn_names;
  Scheme_Object **exn_values;
  int name_count;
  int i;

  exn_type = scheme_builtin_value("struct:exn");

  exnNameCount = 0;

  for (i = 0; i < (int)sizeray(srp_exns); i++) {
    new_exn_name = scheme_intern_symbol(srp_exns[i].name);
    new_exn_type = 
      scheme_make_struct_type(new_exn_name,exn_type,NULL,
			      srp_exns[i].num_fields,0,NULL,NULL);
    new_exn_names = 
      scheme_make_struct_names(new_exn_name,
			       stringArrayToSchemeSymbolList(srp_exns[i].fields,
							     srp_exns[i].num_fields),
			       0,&name_count);
    srp_exns[i].names = schemeSymbolListToStringArray(new_exn_names,name_count);
    exn_values = scheme_make_struct_values(new_exn_type,new_exn_names,name_count,0);    
    *(srp_exns[i].pStructFuns) = exn_values;
    srp_exns[i].name_count = name_count;
    exnNameCount += name_count;
  }
}

void initStructs(void) {
  Scheme_Object *structType,*structNameSymbol,**structNames,**structValues;
  int name_count;
  int i;

  structNameCount = 0;

  for (i = 0; i < (int)sizeray(srpStructs); i++) {
    structNameSymbol = scheme_intern_symbol(srpStructs[i].name);
    structType = scheme_make_struct_type(structNameSymbol,NULL,NULL,
					 srpStructs[i].num_fields,0,
					 NULL,NULL);
    structNames = scheme_make_struct_names(structNameSymbol,
					   stringArrayToSchemeSymbolList(srpStructs[i].fields,srpStructs[i].num_fields),
					   0,&name_count);
    srpStructs[i].names = schemeSymbolListToStringArray(structNames,name_count);
    structValues = scheme_make_struct_values(structType,structNames,name_count,0);    
    *(srpStructs[i].pStructFuns) = structValues;
    srpStructs[i].name_count = name_count;
    structNameCount += name_count;
  }
}

void sortConsts(void) {

#if (ODBCVER >= 0x0300)
  namedBitsDictSort(namedStmtAttributes);
  namedBitsDictSort(namedColAttrIntegers);
  namedBitsDictSort(namedConnectAttrIntegers);
  namedBitsDictSort(namedEnvAttrIntegers);
  namedBitsDictSort(namedFieldDescriptors);
  namedBitsDictSort(namedDiagFieldIntegers);
#endif

  namedBitsDictSort(namedStmtOptions);
  namedBitsDictSort(namedInfoSmallInts);
  namedBitsDictSort(namedInfoIntegers);
  namedBitsDictSort(namedColAttrsIntegers);
  namedBitsDictSort(namedConnectOptionIntegers);
  namedBitsDictSort(bitMaskTable);

#if (ODBCVER >= 0x0300)
  namedTypedConstSort(colAttributes);
  namedTypedConstSort(fieldDescriptors);
  namedTypedConstSort(settableConnectionAttributes);
  namedTypedConstSort(readOnlyConnectionAttributes);
  namedTypedConstSort(envAttributes);
  namedTypedConstSort(stmtAttributes);
#endif

  namedTypedConstSort(sqlInfo);
  namedTypedConstSort(colAttributesOld);
  namedTypedConstSort(connectionOptions);
  namedTypedConstSort(stmtOptions);

#if (ODBCVER >= 0x0300)
  namedConstSort(diagFields);
  namedConstSort(descDataTypes);
  namedConstSort(SQLDescUnnamed);
  namedConstSort(SQLDescSearchable);
  namedConstSort(datetimeIntervalCodes);
#endif

  namedConstSort(sqlFunctions);
  namedConstSort(fetchOrientation);
  namedConstSort(fetchScrolls);
  namedConstSort(scrollConcurrency);
  namedConstSort(scrollCursor);
  namedConstSort(fetchDirections);
  namedConstSort(posOperations);
  namedConstSort(lockTypes);
  namedConstSort(stmtFreeOptions);
  namedConstSort(CDataTypes);
  namedConstSort(SQLDataTypes);
}

Scheme_Object *scheme_module_name(void) {
  srp_name = scheme_intern_symbol(srp_name_string);
  return srp_name;
}

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  int i,j;
  Scheme_Object *srp_val;

  scheme_register_extension_global(&srp_exns,sizeof(srp_exns));
  scheme_register_extension_global(&srpStructs,sizeof(srpStructs));
  scheme_register_extension_global(&numericStructFuns,
				   sizeof(numericStructFuns));
  scheme_register_extension_global(&dateStructFuns,
				   sizeof(dateStructFuns));
  scheme_register_extension_global(&timeStructFuns,
				   sizeof(timeStructFuns));
  scheme_register_extension_global(&timeStampStructFuns,
				   sizeof(timeStampStructFuns));
  scheme_register_extension_global(&guidStructFuns,
				   sizeof(guidStructFuns));
  scheme_register_extension_global(&yearIntervalStructFuns,
				   sizeof(yearIntervalStructFuns));
  scheme_register_extension_global(&monthIntervalStructFuns,
				   sizeof(monthIntervalStructFuns));
  scheme_register_extension_global(&dayIntervalStructFuns,
				   sizeof(dayIntervalStructFuns));
  scheme_register_extension_global(&hourIntervalStructFuns,
				   sizeof(hourIntervalStructFuns));
  scheme_register_extension_global(&minuteIntervalStructFuns,
				   sizeof(minuteIntervalStructFuns));
  scheme_register_extension_global(&secondIntervalStructFuns,
				   sizeof(secondIntervalStructFuns));
  scheme_register_extension_global(&yearToMonthIntervalStructFuns,
				   sizeof(yearToMonthIntervalStructFuns));
  scheme_register_extension_global(&dayToHourIntervalStructFuns,
				   sizeof(dayToHourIntervalStructFuns));
  scheme_register_extension_global(&dayToMinuteIntervalStructFuns,
				   sizeof(dayToMinuteIntervalStructFuns));
  scheme_register_extension_global(&dayToSecondIntervalStructFuns,
				   sizeof(dayToSecondIntervalStructFuns));
  scheme_register_extension_global(&hourToMinuteIntervalStructFuns,
				   sizeof(hourToMinuteIntervalStructFuns));
  scheme_register_extension_global(&hourToSecondIntervalStructFuns,
				   sizeof(hourToSecondIntervalStructFuns));
  scheme_register_extension_global(&minuteToSecondIntervalStructFuns,
				   sizeof(minuteToSecondIntervalStructFuns));
  scheme_register_extension_global(&srp_name,sizeof(srp_name));
  scheme_register_extension_global(&bufferTable,sizeof(bufferTable));
  scheme_register_extension_global(&withInfoFuns,sizeof(withInfoFuns)); 
  scheme_register_extension_global(&noDataFuns,sizeof(noDataFuns));
  scheme_register_extension_global(&invalidHandleFuns,sizeof(invalidHandleFuns));
  scheme_register_extension_global(&errorFuns,sizeof(errorFuns));
  scheme_register_extension_global(&needDataFuns,sizeof(needDataFuns));
  scheme_register_extension_global(&stillExecutingFuns,sizeof(stillExecutingFuns));
  scheme_register_extension_global(&notImplementedFuns,sizeof(notImplementedFuns));

  initTypes();

  initStructs();

  initExns();

  sortConsts();

  if (srp_name == NULL) {
    srp_name = scheme_intern_symbol(srp_name_string);
  }

  env = scheme_primitive_module(srp_name,env);

  for (i = 0; i < sizeray(srpPrims); i++) {
    srp_val = scheme_make_prim_w_arity(srpPrims[i].c_fun,
				       srpPrims[i].name,
				       srpPrims[i].minargs,
				       srpPrims[i].maxargs);
    scheme_add_global(srpPrims[i].name,srp_val,env);
  }

  for (i = 0; i < sizeray(srpStructs); i++) {
    for (j = 0; j < srpStructs[i].name_count; j++) {
      srp_val = (*(srpStructs[i].pStructFuns))[j];
      scheme_add_global(srpStructs[i].names[j],srp_val,env);
    }
  }

  for (i = 0; i < sizeray(srp_exns); i++) {
    for (j = 0; j < srp_exns[i].name_count; j++) {
      srp_val = (*(srp_exns[i].pStructFuns))[j];
      scheme_add_global(srp_exns[i].names[j],srp_val,env);
    }
  }

  scheme_finish_primitive_module(env);

#ifndef __MAC_OS__
  if (isatty(fileno(stdin))) {
    fputs("SisterPersist ODBC extension for PLT Scheme\n"
	  "Copyright (c) 1999-2002 PLT (Paul Steckler)\n",stderr);
  }
#endif

  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *env) {
  return scheme_initialize(env);
}
