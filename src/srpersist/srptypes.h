/* srptypes.h -- types for SisterPersist */

/* declare Scheme types for those SQL types that have
   no existing Scheme analog
*/

extern Scheme_Type sql_date_type;
extern Scheme_Type sql_decimal_type;
extern Scheme_Type sql_pointer_type;
extern Scheme_Type sql_time_type;
extern Scheme_Type sql_timestamp_type;
extern Scheme_Type sql_return_type;
extern Scheme_Type sql_henv_type;
extern Scheme_Type sql_hdbc_type;
extern Scheme_Type sql_hstmt_type;
extern Scheme_Type sql_hdesc_type;
extern Scheme_Type sql_boxed_uint_type;
extern Scheme_Type sql_buffer_type;
extern Scheme_Type sql_length_type;
extern Scheme_Type sql_indicator_type;
extern Scheme_Type sql_row_status_type;
extern Scheme_Type sql_array_status_type;
extern Scheme_Type sql_binding_offset_type;
extern Scheme_Type sql_rows_processed_type;
extern Scheme_Type sql_octet_length_type;
extern Scheme_Type sql_op_parms_type;
extern Scheme_Type sql_guid_type;
extern Scheme_Type sql_paramlength_type;

typedef struct _sql_henv_ {
  Scheme_Type type;
  SQLHENV henv;
} SRP_SQL_HENV;

typedef struct _sql_hdbc_ {
  Scheme_Type type;
  SQLHDBC hdbc;
} SRP_SQL_HDBC;

typedef struct _sql_hstmt_ {
  Scheme_Type type;
  SQLHDBC hstmt;
} SRP_SQL_HSTMT;

#if (ODBCVER >= 0x300)
typedef enum _desctype_ {
  APD,ARD,IPD,IRD,EXPLICIT
} DESCTYPE;

typedef struct _sql_hdesc_ {
  Scheme_Type type;
  DESCTYPE descType;
  SQLHDESC hdesc;
} SRP_SQL_HDESC;
#endif

typedef struct _sql_boxed_uint_ {
  Scheme_Type type;
  SQLUINTEGER *pointer;
} SRP_SQL_BOXED_UINT;

typedef struct _sql_buffer_ {
  Scheme_Type type;
  void *storage;
  SQLSMALLINT CDataType;
  long width;
  long arrayLength;
  int eltSize;
} SRP_SQL_BUFFER;

typedef struct _sql_length_ {
  Scheme_Type type;
  SQLINTEGER value;
} SRP_SQL_LENGTH;

typedef struct _sql_indicator_ {
  Scheme_Type type;
  long arrayLength;
  SQLINTEGER *storage;
} SRP_SQL_INDICATOR;

typedef struct _sql_row_status_ {
  Scheme_Type type;
  BOOL usesSchemeStorage;
  SQLUINTEGER numRows;
  SQLUSMALLINT *values;
} SRP_SQL_ROW_STATUS;

#if (ODBCVER >= 0x300)
typedef struct _sql_array_status_ {
  Scheme_Type type;
  SQLHDESC hdesc;
  DESCTYPE descType;
  SQLUSMALLINT *values;
} SRP_SQL_ARRAY_STATUS;
#endif

typedef struct _sql_binding_offset_ {
  Scheme_Type type;
  SQLINTEGER *val;
} SRP_SQL_BINDING_OFFSET;

typedef struct _sql_rows_processed_ {
  Scheme_Type type;
  SQLUINTEGER *val;
} SRP_SQL_ROWS_PROCESSED;

typedef struct _sql_octet_length_ {
  Scheme_Type type;
  SQLINTEGER *val;
} SRP_SQL_OCTET_LENGTH;

typedef struct _sql_op_parms_ {
  Scheme_Type type;
  SQLUINTEGER paramSetSize;
  SQLUSMALLINT *values;
} SRP_SQL_OP_PARMS;

#if (ODBCVER >= 0x0350)
typedef struct _sql_guid_ {
  Scheme_Type type;
  SQLGUID guid;
} SRP_SQL_GUID;
#endif

#define SQL_HENVP(o) (!SCHEME_INTP(o) && o->type == sql_henv_type) 
#define SQL_HENV_VAL(o) (((SRP_SQL_HENV *)o)->henv)

#define SQL_HDBCP(o) (!SCHEME_INTP(o) && o->type == sql_hdbc_type) 
#define SQL_HDBC_VAL(o) (((SRP_SQL_HDBC *)o)->hdbc)

#define SQL_HSTMTP(o) (!SCHEME_INTP(o) && o->type == sql_hstmt_type) 
#define SQL_HSTMT_VAL(o) (((SRP_SQL_HSTMT *)o)->hstmt)

#define SQL_HDESCP(o) (!SCHEME_INTP(o) && o->type == sql_hdesc_type)
#define SQL_HDESC_VAL(o) (((SRP_SQL_HDESC *)o)->hdesc)
#define SQL_HDESC_DESCTYPE(o) (((SRP_SQL_HDESC *)o)->descType)

#define SQL_BUFFERP(o) (!SCHEME_INTP(o) && o->type == sql_buffer_type) 
#define SQL_BUFFER_VAL(o) (((SRP_SQL_BUFFER *)o)->storage)
#define SQL_BUFFER_WIDTH(o) (((SRP_SQL_BUFFER *)o)->width)
#define SQL_BUFFER_ARRAYLEN(o) (((SRP_SQL_BUFFER *)o)->arrayLength)
#define SQL_BUFFER_ELTSIZE(o) (((SRP_SQL_BUFFER *)o)->eltSize)
#define SQL_BUFFER_LEN(o) (SQL_BUFFER_WIDTH(o) * SQL_BUFFER_ARRAYLEN(o) * SQL_BUFFER_ELTSIZE(o))
#define SQL_BUFFER_CTYPE(o) (((SRP_SQL_BUFFER *)o)->CDataType)

#define SQL_LENGTHP(o) (!SCHEME_INTP(o) && o->type == sql_length_type) 
#define SQL_LENGTH_VAL(o) (((SRP_SQL_LENGTH *)o)->value)

#define SQL_INDICATORP(o) (!SCHEME_INTP(o) && o->type == sql_indicator_type) 
#define SQL_INDICATOR_VAL(o) (((SRP_SQL_INDICATOR *)o)->storage)
#define SQL_INDICATOR_LEN(o) (((SRP_SQL_INDICATOR *)o)->arrayLength)

#define SQL_OP_PARMSP(o) (!SCHEME_INTP(o) && o->type == sql_op_parms_type) 
#define SQL_OP_PARMS_LEN(o) (((SRP_SQL_OP_PARMS *)o)->paramSetSize)
#define SQL_OP_PARMS_VAL(o) (((SRP_SQL_OP_PARMS *)o)->values)

#define SQL_AP_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ap_desc_type) 
#define SQL_AP_DESC_VAL(o) (((SRP_SQL_AP_DESC *)o)->handle)

#define SQL_AR_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ar_desc_type) 
#define SQL_AR_DESC_VAL(o) (((SRP_SQL_AR_DESC *)o)->handle)

#define SQL_IP_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ip_desc_type) 
#define SQL_IP_DESC_VAL(o) (((SRP_SQL_IP_DESC *)o)->handle)

#define SQL_IR_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ir_desc_type) 
#define SQL_IR_DESC_VAL(o) (((SRP_SQL_IR_DESC *)o)->handle)

#define SQL_BOXED_UINTP(o) (!SCHEME_INTP(o) && o->type == sql_boxed_uint_type) 
#define SQL_BOXED_UINT_VAL(o) (((SRP_SQL_BOXED_UINT *)o)->pointer)

#define SQL_ROW_STATUSP(o) (!SCHEME_INTP(o) && o->type == sql_row_status_type) 
#define SQL_ROW_STATUS_VAL(o) (((SRP_SQL_ROW_STATUS *)o)->values)
#define SQL_ROW_STATUS_LEN(o) (((SRP_SQL_ROW_STATUS *)o)->numRows)

#define SQL_ARRAY_STATUSP(o) (!SCHEME_INTP(o) && o->type == sql_array_status_type) 
#define SQL_ARRAY_STATUS_VAL(o) (((SRP_SQL_ARRAY_STATUS *)o)->values)
#define SQL_ARRAY_STATUS_HDESC(o) (((SRP_SQL_ARRAY_STATUS *)o)->hdesc)
#define SQL_ARRAY_STATUS_DESCTYPE(o) (((SRP_SQL_ARRAY_STATUS *)o)->descType)

#define SQL_BINDING_OFFSETP(o) (!SCHEME_INTP(o) && o->type == sql_binding_offset_type) 
#define SQL_BINDING_OFFSET_VAL(o) (((SRP_SQL_BINDING_OFFSET *)o)->val)

#define SQL_ROWS_PROCESSEDP(o) (!SCHEME_INTP(o) && o->type == sql_rows_processed_type) 
#define SQL_ROWS_PROCESSED_VAL(o) (((SRP_SQL_ROWS_PROCESSED *)o)->val)

#define SQL_OCTET_LENGTHP(o) (!SCHEME_INTP(o) && o->type == sql_octet_length_type) 
#define SQL_OCTET_LENGTH_VAL(o) (((SRP_SQL_OCTET_LENGTH *)o)->val)

#define SQL_BUFFER_UNDEFINED_LEN  (LONG_MIN)



