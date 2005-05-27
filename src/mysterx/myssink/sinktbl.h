// sinktbl.h

#ifndef _SINKTBL_
#define _SINKTBL_
#endif

typedef struct _myssink_table_ {
  Scheme_Object *(*pmake_cy)(CY *);
  Scheme_Object *(*pmake_date)(DATE *);
  Scheme_Object *(*pmake_bool)(unsigned);
  Scheme_Object *(*pmake_scode)(SCODE);
  Scheme_Object *(*pmake_idispatch)(IDispatch *);
  Scheme_Object *(*pmake_iunknown)(IUnknown *);

  BOOL (*pcy_pred)(Scheme_Object *);
  BOOL (*pdate_pred)(Scheme_Object *);
  BOOL (*pscode_pred)(Scheme_Object *);
  BOOL (*pcomobj_pred)(Scheme_Object *);
  BOOL (*piunknown_pred)(Scheme_Object *);

  CY (*pcy_val)(Scheme_Object *);
  DATE (*pdate_val)(Scheme_Object *);
  SCODE (*pscode_val)(Scheme_Object *);
  IDispatch *(*pcomobj_val)(Scheme_Object *);
  IUnknown *(*piunknown_val)(Scheme_Object *);

} MYSSINK_TABLE;

#define make_cy (myssink_table->pmake_cy)
#define make_date (myssink_table->pmake_date)
#define make_bool (myssink_table->pmake_bool)
#define make_scode (myssink_table->pmake_scode)
#define make_idispatch (myssink_table->pmake_idispatch)
#define make_iunknown (myssink_table->pmake_iunknown)

#define cy_pred (myssink_table->pcy_pred)
#define date_pred (myssink_table->pdate_pred)
#define scode_pred (myssink_table->pscode_pred)
#define comobj_pred (myssink_table->pcomobj_pred)
#define iunknown_pred (myssink_table->piunknown_pred)

#define cy_val (myssink_table->pcy_val)
#define date_val (myssink_table->pdate_val)
#define scode_val (myssink_table->pscode_val)
#define comobj_val (myssink_table->pcomobj_val)
#define iunknown_val (myssink_table->piunknown_val)

