/* "#(" has been read */
/* or "#fl(" has been read */
/* or "#fx(" has been read */
static Scheme_Object *
FUNC_NAME   (Scheme_Object *port,
	     Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	     int opener, char closer,
	     intptr_t requestLength, const mzchar *reqBuffer,
	     Scheme_Hash_Table **ht,
	     Scheme_Object *indentation,
             ReadParams *params, Readtable *table,
             int allow_infix)
/* requestLength == -1 => no request
   requestLength == -2 => overflow */
{
  Scheme_Object *lresult, *obj;
  VEC_TYPE *vec;
  ELMS_TYPE els;
  ELM_TYPE elm;
  int len, i;
  char *vtype_str;

  vtype_str = VTYPE_STR;

  lresult = read_list(port, stxsrc, line, col, pos, opener, closer, 
                      MZ_SHAPE,
                      1, ht, indentation, params, table);

  if (requestLength == -2) {
    scheme_raise_out_of_memory("read", "making %s of size %5", vtype_str, reqBuffer);
    return NULL;
  }

  if (stxsrc)
    obj = ((Scheme_Stx *)lresult)->val;
  else
    obj = lresult;

  len = scheme_list_length(obj);
  if (requestLength >= 0 && len > requestLength) {
    char buffer[20];
    sprintf(buffer, "%" PRIdPTR, requestLength);
    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
		    "read: %s length %ld is too small, "
		    "%d values provided",
		    vtype_str, requestLength, len);
    return NULL;
  }
  if (requestLength < 0)
    requestLength = len;

  vec = MK_VEC();
  els = ELMS_SELECTOR(vec);
  for (i = 0; i < len ; i++) {
    els[i] = ELM_SELECTOR(SCHEME_CAR(obj));
    obj = SCHEME_CDR(obj);
  }
  els = NULL;
  if (i < requestLength) {
    if (len)
      elm = ELMS_SELECTOR(vec)[len - 1];
    else {
      elm = ELM_MAKE_ZERO;
      if (stxsrc)
	elm = ELM_STX(elm);
    }

    els = ELMS_SELECTOR(vec);
    for (; i < requestLength; i++) {
      els[i] = elm;
    }
    els = NULL;
  }

  if (stxsrc) {
    if (VEC_SIZE(vec) > 0)
      SCHEME_SET_VECTOR_IMMUTABLE(vec);
    ((Scheme_Stx *)lresult)->val = (Scheme_Object *) vec;
    return lresult;
  } else
    return (Scheme_Object *) vec;
}
#undef FUNC_NAME
#undef VTYPE_STR
#undef VEC_TYPE
#undef ELMS_TYPE
#undef ELM_TYPE
#undef MZ_SHAPE
#undef MK_VEC
#undef ELMS_SELECTOR
#undef ELM_SELECTOR
#undef ELM_MAKE_ZERO
#undef ELM_STX
#undef VEC_SIZE

/* vim: ft=c 
*/
