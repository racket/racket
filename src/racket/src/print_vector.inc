static void
FUNC_NAME (Scheme_Object *vec, int notdisplay, int compact, 
	     Scheme_Hash_Table *ht, 
             Scheme_Marshal_Tables *mt,
	     PrintParams *pp,
             int as_prefab)
{
  int i, size, common = 0;
  ELMS_TYPE elems;
  ELM_TYPE elem;
  DEFINEEXTRA

  DO_VEC_SIZE()
  if (compact) {
    DO_COMPACT()
  } else {
    DO_ELMS_SELECTOR()
    for (i = size; i--; common++) {
      if (!i || (elems[i] != elems[i - 1]))
	break;
    }
    elems = NULL; /* Precise GC: because VEC_ELS is not aligned */
    
    if (as_prefab) {
      print_utf8_string(pp, "#s(", 0, 3);
    } else if (notdisplay && pp->print_vec_shorthand && (notdisplay != 3)) {
      if (size == 0) {
	F_0;
      } else {
	char buffer[100];
	F_D;
	print_utf8_string(pp, buffer, 0, -1);
	size -= common;
      }
    } else if (notdisplay == 3)
      F_VECTOR;
    else
      F_;
  }

  for (i = 0; i < size; i++) {
    DO_ELM_SELECTOR()
    PRINT_ELM()
    if (i < (size - 1)) {
      if (!compact)
	print_utf8_string(pp, " ", 0, 1);
    }
  }

  if (!compact)
    print_utf8_string(pp, ")", 0, 1);
}

#undef FUNC_NAME
#undef DEFINEEXTRA
#undef ELMS_TYPE
#undef ELM_TYPE
#undef DO_COMPACT
#undef DO_VEC_SIZE
#undef DO_ELMS_SELECTOR
#undef DO_ELM_SELECTOR
#undef F_0
#undef F_D
#undef F_VECTOR
#undef F_
#undef PRINT_ELM

/* vim: ft=c
*/
