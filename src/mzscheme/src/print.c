/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "schmach.h"
#include "schcpt.h"
#include <ctype.h>
#ifdef USE_STACKAVAIL
# include <malloc.h>
#endif

int (*scheme_check_print_is_obj)(Scheme_Object *o);

/* Flag for debugging compiled code in printed form: */
#define NO_COMPACT 0

#define PRINT_MAXLEN_MIN 3

/* locals */
#define MAX_PRINT_BUFFER 500

typedef struct Scheme_Print_Params {
  MZTAG_IF_REQUIRED
  
  char print_struct;
  char print_graph;
  char print_box;
  char print_vec_shorthand;
  char print_hash_table;
  char print_unreadable;
  char can_read_pipe_quote;
  char case_sens;
  char honu_mode;
  Scheme_Object *inspector;

  /* Used during `display' and `write': */
  char *print_buffer;
  long print_position;
  long print_allocated;
  long print_maxlen;
  long print_offset;
  Scheme_Object *print_port;
  mz_jmp_buf *print_escape;
} PrintParams;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static void print_to_port(char *name, Scheme_Object *obj, Scheme_Object *port, 
			  int notdisplay, long maxl, int check_honu);
static int print(Scheme_Object *obj, int notdisplay, int compact, 
		 Scheme_Hash_Table *ht,
                 Scheme_Marshal_Tables *mt,
		 PrintParams *p);
static void print_char_string(const char *s, int l, const mzchar *us, int delta, int ul, 
			      int notdisplay, int honu_char, PrintParams *pp);
static void print_byte_string(const char *s, int delta, int l, int notdisplay, PrintParams *pp);
static void print_pair(Scheme_Object *pair, int notdisplay, int compact, 
		       Scheme_Hash_Table *ht, 
                       Scheme_Marshal_Tables *mt,
		       PrintParams *pp);
static void print_vector(Scheme_Object *vec, int notdisplay, int compact, 
			 Scheme_Hash_Table *ht, 
                         Scheme_Marshal_Tables *mt,
			 PrintParams *pp);
static void print_char(Scheme_Object *chobj, int notdisplay, PrintParams *pp);
static char *print_to_string(Scheme_Object *obj, long * volatile len, int write,
			     Scheme_Object *port, long maxl, int check_honu);

static void custom_write_struct(Scheme_Object *s, Scheme_Hash_Table *ht, 
                                Scheme_Marshal_Tables *mt,
				PrintParams *pp, int notdisplay);
static Scheme_Object *writable_struct_subs(Scheme_Object *s, PrintParams *pp);

static Scheme_Object *quote_link_symbol = NULL;
static char *quick_buffer = NULL;
static char *quick_encode_buffer = NULL;

static Scheme_Type_Printer *printers;
static int printers_count;

#define QUICK_ENCODE_BUFFER_SIZE 256

static char compacts[_CPT_COUNT_];

static Scheme_Hash_Table *global_constants_ht;

#define print_compact(pp, v) print_this_string(pp, &compacts[v], 0, 1)

#define PRINTABLE_STRUCT(obj, pp) (scheme_inspector_sees_part(obj, pp->inspector, -1))

#define HAS_SUBSTRUCT(obj, qk) \
   (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) \
    || (qk(pp->print_box, 1) && SCHEME_BOXP(obj)) \
    || (qk(pp->print_struct  \
	   && SCHEME_STRUCTP(obj) \
	   && PRINTABLE_STRUCT(obj, pp), 0)) \
    || (qk(SCHEME_STRUCTP(obj) && scheme_is_writable_struct(obj), 0)) \
    || (qk(pp->print_hash_table, 1) && SCHEME_HASHTP(obj)))
#define ssQUICK(x, isbox) x
#define ssQUICKp(x, isbox) (pp ? x : isbox)
#define ssALL(x, isbox) 1
#define ssALLp(x, isbox) isbox

static Scheme_Hash_Table *cache_ht;

void scheme_init_print(Scheme_Env *env)
{
  int i;

  REGISTER_SO(quick_buffer);
  REGISTER_SO(quick_encode_buffer);
  
  quick_buffer = (char *)scheme_malloc_atomic(100);
  quick_encode_buffer = (char *)scheme_malloc_atomic(QUICK_ENCODE_BUFFER_SIZE);

  REGISTER_SO(quote_link_symbol);
  
  quote_link_symbol = scheme_intern_symbol("-q");
  
  for (i = 0; i < _CPT_COUNT_; i++) {
    compacts[i] = i;
  }

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(cache_ht);
}

Scheme_Object *scheme_make_svector(mzshort c, mzshort *a)
{
  Scheme_Object *o;
  o = scheme_alloc_object();

  o->type = scheme_svector_type;
  SCHEME_SVEC_LEN(o) = c;
  SCHEME_SVEC_VEC(o) = a;

  return o;
}

PrintParams *copy_print_params(PrintParams *pp)
{
  PrintParams *pp2;

  pp2 = MALLOC_ONE_RT(PrintParams);
  memcpy(pp2, pp, sizeof(PrintParams));
#ifdef MZTAG_REQUIRED
  pp2->type = scheme_rt_print_params;
#endif
  return pp2;
}

void
scheme_debug_print (Scheme_Object *obj)
{
  scheme_write(obj, scheme_orig_stdout_port);
  fflush (stdout);
}

static void *print_to_port_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj, *port;

  port = (Scheme_Object *)p->ku.k.p1;
  obj = (Scheme_Object *)p->ku.k.p2;

  print_to_port(p->ku.k.i2 ? "write" : "display", 
		obj, port,
		p->ku.k.i2, p->ku.k.i1, p->ku.k.i3);

  return NULL;
}

static void do_handled_print(Scheme_Object *obj, Scheme_Object *port,
			     Scheme_Object *proc, long maxl)
{
  Scheme_Object *a[2];

  a[0] = obj;
  
  if (maxl > 0) {
    a[1] = scheme_make_byte_string_output_port();
  } else
    a[1] = port;
  
  scheme_apply_multi(scheme_write_proc, 2, a);
  
  if (maxl > 0) {
    char *s;
    long len;

    s = scheme_get_sized_byte_string_output(a[1], &len);
    if (len > maxl)
      len = maxl;

    scheme_write_byte_string(s, len, port);
  }
}

void scheme_write_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl)
{
  if (((Scheme_Output_Port *)port)->write_handler)
    do_handled_print(obj, port, scheme_write_proc, maxl);
  else {
    Scheme_Thread *p = scheme_current_thread;
    
    p->ku.k.p1 = port;
    p->ku.k.p2 = obj;
    p->ku.k.i1 = maxl;
    p->ku.k.i2 = 1;
    p->ku.k.i3 = 0;
    
    (void)scheme_top_level_do(print_to_port_k, 0);
  }
}

void scheme_write(Scheme_Object *obj, Scheme_Object *port)
{
  scheme_write_w_max(obj, port, -1);
}

void scheme_display_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl)
{
  if (((Scheme_Output_Port *)port)->display_handler)
    do_handled_print(obj, port, scheme_display_proc, maxl);
  else {
    Scheme_Thread *p = scheme_current_thread;
    
    p->ku.k.p1 = port;
    p->ku.k.p2 = obj;
    p->ku.k.i1 = maxl;
    p->ku.k.i2 = 0;
    p->ku.k.i3 = 0;
    
    (void)scheme_top_level_do(print_to_port_k, 0);
  }
}

void scheme_display(Scheme_Object *obj, Scheme_Object *port)
{
  scheme_display_w_max(obj, port, -1);
}

void scheme_print_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl)
{
  if (((Scheme_Output_Port *)port)->print_handler)
    do_handled_print(obj, port, scheme_print_proc, maxl);
  else {
    Scheme_Thread *p = scheme_current_thread;
    
    p->ku.k.p1 = port;
    p->ku.k.p2 = obj;
    p->ku.k.i1 = maxl;
    p->ku.k.i2 = 1;
    p->ku.k.i3 = 1;
    
    (void)scheme_top_level_do(print_to_port_k, 0);
  }
}

void scheme_print(Scheme_Object *obj, Scheme_Object *port)
{
  scheme_print_w_max(obj, port, -1);
}

static void *print_to_string_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj;
  long *len, maxl;
  int iswrite, check_honu;

  obj = (Scheme_Object *)p->ku.k.p1;
  len = (long *) mzALIAS p->ku.k.p2;
  maxl = p->ku.k.i1;
  iswrite = p->ku.k.i2;
  check_honu = p->ku.k.i3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return (void *)print_to_string(obj, len, iswrite, NULL, maxl, check_honu);
}

char *scheme_write_to_string_w_max(Scheme_Object *obj, long *len, long maxl)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = len;
  p->ku.k.i1 = maxl;
  p->ku.k.i2 = 1;
  p->ku.k.i3 = 0;

  return (char *)scheme_top_level_do(print_to_string_k, 0);
}

char *scheme_write_to_string(Scheme_Object *obj, long *len)
{
  return scheme_write_to_string_w_max(obj, len, -1);
}

char *scheme_display_to_string_w_max(Scheme_Object *obj, long *len, long maxl)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = len;
  p->ku.k.i1 = maxl;
  p->ku.k.i2 = 0;
  p->ku.k.i3 = 0;

  return (char *)scheme_top_level_do(print_to_string_k, 0);
}

char *scheme_display_to_string(Scheme_Object *obj, long *len)
{
  return scheme_display_to_string_w_max(obj, len, -1);
}

char *scheme_print_to_string_w_max(Scheme_Object *obj, long *len, long maxl)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = len;
  p->ku.k.i1 = maxl;
  p->ku.k.i2 = 1;
  p->ku.k.i3 = 1;

  return (char *)scheme_top_level_do(print_to_string_k, 0);
}

char *scheme_print_to_string(Scheme_Object *obj, long *len)
{
  return scheme_print_to_string_w_max(obj, len, -1);
}

void
scheme_internal_write(Scheme_Object *obj, Scheme_Object *port)
{
  print_to_port("write", obj, port, 1, -1, 0);
}

void
scheme_internal_display(Scheme_Object *obj, Scheme_Object *port)
{
  print_to_port("display", obj, port, 0, -1, 0);
}

void
scheme_internal_print(Scheme_Object *obj, Scheme_Object *port)
{
  print_to_port("print", obj, port, 1, -1, 1);
}

#ifdef DO_STACK_CHECK
static int check_cycles(Scheme_Object *, Scheme_Hash_Table *ht, PrintParams *);

static Scheme_Object *check_cycle_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p2;
  PrintParams *pp = (PrintParams *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return check_cycles(o, ht, pp)
    ? scheme_true : scheme_false;
}
#endif

static int check_cycles(Scheme_Object *obj, Scheme_Hash_Table *ht, PrintParams *pp)
{
  Scheme_Type t;

#ifdef DO_STACK_CHECK
  {
#include "mzstkchk.h"
    {
      pp = copy_print_params(pp);
      scheme_current_thread->ku.k.p1 = (void *)obj;
      scheme_current_thread->ku.k.p2 = (void *)ht;
      scheme_current_thread->ku.k.p3 = (void *)pp;
      return SCHEME_TRUEP(scheme_handle_stack_overflow(check_cycle_k));
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  t = SCHEME_TYPE(obj);

  if (SCHEME_PAIRP(obj)
      || (pp->print_box && SCHEME_BOXP(obj))
      || SCHEME_VECTORP(obj)
      || ((SAME_TYPE(t, scheme_structure_type)
	   || SAME_TYPE(t, scheme_proc_struct_type))
          && ((pp->print_struct 
	       && PRINTABLE_STRUCT(obj, pp))
	      || scheme_is_writable_struct(obj)))
      || (pp->print_hash_table
	  && SAME_TYPE(t, scheme_hash_table_type))) {
    if (scheme_hash_get(ht, obj))
      return 1;
    scheme_hash_set(ht, obj, (Scheme_Object *)0x1);
  } else 
    return 0;

  if (SCHEME_PAIRP(obj)) {
    if (check_cycles(SCHEME_CAR(obj), ht, pp))
      return 1;
    if (check_cycles(SCHEME_CDR(obj), ht, pp))
      return 1;
  } else if (SCHEME_BOXP(obj)) {
    /* got here => printable */
    if (check_cycles(SCHEME_BOX_VAL(obj), ht, pp))
      return 1;
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;

    len = SCHEME_VEC_SIZE(obj);
    for (i = 0; i < len; i++) {
      if (check_cycles(SCHEME_VEC_ELS(obj)[i], ht, pp)) {
	return 1;
      }
    }
  } else if (SAME_TYPE(t, scheme_structure_type)
	     || SAME_TYPE(t, scheme_proc_struct_type)) {
    if (scheme_is_writable_struct(obj)) {
      if (check_cycles(writable_struct_subs(obj, pp), ht, pp))
	return 1;
    } else {
      /* got here => printable */
      int i = SCHEME_STRUCT_NUM_SLOTS(obj);

      while (i--) {
	if (scheme_inspector_sees_part(obj, pp->inspector, i)) {
	  if (check_cycles(((Scheme_Structure *)obj)->slots[i], ht, pp)) {
	    return 1;
	  }
	}
      }
    }
  }  else if (SCHEME_HASHTP(obj)) {
    /* got here => printable */
    Scheme_Hash_Table *t;
    Scheme_Object **keys, **vals, *val;
    int i;
    
    t = (Scheme_Hash_Table *)obj;
    keys = t->keys;
    vals = t->vals;
    for (i = t->size; i--; ) {
      if (vals[i]) {
	val = vals[i];
	if (check_cycles(keys[i], ht, pp))
	  return 1;
	if (check_cycles(val, ht, pp))
	  return 1;
      }
    }
  }

  scheme_hash_set(ht, obj, NULL);

  return 0;
}

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

/* The fast cycle-checker plays a dangerous game: it changes type
   tags. No GCs can occur here, and no thread switches. If the fast
   version takes to long, we back out to the general case. (We don't
   even check for stack overflow, so keep the max limit low.) */

static int fast_checker_counter;

static int check_cycles_fast(Scheme_Object *obj, PrintParams *pp)
{
  Scheme_Type t;
  int cycle = 0;

  t = SCHEME_TYPE(obj);
  if (t < 0)
    return 1;

  if (fast_checker_counter-- < 0)
    return -1;

  if (SCHEME_PAIRP(obj)) {
    obj->type = -t;
    cycle = check_cycles_fast(SCHEME_CAR(obj), pp);
    if (!cycle)
      cycle = check_cycles_fast(SCHEME_CDR(obj), pp);
    obj->type = t;
  } else if (pp->print_box && SCHEME_BOXP(obj)) {
    obj->type = -t;
    cycle = check_cycles_fast(SCHEME_BOX_VAL(obj), pp);
    obj->type = t;
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;

    obj->type = -t;
    len = SCHEME_VEC_SIZE(obj);
    for (i = 0; i < len; i++) {
      cycle = check_cycles_fast(SCHEME_VEC_ELS(obj)[i], pp);
      if (cycle)
	break;
    }
    obj->type = t;
  } else if (SAME_TYPE(t, scheme_structure_type)
	     || SAME_TYPE(t, scheme_proc_struct_type)) {
    if (scheme_is_writable_struct(obj)) {
      if (!pp->print_unreadable)
	cycle = 0;
      else
	/* don't bother with fast checks for writeable structs */
	cycle = -1;
    } else if (pp->print_struct && PRINTABLE_STRUCT(obj, pp)) {
      int i = SCHEME_STRUCT_NUM_SLOTS(obj);
      
      obj->type = -t;
      while (i--) {
	if (scheme_inspector_sees_part(obj, pp->inspector, i)) {
	  cycle = check_cycles_fast(((Scheme_Structure *)obj)->slots[i], pp);
	  if (cycle)
	    break;
	}
      }
      obj->type = t;
    } else
      cycle = 0;
  } else if (pp->print_hash_table
	     && SCHEME_HASHTP(obj)) {
    if (!((Scheme_Hash_Table *)obj)->count)
      cycle = 0;
    else
      /* don't bother with fast checks for non-empty hash tables */
      cycle = -1;
  } else
    cycle = 0;

  return cycle;
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

#ifdef DO_STACK_CHECK
static void setup_graph_table(Scheme_Object *obj, Scheme_Hash_Table *ht, int *counter, PrintParams *pp);

static Scheme_Object *setup_graph_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p2;
  int *counter = (int *)p->ku.k.p3;
  PrintParams *pp = (PrintParams *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  setup_graph_table(o, ht, counter, pp);

  return scheme_false;
}
#endif

static void setup_graph_table(Scheme_Object *obj, Scheme_Hash_Table *ht,
			      int *counter, PrintParams *pp)
{
  if (HAS_SUBSTRUCT(obj, ssQUICKp)) {
    Scheme_Object *v;

#ifdef DO_STACK_CHECK
    {
# include "mzstkchk.h"
      {
	if (pp)
	  pp = copy_print_params(pp);
	scheme_current_thread->ku.k.p1 = (void *)obj;
	scheme_current_thread->ku.k.p2 = (void *)ht;
	scheme_current_thread->ku.k.p3 = (void *)counter;
	scheme_current_thread->ku.k.p4 = (void *)pp;
	scheme_handle_stack_overflow(setup_graph_k);
	return;
      }
    }
#endif

    v = scheme_hash_get(ht, obj);

    if (!v)
      scheme_hash_set(ht, obj, (Scheme_Object *)0x1);
    else {
      if ((long)v == 1) {
	(*counter) += 2;
	scheme_hash_set(ht, obj, (Scheme_Object *)(long)*counter);
      }
      return;
    }
  } else
    return;

  SCHEME_USE_FUEL(1);

  if (SCHEME_PAIRP(obj)) {
    setup_graph_table(SCHEME_CAR(obj), ht, counter, pp);
    setup_graph_table(SCHEME_CDR(obj), ht, counter, pp);
  } else if ((!pp || pp->print_box) && SCHEME_BOXP(obj)) {
    setup_graph_table(SCHEME_BOX_VAL(obj), ht, counter, pp);
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;

    len = SCHEME_VEC_SIZE(obj);
    for (i = 0; i < len; i++) {
      setup_graph_table(SCHEME_VEC_ELS(obj)[i], ht, counter, pp);
    }
  } else if (pp && SCHEME_STRUCTP(obj)) { /* got here => printable */
    if (scheme_is_writable_struct(obj)) {
      if (pp->print_unreadable) {
	obj = writable_struct_subs(obj, pp);
	setup_graph_table(obj, ht, counter, pp);
      }
    } else {
      int i = SCHEME_STRUCT_NUM_SLOTS(obj);

      while (i--) {
	if (scheme_inspector_sees_part(obj, pp->inspector, i))
	  setup_graph_table(((Scheme_Structure *)obj)->slots[i], ht, counter, pp);
      }
    }
  } else if (pp && SCHEME_HASHTP(obj)) { /* got here => printable */
    Scheme_Hash_Table *t;
    Scheme_Object **keys, **vals, *val;
    int i;
    
    t = (Scheme_Hash_Table *)obj;
    keys = t->keys;
    vals = t->vals;
    for (i = t->size; i--; ) {
      if (vals[i]) {
	val = vals[i];
	setup_graph_table(keys[i], ht, counter, pp);
	setup_graph_table(val, ht, counter, pp);
      }
    }
  }
}

#define CACHE_HT_SIZE_LIMIT 32

Scheme_Hash_Table *scheme_setup_datum_graph(Scheme_Object *o, void *for_print)
{
  Scheme_Hash_Table *ht;
  int counter = 1;

  if (cache_ht) {
    ht = cache_ht;
    cache_ht = NULL;
  } else
    ht = scheme_make_hash_table(SCHEME_hash_ptr);

  setup_graph_table(o, ht, &counter, (PrintParams *)for_print);

  if (counter > 1)
    return ht;
  else {
    if (ht->size < CACHE_HT_SIZE_LIMIT) {
      int i;
      for (i = 0; i < ht->size; i++) {
	ht->keys[i] = NULL;
	ht->vals[i] = NULL;
      }
      cache_ht = ht;
    }
    return NULL;
  }
}

static char *
print_to_string(Scheme_Object *obj, 
		long * volatile len, int write,
		Scheme_Object *port, long maxl,
		int check_honu)
{
  Scheme_Hash_Table * volatile ht;
  Scheme_Object *v;
  char *ca;
  int cycles;
  Scheme_Config *config;
  mz_jmp_buf escape;
  volatile PrintParams params;

  params.print_allocated = 50;
  ca = (char *)scheme_malloc_atomic(params.print_allocated);
  params.print_buffer = ca;
  params.print_position = 0;
  params.print_offset = 0;
  params.print_maxlen = maxl;
  params.print_port = port;

  /* Getting print params can take a while, and they're irrelevant
     for simple things like displaying numbers. So try a shortcut: */
  if (!write
      && (SCHEME_NUMBERP(obj)
	  || SCHEME_BYTE_STRINGP(obj)
	  || SCHEME_CHAR_STRINGP(obj)
	  || SCHEME_SYMBOLP(obj))) {
    params.print_graph = 0;
    params.print_box = 0;
    params.print_struct = 0;
    params.print_vec_shorthand = 0;
    params.print_hash_table = 0;
    params.print_unreadable = 1;
    params.can_read_pipe_quote = 1;
    params.case_sens = 1;
    params.honu_mode = 0;
    params.inspector = scheme_false;
  } else {
    config = scheme_current_config();

    v = scheme_get_param(config, MZCONFIG_PRINT_GRAPH);
    params.print_graph = SCHEME_TRUEP(v);
    v = scheme_get_param(config, MZCONFIG_PRINT_BOX);
    params.print_box = SCHEME_TRUEP(v);
    v = scheme_get_param(config, MZCONFIG_PRINT_STRUCT);
    params.print_struct = SCHEME_TRUEP(v);
    v = scheme_get_param(config, MZCONFIG_PRINT_VEC_SHORTHAND);
    params.print_vec_shorthand = SCHEME_TRUEP(v);
    v = scheme_get_param(config, MZCONFIG_PRINT_HASH_TABLE);
    params.print_hash_table = SCHEME_TRUEP(v);
    if (write) {
      if (maxl > 0)
	params.print_unreadable = 1;
      else {
	v = scheme_get_param(config, MZCONFIG_PRINT_UNREADABLE);
	params.print_unreadable = SCHEME_TRUEP(v);
      }
    } else
      params.print_unreadable = 1;
    v = scheme_get_param(config, MZCONFIG_CAN_READ_PIPE_QUOTE);
    params.can_read_pipe_quote = SCHEME_TRUEP(v);
    v = scheme_get_param(config, MZCONFIG_CASE_SENS);
    params.case_sens = SCHEME_TRUEP(v);
    if (check_honu) {
      v = scheme_get_param(config, MZCONFIG_HONU_MODE);
      params.honu_mode = SCHEME_TRUEP(v);
    } else
      params.honu_mode = 0;
    v = scheme_get_param(config, MZCONFIG_INSPECTOR);
    params.inspector = v;
  }

  if (params.print_graph)
    cycles = 1;
  else {
    fast_checker_counter = 50;
    cycles = check_cycles_fast(obj, (PrintParams *)&params);
    if (cycles == -1) {
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      cycles = check_cycles(obj, ht, (PrintParams *)&params);
    }
  }

  if (cycles)
    ht = scheme_setup_datum_graph(obj, (PrintParams *)&params);
  else
    ht = NULL;

  if (maxl > 0)
    params.print_escape = &escape;
  else
    params.print_escape = NULL;

  if ((maxl <= PRINT_MAXLEN_MIN) 
      || !scheme_setjmp(escape))
    print(obj, write, 0, ht, NULL, (PrintParams *)&params);

  params.print_buffer[params.print_position] = '\0';

  if (len)
    *len = params.print_position;

  params.inspector = NULL;

  return params.print_buffer;
}

static void 
print_to_port(char *name, Scheme_Object *obj, Scheme_Object *port, int notdisplay, long maxl, int check_honu)
{
  Scheme_Output_Port *op;
  char *str;
  long len;
  
  op = (Scheme_Output_Port *)port;
  if (op->closed)
    scheme_raise_exn(MZEXN_FAIL, "%s: output port is closed", name);

  str = print_to_string(obj, &len, notdisplay, port, maxl, check_honu);

  scheme_write_byte_string(str, len, port);
}

static void print_this_string(PrintParams *pp, const char *str, int offset, int autolen)
     /* If str is NULL and autolen is 0, flush print buffer */
{
  long len;
  char *oldstr;

  if (!autolen) {
    if (!str)
      len = 0;
    else
      return;
  } else if (autolen > 0)
    len = autolen;
  else
    len = strlen(str XFORM_OK_PLUS offset);

  if (!pp->print_buffer) {
    /* Just getting the length */
    pp->print_position += len;
    pp->print_offset += len;
    return;
  }

  if (len + pp->print_position + 1 > pp->print_allocated) {
    if (len + 1 >= pp->print_allocated)
      pp->print_allocated = 2 * pp->print_allocated + len + 1;
    else
      pp->print_allocated = 2 * pp->print_allocated;

    oldstr = pp->print_buffer;
    {
      char *ca;
      ca = (char *)scheme_malloc_atomic(pp->print_allocated);
      pp->print_buffer = ca;
    }
    memcpy(pp->print_buffer, oldstr, pp->print_position);
  }

  memcpy(pp->print_buffer + pp->print_position, str + offset, len);
  pp->print_position += len;
  pp->print_offset += len;

  /* ----------- Do not use str after this point --------------- */
  /*  It might be quick_buffer, and another thread might try to  */
  /*  use the buffer.                                            */

  SCHEME_USE_FUEL(len);
  
  if (pp->print_maxlen > PRINT_MAXLEN_MIN) {
    if (pp->print_position > pp->print_maxlen) {
      long l = pp->print_maxlen;

      pp->print_buffer[l] = 0;
      pp->print_buffer[l - 1] = '.';
      pp->print_buffer[l - 2] = '.';
      pp->print_buffer[l - 3] = '.';

      pp->print_position = l;

      scheme_longjmp(*pp->print_escape, 1);
    }
  } else if ((pp->print_position > MAX_PRINT_BUFFER) || !str) {
    if (pp->print_port) {
      pp->print_buffer[pp->print_position] = 0;
      scheme_write_byte_string(pp->print_buffer, pp->print_position, pp->print_port);
      
      pp->print_position = 0;
    }
  }
}

static void print_utf8_string(PrintParams *pp, const char *str, int offset, int autolen)
{
  print_this_string(pp, str, offset, autolen);
}

void scheme_print_bytes(Scheme_Print_Params *pp, const char *str, int offset, int len)
{
  print_this_string(pp, str, offset, len);
}

void scheme_print_utf8(Scheme_Print_Params *pp, const char *str, int offset, int len)
{
  print_utf8_string(pp, str, offset, len);
}

static void print_number(PrintParams *pp, long n)
{
  unsigned char s[4];

  s[0] = (unsigned char)(n & 0xFF);
  s[1] = (unsigned char)((n >> 8) & 0xFF);
  s[2] = (unsigned char)((n >> 16) & 0xFF);
  s[3] = (unsigned char)((n >> 24) & 0xFF);  
  
  print_this_string(pp, (char *)s, 0, 4);
}

static void print_short_number(PrintParams *pp, long n)
{
  unsigned char s[2];

  s[0] = (unsigned char)(n & 0xFF);
  s[1] = (unsigned char)((n >> 8) & 0xFF);
  
  print_this_string(pp, (char *)s, 0, 2);
}

static void print_one_byte(PrintParams *pp, int n)
{
  unsigned char s[1];

  s[0] = n;
  
  print_this_string(pp, (char *)s, 0, 1);
}

static void print_compact_number(PrintParams *pp, long n)
{
  unsigned char s[2];

  if (n < 0) {
    if (n > -32) {
      s[0] = (unsigned char)(0xC0 | (-n));
      print_this_string(pp, (char *)s, 0, 1);
      return;
    } else {
      n = -n;
      s[0] = 0xE0;
    }
  } else if (n < 128) {
    s[0] = (unsigned char)n;
    print_this_string(pp, (char *)s, 0, 1);
    return;
  } else if (n < 0x4000) {
    s[0] = (unsigned char)(0x80 | (n & 0x3F));
    s[1] = (unsigned char)((n >> 6) & 0xFF);
    print_this_string(pp, (char *)s, 0, 2);
    return;
  } else {
    s[0] = 0xF0;
  }

  print_this_string(pp, (char *)s, 0, 1);
  print_number(pp, n);
}

static void do_print_string(int compact, int notdisplay, 
			    Scheme_Print_Params *pp, const mzchar *s, int offset, int l)
{
  int el, reset;
  char *buf;

  el = l * MAX_UTF8_CHAR_BYTES;
  if (el <= QUICK_ENCODE_BUFFER_SIZE) {
    if (quick_encode_buffer) {
      buf = quick_encode_buffer;
      quick_encode_buffer = NULL;
    } else
      buf = (char *)scheme_malloc_atomic(QUICK_ENCODE_BUFFER_SIZE);
    reset = 1;
  } else {
    buf = (char *)scheme_malloc_atomic(el);
    reset = 0;
  }
  el = scheme_utf8_encode(s, offset, offset + l, 
			  (unsigned char *)buf, 0, 0);
  if (compact) {
    print_compact(pp, CPT_CHAR_STRING);
    print_compact_number(pp, el);
    print_compact_number(pp, l);
    print_this_string(pp, buf, 0, el);
  } else {
    print_char_string(buf, el, s, offset, l, notdisplay, 0, pp);
  }
  if (reset)
    quick_encode_buffer = buf;
}

void scheme_print_string(Scheme_Print_Params *pp, const mzchar *s, int offset, int l)
{
  do_print_string(0, 0, pp, s, offset, l);
}

static void print_string_in_angle(PrintParams *pp, const char *start, const char *prefix, int slen)
{
  /* Used to do something special for type symbols. No more. */
  print_utf8_string(pp, prefix, 0, -1);
  print_utf8_string(pp, start, 0, slen);
}

#ifdef DO_STACK_CHECK

static Scheme_Object *print_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p2;
  Scheme_Marshal_Tables *mt = (Scheme_Marshal_Tables *)p->ku.k.p3;
  PrintParams *pp = (PrintParams *)p->ku.k.p5;
  mz_jmp_buf * volatile save;
  mz_jmp_buf newbuf;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p5 = NULL;

  save = pp->print_escape;
  pp->print_escape = &newbuf;
  if (scheme_setjmp(newbuf)) {
#ifdef MZ_PRECISE_GC
    scheme_make_pair(scheme_void, scheme_void);
#endif
    pp->print_escape = save;
    return scheme_void;
  } else {
    return print(o, 
		 p->ku.k.i1, 
		 p->ku.k.i2, 
		 ht,
                 mt,
		 pp) 
      ? scheme_true : scheme_false;
  }
}
#endif

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif
#include "../gc2/my_qsort.c"
#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

static int compare_keys(const void *a, const void *b)
{
  Scheme_Object *av, *bv;

  /* Atomic things first, because they could be used by
     marshaled syntax. This cuts donw on recursive reads
     at load time. */
# define SCHEME_FIRSTP(v) (SCHEME_SYMBOLP(v) \
                           || SCHEME_PATHP(v) \
                           || SCHEME_KEYWORDP(v) \
                           || SCHEME_CHAR_STRINGP(v) \
                           || SCHEME_BYTE_STRINGP(v) \
                           || SCHEME_CHARP(v) \
                           || SAME_TYPE(SCHEME_TYPE(v), scheme_module_index_type))
  av = ((Scheme_Object **)a)[0];
  bv = ((Scheme_Object **)b)[0];
  if (SCHEME_FIRSTP(av)) {
    if (!SCHEME_FIRSTP(bv))
      return -1;
  } else if (SCHEME_FIRSTP(bv))
    return 1;

  return ((long *)a)[1] - ((long *)b)[1];
}

static void sort_referenced_keys(Scheme_Marshal_Tables *mt)
{
  long j, size, pos = 0;
  Scheme_Object **keys;
  Scheme_Hash_Table *key_map;

  size = mt->st_refs->count;
  keys = MALLOC_N(Scheme_Object *, (2 * size));

  for (j = 0; j < mt->st_refs->size; j++) {
    if (mt->st_refs->vals[j]) {
      keys[pos] = mt->st_refs->keys[j];
      keys[pos + 1] = mt->st_refs->vals[j];
      pos += 2;
    }
  }

  my_qsort(keys, size, 2 * sizeof(Scheme_Object *), compare_keys);

  key_map = scheme_make_hash_table(SCHEME_hash_ptr);
  for (j = 0; j < size; j++) {
    scheme_hash_set(key_map, keys[(j << 1) + 1], scheme_make_integer(j+1));
  }
  mt->key_map = key_map;

  mt->sorted_keys = keys;
  mt->sorted_keys_count = size;
}

static void print_table_keys(int notdisplay, int compact, Scheme_Hash_Table *ht,
                             Scheme_Marshal_Tables *mt,
                             PrintParams *pp)
{
  long j, size, offset;
  Scheme_Object **keys, *key, *obj;

  size = mt->sorted_keys_count;
  keys = mt->sorted_keys;

  for (j = 0; j < size; j++) {
    offset = pp->print_offset;
    mt->shared_offsets[j] = offset;
    key = keys[j << 1];
    if (mt->rn_saved) {
      obj = scheme_hash_get(mt->rn_saved, key);
    } else {
      obj = NULL;
    }
    if (!obj)
      obj = key;
    mt->print_now = j + 1;
    print(obj ? obj : key, notdisplay, compact, ht, mt, pp);
    mt->print_now = 0;
  }
}

static int
print_substring(Scheme_Object *obj, int notdisplay, int compact, Scheme_Hash_Table *ht,
                Scheme_Marshal_Tables *mt,
		PrintParams *pp, char **result, long *rlen,
                int print_keys, long *klen)
{
  int closed;
  long save_alloc, save_pos, save_off, save_maxl;
  char *save_buf;
  Scheme_Object *save_port;

  save_alloc = pp->print_allocated;
  save_buf = pp->print_buffer;
  save_pos = pp->print_position;
  save_off = pp->print_offset;
  save_maxl = pp->print_maxlen;
  save_port = pp->print_port;
  
  /* If result is NULL, just measure the output. */
  if (result) {
    char *ca;
    pp->print_allocated = 50;
    ca = (char *)scheme_malloc_atomic(pp->print_allocated);
    pp->print_buffer = ca;
  } else {
    pp->print_allocated = 0;
    pp->print_buffer = NULL;
  }
  pp->print_position = 0;
  pp->print_offset = 0;
  pp->print_port = NULL;

  if (print_keys < 0) {
    print_table_keys(notdisplay, compact, ht, mt, pp);
    *klen = pp->print_offset;
  }

  closed = print(obj, notdisplay, compact, ht, mt, pp);
  
  if (print_keys > 0) {
    print_table_keys(notdisplay, compact, ht, mt, pp);
    *klen = pp->print_offset;
  }

  if (result)
    *result = pp->print_buffer;
  *rlen = pp->print_position;

  pp->print_allocated = save_alloc;
  pp->print_buffer = save_buf;
  pp->print_position = save_pos;
  pp->print_offset = save_off;
  pp->print_maxlen = save_maxl;
  pp->print_port = save_port;
  
  return closed;
}

static Scheme_Object *get_symtab_idx(Scheme_Marshal_Tables *mt, Scheme_Object *obj)
{
  Scheme_Object *idx;

  idx = scheme_hash_get(mt->symtab, obj);

  if (idx) {
    if (!mt->pass) {
      /* Record that we're referencing it */
      scheme_hash_set(mt->st_refs, obj, idx);
    }
  } else {
    if (mt->pass && mt->print_now) {
      idx = scheme_hash_get(mt->st_refs, obj);
      if (idx) {
        idx = scheme_hash_get(mt->key_map, idx);
        if (SCHEME_INT_VAL(idx) != mt->print_now)
          return idx; /* due to a cycle, we're refering to
                         something before it is printed. */
        idx = NULL; /* ok to print */
      }
    }
  }

  return idx;
}

static void set_symtab_shared(Scheme_Marshal_Tables *mt, Scheme_Object *obj)
{
  (void)get_symtab_idx(mt, obj);
}

static void print_general_symtab_ref(PrintParams *pp, Scheme_Object *idx, int cpt_id)
{
  int l;
  print_compact(pp, cpt_id);
  l = SCHEME_INT_VAL(idx);
  print_compact_number(pp, l);
}

static void print_symtab_ref(PrintParams *pp, Scheme_Object *idx)
{
  print_general_symtab_ref(pp, idx, CPT_SYMREF);
}

static int add_symtab(Scheme_Marshal_Tables *mt, Scheme_Object *obj)
{
  if (!mt->pass) {
    int l;
    l = mt->symtab->count + 1;
    scheme_hash_set(mt->symtab, obj, scheme_make_integer(l));
    return l;
  } else {
    Scheme_Object *key, *l;

    key = scheme_hash_get(mt->st_refs, obj);
    for (l = mt->st_ref_stack; !key && SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      key = scheme_hash_get((Scheme_Hash_Table *)SCHEME_CAR(l), obj);
    }

    if (!key) {
      /* There's no other reference to this object, so use dummy slot 0. */
      return 0;
    }

    key = scheme_hash_get(mt->key_map, key);

    scheme_hash_set(mt->symtab, obj, key);

    return SCHEME_INT_VAL(key);
  }
}

static void symtab_set(PrintParams *pp, Scheme_Marshal_Tables *mt, Scheme_Object *obj)
{
  (void)add_symtab(mt, obj);
}

static void print_symtab_set(PrintParams *pp, Scheme_Marshal_Tables *mt, Scheme_Object *obj)
{
  int l;
  l = add_symtab(mt, obj);
  print_compact_number(pp, l);
}

Scheme_Object *scheme_marshal_wrap_set(Scheme_Marshal_Tables *mt, Scheme_Object *obj, Scheme_Object *val)
{
  int l;
  l = add_symtab(mt, obj);
  if (l) {
    if (!mt->rn_saved) {
      Scheme_Hash_Table *rn_saved;
      rn_saved = scheme_make_hash_table(SCHEME_hash_ptr);
      mt->rn_saved = rn_saved;
    }
    if (mt->pass >= 2) {
      /* Done already */
    } else
      scheme_hash_set(mt->rn_saved, obj, val);

    if (mt->pass)
      return scheme_make_integer(l);
  }
  return val;
}

Scheme_Object *scheme_marshal_lookup(Scheme_Marshal_Tables *mt, Scheme_Object *obj)
{
  return get_symtab_idx(mt, obj);
}

void scheme_marshal_using_key(Scheme_Marshal_Tables *mt, Scheme_Object *obj)
{
  set_symtab_shared(mt, obj);
}

void scheme_marshal_push_refs(Scheme_Marshal_Tables *mt)
{
  Scheme_Object *p;
  Scheme_Hash_Table *st_refs;

  p = scheme_make_pair((Scheme_Object *)mt->st_refs,
                       mt->st_ref_stack);
  mt->st_ref_stack = p;

  st_refs = scheme_make_hash_table(SCHEME_hash_ptr);

  mt->st_refs = st_refs;
}

void scheme_marshal_pop_refs(Scheme_Marshal_Tables *mt, int keep)
{
  Scheme_Hash_Table *st_refs = mt->st_refs;

  mt->st_refs = (Scheme_Hash_Table *)SCHEME_CAR(mt->st_ref_stack);
  mt->st_ref_stack = SCHEME_CDR(mt->st_ref_stack);
  
  if (keep) {
    if (!mt->st_refs->count)
      mt->st_refs = st_refs;
    else {
      long i;
      for (i = 0; i < st_refs->size; i++) {
        if (st_refs->vals[i]) {
          scheme_hash_set(mt->st_refs, st_refs->keys[i], st_refs->vals[i]);
        }
      }
    }
  }
}

static void print_escaped(PrintParams *pp, int notdisplay, 
			  Scheme_Object *obj, Scheme_Hash_Table *ht,
                          Scheme_Marshal_Tables *mt, int shared)
{
  char *r;
  long len;
  Scheme_Object *idx;

  if (shared) {
    idx = get_symtab_idx(mt, obj);
    if (idx) {
      print_symtab_ref(pp, idx);
      return;
    }
  }

  print_substring(obj, notdisplay, 0, ht, NULL, pp, &r, &len, 0, NULL);

  print_compact(pp, CPT_ESCAPE);
  print_compact_number(pp, len);
  print_this_string(pp, r, 0, len);

  if (mt) {
    symtab_set(pp, mt, obj);
  }
}

static void cannot_print(PrintParams *pp, int notdisplay, 
			 Scheme_Object *obj, Scheme_Hash_Table *ht,
			 int compact)
{
  scheme_raise_exn(MZEXN_FAIL,
		   (compact
		    ? "%s: cannot marshal constant that is embedded in compiled code: %V"
		    : "%s: printing disabled for unreadable value: %V"),
		   notdisplay ? "write" : "display",
		   obj);
}

#ifdef SGC_STD_DEBUGGING
static void printaddress(PrintParams *pp, Scheme_Object *o)
{
  char buf[40];
  sprintf(buf, ":%lx", (long)o);
  print_this_string(pp, buf, 0, -1);
}
# define PRINTADDRESS(pp, obj) printaddress(pp, obj)
#else
# define PRINTADDRESS(pp, obj) /* empty */
#endif

static void print_named(Scheme_Object *obj, const char *kind,
			const char *s, int len, PrintParams *pp)
{
  print_utf8_string(pp, "#<", 0, 2);
  print_utf8_string(pp, kind, 0, -1);

  if (s) {
    print_utf8_string(pp, ":", 0, 1);

    print_utf8_string(pp, s, 0, len);
  }
   
  PRINTADDRESS(pp, obj);
  print_utf8_string(pp, ">", 0, 1);
}

static void always_scheme(PrintParams *pp, int reset)
{
  if (pp->honu_mode) {
    print_utf8_string(pp, "#sx", 0, 3);
    if (reset)
      pp->honu_mode = 0;
  }
}

static int
print(Scheme_Object *obj, int notdisplay, int compact, Scheme_Hash_Table *ht,
      Scheme_Marshal_Tables *mt, PrintParams *pp)
{
  int closed = 0;
  int save_honu_mode;

#if NO_COMPACT
  compact = 0;
#endif

#ifdef DO_STACK_CHECK
#define PRINT_COUNT_START 20
  {
    static int check_counter = PRINT_COUNT_START;

    if (!--check_counter) {
      check_counter = PRINT_COUNT_START;
      {
#include "mzstkchk.h"
	{
	  Scheme_Thread *p = scheme_current_thread;
	  PrintParams *pp2;

	  pp2 = copy_print_params(pp);

	  p->ku.k.p1 = (void *)obj;
	  p->ku.k.i1 = notdisplay;
	  p->ku.k.i2 = compact;
	  p->ku.k.p2 = (void *)ht;
	  p->ku.k.p3 = mt;
	  p->ku.k.p5 = pp2;

	  obj = scheme_handle_stack_overflow(print_k);

	  memcpy(pp, pp2, sizeof(PrintParams));

	  if (SCHEME_VOIDP(obj)) {
	    scheme_longjmp(*pp->print_escape, 1);
	  }

	  return closed;
	}
      }
    }
  }
#endif

  if (scheme_check_print_is_obj && !scheme_check_print_is_obj(obj)) {
    print_utf8_string(pp, "#<" "???" ">", 0, 6);
    return 1;
  }

  /* Built-in functions, exception types, eof, prop:waitable, ... */
  if (compact && (SCHEME_PROCP(obj) 
		  || SCHEME_STRUCT_TYPEP(obj) 
		  || SCHEME_EOFP(obj)
		  || SAME_TYPE(scheme_always_evt_type, SCHEME_TYPE(obj))
		  || SAME_TYPE(scheme_never_evt_type, SCHEME_TYPE(obj))
		  || SAME_TYPE(scheme_struct_property_type, SCHEME_TYPE(obj)))) {
    /* Check whether this is a global constant */
    Scheme_Object *val;
    val = scheme_hash_get(global_constants_ht, obj);
    if (val) {
      /* val is a scheme_variable_type object, instead of something else */
      obj = val;
    }
  }

  save_honu_mode = pp->honu_mode;

  if (ht && HAS_SUBSTRUCT(obj, ssQUICK)) {
    long val;
    
    val = (long)scheme_hash_get(ht, obj);
    
    if (val) {
      if (val != 1) {
	if (compact) {
	  print_escaped(pp, notdisplay, obj, ht, mt, 0);
	  return 1;
	} else {
	  if (val > 0) {
	    always_scheme(pp, 1);
	    sprintf(quick_buffer, "#%ld=", (val - 3) >> 1);
	    print_utf8_string(pp, quick_buffer, 0, -1);
	    scheme_hash_set(ht, obj, (Scheme_Object *)(-val));
	  } else {
	    always_scheme(pp, 0);
	    sprintf(quick_buffer, "#%ld#", ((-val) - 3) >> 1);
	    print_utf8_string(pp, quick_buffer, 0, -1);
	    return 0;
	  }
	}
      }
    }
  }

  if (SCHEME_SYMBOLP(obj)
      || SCHEME_KEYWORDP(obj))
    {
      int l;
      Scheme_Object *idx;
      int is_kw;

      is_kw = SCHEME_KEYWORDP(obj);

      if (compact)
	idx = get_symtab_idx(mt, obj);
      else
	idx = NULL;

      if (idx) {
        print_symtab_ref(pp, idx);
      } else if (compact) {
	int weird;

	weird = SCHEME_SYM_WEIRDP(obj);
	l = SCHEME_SYM_LEN(obj);
	if (!weird && !is_kw && (l < CPT_RANGE(SMALL_SYMBOL))) {
	  unsigned char s[1];
	  s[0] = l + CPT_SMALL_SYMBOL_START;
	  print_this_string(pp, (char *)s, 0, 1);
	} else {
	  print_compact(pp, (is_kw
			     ? CPT_KEYWORD
			     : (weird ? CPT_WEIRD_SYMBOL : CPT_SYMBOL)));
	  if (weird) {
	    print_compact_number(pp, SCHEME_SYM_UNINTERNEDP(obj) ? 1 : 0);
	  }
	  print_compact_number(pp, l);
	  /* Note: the written symbol table will preserve equivalence
             of uninterned symbols for a single compiled
             expression. */
	}
	print_this_string(pp, scheme_symbol_val(obj), 0, l);

        symtab_set(pp, mt, obj);
      } else if (notdisplay) {
	if (pp->honu_mode) {
	  /* Honu symbol... */
	  if (is_kw)
	    print_utf8_string(pp, "key(", 0, 4);
	  else
	    print_utf8_string(pp, "sym(", 0, 4);
	  {
	    int i;
	    /* Check for fast case: */
	    for (i = SCHEME_SYM_LEN(obj); i--; ) {
	      if (((unsigned char *)SCHEME_SYM_VAL(obj))[i] > 127)
		break;
	    }
	    if (i < 0) {
	      /* Fits as byte string (fast case) */
	      print_byte_string((char *)obj, SCHEME_SYMSTR_OFFSET(obj), SCHEME_SYM_LEN(obj),
				notdisplay, pp);
	    } else {
	      /* Coerce to string (slower) */
	      Scheme_Object *s;
	      s = scheme_make_sized_offset_utf8_string((char *)obj,
						       SCHEME_SYMSTR_OFFSET(obj),
						       SCHEME_SYM_LEN(obj));
	      do_print_string(0, notdisplay, pp, SCHEME_CHAR_STR_VAL(s), 0, SCHEME_CHAR_STRLEN_VAL(s));
	    }
	  }
	  print_utf8_string(pp, ")", 0, 1);
	} else {
	  const char *s;
	  
	  if (is_kw)
	    print_utf8_string(pp, "#:", 0, 2);
	  s = scheme_symbol_name_and_size(obj, (unsigned int *)&l, 
					  ((pp->can_read_pipe_quote 
					    ? SCHEME_SNF_PIPE_QUOTE
					    : SCHEME_SNF_NO_PIPE_QUOTE)
					   | (pp->case_sens
					      ? 0
					      : SCHEME_SNF_NEED_CASE)
					   | (is_kw
					      ? SCHEME_SNF_KEYWORD
					      : 0)));
	  print_utf8_string(pp, s, 0, l);
	}
      } else {
	if (is_kw)
	  print_utf8_string(pp, "#:", 0, 2);
	print_utf8_string(pp, (char *)obj, ((char *)(SCHEME_SYM_VAL(obj))) - ((char *)obj), 
			  SCHEME_SYM_LEN(obj));
      }
    }
  else if (SCHEME_BYTE_STRINGP(obj))
    {
      if (compact) {
	int l;
        Scheme_Object *idx;

	idx = get_symtab_idx(mt, obj);
        if (idx) {
          print_symtab_ref(pp, idx);
        } else {
          print_compact(pp, CPT_BYTE_STRING);
          l = SCHEME_BYTE_STRTAG_VAL(obj);
          print_compact_number(pp, l);
          print_this_string(pp, SCHEME_BYTE_STR_VAL(obj), 0, l);

          symtab_set(pp, mt, obj);
        }
      } else {
	if (notdisplay) {
	  always_scheme(pp, 0);
	  print_utf8_string(pp, "#", 0, 1);
	}
	print_byte_string(SCHEME_BYTE_STR_VAL(obj), 
			  0,
			  SCHEME_BYTE_STRLEN_VAL(obj), 
			  notdisplay, pp);
	closed = 1;
      }
    }
  else if (SCHEME_CHAR_STRINGP(obj))
    {
      Scheme_Object *idx;

      if (compact)
        idx = get_symtab_idx(mt, obj);
      else
        idx = NULL;

      if (idx) {
        print_symtab_ref(pp, idx);
      } else {
        do_print_string(compact, notdisplay, pp, 
                        SCHEME_CHAR_STR_VAL(obj), 0, SCHEME_CHAR_STRTAG_VAL(obj));
        if (compact)
          symtab_set(pp, mt, obj);
      }
      closed = 1;
    }
  else if (SCHEME_CHARP(obj))
    {
      if (compact) {
	int cv;
	print_compact(pp, CPT_CHAR);
	cv = SCHEME_CHAR_VAL(obj);
	print_compact_number(pp, cv);
      } else if (notdisplay && pp->honu_mode) {
	/* Honu char */
	char s[MAX_UTF8_CHAR_BYTES];
	mzchar us[1];
	int l;
	us[0] = SCHEME_CHAR_VAL(obj);
	l = scheme_utf8_encode(us, 0, 1, (unsigned char *)s, 0, 0);
	print_char_string(s, l, us, 0, 1, notdisplay, 1, pp);
      } else
	print_char(obj, notdisplay, pp);
    }
  else if (SCHEME_INTP(obj))
    {
      if (compact) {
	long v = SCHEME_INT_VAL(obj);
	if (v >= 0 && v < CPT_RANGE(SMALL_NUMBER)) {
	  unsigned char s[1];
	  s[0] = (unsigned char)(v + CPT_SMALL_NUMBER_START);
	  print_this_string(pp, (char *)s, 0, 1);
	} else {
          /* Make sure it's a fixnum on all platforms... */
          if ((v >= -1073741824) && (v <= 1073741823)) {
            print_compact(pp, CPT_INT);
            print_compact_number(pp, v);
          } else {
            print_escaped(pp, notdisplay, obj, ht, mt, 1);
          }
	}
      } else {
	sprintf(quick_buffer, "%ld", SCHEME_INT_VAL(obj));
	print_utf8_string(pp, quick_buffer, 0, -1);
      }
    }
  else if (SCHEME_NUMBERP(obj))
    {
      if (compact) {
	print_escaped(pp, notdisplay, obj, ht, mt, 1);
	closed = 1;
      } else {
	if (SCHEME_COMPLEXP(obj))
	  always_scheme(pp, 0);
	print_utf8_string(pp, scheme_number_to_string(10, obj), 0, -1);
      }
    }
  else if (SCHEME_NULLP(obj))
    {
      if (compact) {
	print_compact(pp, CPT_NULL);
      } else {
	if (pp->honu_mode)
	  print_utf8_string(pp, "null", 0, 4);
	else
	  print_utf8_string(pp, "()", 0, 2);
	closed = 1;
      }
    }
  else if (SCHEME_PAIRP(obj))
    {
      print_pair(obj, notdisplay, compact, ht, mt, pp);
      closed = 1;
    }
  else if (SCHEME_VECTORP(obj))
    {
      print_vector(obj, notdisplay, compact, ht, mt, pp);
      closed = 1;
    }
  else if ((compact || pp->print_box) && SCHEME_BOXP(obj))
    {
      if (compact && !pp->print_box) {
	closed = print(scheme_protect_quote(obj), notdisplay, compact, ht, mt, pp);
      } else {
	if (compact)
	  print_compact(pp, CPT_BOX);
	else {
	  always_scheme(pp, 1);
	  print_utf8_string(pp, "#&", 0, 2);
	}
	closed = print(SCHEME_BOX_VAL(obj), notdisplay, compact, ht, mt, pp);
      }
    }
  else if ((compact || pp->print_hash_table) && SCHEME_HASHTP(obj))
    {
      Scheme_Hash_Table *t;
      Scheme_Object **keys, **vals, *val;
      int i, size, did_one = 0;

      if (compact) {
	print_compact(pp, CPT_HASH_TABLE);
	if (scheme_is_hash_table_equal(obj))
	  print_compact_number(pp, 1);
	else
	  print_compact_number(pp, 0);
      } else {
	always_scheme(pp, 1);
	print_utf8_string(pp, "#hash", 0, 5);
	if (!scheme_is_hash_table_equal(obj))
	  print_utf8_string(pp, "eq", 0, 2);
	print_utf8_string(pp, "(", 0, 1);
      }

      t = (Scheme_Hash_Table *)obj;
      if (compact)
	print_compact_number(pp, t->count);

      keys = t->keys;
      vals = t->vals;
      size = t->size;
      for (i = 0; i < size; i++) {
	if (vals[i]) {
	  if (!compact) {
	    if (did_one)
	      print_utf8_string(pp, " ", 0, 1);
	    print_utf8_string(pp, "(", 0, 1);
	  }
	  val = vals[i];
	  print(keys[i], notdisplay, compact, ht, mt, pp);
	  if (!compact)
	    print_utf8_string(pp, " . ", 0, 3);
	  print(val, notdisplay, compact, ht, mt, pp);
	  if (!compact)
	    print_utf8_string(pp, ")", 0, 1);
	  did_one++;
	}
      }

      if (!compact)
	print_utf8_string(pp, ")", 0, 1);

      closed = 1;
    }
  else if (SAME_OBJ(obj, scheme_true))
    {
      if (compact)
	print_compact(pp, CPT_TRUE);
      else if (pp->honu_mode)
	print_utf8_string(pp, "true", 0, 4);
      else
	print_utf8_string(pp, "#t", 0, 2);
    }
  else if (SAME_OBJ(obj, scheme_false))
    {
      if (compact)
	print_compact(pp, CPT_FALSE);
      else if (pp->honu_mode)
	print_utf8_string(pp, "false", 0, 5);
      else
	print_utf8_string(pp, "#f", 0, 2);
    }
  else if (compact && SAME_OBJ(obj, scheme_void))
    {
      print_compact(pp, CPT_VOID);
    }
  else if (SCHEME_STRUCTP(obj))
    {
      if (compact || !pp->print_unreadable)
	cannot_print(pp, notdisplay, obj, ht, compact);
      else if (scheme_is_writable_struct(obj)) {
	custom_write_struct(obj, ht, mt, pp, notdisplay);
      } else {
	int pb;

	pb = pp->print_struct && PRINTABLE_STRUCT(obj, pp);

	if (pb) {
	  obj = scheme_struct_to_vector(obj, NULL, pp->inspector);
	  closed = print(obj, notdisplay, compact, ht, mt, pp);
	} else {
	  Scheme_Object *src;

	  if (SCHEME_PROC_STRUCTP(obj)) {
	    /* Name by procedure? */
	    src = scheme_proc_struct_name_source(obj);
	  } else
	    src = obj;

	  if (SAME_OBJ(src, obj)) {
	    print_utf8_string(pp, "#<struct:", 0, 9);
	    {
	      int l;
	      const char *s;
	      Scheme_Object *name = SCHEME_STRUCT_NAME_SYM(obj);
	      
	      s = scheme_symbol_name_and_size(name, (unsigned int *)&l, 
					      (pp->print_struct
					       ? SCHEME_SNF_FOR_TS
					       : (pp->can_read_pipe_quote 
						  ? SCHEME_SNF_PIPE_QUOTE
						  : SCHEME_SNF_NO_PIPE_QUOTE)));
	      print_utf8_string(pp, s, 0, l);
	    }
	    PRINTADDRESS(pp, obj);
	    print_utf8_string(pp, ">", 0, 1);
	  } else {
	    closed = print(src, notdisplay, compact, ht, mt, pp);
	  }
	}
      }

      closed = 1;
    }
  else if (SCHEME_GENERAL_PATHP(obj))
    {
      if (compact && SCHEME_PATHP(obj)) {
	/* Needed for srclocs in procedure names */
	Scheme_Object *idx;
	int l;
	
	idx = get_symtab_idx(mt, obj);
	if (idx) {
          print_symtab_ref(pp, idx);
	} else {
	  Scheme_Object *orig_obj = obj, *dir;
	  
	  dir = scheme_get_param(scheme_current_config(),
				 MZCONFIG_WRITE_DIRECTORY);
	  if (SCHEME_PATHP(dir)) {
	    obj = scheme_extract_relative_to(obj, dir);
	  }

	  print_compact(pp, CPT_PATH);

	  l = SCHEME_PATH_LEN(obj);
	  print_compact_number(pp, l);
	  print_this_string(pp, SCHEME_PATH_VAL(obj), 0, l);

          symtab_set(pp, mt, orig_obj);
	}
      } else if (!pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	if (notdisplay) {
          if (SCHEME_PATHP(obj)) {
            print_utf8_string(pp, "#<path:", 0, 7);
          } else {
            switch (SCHEME_TYPE(obj)) {
            case scheme_windows_path_type:
              print_utf8_string(pp, "#<windows-path:", 0, 15);
              break;
            default:
            case scheme_unix_path_type:
              print_utf8_string(pp, "#<unix-path:", 0, 12);
              break;
            }
          }
        }
	{
	  Scheme_Object *str;
	  str = scheme_path_to_char_string(obj);
	  print(str, 0, 0, ht, mt, pp);
	}
	if (notdisplay) {
	  PRINTADDRESS(pp, obj);
	  print_utf8_string(pp, ">", 0, 1);
	}
      }
    }
  else if (SCHEME_PRIMP(obj) && ((Scheme_Primitive_Proc *)obj)->name)
    {
      if (compact || !pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	print_utf8_string(pp, "#<", 0, 2);
	print_string_in_angle(pp, ((Scheme_Primitive_Proc *)obj)->name, "primitive:", -1);
	PRINTADDRESS(pp, obj);
	print_utf8_string(pp, ">", 0, 1);
      }
      closed = 1;
    }
  else if (SCHEME_CLSD_PRIMP(obj) && ((Scheme_Closed_Primitive_Proc *)obj)->name)
    {
      if (compact || !pp->print_unreadable)
	cannot_print(pp, notdisplay, obj, ht, compact);
      else {
	if (SCHEME_STRUCT_PROCP(obj)) {
	  print_named(obj, "struct-procedure", 
		      ((Scheme_Closed_Primitive_Proc *)obj)->name, 
		      -1, pp);
	} else {
	  print_utf8_string(pp, "#<", 0, 2);
	  print_string_in_angle(pp, ((Scheme_Closed_Primitive_Proc *)obj)->name, "primitive:", -1);
	  PRINTADDRESS(pp, obj);
	  print_utf8_string(pp, ">", 0, 1);
	}
      }

      closed = 1;
    }
  else if (SCHEME_CLOSUREP(obj)
	   || SAME_TYPE(SCHEME_TYPE(obj), scheme_native_closure_type))
    {
      if (compact || !pp->print_unreadable) {
	int done = 0;
	if (compact) {
	  if (SCHEME_TYPE(obj) == scheme_closure_type) {
	    Scheme_Closure *closure = (Scheme_Closure *)obj;
	    if (ZERO_SIZED_CLOSUREP(closure)) {
	      /* Print original `lambda' code. Closure conversion can cause
                 an empty closure to be duplicated in the code tree, so hash it. */
              Scheme_Object *idx;
              idx = get_symtab_idx(mt, obj);
              if (idx) {
                print_symtab_ref(pp, idx);
              } else {
                print_compact(pp, CPT_CLOSURE);
                print_symtab_set(pp, mt, obj);
                print((Scheme_Object *)SCHEME_COMPILED_CLOS_CODE(closure), notdisplay, compact, ht, mt, pp);
              }
              compact = 1;
              done = 1;
	    }
	  } else if (SCHEME_TYPE(obj) == scheme_case_closure_type) {
	    obj = scheme_unclose_case_lambda(obj, 0);
	    if (!SCHEME_PROCP(obj)) {
	      /* Print original `case-lambda' code: */
	      compact = print(obj, notdisplay, compact, ht, mt, pp);
	      done = 1;
	    }
	  }
	}
	if (!done)
	  cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	int len;
	const char *s;
	s = scheme_get_proc_name(obj, &len, 0);
	
	print_named(obj, "procedure", s, len, pp);
      }
      closed = 1;
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_type_type))
    {
      if (compact || !pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	print_utf8_string(pp, "#<", 0, 2);
	print_string_in_angle(pp, scheme_symbol_val(((Scheme_Struct_Type *)obj)->name),
			      "struct-type:",
			      SCHEME_SYM_LEN(((Scheme_Struct_Type *)obj)->name));
	PRINTADDRESS(pp, obj);
	print_utf8_string(pp, ">", 0, 1);
      }
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_property_type))
    {
      if (compact || !pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	print_utf8_string(pp, "#<", 0, 2);
	print_string_in_angle(pp, scheme_symbol_val(((Scheme_Struct_Property *)obj)->name),
			      "struct-type-property:", 
			      SCHEME_SYM_LEN(((Scheme_Struct_Property *)obj)->name));
	PRINTADDRESS(pp, obj);
	print_utf8_string(pp, ">", 0, 1);
      }
    }
  else if (SCHEME_THREADP(obj) && (((Scheme_Thread *)obj)->name))
    {
      if (compact || !pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	Scheme_Thread *t = (Scheme_Thread *)obj;
	print_utf8_string(pp, "#<thread:", 0, 9);
	print_utf8_string(pp, scheme_symbol_val(t->name), 0, SCHEME_SYM_LEN(t->name));
	print_utf8_string(pp, ">", 0, 1);
      }
    }
  else if (SCHEME_INPORTP(obj))
    {
      if (compact || !pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	Scheme_Input_Port *ip;
	ip = (Scheme_Input_Port *)obj;
	print_utf8_string(pp, "#<input-port", 0, 12);
	if (ip->name) {
	  if (SCHEME_PATHP(ip->name)) {
	    print_utf8_string(pp, ":", 0, 1);
	    print_utf8_string(pp, SCHEME_BYTE_STR_VAL(ip->name), 0, SCHEME_BYTE_STRLEN_VAL(ip->name));
	  } else if (SCHEME_SYMBOLP(ip->name)) {
	    print_utf8_string(pp, ":", 0, 1);
	    print_utf8_string(pp, scheme_symbol_val(ip->name), 0, SCHEME_SYM_LEN(ip->name));
	  }
	}
	print_utf8_string(pp, ">", 0, 1);
      }
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_regexp_type))
    {
       if (compact) {
	 print_escaped(pp, notdisplay, obj, ht, mt, 1);
       } else {
	 Scheme_Object *src;
	 src = scheme_regexp_source(obj);
	 if (src) {
	   if (scheme_is_pregexp(obj))
	     print_utf8_string(pp, "#px", 0, 3);
	   else
	     print_utf8_string(pp, "#rx", 0, 3);
	   print(src, 1, 0, ht, mt, pp);
	 } else if (compact || !pp->print_unreadable)
	   cannot_print(pp, notdisplay, obj, ht, compact);
	 else
	   print_utf8_string(pp, "#<regexp>", 0, 9);
	 closed = 1;
       }
    }
  else if (SCHEME_OUTPORTP(obj))
    {
      if (compact || !pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else {
	Scheme_Output_Port *op;
	op = (Scheme_Output_Port *)obj;
	print_utf8_string(pp, "#<output-port", 0, 13);
	if (op->name) {
	  if (SCHEME_PATHP(op->name)) {
	    print_utf8_string(pp, ":", 0, 1);
	    print_utf8_string(pp, SCHEME_BYTE_STR_VAL(op->name), 0, SCHEME_BYTE_STRLEN_VAL(op->name));
	  } else if (SCHEME_SYMBOLP(op->name)) {
	    print_utf8_string(pp, ":", 0, 1);
	    print_utf8_string(pp, scheme_symbol_val(op->name), 0, SCHEME_SYM_LEN(op->name));
	  }
	}
	print_utf8_string(pp, ">", 0, 1);
      }
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_prompt_tag_type)
           && SCHEME_CDR(obj) && !(compact || !pp->print_unreadable))
    {
      print_utf8_string(pp, "#<", 0, 2);
      print_string_in_angle(pp, scheme_symbol_val(SCHEME_CDR(obj)),
                            "continuation-prompt-tag:", 
                            SCHEME_SYM_LEN(SCHEME_CDR(obj)));
      PRINTADDRESS(pp, obj);
      print_utf8_string(pp, ">", 0, 1);
    }
    else if (SCHEME_CPTRP(obj))
    {
      Scheme_Object *tag = SCHEME_CPTR_TYPE(obj);
      if (compact || !pp->print_unreadable) {
	cannot_print(pp, notdisplay, obj, ht, compact);
      } else if (tag == NULL) {
	print_utf8_string(pp, "#<cpointer>", 0, 11);
      } else {
        Scheme_Object *name = tag;
        if (SCHEME_PAIRP(name)) name = SCHEME_CAR(name);
	print_utf8_string(pp, "#<cpointer:", 0, 11);
        if (SCHEME_SYMBOLP(name)) {
          print_this_string(pp,
                            (char*)name,
                            ((char*)(SCHEME_SYM_VAL(name))) - ((char*)name),
                            SCHEME_SYM_LEN(name));
        } else if (SCHEME_BYTE_STRINGP(name)) {
          print_byte_string(SCHEME_BYTE_STR_VAL(name),
			    0,
                            SCHEME_BYTE_STRLEN_VAL(name),
                            0, pp);
        } else if (SCHEME_CHAR_STRINGP(name)) {
	  scheme_print_string(pp, SCHEME_CHAR_STR_VAL(name), 0,
                              SCHEME_CHAR_STRTAG_VAL(name));
        } else {
          print_utf8_string(pp, "#", 0, 1);
        }
	print_utf8_string(pp, ">", 0, 1);
	closed = 1;
      }
    }
  else if (SCHEME_STXP(obj))
    {
      if (compact) {
	if (scheme_syntax_is_graph(obj))
	  print_compact(pp, CPT_GSTX);
	else
	  print_compact(pp, CPT_STX);
	
	/* "2" in scheme_syntax_to_datum() call preserves wraps. */
	closed = print(scheme_syntax_to_datum(obj, 2, mt), 
		       notdisplay, 1, ht, mt, pp);
      } else if (pp->print_unreadable) {
	Scheme_Stx *stx = (Scheme_Stx *)obj;
	if ((stx->srcloc->line >= 0) || (stx->srcloc->pos >= 0)) {
	  print_utf8_string(pp, "#<syntax:", 0, 9);
	  if (stx->srcloc->src && SCHEME_PATHP(stx->srcloc->src)) {
	    print_utf8_string(pp, SCHEME_BYTE_STR_VAL(stx->srcloc->src), 0, SCHEME_BYTE_STRLEN_VAL(stx->srcloc->src));
	    print_utf8_string(pp, ":", 0, 1);
	  }
	  if (stx->srcloc->line >= 0)
	    sprintf(quick_buffer, "%ld:%ld", stx->srcloc->line, stx->srcloc->col-1);
	  else
	    sprintf(quick_buffer, ":%ld", stx->srcloc->pos);
	  print_utf8_string(pp, quick_buffer, 0, -1);
	  print_utf8_string(pp, ">", 0, 1);
	} else
	  print_utf8_string(pp, "#<syntax>", 0, 9);
      } else {
	cannot_print(pp, notdisplay, obj, ht, compact);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_module_index_type)) 
    {
      Scheme_Object *idx;

      idx = get_symtab_idx(mt, obj);
      if (idx) {
        print_symtab_ref(pp, idx);
      } else {
	print_compact(pp, CPT_MODULE_INDEX);
	print(((Scheme_Modidx *)obj)->path, notdisplay, 1, ht, mt, pp);
	print(((Scheme_Modidx *)obj)->base, notdisplay, 1, ht, mt, pp);
        symtab_set(pp, mt, obj);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_module_variable_type)
	   && !((Module_Variable *)obj)->mod_phase)
    {
      Scheme_Object *idx;

      idx = get_symtab_idx(mt, obj);
      if (idx) {
        print_symtab_ref(pp, idx);
      } else {
	Module_Variable *mv;

	print_compact(pp, CPT_MODULE_VAR);
	mv = (Module_Variable *)obj;
	print(mv->modidx, notdisplay, 1, ht, mt, pp);
	print(mv->sym, notdisplay, 1, ht, mt, pp);
	print_compact_number(pp, mv->pos);

        symtab_set(pp, mt, obj);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_variable_type)
	   && (((Scheme_Bucket_With_Flags *)obj)->flags & GLOB_HAS_REF_ID))
    {
      int pos;
      pos = ((Scheme_Bucket_With_Ref_Id *)obj)->id;
      print_compact(pp, CPT_REFERENCE);
      print_compact_number(pp, pos);
    }   
  else if (compact 
	   && (SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)
	       || SAME_TYPE(SCHEME_TYPE(obj), scheme_local_unbox_type)))
    {
      int unbox = SAME_TYPE(SCHEME_TYPE(obj), scheme_local_unbox_type);
      Scheme_Local *loc = (Scheme_Local *)obj;
      if (loc->position < CPT_RANGE(SMALL_LOCAL)) {
	unsigned char s[1];
	s[0] = loc->position + (unbox 
				? CPT_SMALL_LOCAL_UNBOX_START 
				: CPT_SMALL_LOCAL_START);
	print_this_string(pp, (char *)s, 0, 1);
      } else {
	print_compact(pp, unbox ? CPT_LOCAL_UNBOX : CPT_LOCAL);
	print_compact_number(pp, loc->position);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_application_type))
    {
      Scheme_App_Rec *app;
      int i;

      app = (Scheme_App_Rec *)obj;

      if (app->num_args < CPT_RANGE(SMALL_APPLICATION)) {
	unsigned char s[1];
	s[0] = CPT_SMALL_APPLICATION_START + app->num_args;
	print_this_string(pp, (char *)s, 0, 1);
      } else {
	print_compact(pp, CPT_APPLICATION);
	print_compact_number(pp, app->num_args);
      }

      for (i = 0; i < app->num_args + 1; i++) {
	closed = print(scheme_protect_quote(app->args[i]), notdisplay, 1, NULL, mt, pp);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_application2_type))
    {
      Scheme_App2_Rec *app;
      unsigned char s[1];

      app = (Scheme_App2_Rec *)obj;

      s[0] = CPT_SMALL_APPLICATION_START + 1;
      print_this_string(pp, (char *)s, 0, 1);

      print(scheme_protect_quote(app->rator), notdisplay, 1, NULL, mt, pp);
      closed = print(scheme_protect_quote(app->rand), notdisplay, 1, NULL, mt, pp);
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_application3_type))
    {
      Scheme_App3_Rec *app;
      unsigned char s[1];

      app = (Scheme_App3_Rec *)obj;

      s[0] = CPT_SMALL_APPLICATION_START + 2;
      print_this_string(pp, (char *)s, 0, 1);

      print(scheme_protect_quote(app->rator), notdisplay, 1, NULL, mt, pp);
      print(scheme_protect_quote(app->rand1), notdisplay, 1, NULL, mt, pp);
      closed = print(scheme_protect_quote(app->rand2), notdisplay, 1, NULL, mt, pp);
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_let_one_type))
    {
      Scheme_Let_One *lo;

      lo = (Scheme_Let_One *)obj;

      print_compact(pp, CPT_LET_ONE);
      print(scheme_protect_quote(lo->value), notdisplay, 1, NULL, mt, pp);
      closed = print(scheme_protect_quote(lo->body), notdisplay, 1, NULL, mt, pp);
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_branch_type))
    {
      Scheme_Branch_Rec *b;

      b = (Scheme_Branch_Rec *)obj;

      print_compact(pp, CPT_BRANCH);
      print(scheme_protect_quote(b->test), notdisplay, 1, NULL, mt, pp);
      print(scheme_protect_quote(b->tbranch), notdisplay, 1, NULL, mt, pp);
      closed = print(scheme_protect_quote(b->fbranch), notdisplay, 1, NULL, mt, pp);
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_quote_compilation_type))
    {
      Scheme_Hash_Table *q_ht;
      Scheme_Object *v;
      int counter = 1, qpht, qpb;

      v = SCHEME_PTR_VAL(obj);

      /* A quoted expression may have graph structure. We assume that
	 this structure is local within the quoted expression. */

      qpht = pp->print_hash_table;
      qpb = pp->print_box;
      /* Boxes and hash tables can be literals, so we need to
	 enable printing as we write compiled code: */
      pp->print_hash_table = 1;
      pp->print_box = 1;
      
      q_ht = scheme_make_hash_table(SCHEME_hash_ptr);
      setup_graph_table(v, q_ht, &counter, pp);

      if (compact)
	print_compact(pp, CPT_QUOTE);
      else {
#if !NO_COMPACT
	/* Doesn't happen: */
	scheme_signal_error("internal error: non-compact quote compilation");
	return 0;
#endif
      }

      compact = print(v, notdisplay, 1, q_ht, mt, pp);

      pp->print_hash_table = qpht;
      pp->print_box = qpb;
    }
  else if (
#if !NO_COMPACT
	   compact && 
#endif
	   SAME_TYPE(SCHEME_TYPE(obj), scheme_svector_type))
    {
      mzshort l, *v;
      l = SCHEME_SVEC_LEN(obj);
      v = (mzshort *)SCHEME_SVEC_VEC(obj);
      
#if NO_COMPACT
      print_this_string(pp, "[", 0, 1);
      {
	int i; 
	char s[10];

	for (i = 0; i < l; i++) {
	  if (i)
	    print_this_string(pp, " ", 0, 1);
	  sprintf(s, "%d", (int)v[i]);
	  print_this_string(pp, s, 0, -1);
	}
      }
      print_this_string(pp, "]", 0, 1);
#else
      if (l < CPT_RANGE(SMALL_SVECTOR)) {
	unsigned char s[1];
	s[0] = l + CPT_SMALL_SVECTOR_START;
	print_this_string(pp, (char *)s, 0, 1);
      } else {
	print_compact(pp, CPT_SVECTOR);
	print_compact_number(pp, l);
      }
      while (l--) {
	int n = v[l];
	print_compact_number(pp, n);
      }
#endif
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_delay_syntax_type))
    {
      /* wraps a syntax object that we might load on demand,
         instead of when the using code is loaded */
      Scheme_Object *idx;

      if (!mt->pass) {
        if (!mt->delay_map) {
          Scheme_Hash_Table *delay_map;
          delay_map = scheme_make_hash_table(SCHEME_hash_ptr);
          mt->delay_map = delay_map;
        }
        scheme_hash_set(mt->delay_map, SCHEME_PTR_VAL(obj), obj);
      } else
        obj = scheme_hash_get(mt->delay_map, SCHEME_PTR_VAL(obj));

      idx = get_symtab_idx(mt, obj);

      if (idx) {
        print_general_symtab_ref(pp, idx, CPT_DELAY_REF);
      } else {
        print(SCHEME_PTR_VAL(obj), notdisplay, 1, ht, mt, pp);
        symtab_set(pp, mt, obj);
        set_symtab_shared(mt, obj);
      }
    }
  else if (scheme_type_writers[SCHEME_TYPE(obj)]
#if !NO_COMPACT
	   && (compact || SAME_TYPE(SCHEME_TYPE(obj), scheme_compilation_top_type))
#endif
	   )
    {
      Scheme_Type t = SCHEME_TYPE(obj);
      Scheme_Object *v;
      long slen;

      if (t >= _scheme_last_type_) {
	/* Doesn't happen: */
	scheme_signal_error("internal error: bad type with writer");
	return 0;
      }

      if (!global_constants_ht) {
	REGISTER_SO(global_constants_ht);
	global_constants_ht = scheme_map_constants_to_globals();
      }

      if (compact) {
	if (t < CPT_RANGE(SMALL_MARSHALLED)) {
	  unsigned char s[1];
	  s[0] = t + CPT_SMALL_MARSHALLED_START;
	  print_this_string(pp, (char *)s, 0, 1);
	} else {
	  print_compact(pp, CPT_MARSHALLED);
	  print_compact_number(pp, t);
	}
      } else {
	print_this_string(pp, "#~", 0, 2);
#if NO_COMPACT
	if (t < _scheme_last_type_) {
	  sprintf (quick_buffer, "%ld", (long)SCHEME_TYPE(obj));
	  print_this_string(pp, quick_buffer, 0, -1);
	} else
	  print_this_string(pp, scheme_get_type_name(t), 0, -1);
#endif
      }

      {
	Scheme_Type_Writer writer;
	writer = scheme_type_writers[t];
	v = writer(obj);
      }

      if (compact)
	closed = print(v, notdisplay, 1, NULL, mt, pp);
      else {
        Scheme_Hash_Table *st_refs, *symtab, *rns;
        long *shared_offsets;
        long st_len, j, shared_offset, start_offset;

        mt = MALLOC_ONE_RT(Scheme_Marshal_Tables);
        SET_REQUIRED_TAG(mt->type = scheme_rt_marshal_info);

        /* Track which shared values are referenced: */
        st_refs = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->st_refs = st_refs;
        mt->st_ref_stack = scheme_null;

	/* "Print" the string once to determine graph references. On this pass,
           we first assume that everything is shared and make up sequential
           keys, but we also keep track of which things are actually shared;
           we'll map the original keys to a compacted set of keys for the
           later passes. */
	symtab = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->symtab = symtab;
	rns = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->rns = rns;
        mt->reverse_map = NULL;
        mt->pass = 0;
        scheme_hash_set(symtab, scheme_void, scheme_true); /* indicates registration phase */
	print_substring(v, notdisplay, 1, NULL, mt, pp, NULL, &slen, 0, NULL);

        sort_referenced_keys(mt);
        mt->rn_saved = NULL;

	/* "Print" again, now that we know which values are actually
           shared. On this pass, shared values that reference other shared values
           are re-computed with the compacted keys. */
        shared_offsets = MALLOC_N_ATOMIC(long, mt->st_refs->count);
        mt->shared_offsets = shared_offsets;
	symtab = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->symtab = symtab;
	rns = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->rns = rns;
        mt->reverse_map = NULL;
        mt->top_map = NULL;
        mt->pass = 1;
	print_substring(v, notdisplay, 1, NULL, mt, pp, NULL, &slen, 
                        1, &st_len);

        /* "Print" the string again to get a measurement and symtab size. */
        symtab = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->symtab = symtab;
	rns = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->rns = rns;
        mt->reverse_map = NULL;
        mt->top_map = NULL;
        mt->pass = 2;
	print_substring(v, notdisplay, 1, NULL, mt, pp, NULL, &slen, 
                        -1, &st_len);

	/* Remember version: */
        print_one_byte(pp, strlen(MZSCHEME_VERSION));
	print_this_string(pp, MZSCHEME_VERSION, 0, -1);

        if (mt->st_refs->count != mt->sorted_keys_count)
          scheme_signal_error("shared key count somehow changed");

	print_number(pp, mt->st_refs->count + 1);

        /* Print shared-value offsets: */
        if (mt->st_refs->count) {
          int all_short = shared_offsets[mt->st_refs->count-1] < 0xFFFF;
          print_one_byte(pp, all_short);
          for (j = 0; j < mt->st_refs->count; j++) {
            if (all_short)
              print_short_number(pp, shared_offsets[j]);
            else
              print_number(pp, shared_offsets[j]);
          }
        } else {
          print_one_byte(pp, 1);
        }
        
	print_number(pp, st_len);
	print_number(pp, slen);

	/* Make symtab and rns again to ensure the same results 
           for the final print: */
	symtab = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->symtab = symtab;
	rns = scheme_make_hash_table(SCHEME_hash_ptr);
        mt->rns = rns;
        mt->reverse_map = NULL;
        mt->top_map = NULL;
        mt->pass = 3;

        start_offset = pp->print_offset;

        /* Print shared values first, so read can easily skip them
           and load them lazily. */
        print_table_keys(notdisplay, 1, NULL, mt, pp);
        shared_offset = pp->print_offset;
	closed = print(v, notdisplay, 1, NULL, mt, pp);

        if (shared_offset - start_offset != st_len) {
          scheme_signal_error("compiled code shared printed size changed on third pass:"
                              " %ld versus %ld (total %ld)",
                              st_len, shared_offset - start_offset, slen);
        }
        if (pp->print_offset - start_offset != slen) {
          scheme_signal_error("compiled code printed size changed on third pass:"
                              " %ld versus %ld",
                              slen, pp->print_offset - start_offset);
        }

        mt = NULL;
      }
    } 
  else 
    {
      if (compact || !pp->print_unreadable)
	cannot_print(pp, notdisplay, obj, ht, compact);
      else if ((SCHEME_TYPE(obj) < printers_count)
	       && printers[SCHEME_TYPE(obj)]) {
	Scheme_Type_Printer printer;
	printer = printers[SCHEME_TYPE(obj)];
	printer(obj, !notdisplay, pp);
      } else {
	char *s;
	long len = -1;
	s = scheme_get_type_name((SCHEME_TYPE(obj)));
	print_utf8_string(pp, "#", 0, 1);
#ifdef SGC_STD_DEBUGGING
	len = strlen(s) - 1;
#endif
	if (!s) {
	  char s[8];
	  print_utf8_string(pp, "<???:", 0, 5);
	  sprintf(s, "%d", SCHEME_TYPE(obj));
	  print_utf8_string(pp, s, 0, -1);
	  print_utf8_string(pp, ">", 0, 1);
	} else {
	  print_utf8_string(pp, s, 0, len);
	}
#ifdef SGC_STD_DEBUGGING
	PRINTADDRESS(pp, obj);
	print_utf8_string(pp, ">", 0, 1);
#endif
      }

      closed = 1;
    }

  if (save_honu_mode != pp->honu_mode)
    pp->honu_mode = save_honu_mode;

  return (closed || compact);
}

static void
print_char_string(const char *str, int len, 
		  const mzchar *ustr, int delta, int ulen,
		  int notdisplay, int honu_char, PrintParams *pp)
{
  char minibuf[12], *esc;
  int a, i, v, ui, cont_utf8 = 0, isize;

  if (notdisplay) {
    print_utf8_string(pp, honu_char ? "'" : "\"", 0, 1);

    for (a = i = ui = 0; i < len; i += isize, ui++) {
      v = ((unsigned char *)str)[i];
      isize = 1;

      switch (v) {
      case '\"': 
	if (honu_char)
	  esc = NULL;
	else
	  esc = "\\\""; 
	break;
      case '\'': 
	if (honu_char)
	  esc = "\\'"; 
	else
	  esc = NULL;
	break;
      case '\\': esc = "\\\\"; break;
      case '\a': esc = "\\a";  break;
      case '\b': esc = "\\b";  break;
      case 27: esc = "\\e";  break;
      case '\f': esc = "\\f";  break;
      case '\n': esc = "\\n";  break;
      case '\r': esc = "\\r";  break;
      case '\t': esc = "\\t";  break;
      case '\v': esc = "\\v";  break;
      default:
	if (v > 127) {
	  if (cont_utf8) {
	    cont_utf8--;
	    ui--;
	    esc = NULL;
	  } else {
	    int clen;
	    clen = scheme_utf8_encode(ustr, ui+delta, ui+delta+1, NULL, 0, 0);
	    if (scheme_isgraphic(ustr[ui+delta])
		|| scheme_isblank(ustr[ui+delta])) {
	      cont_utf8 = clen - 1;
	      esc = NULL;
	    } else {
	      esc = minibuf;
	      isize = clen;
	    }
	  }
	} else if (scheme_isgraphic(v)
		   || scheme_isblank(v)) {
	  esc = NULL;
	} else {
	  esc = minibuf;
	}
	break;
      }

      if (esc) {
	if (esc == minibuf) {
	  if (ustr[ui+delta] > 0xFFFF) {
	    sprintf(minibuf, "\\U%.8X", ustr[ui+delta]);
	  } else
	    sprintf(minibuf, "\\u%.4X", ustr[ui+delta]);
	}

        if (a < i)
	  print_utf8_string(pp, str, a, i-a);
        print_utf8_string(pp, esc, 0, -1);
        a = i+isize;
      }
    }
    if (a < i)
      print_utf8_string(pp, str, a, i-a);

    print_utf8_string(pp, honu_char ? "'" : "\"", 0, 1);
  } else if (len) {
    print_utf8_string(pp, str, 0, len);
  }
}

static void
print_byte_string(const char *str, int delta, int len, int notdisplay, PrintParams *pp)
{
  char minibuf[8], *esc;
  int a, i, v;

  if (notdisplay) {
    print_utf8_string(pp, "\"", 0, 1);

    for (a = i = delta; i < delta + len; i++) {
      /* Escape-sequence handling by Eli Barzilay. */
      switch (((unsigned char *)str)[i]) {
      case '\"': esc = "\\\""; break;
      case '\\': esc = "\\\\"; break;
      case '\a': esc = "\\a";  break;
      case '\b': esc = "\\b";  break;
      case 27: esc = "\\e";  break;
      case '\f': esc = "\\f";  break;
      case '\n': esc = "\\n";  break;
      case '\r': esc = "\\r";  break;
      case '\t': esc = "\\t";  break;
      case '\v': esc = "\\v";  break;
      default:
	v = ((unsigned char *)str)[i];
	if (v > 127) {
	  esc = minibuf;
	} else if (scheme_isgraphic(v) || scheme_isblank(v)) {
	  esc = NULL;
	} else {
	  esc = minibuf;
	}
	break;
      }

      if (esc) {
	if (esc == minibuf) {
	  sprintf(minibuf,
                  ((i+1>=len) || (str[i+1] < '0') || (str[i+1] > '7')) ? "\\%o" : "\\%03o",
                  ((unsigned char *)str)[i]);
	}

        if (a < i)
	  print_utf8_string(pp, str, a, i-a);
        print_utf8_string(pp, esc, 0, -1);
        a = i+1;
      }
    }
    if (a < i)
      print_utf8_string(pp, str, a, i-a);

    print_utf8_string(pp, "\"", 0, 1);
  } else if (len) {
    print_this_string(pp, str, delta, len);
  }
}


static void
print_pair(Scheme_Object *pair, int notdisplay, int compact, 
	   Scheme_Hash_Table *ht, 
           Scheme_Marshal_Tables *mt, 
	   PrintParams *pp)
{
  Scheme_Object *cdr;
  int super_compact = 0;

  if (compact) {
    int c = 0;
    Scheme_Object *pr;

    pr = pair;
    while (SCHEME_PAIRP(pr)) {
      if (ht)
	if ((long)scheme_hash_get(ht, pr) != 1) {
	  c = -1;
	  break;
	}
      c++;
      pr = SCHEME_CDR(pr);
    }

    if (c > -1) {
      super_compact = 1;
      if (c < CPT_RANGE(SMALL_LIST)) {
	unsigned char s[1];
	s[0] = c + (SCHEME_NULLP(pr) 
		    ? CPT_SMALL_PROPER_LIST_START
		    : CPT_SMALL_LIST_START);
	print_this_string(pp, (char *)s, 0, 1);
      } else {
	print_compact(pp, CPT_LIST);
	print_compact_number(pp, c);
	super_compact = -1;
      }
    }
  } else if (pp->honu_mode) {
    /* Honu list printing */
    cdr = SCHEME_CDR(pair);
    while (SCHEME_PAIRP(cdr)) {
      if (ht) {
	if ((long)scheme_hash_get(ht, cdr) != 1) {
	  /* This needs a tag */
	  break;
	}
      }
      cdr = SCHEME_CDR(cdr);
    }
    if (SCHEME_NULLP(cdr)) {
      /* Proper list without sharing. */
      print_utf8_string(pp, "list(", 0, 5);
      (void)print(SCHEME_CAR(pair), notdisplay, compact, ht, mt, pp);
      cdr = SCHEME_CDR(pair);
      while (SCHEME_PAIRP(cdr)) {
	print_utf8_string(pp, ", ", 0, 2);
	(void)print(SCHEME_CAR(cdr), notdisplay, compact, ht, mt, pp);
	cdr = SCHEME_CDR(cdr);
      }
      print_utf8_string(pp, ")", 0, 1);
    } else {
      /* Use cons cells. */
      int cnt = 1;
      print_utf8_string(pp, "cons(", 0, 5);
      (void)print(SCHEME_CAR(pair), notdisplay, compact, ht, mt, pp);
      cdr = SCHEME_CDR(pair);
      while (SCHEME_PAIRP(cdr)) {
	print_utf8_string(pp, ", ", 0, 2);
	if (ht) {
	  if ((long)scheme_hash_get(ht, cdr) != 1) {
	    /* This needs a tag */
	    (void)print(cdr, notdisplay, compact, ht, mt, pp);
	    break;
	  }
	}
	 
	print_utf8_string(pp, "cons(", 0, 5);
	(void)print(SCHEME_CAR(cdr), notdisplay, compact, ht, mt, pp);
	cnt++;
	cdr = SCHEME_CDR(cdr);
      }
      print_utf8_string(pp, ", ", 0, 2);
      (void)print(cdr, notdisplay, compact, ht, mt, pp);
      while (cnt--) {
	print_utf8_string(pp, ")", 0, 1);
      }
    }
    return;
  }

  if (compact) {
    if (!super_compact)
      print_compact(pp, CPT_PAIR);
  } else
    print_utf8_string(pp, "(", 0, 1);

  print(SCHEME_CAR(pair), notdisplay, compact, ht, mt, pp);

  cdr = SCHEME_CDR (pair);
  while (SCHEME_PAIRP(cdr)) {
    if (ht && !super_compact) {
      if ((long)scheme_hash_get(ht, cdr) != 1) {
	/* This needs a tag */
	if (!compact)
	  print_utf8_string(pp, " . ", 0, 3);
	(void)print(cdr, notdisplay, compact, ht, mt, pp);
	if (!compact)
	  print_utf8_string(pp, ")", 0, 1);
	return;
      }
    }
    if (compact && !super_compact)
      print_compact(pp, CPT_PAIR);
    if (!compact)
      print_utf8_string(pp, " ", 0, 1);
    print(SCHEME_CAR(cdr), notdisplay, compact, ht, mt, pp);
    cdr = SCHEME_CDR(cdr);
  }

  if (!SCHEME_NULLP(cdr)) {
    if (!compact)
      print_utf8_string(pp, " . ", 0, 3);
    print(cdr, notdisplay, compact, ht, mt, pp);
  } else if (compact && (super_compact < 1))
    print_compact(pp, CPT_NULL);

  if (!compact)
    print_utf8_string(pp, ")", 0, 1);
}

static void
print_vector(Scheme_Object *vec, int notdisplay, int compact, 
	     Scheme_Hash_Table *ht, 
             Scheme_Marshal_Tables *mt,
	     PrintParams *pp)
{
  int i, size, common = 0;
  Scheme_Object **elems;

  size = SCHEME_VEC_SIZE(vec);

  if (compact) {
    print_compact(pp, CPT_VECTOR);
    print_compact_number(pp, size);
  } else {
    elems = SCHEME_VEC_ELS(vec);
    for (i = size; i--; common++) {
      if (!i || (elems[i] != elems[i - 1]))
	break;
    }
    elems = NULL; /* Precise GC: because VEC_ELS is not aligned */
    
    if (notdisplay && pp->print_vec_shorthand) {
      if (size == 0) {
	if (pp->honu_mode)
	  print_utf8_string(pp, "vectorN(0", 0, 7);
	else
	  print_utf8_string(pp, "#0(", 0, 3);
      } else {
	char buffer[100];
	sprintf(buffer, pp->honu_mode ? "vectorN(%d, " : "#%d(", size);
	print_utf8_string(pp, buffer, 0, -1);
	size -= common;
      }
    } else if (pp->honu_mode)
      print_utf8_string(pp, "vector(", 0, 7);
    else
      print_utf8_string(pp, "#(", 0, 2);
  }

  for (i = 0; i < size; i++) {
    print(SCHEME_VEC_ELS(vec)[i], notdisplay, compact, ht, mt, pp);
    if (i < (size - 1)) {
      if (!compact) {
	if (pp->honu_mode)
	  print_utf8_string(pp, ", ", 0, 2);
	else
	  print_utf8_string(pp, " ", 0, 1);
      }
    }
  }

  if (!compact)
    print_utf8_string(pp, ")", 0, 1);
}

static void
print_char(Scheme_Object *charobj, int notdisplay, PrintParams *pp)
{
  int ch;
  char minibuf[10+MAX_UTF8_CHAR_BYTES], *str;
  int len = -1;

  ch = SCHEME_CHAR_VAL(charobj);
  if (notdisplay) {
    switch ( ch )
      {
      case '\0':
	str = "#\\nul";
	break;
      case '\n':
	str = "#\\newline";
	break;
      case '\t':
	str = "#\\tab";
	break;
      case 0xb:
	str = "#\\vtab";
	break;
      case ' ':
	str = "#\\space";
	break;
      case '\r':
	str = "#\\return";
	break;
      case '\f':
	str = "#\\page";
	break;
      case '\b':
	str = "#\\backspace";
	break;
      case 0x7f:
	str = "#\\rubout";
	break;
      default:
	if (scheme_isgraphic(ch)) {
	  minibuf[0] = '#';
	  minibuf[1] = '\\';
	  ch = scheme_utf8_encode((unsigned int *)&ch, 0, 1,
				  (unsigned char *)minibuf, 2,
				  0);
	  minibuf[2 + ch] = 0;
	} else {
	  if (ch > 0xFFFF)
	    sprintf(minibuf, "#\\U%.8X", ch);
	  else
	    sprintf(minibuf, "#\\u%.4X", ch);
	}
	str = minibuf;
	break;
      }
  } else {
    len = scheme_utf8_encode((unsigned int *)&ch, 0, 1,
			     (unsigned char *)minibuf, 0,
			     0);
    minibuf[len] = 0;
    str = minibuf;
  }

  print_utf8_string(pp, str, 0, len);
}

/***************************************************/

Scheme_Object *scheme_protect_quote(Scheme_Object *expr)
{
  if (HAS_SUBSTRUCT(expr, ssALLp)) {
    Scheme_Object *q;
    q = scheme_alloc_small_object();
    q->type = scheme_quote_compilation_type;
    SCHEME_PTR_VAL(q) = expr;
    return q;
  } else
    return expr;
}

/*========================================================================*/
/*                       external printers                                */
/*========================================================================*/

void scheme_set_type_printer(Scheme_Type stype, Scheme_Type_Printer printer)
{
  if (!printers) {
    REGISTER_SO(printers);
  }

  if (stype >= printers_count) {
    Scheme_Type_Printer *naya;
    naya = MALLOC_N(Scheme_Type_Printer, stype + 10);
    memset(naya, 0, sizeof(Scheme_Type_Printer) * (stype + 10));
    memcpy(naya, printers, sizeof(Scheme_Type_Printer) * printers_count);
    printers_count = stype + 10;
    printers = naya;
  }

  printers[stype] = printer;
}

/*========================================================================*/
/*                           custom writing                               */
/*========================================================================*/

static Scheme_Object *accum_write(void *_b, int argc, Scheme_Object **argv)
{
  if (SCHEME_BOX_VAL(_b)) {
    Scheme_Object *v;
    v = scheme_make_pair(argv[0], SCHEME_BOX_VAL(_b));
    SCHEME_BOX_VAL(_b) = v;
  }

  return scheme_void;
}

static Scheme_Object *writable_struct_subs(Scheme_Object *s, PrintParams *pp)
{
  Scheme_Object *v, *o, *a[3], *b, *accum_proc;
  Scheme_Output_Port *op;

  v = scheme_is_writable_struct(s);

  o = scheme_make_null_output_port(pp->print_port
				   && ((Scheme_Output_Port *)pp->print_port)->write_special_fun);

  op = (Scheme_Output_Port *)o;
  
  b = scheme_box(scheme_null);
  accum_proc = scheme_make_closed_prim_w_arity(accum_write,
					       b,
					       "custom-write-recur-handler",
					       2, 2);

  op->display_handler = accum_proc;
  op->write_handler = accum_proc;
  op->print_handler = accum_proc;

  a[0] = s;
  a[1] = o;
  a[2] = scheme_false;

  scheme_apply_multi(v, 3, a);

  scheme_close_output_port(o);

  v = SCHEME_BOX_VAL(b);
  SCHEME_BOX_VAL(b) = NULL;

  return v;
}

static void flush_from_byte_port(Scheme_Object *orig_port, PrintParams *pp)
{
  char *bytes;
  long len;
  bytes = scheme_get_sized_byte_string_output(orig_port, &len);
  print_this_string(pp, bytes, 0, len);
}

static Scheme_Object *custom_recur(int notdisplay, void *_vec, int argc, Scheme_Object **argv)
{
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)SCHEME_VEC_ELS(_vec)[0];
  Scheme_Marshal_Tables *mt = (Scheme_Marshal_Tables *)SCHEME_VEC_ELS(_vec)[1];
  PrintParams * volatile pp = (PrintParams *)SCHEME_VEC_ELS(_vec)[2];
  Scheme_Object * volatile save_port;
  mz_jmp_buf escape, * volatile save;
  volatile long save_max;

  if (!SCHEME_OUTPORTP(argv[1])) {
    scheme_wrong_type(notdisplay ? "write/recusrive" : "display/recursive",
		      "output-port", 1, argc, argv);
    return NULL;
  }

  if (SCHEME_VEC_ELS(_vec)[3]) {
    /* Recur: */
    {
      if (pp->print_escape) {
	save = pp->print_escape;
	pp->print_escape = &escape;
      } else
	save = NULL;

      save_port = pp->print_port;
      save_max = pp->print_maxlen;
      
      if (!pp->print_escape
	  || !scheme_setjmp(escape)) {
	/* If printing to string, flush it and reset first: */
	Scheme_Object *sp;
	sp = SCHEME_VEC_ELS(_vec)[4];
	if (sp) {
	  flush_from_byte_port(sp, pp);
	  sp = scheme_make_byte_string_output_port();
	  ((Scheme_Output_Port *)SCHEME_VEC_ELS(_vec)[5])->port_data = sp;
	  SCHEME_VEC_ELS(_vec)[4] = sp;
	}

	/* If printing to a different output port, flush print cache,
	   first. */
	if (!SAME_OBJ(save_port, argv[1])) {
	  print_this_string(pp, NULL, 0, 0);
	  /* Disable maxlen, because it interferes with flushing.
	     It would be good to improve on this (to avoid work),
	     but it's unlikey to ever matter. */
	  pp->print_maxlen = 0;
	}

	pp->print_port = argv[1];

	/* Recur */
	print(argv[0], notdisplay, 0, ht, mt, pp);

	/* Flush print cache, to ensure that future writes to the
	   port go after printed data. */
	print_this_string(pp, NULL, 0, 0);
      }

      pp->print_port = save_port;
      pp->print_escape = save;
      pp->print_maxlen = save_max;
    }
  }

  return scheme_void;
}

static Scheme_Object *custom_write_recur(void *_vec, int argc, Scheme_Object **argv)
{
  return custom_recur(1, _vec, argc, argv);
}

static Scheme_Object *custom_display_recur(void *_vec, int argc, Scheme_Object **argv)
{
  return custom_recur(0, _vec, argc, argv);
}

static void custom_write_struct(Scheme_Object *s, Scheme_Hash_Table *ht, 
				Scheme_Marshal_Tables *mt,
				PrintParams *orig_pp, int notdisplay)
{
  Scheme_Object *v, *a[3], *o, *vec, *orig_port;
  Scheme_Output_Port *op;
  Scheme_Object *recur_write, *recur_display;
  PrintParams *pp;

  v = scheme_is_writable_struct(s);

  /* In case orig_pp is on the stack: */
  pp = copy_print_params(orig_pp);

  if (pp->print_port)
    orig_port = pp->print_port;
  else
    orig_port = scheme_make_byte_string_output_port();

  o = scheme_make_redirect_output_port(orig_port);
  
  op = (Scheme_Output_Port *)o;

  vec = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)ht;
  SCHEME_VEC_ELS(vec)[1] = (Scheme_Object *)mt;
  SCHEME_VEC_ELS(vec)[2] = (Scheme_Object *)pp;
  SCHEME_VEC_ELS(vec)[3] = scheme_true;
  SCHEME_VEC_ELS(vec)[4] = (pp->print_port ? NULL : orig_port);
  SCHEME_VEC_ELS(vec)[5] = o;

  recur_write = scheme_make_closed_prim_w_arity(custom_write_recur,
						vec,
						"custom-write-recur-handler",
						2, 2);
  recur_display = scheme_make_closed_prim_w_arity(custom_display_recur,
						  vec,
						  "custom-display-recur-handler",
						  2, 2);


  op->write_handler = recur_write;
  op->display_handler = recur_display;
  op->print_handler = recur_write;

  /* First, flush print cache to actual port,
     so further writes go after current writes: */
  if (pp->print_port)
    print_this_string(pp, NULL, 0, 0);

  a[0] = s;
  a[1] = o;
  a[2] = (notdisplay ? scheme_true : scheme_false);
  scheme_apply_multi(v, 3, a);

  scheme_close_output_port(o);

  memcpy(orig_pp, pp, sizeof(PrintParams));

  SCHEME_VEC_ELS(vec)[3] = NULL;

  /* This must go last, because it might escape: */
  if (!orig_pp->print_port)
    flush_from_byte_port(SCHEME_VEC_ELS(vec)[4], orig_pp);
}

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_PRINT_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_print_params, mark_print_params);
  GC_REG_TRAV(scheme_rt_marshal_info, mark_marshal_tables);
}

END_XFORM_SKIP;

#endif
