/* >>>> Generated by mkmark.rkt from mzmarksrc.c <<<< */

#ifdef MZ_USE_PLACES
static int mark_serialized_struct_val_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  Scheme_Serialized_Structure *s = (Scheme_Serialized_Structure *)p;
  int num_slots = s->num_slots;

  gcBYTES_TO_WORDS((sizeof(Scheme_Serialized_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
#else
  return 0;
#endif
}

static int mark_serialized_struct_val_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Scheme_Serialized_Structure *s = (Scheme_Serialized_Structure *)p;
  int num_slots = s->num_slots;

  int i;

  gcMARK2(s->prefab_key, gc);
  
  for(i = num_slots; i--; )
    gcMARK2(s->slots[i], gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS((sizeof(Scheme_Serialized_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
# endif
#endif
}

static int mark_serialized_struct_val_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Scheme_Serialized_Structure *s = (Scheme_Serialized_Structure *)p;
  int num_slots = s->num_slots;

  int i;

  gcFIXUP2(s->prefab_key, gc);
  
  for(i = num_slots; i--; )
    gcFIXUP2(s->slots[i], gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS((sizeof(Scheme_Serialized_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
# endif
#endif
}

#define mark_serialized_struct_val_IS_ATOMIC 0
#define mark_serialized_struct_val_IS_CONST_SIZE 0

#endif

static int mark_struct_val_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  Scheme_Structure *s = (Scheme_Structure *)p;
  int num_slots = ((Scheme_Struct_Type *)GC_resolve2(s->stype, gc))->num_slots;

  gcBYTES_TO_WORDS((sizeof(Scheme_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
#else
  return 0;
#endif
}

static int mark_struct_val_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Scheme_Structure *s = (Scheme_Structure *)p;
  int num_slots = ((Scheme_Struct_Type *)GC_resolve2(s->stype, gc))->num_slots;

  int i;

  gcMARK2( s->stype, gc);
  
  for(i = num_slots; i--; )
    gcMARK2(s->slots[i], gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS((sizeof(Scheme_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
# endif
#endif
}

static int mark_struct_val_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Scheme_Structure *s = (Scheme_Structure *)p;
  int num_slots = ((Scheme_Struct_Type *)GC_resolve2(s->stype, gc))->num_slots;

  int i;

  gcFIXUP2_TYPED_NOW(Scheme_Struct_Type *, s->stype, gc);
  
  for(i = num_slots; i--; )
    gcFIXUP2(s->slots[i], gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS((sizeof(Scheme_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
# endif
#endif
}

#define mark_struct_val_IS_ATOMIC 0
#define mark_struct_val_IS_CONST_SIZE 0


static int mark_struct_type_val_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  Scheme_Struct_Type *t = (Scheme_Struct_Type *)p;

  gcBYTES_TO_WORDS((sizeof(Scheme_Struct_Type)
		    + ((t->name_pos + 1 - mzFLEX_DELTA) 
                       * sizeof(Scheme_Struct_Type *))));
#else
  return 0;
#endif
}

static int mark_struct_type_val_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Scheme_Struct_Type *t = (Scheme_Struct_Type *)p;

  int i;
  for (i = t->name_pos + 1; i--; ) {
    gcMARK2(t->parent_types[i], gc);
  }
  gcMARK2(t->name, gc);
  gcMARK2(t->inspector, gc);
  gcMARK2(t->accessor, gc);
  gcMARK2(t->mutator, gc);
  gcMARK2(t->prefab_key, gc);
  gcMARK2(t->uninit_val, gc);
  gcMARK2(t->props, gc);
  gcMARK2(t->proc_attr, gc);
  gcMARK2(t->guard, gc);
  gcMARK2(t->immutables, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS((sizeof(Scheme_Struct_Type)
		    + ((t->name_pos + 1 - mzFLEX_DELTA) 
                       * sizeof(Scheme_Struct_Type *))));
# endif
#endif
}

static int mark_struct_type_val_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Scheme_Struct_Type *t = (Scheme_Struct_Type *)p;

  int i;
  for (i = t->name_pos + 1; i--; ) {
    gcFIXUP2(t->parent_types[i], gc);
  }
  gcFIXUP2(t->name, gc);
  gcFIXUP2(t->inspector, gc);
  gcFIXUP2(t->accessor, gc);
  gcFIXUP2(t->mutator, gc);
  gcFIXUP2(t->prefab_key, gc);
  gcFIXUP2(t->uninit_val, gc);
  gcFIXUP2(t->props, gc);
  gcFIXUP2(t->proc_attr, gc);
  gcFIXUP2(t->guard, gc);
  gcFIXUP2(t->immutables, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS((sizeof(Scheme_Struct_Type)
		    + ((t->name_pos + 1 - mzFLEX_DELTA) 
                       * sizeof(Scheme_Struct_Type *))));
# endif
#endif
}

#define mark_struct_type_val_IS_ATOMIC 0
#define mark_struct_type_val_IS_CONST_SIZE 0


static int mark_struct_property_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  gcBYTES_TO_WORDS(sizeof(Scheme_Struct_Property));
#else
  return 0;
#endif
}

static int mark_struct_property_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Scheme_Struct_Property *i = (Scheme_Struct_Property *)p;
  gcMARK2(i->name, gc);
  gcMARK2(i->guard, gc);
  gcMARK2(i->supers, gc);
  gcMARK2(i->contract_name, gc);
  gcMARK2(i->realm, gc);
# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Scheme_Struct_Property));
# endif
#endif
}

static int mark_struct_property_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Scheme_Struct_Property *i = (Scheme_Struct_Property *)p;
  gcFIXUP2(i->name, gc);
  gcFIXUP2(i->guard, gc);
  gcFIXUP2(i->supers, gc);
  gcFIXUP2(i->contract_name, gc);
  gcFIXUP2(i->realm, gc);
# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Scheme_Struct_Property));
# endif
#endif
}

#define mark_struct_property_IS_ATOMIC 0
#define mark_struct_property_IS_CONST_SIZE 1


static int mark_wrapped_evt_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  gcBYTES_TO_WORDS(sizeof(Wrapped_Evt));
#else
  return 0;
#endif
}

static int mark_wrapped_evt_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Wrapped_Evt *ww = (Wrapped_Evt *)p;

  gcMARK2(ww->evt, gc);
  gcMARK2(ww->wrapper, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Wrapped_Evt));
# endif
#endif
}

static int mark_wrapped_evt_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Wrapped_Evt *ww = (Wrapped_Evt *)p;

  gcFIXUP2(ww->evt, gc);
  gcFIXUP2(ww->wrapper, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Wrapped_Evt));
# endif
#endif
}

#define mark_wrapped_evt_IS_ATOMIC 0
#define mark_wrapped_evt_IS_CONST_SIZE 1


static int mark_nack_guard_evt_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  gcBYTES_TO_WORDS(sizeof(Nack_Guard_Evt));
#else
  return 0;
#endif
}

static int mark_nack_guard_evt_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Nack_Guard_Evt *nw = (Nack_Guard_Evt *)p;

  gcMARK2(nw->maker, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Nack_Guard_Evt));
# endif
#endif
}

static int mark_nack_guard_evt_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Nack_Guard_Evt *nw = (Nack_Guard_Evt *)p;

  gcFIXUP2(nw->maker, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Nack_Guard_Evt));
# endif
#endif
}

#define mark_nack_guard_evt_IS_ATOMIC 0
#define mark_nack_guard_evt_IS_CONST_SIZE 1


static int mark_active_replace_evt_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  gcBYTES_TO_WORDS(sizeof(Active_Replace_Evt));
#else
  return 0;
#endif
}

static int mark_active_replace_evt_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Active_Replace_Evt *a = (Active_Replace_Evt *)p;

  gcMARK2(a->syncing, gc);
  gcMARK2(a->wrapper, gc);
  gcMARK2(a->orig, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Active_Replace_Evt));
# endif
#endif
}

static int mark_active_replace_evt_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Active_Replace_Evt *a = (Active_Replace_Evt *)p;

  gcFIXUP2(a->syncing, gc);
  gcFIXUP2(a->wrapper, gc);
  gcFIXUP2(a->orig, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Active_Replace_Evt));
# endif
#endif
}

#define mark_active_replace_evt_IS_ATOMIC 0
#define mark_active_replace_evt_IS_CONST_SIZE 1


static int mark_chaperone_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  gcBYTES_TO_WORDS(sizeof(Scheme_Chaperone));
#else
  return 0;
#endif
}

static int mark_chaperone_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Scheme_Chaperone *px = (Scheme_Chaperone *)p;

  gcMARK2(px->val, gc);
  gcMARK2(px->prev, gc);
  gcMARK2(px->props, gc);
  gcMARK2(px->redirects, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Scheme_Chaperone));
# endif
#endif
}

static int mark_chaperone_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Scheme_Chaperone *px = (Scheme_Chaperone *)p;

  gcFIXUP2(px->val, gc);
  gcFIXUP2(px->prev, gc);
  gcFIXUP2(px->props, gc);
  gcFIXUP2(px->redirects, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Scheme_Chaperone));
# endif
#endif
}

#define mark_chaperone_IS_ATOMIC 0
#define mark_chaperone_IS_CONST_SIZE 1


