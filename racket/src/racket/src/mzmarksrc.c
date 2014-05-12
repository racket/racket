
START type;

variable_obj {
 mark:
  Scheme_Bucket *b = (Scheme_Bucket *)p;

  gcMARK2(b->key, gc);
  gcMARK2(b->val, gc);
  gcMARK2(((Scheme_Bucket_With_Home *)b)->home_link, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Bucket_With_Home));
}

module_var {
 mark:
  Module_Variable *mv = (Module_Variable *)p;

  gcMARK2(mv->modidx, gc);
  gcMARK2(mv->sym, gc);
  gcMARK2(mv->insp, gc);
  gcMARK2(mv->shape, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Module_Variable));
}

bucket_obj {
 mark:
  Scheme_Bucket *b = (Scheme_Bucket *)p;

  gcMARK2(b->key, gc);
  gcMARK2(b->val, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Bucket));
}

local_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Local));
}

toplevel_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Toplevel));
}

quotesyntax_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Quote_Syntax));
}

cpointer_obj {
 mark:
  if (!(SCHEME_CPTR_FLAGS(p) & 0x1)) {
    gcMARK2(SCHEME_CPTR_VAL(p), gc);
  }
  gcMARK2(SCHEME_CPTR_TYPE(p), gc);
 size:
   (SCHEME_CPTR_HAS_OFFSET(p)
    ? gcBYTES_TO_WORDS(sizeof(Scheme_Offset_Cptr))
    : gcBYTES_TO_WORDS(sizeof(Scheme_Cptr)));
}

twoptr_obj {
 mark:
  gcMARK2(SCHEME_PTR1_VAL((Scheme_Object *)p), gc);
  gcMARK2(SCHEME_PTR2_VAL((Scheme_Object *)p), gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

iptr_obj {
 mark:
  gcMARK2(SCHEME_IPTR_VAL((Scheme_Object *)p), gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

small_object {
 mark:
  gcMARK2(((Scheme_Small_Object *)p)->u.ptr_value, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Small_Object));
}

small_atomic_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Small_Object));
}

app_rec {
  Scheme_App_Rec *r = (Scheme_App_Rec *)p;

 mark:
  int i = r->num_args + 1;
  while (i--) 
    gcMARK2(r->args[i], gc);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_App_Rec) 
		    + ((r->num_args + 1 - mzFLEX_DELTA) * sizeof(Scheme_Object *))
		    + ((r->num_args + 1) * sizeof(char))));
}

app2_rec {
 mark:
  Scheme_App2_Rec *r = (Scheme_App2_Rec *)p;
  gcMARK2(r->rator, gc);
  gcMARK2(r->rand, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_App2_Rec));
}

app3_rec {
 mark:
  Scheme_App3_Rec *r = (Scheme_App3_Rec *)p;
  gcMARK2(r->rator, gc);
  gcMARK2(r->rand1, gc);
  gcMARK2(r->rand2, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_App3_Rec));
}

seq_rec {
  Scheme_Sequence *s = (Scheme_Sequence *)p;

 mark:
  int i = s->count;
  while (i--)
    gcMARK2(s->array[i], gc);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Sequence)
		    + ((s->count - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
}

branch_rec {
 mark:
  Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)p;
  
  gcMARK2(b->test, gc);
  gcMARK2(b->tbranch, gc);
  gcMARK2(b->fbranch, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Branch_Rec));
}

unclosed_proc {
 mark:
  Scheme_Closure_Data *d = (Scheme_Closure_Data *)p;

  gcMARK2(d->name, gc);
  gcMARK2(d->code, gc);
  gcMARK2(d->closure_map, gc);
  gcMARK2(d->tl_map, gc);
#ifdef MZ_USE_JIT
  gcMARK2(d->u.native_code, gc);
  gcMARK2(d->context, gc);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Closure_Data));
}

let_value {
 mark:
  Scheme_Let_Value *l = (Scheme_Let_Value *)p;
  
  gcMARK2(l->value, gc);
  gcMARK2(l->body, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Value));
}

let_void {
 mark:
  Scheme_Let_Void *l = (Scheme_Let_Void *)p;

  gcMARK2(l->body, gc);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Void));
}

letrec {
 mark:
  Scheme_Letrec *l = (Scheme_Letrec *)p;
  
  gcMARK2(l->procs, gc);
  gcMARK2(l->body, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Letrec));
}

let_one {
 mark:
  Scheme_Let_One *l = (Scheme_Let_One *)p;
  
  gcMARK2(l->value, gc);
  gcMARK2(l->body, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_One));
}

with_cont_mark {
 mark:
  Scheme_With_Continuation_Mark *w = (Scheme_With_Continuation_Mark *)p;

  gcMARK2(w->key, gc);
  gcMARK2(w->val, gc);
  gcMARK2(w->body, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_With_Continuation_Mark));
}

comp_let_value {
 mark:
  Scheme_Compiled_Let_Value *c = (Scheme_Compiled_Let_Value *)p;

  gcMARK2(c->flags, gc);
  gcMARK2(c->value, gc);
  gcMARK2(c->body, gc);
  gcMARK2(c->names, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compiled_Let_Value));
}

let_header {
 mark:
  Scheme_Let_Header *h = (Scheme_Let_Header *)p;
  
  gcMARK2(h->body, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Header));
}

set_bang {
 mark:
  Scheme_Set_Bang *b = (Scheme_Set_Bang *)p;
  
  gcMARK2(b->var, gc);
  gcMARK2(b->val, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Set_Bang));
}

prim_proc {
  Scheme_Primitive_Proc *prim = (Scheme_Primitive_Proc *)p;

 mark:
  gcMARK2(prim->name, gc);
  if (prim->mina < 0) {
    gcMARK2(prim->mu.cases, gc);
  }
  if (prim->pp.flags & SCHEME_PRIM_IS_CLOSURE) {
    Scheme_Primitive_Closure *cc = (Scheme_Primitive_Closure *)prim;
    int i;
    for (i = cc->count; i--; ) {
      gcMARK2(cc->val[i], gc);
    }
  }  
  
 size:
  ((prim->pp.flags & SCHEME_PRIM_IS_CLOSURE)
   ? (gcBYTES_TO_WORDS(sizeof(Scheme_Primitive_Closure))
      + ((Scheme_Primitive_Closure *)prim)->count - mzFLEX_DELTA)
   : ((prim->pp.flags & SCHEME_PRIM_IS_MULTI_RESULT)
      ? gcBYTES_TO_WORDS(sizeof(Scheme_Prim_W_Result_Arity))
      : gcBYTES_TO_WORDS(sizeof(Scheme_Primitive_Proc))));
}

closed_prim_proc {
  Scheme_Closed_Primitive_Proc *c = (Scheme_Closed_Primitive_Proc *)p;

 mark:
  gcMARK2(c->name, gc);
  gcMARK2(SCHEME_CLSD_PRIM_DATA(c), gc);
  if (c->mina == -2) {
    gcMARK2(((Scheme_Closed_Case_Primitive_Proc *)c)->cases, gc);
  }
  
 size:
  ((c->pp.flags & SCHEME_PRIM_IS_MULTI_RESULT)
   ? gcBYTES_TO_WORDS(sizeof(Scheme_Closed_Prim_W_Result_Arity))
   : ((c->mina == -2)
      ? gcBYTES_TO_WORDS(sizeof(Scheme_Closed_Case_Primitive_Proc))
      : gcBYTES_TO_WORDS(sizeof(Scheme_Closed_Primitive_Proc))));
}

scm_closure {
  Scheme_Closure *c = (Scheme_Closure *)p;
  int closure_size = (c->code 
                      ? ((Scheme_Closure_Data *)GC_resolve2(c->code, gc))->closure_size
                      : 0);

 mark:

  int i = closure_size;
  START_MARK_ONLY;
# define CLOSURE_DATA_TYPE Scheme_Closure_Data
# include "mzclpf_decl.inc"
  END_MARK_ONLY;

  gcMARK2(c->code, gc);

  START_MARK_ONLY;
# include "mzclpf_pre.inc"
  END_MARK_ONLY;

  while (i--)
    gcMARK2(c->vals[i], gc);

  START_MARK_ONLY;
# include "mzclpf_post.inc"
# undef CLOSURE_DATA_TYPE
  END_MARK_ONLY;
  
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Closure)
		    + (closure_size - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
}

case_closure {
  Scheme_Case_Lambda *c = (Scheme_Case_Lambda *)p;

 mark:
  int i;
  
  for (i = c->count; i--; )
    gcMARK2(c->array[i], gc);
  gcMARK2(c->name, gc);
#ifdef MZ_USE_JIT
  gcMARK2(c->native_code, gc);
#endif

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Case_Lambda)
		    + ((c->count - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
}

cont_proc {
 mark:
  Scheme_Cont *c = (Scheme_Cont *)p;
  
  gcMARK2(c->dw, gc);
  gcMARK2(c->prompt_tag, gc);
  gcMARK2(c->meta_continuation, gc);
  gcMARK2(c->common_dw, gc);
  gcMARK2(c->save_overflow, gc);
  gcMARK2(c->runstack_copied, gc);
  gcMARK2(c->runstack_owner, gc);
  gcMARK2(c->cont_mark_stack_copied, gc);
  gcMARK2(c->cont_mark_stack_owner, gc);
  gcMARK2(c->init_config, gc);
  gcMARK2(c->init_break_cell, gc);
#ifdef MZ_USE_JIT
  gcMARK2(c->native_trace, gc);
#endif

  gcMARK2(c->buf_ptr, gc);
  MARK_cjs(&c->cjs, gc);
  MARK_stack_state(&c->ss, gc);
  gcMARK2(c->barrier_prompt, gc);
  if (!GC_merely_accounting()) {
    gcMARK2(c->runstack_start, gc);
    gcMARK2(c->runstack_saved, gc);
  }

  gcMARK2(c->prompt_id, gc);
  gcMARK2(c->prompt_buf, gc);

  gcMARK2(c->escape_cont, gc);

  gcMARK2(c->value, gc);
  gcMARK2(c->resume_to, gc);
  gcMARK2(c->use_next_cont, gc);
  gcMARK2(c->extra_marks, gc);
  gcMARK2(c->shortcut_prompt, gc);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont));
}

cont_jmp_proc {
 mark:
  Scheme_Cont_Jmp *c = (Scheme_Cont_Jmp *)p;

  MARK_jmpup(&c->buf, gc);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont_Jmp));
}

meta_cont_proc {
 mark:
  Scheme_Meta_Continuation *c = (Scheme_Meta_Continuation *)p;
  
  gcMARK2(c->prompt_tag, gc);
  gcMARK2(c->overflow, gc);
  gcMARK2(c->next, gc);
  gcMARK2(c->cont_mark_stack_copied, gc);
  gcMARK2(c->cont, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Meta_Continuation));
}

mark_dyn_wind {
 mark:
  Scheme_Dynamic_Wind *dw = (Scheme_Dynamic_Wind *)p;
  
  gcMARK2(dw->id, gc);
  gcMARK2(dw->data, gc);
  gcMARK2(dw->prompt_tag, gc);
  gcMARK2(dw->prev, gc);
    
  MARK_stack_state(&dw->envss, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Dynamic_Wind));
}

mark_overflow {
 mark:
  Scheme_Overflow *o = (Scheme_Overflow *)p;

  gcMARK2(o->prev, gc);
  gcMARK2(o->jmp, gc);
  gcMARK2(o->id, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Overflow));
}

mark_overflow_jmp {
 mark:
  Scheme_Overflow_Jmp *o = (Scheme_Overflow_Jmp *)p;

  MARK_jmpup(&o->cont, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Overflow_Jmp));
}

escaping_cont_proc {
 mark:
  Scheme_Escaping_Cont *c = (Scheme_Escaping_Cont *)p;

#ifdef MZ_USE_JIT
  gcMARK2(c->native_trace, gc);
#endif

  gcMARK2(c->barrier_prompt, gc);
  MARK_stack_state(&c->envss, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Escaping_Cont));
}

bignum_obj {
  Scheme_Bignum *b = (Scheme_Bignum *)p;

 mark:
  if (!SCHEME_BIGINLINE(b)) {
    gcMARK2(b->digits, gc);
  } else {
    FIXUP_ONLY(b->digits = ((Small_Bignum *)GC_fixup_self(b))->v;)
  }

 size:
  ((!SCHEME_BIGINLINE(b))
   ? gcBYTES_TO_WORDS(sizeof(Scheme_Bignum))
   : gcBYTES_TO_WORDS(sizeof(Small_Bignum)));
}

rational_obj {
 mark:
  Scheme_Rational *r = (Scheme_Rational *)p;
  
  gcMARK2(r->num, gc);
  gcMARK2(r->denom, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Rational));
}

float_obj {
 mark:
 size:
#ifdef MZ_USE_SINGLE_FLOATS
  gcBYTES_TO_WORDS(sizeof(Scheme_Float));
#else
  0;
#endif
}

double_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Double));
}

#ifdef MZ_LONG_DOUBLE
long_double_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Long_Double));
}
#else
long_double_obj {
 mark:
  Scheme_Long_Double *ld = (Scheme_Long_Double *)p;
  gcMARK2(ld->printed_form, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Long_Double));
}
#endif

complex_obj {
 mark:
  Scheme_Complex *c = (Scheme_Complex *)p;
  
  gcMARK2(c->r, gc);
  gcMARK2(c->i, gc);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Complex));
}

string_obj {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  gcMARK2(SCHEME_CHAR_STR_VAL(o), gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

bstring_obj {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  gcMARK2(SCHEME_BYTE_STR_VAL(o), gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

symbol_obj {
  Scheme_Symbol *s = (Scheme_Symbol *)p;

 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Symbol) + s->len + 1 - mzFLEX4_DELTA);
}

cons_cell {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  
  gcMARK2(SCHEME_CAR(o), gc);
  gcMARK2(SCHEME_CDR(o), gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

vector_obj {
  Scheme_Vector *vec = (Scheme_Vector *)p;

 mark:
  int i;
  for (i = vec->size; i--; )
    gcMARK2(vec->els[i], gc);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Vector) 
		    + ((vec->size - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
}

fxvector_obj {
  Scheme_Vector *vec = (Scheme_Vector *)p;

 mark:
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Vector) 
		    + ((vec->size - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
}

flvector_obj {
  Scheme_Double_Vector *vec = (Scheme_Double_Vector *)p;

 mark:
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Double_Vector) 
		    + ((vec->size - mzFLEX_DELTA) * sizeof(double))));
}

#ifdef MZ_LONG_DOUBLE
extflvector_obj {
  Scheme_Long_Double_Vector *vec = (Scheme_Long_Double_Vector *)p;

 mark:
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Long_Double_Vector) 
		    + ((vec->size - mzFLEX_DELTA) * sizeof(long double))));
}
#endif

input_port {
 mark:
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
  
  gcMARK2(ip->p.position_redirect, gc);
  gcMARK2(ip->sub_type, gc);
  gcMARK2(ip->port_data, gc);
  gcMARK2(ip->name, gc);
  gcMARK2(ip->peeked_read, gc);
  gcMARK2(ip->peeked_write, gc);
  gcMARK2(ip->read_handler, gc);
  gcMARK2(ip->closed_evt, gc);
  gcMARK2(ip->mref, gc);
  gcMARK2(ip->output_half, gc);
  gcMARK2(ip->special, gc);
  gcMARK2(ip->ungotten_special, gc);
  gcMARK2(ip->progress_evt, gc);
  gcMARK2(ip->input_lock, gc);
  gcMARK2(ip->input_giveup, gc);
  gcMARK2(ip->input_extras, gc);
  gcMARK2(ip->input_extras_ready, gc);
  gcMARK2(ip->unless, gc);
  gcMARK2(ip->unless_cache, gc);
#ifdef WINDOWS_FILE_HANDLES
  gcMARK2(ip->bufwidths, gc);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Input_Port));
}

output_port {
 mark:
  Scheme_Output_Port *op = (Scheme_Output_Port *)p;

  gcMARK2(op->p.position_redirect, gc);
  gcMARK2(op->sub_type, gc);
  gcMARK2(op->port_data, gc);
  gcMARK2(op->name, gc);
  gcMARK2(op->display_handler, gc);
  gcMARK2(op->write_handler, gc);
  gcMARK2(op->print_handler, gc);
  gcMARK2(op->closed_evt, gc);
  gcMARK2(op->mref, gc);
  gcMARK2(op->input_half, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Output_Port));
}


syntax_compiler {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

thread_val {
 mark:
  Scheme_Thread *pr = (Scheme_Thread *)p;

  gcMARK2(pr->next, gc);
  gcMARK2(pr->prev, gc);

  gcMARK2(pr->t_set_parent, gc);
  gcMARK2(pr->t_set_next, gc);
  gcMARK2(pr->t_set_prev, gc);

  MARK_cjs(&pr->cjs, gc);
  gcMARK2(pr->decompose_mc, gc);

  gcMARK2(pr->cell_values, gc);
  gcMARK2(pr->init_config, gc);
  gcMARK2(pr->init_break_cell, gc);

  if (!pr->runstack_owner
      || !GC_merely_accounting()
      || (*pr->runstack_owner == pr)) {
    Scheme_Object **rs = pr->runstack_start;
    gcFIXUP2_TYPED_NOW(Scheme_Object **, pr->runstack_start, gc);
    if (pr->runstack != pr->runstack_start + (pr->runstack - rs))
      pr->runstack = pr->runstack_start + (pr->runstack - rs);

    gcMARK2(pr->runstack_saved, gc);
  }
  gcMARK2(pr->runstack_owner, gc);
  gcMARK2(pr->runstack_swapped, gc);
  pr->spare_runstack = NULL; /* just in case */

  gcMARK2(pr->meta_prompt, gc);
  gcMARK2(pr->meta_continuation, gc);
  gcMARK2(pr->acting_barrier_prompt, gc);
  
  gcMARK2(pr->cont_mark_stack_segments, gc);
  gcMARK2(pr->cont_mark_stack_owner, gc);
  gcMARK2(pr->cont_mark_stack_swapped, gc);

  MARK_jmpup(&pr->jmpup_buf, gc);
  
  gcMARK2(pr->dw, gc);
  
  gcMARK2(pr->nester, gc);
  gcMARK2(pr->nestee, gc);

  gcMARK2(pr->current_ft, gc);
  
  gcMARK2(pr->blocker, gc);
  gcMARK2(pr->overflow, gc);

  gcMARK2(pr->return_marks_to, gc);
  gcMARK2(pr->returned_marks, gc);
  
  gcMARK2(pr->current_local_env, gc);
  gcMARK2(pr->current_local_mark, gc);
  gcMARK2(pr->current_local_name, gc);
  gcMARK2(pr->current_local_modidx, gc);
  gcMARK2(pr->current_local_menv, gc);
  gcMARK2(pr->current_local_bindings, gc);

  gcMARK2(pr->current_mt, gc);

  gcMARK2(pr->constant_folding, gc);
  gcMARK2(pr->reading_delayed, gc);
  
  gcMARK2(pr->overflow_reply, gc);

  gcMARK2(pr->values_buffer, gc);

  gcMARK2(pr->tail_buffer, gc);
  
  gcMARK2(pr->ku.eval.wait_expr, gc);

  gcMARK2(pr->ku.apply.tail_rator, gc);
  gcMARK2(pr->ku.apply.tail_rands, gc);

  gcMARK2(pr->ku.multiple.array, gc);

  gcMARK2(pr->ku.k.p1, gc);
  gcMARK2(pr->ku.k.p2, gc);
  gcMARK2(pr->ku.k.p3, gc);
  gcMARK2(pr->ku.k.p4, gc);
  gcMARK2(pr->ku.k.p5, gc);
  
  gcMARK2(pr->list_stack, gc);
  
  gcMARK2(pr->kill_data, gc);
  gcMARK2(pr->private_kill_data, gc);
  gcMARK2(pr->private_kill_next, gc);
  
  gcMARK2(pr->user_tls, gc);
  gcMARK2(pr->gmp_tls_data, gc);
  
  gcMARK2(pr->mr_hop, gc);
  gcMARK2(pr->mref, gc);
  gcMARK2(pr->extra_mrefs, gc);

  gcMARK2(pr->name, gc);

  gcMARK2(pr->transitive_resumes, gc);

  gcMARK2(pr->suspended_box, gc);
  gcMARK2(pr->resumed_box, gc);
  gcMARK2(pr->dead_box, gc);
  gcMARK2(pr->running_box, gc);
  gcMARK2(pr->sync_box, gc);

  gcMARK2(pr->mbox_first, gc);
  gcMARK2(pr->mbox_last, gc);
  gcMARK2(pr->mbox_sema, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Thread));
}

runstack_val {
  intptr_t *s = (intptr_t *)p;
 mark:
  void **a, **b;
  a = (void **)s + 4 + s[2];
  b = (void **)s + 4 + s[3];
  while (a < b) {
    gcMARK2(*a, gc);
    a++;
  }
 more:
 fixup:
  /* Zero out the part that we didn't mark, in case it becomes
     live later. */
  a = (void **)s + 4;
  b = (void **)s + 4 + s[2];
  while (a < b) {
    *a = RUNSTACK_ZERO_VAL;
    a++;
  }
  a = (void **)s + 4 + s[3];
  b = (void **)s + 4 + (s[1] - 4);
  while (a < b) {
    *a = RUNSTACK_ZERO_VAL;
    a++;
  }
 size:
  s[1];
}

prompt_val {
 mark: 
  Scheme_Prompt *pr = (Scheme_Prompt *)p;
  gcMARK2(pr->boundary_overflow_id, gc);
  if (!GC_merely_accounting())
    gcMARK2(pr->runstack_boundary_start, gc);
  gcMARK2(pr->tag, gc);
  gcMARK2(pr->id, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Prompt));
}

cont_mark_set_val {
 mark:
  Scheme_Cont_Mark_Set *s = (Scheme_Cont_Mark_Set *)p;
  gcMARK2(s->chain, gc);
  gcMARK2(s->native_stack_trace, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont_Mark_Set));
}

sema_val {
 mark:
  Scheme_Sema *s = (Scheme_Sema *)p;

  gcMARK2(s->first, gc);
  gcMARK2(s->last, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Sema));
}

channel_val {
 mark:
  Scheme_Channel *s = (Scheme_Channel *)p;

  gcMARK2(s->get_first, gc);
  gcMARK2(s->get_last, gc);
  gcMARK2(s->put_first, gc);
  gcMARK2(s->put_last, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Channel));
}

channel_put_val {
 mark:
  Scheme_Channel_Put *s = (Scheme_Channel_Put *)p;

  gcMARK2(s->ch, gc);
  gcMARK2(s->val, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Channel_Put));
}

hash_table_val {
 mark:
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p;

  gcMARK2(ht->keys, gc);
  gcMARK2(ht->vals, gc);
  gcMARK2(ht->mutex, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Hash_Table));
}

bucket_table_val {
 mark:
  Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)p;

  gcMARK2(ht->buckets, gc);
  gcMARK2(ht->mutex, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Bucket_Table));
}

namespace_val {
 mark:
  Scheme_Env *e = (Scheme_Env *)p;

  gcMARK2(e->module, gc);
  gcMARK2(e->module_registry, gc);
  gcMARK2(e->module_pre_registry, gc);
  gcMARK2(e->guard_insp, gc);
  gcMARK2(e->access_insp, gc);

  gcMARK2(e->rename_set, gc);
  gcMARK2(e->temp_marked_names, gc);
  gcMARK2(e->post_ex_rename_set, gc);

  gcMARK2(e->syntax, gc);
  gcMARK2(e->exp_env, gc);
  gcMARK2(e->template_env, gc);
  gcMARK2(e->label_env, gc);
  gcMARK2(e->instance_env, gc);

  gcMARK2(e->shadowed_syntax, gc);

  gcMARK2(e->lift_key, gc);

  gcMARK2(e->link_midx, gc);
  gcMARK2(e->require_names, gc);
  gcMARK2(e->et_require_names, gc);
  gcMARK2(e->tt_require_names, gc);
  gcMARK2(e->dt_require_names, gc);
  gcMARK2(e->other_require_names, gc);
  gcMARK2(e->running, gc);
  gcMARK2(e->did_starts, gc);
  gcMARK2(e->available_next[0], gc);
  gcMARK2(e->available_next[1], gc);

  gcMARK2(e->toplevel, gc);
  gcMARK2(e->modchain, gc);

  gcMARK2(e->modvars, gc);

  gcMARK2(e->weak_self_link, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Env));
}

module_reg_val {
 mark:
  Scheme_Module_Registry *r = (Scheme_Module_Registry *)p;
  gcMARK2(r->loaded, gc);
  gcMARK2(r->exports, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module_Registry));
}

random_state_val {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Random_State));
}

compilation_top_val {
 mark:
  Scheme_Compilation_Top *t = (Scheme_Compilation_Top *)p;
  gcMARK2(t->code, gc);
  gcMARK2(t->prefix, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compilation_Top));
}

prefix_val {
  Scheme_Prefix *pf = (Scheme_Prefix *)p;
 mark:
  int i;
  for (i = pf->num_slots; i--; )
    gcMARK2(pf->a[i], gc);
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Prefix) 
		    + ((pf->num_slots-mzFLEX_DELTA) * sizeof(Scheme_Object *))
                    + ((((pf->num_slots - (pf->num_stxes ? (pf->num_stxes+1) : 0)) + 31) / 32) 
                       * sizeof(int))));
}

resolve_prefix_val {
 mark:
  Resolve_Prefix *rp = (Resolve_Prefix *)p;
  gcMARK2(rp->toplevels, gc);
  gcMARK2(rp->stxes, gc);
  gcMARK2(rp->delay_info_rpair, gc);
  gcMARK2(rp->uses_unsafe, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Resolve_Prefix));
}

comp_prefix_val {
 mark:
  Comp_Prefix *cp = (Comp_Prefix *)p;
  gcMARK2(cp->toplevels, gc);
  gcMARK2(cp->inline_variants, gc);
  gcMARK2(cp->unbound, gc);
  gcMARK2(cp->stxes, gc);
  gcMARK2(cp->uses_unsafe, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Comp_Prefix));
}

svector_val {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;

  gcMARK2(SCHEME_SVEC_VEC(o), gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

stx_val {
 mark:
  Scheme_Stx *stx = (Scheme_Stx *)p;
  gcMARK2(stx->val, gc);
  gcMARK2(stx->srcloc, gc);
  gcMARK2(stx->wraps, gc);
  gcMARK2(stx->taints, gc);
  gcMARK2(stx->props, gc);
  if (!(MZ_OPT_HASH_KEY(&(stx)->iso) & STX_SUBSTX_FLAG))
    gcMARK2(stx->u.modinfo_cache, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Stx));
}

stx_off_val {
 mark:
  Scheme_Stx_Offset *o = (Scheme_Stx_Offset *)p;
  gcMARK2(o->src, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Stx_Offset));
}

module_val {
 mark:
  Scheme_Module *m = (Scheme_Module *)p;

  gcMARK2(m->phaseless, gc);

  gcMARK2(m->code_key, gc);

  gcMARK2(m->modname, gc);
  gcMARK2(m->modsrc, gc);

  gcMARK2(m->et_requires, gc);
  gcMARK2(m->requires, gc);
  gcMARK2(m->tt_requires, gc);
  gcMARK2(m->dt_requires, gc);
  gcMARK2(m->other_requires, gc);

  gcMARK2(m->bodies, gc);

  gcMARK2(m->me, gc);

  gcMARK2(m->exp_infos, gc);

  gcMARK2(m->self_modidx, gc);

  gcMARK2(m->insp, gc);

  gcMARK2(m->lang_info, gc);

  gcMARK2(m->hints, gc);
  gcMARK2(m->ii_src, gc);
  gcMARK2(m->super_bxs_info, gc);

  gcMARK2(m->comp_prefix, gc);
  gcMARK2(m->prefix, gc);
  gcMARK2(m->dummy, gc);

  gcMARK2(m->rn_stx, gc);

  gcMARK2(m->submodule_path, gc);
  gcMARK2(m->pre_submodules, gc);
  gcMARK2(m->post_submodules, gc);
  gcMARK2(m->pre_submodule_names, gc);
  gcMARK2(m->supermodule, gc);
  gcMARK2(m->submodule_ancestry, gc);

  gcMARK2(m->primitive, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module));
}

exp_info_val {
 mark:
  Scheme_Module_Export_Info *m = (Scheme_Module_Export_Info *)p;

  gcMARK2(m->provide_protects, gc);
  gcMARK2(m->indirect_provides, gc);

  gcMARK2(m->indirect_syntax_provides, gc);

  gcMARK2(m->accessible, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module_Export_Info));
}

module_phase_exports_val {
 mark:
  Scheme_Module_Phase_Exports *m = (Scheme_Module_Phase_Exports *)p;

  gcMARK2(m->phase_index, gc);

  gcMARK2(m->src_modidx, gc);

  gcMARK2(m->provides, gc);
  gcMARK2(m->provide_srcs, gc);
  gcMARK2(m->provide_src_names, gc);
  gcMARK2(m->provide_nominal_srcs, gc);
  gcMARK2(m->provide_src_phases, gc);

  gcMARK2(m->ht, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module_Phase_Exports));
}

module_exports_val {
 mark:
  Scheme_Module_Exports *m = (Scheme_Module_Exports *)p;

  gcMARK2(m->rt, gc);
  gcMARK2(m->et, gc);
  gcMARK2(m->dt, gc);
  gcMARK2(m->other_phases, gc);

  gcMARK2(m->src_modidx, gc);
  gcMARK2(m->modsrc, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module_Exports));
}

modidx_val {
 mark:
  Scheme_Modidx *modidx = (Scheme_Modidx *)p;

  gcMARK2(modidx->path, gc);
  gcMARK2(modidx->base, gc);
  gcMARK2(modidx->resolved, gc);
  gcMARK2(modidx->shift_cache, gc);
  gcMARK2(modidx->cache_next, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Modidx));
}

guard_val {
 mark:
  Scheme_Security_Guard *g = (Scheme_Security_Guard *)p;

  gcMARK2(g->parent, gc);
  gcMARK2(g->file_proc, gc);
  gcMARK2(g->network_proc, gc);
  gcMARK2(g->link_proc, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Security_Guard));
}

buf_holder {
 mark:
  Scheme_Jumpup_Buf_Holder *h = (Scheme_Jumpup_Buf_Holder *)p;
 
  MARK_jmpup(&h->buf, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Jumpup_Buf_Holder));
}

mark_inspector {
 mark:
  Scheme_Inspector *i = (Scheme_Inspector *)p;
  gcMARK2(i->superior, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Inspector));
}

mark_pipe {
 mark:
  Scheme_Pipe *pp = (Scheme_Pipe *)p;
    
  gcMARK2(pp->buf, gc);
  gcMARK2(pp->wakeup_on_read, gc);
  gcMARK2(pp->wakeup_on_write, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Pipe));
}

mark_logger {
 mark:
  Scheme_Logger *l = (Scheme_Logger *)p;
  gcMARK2(l->name, gc);
  gcMARK2(l->parent, gc);
  gcMARK2(l->callback, gc);
  gcMARK2(l->timestamp, gc);
  gcMARK2(l->syslog_level, gc);
  gcMARK2(l->stderr_level, gc);
  gcMARK2(l->readers, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Logger));
}

mark_log_reader {
 mark:
  Scheme_Log_Reader *lr = (Scheme_Log_Reader *)p;
  gcMARK2(lr->level, gc);
  gcMARK2(lr->sema, gc);
  gcMARK2(lr->head, gc);
  gcMARK2(lr->tail, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Log_Reader));
}

END type;

/**********************************************************************/

START env;

END env;

/**********************************************************************/

START compenv;

mark_comp_env {
 mark:
  Scheme_Full_Comp_Env *e = (Scheme_Full_Comp_Env *)p;

  gcMARK2(e->base.genv, gc);
  gcMARK2(e->base.insp, gc);
  gcMARK2(e->base.prefix, gc);
  gcMARK2(e->base.next, gc);
  gcMARK2(e->base.values, gc);
  gcMARK2(e->base.renames, gc);
  gcMARK2(e->base.uid, gc);
  gcMARK2(e->base.uids, gc);
  gcMARK2(e->base.dup_check, gc);
  gcMARK2(e->base.intdef_name, gc);
  gcMARK2(e->base.in_modidx, gc);
  gcMARK2(e->base.skip_table, gc);
  
  gcMARK2(e->data.const_names, gc);
  gcMARK2(e->data.const_vals, gc);
  gcMARK2(e->data.const_uids, gc);
  gcMARK2(e->data.sealed, gc);
  gcMARK2(e->data.use, gc);
  gcMARK2(e->data.lifts, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Full_Comp_Env));
}

END compenv;

/**********************************************************************/

START resolve;

mark_resolve_info {
 mark:
  Resolve_Info *i = (Resolve_Info *)p;
  
  gcMARK2(i->prefix, gc);
  gcMARK2(i->stx_map, gc);
  gcMARK2(i->tl_map, gc);
  gcMARK2(i->old_pos, gc);
  gcMARK2(i->new_pos, gc);
  gcMARK2(i->old_stx_pos, gc);
  gcMARK2(i->flags, gc);
  gcMARK2(i->lifts, gc);
  gcMARK2(i->lifted, gc);
  gcMARK2(i->next, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Resolve_Info));
}

mark_unresolve_info {
 mark:
  Unresolve_Info *i = (Unresolve_Info *)p;
  
  gcMARK2(i->flags, gc);
  gcMARK2(i->depths, gc);
  gcMARK2(i->prefix, gc);
  gcMARK2(i->closures, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Unresolve_Info));
}

END resolve;

/**********************************************************************/

START sfs;

mark_sfs_info {
 mark:
  SFS_Info *i = (SFS_Info *)p;
  
  gcMARK2(i->max_used, gc);
  gcMARK2(i->max_calls, gc);
  gcMARK2(i->saved, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(SFS_Info));
}

END sfs;

/**********************************************************************/

START letrec_check;

mark_letrec_check_frame {
 mark:
  Letrec_Check_Frame *frame = (Letrec_Check_Frame *)p;
  
  gcMARK2(frame->def, gc);
  gcMARK2(frame->next, gc);
  gcMARK2(frame->ref, gc);
  gcMARK2(frame->checked, gc);
  gcMARK2(frame->head, gc);
  gcMARK2(frame->deferred_with_rhs_ref, gc);
  gcMARK2(frame->deferred_with_body_ref, gc);
  gcMARK2(frame->deferred_with_no_ref, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Letrec_Check_Frame));
}

mark_scheme_deferred_expr {
 mark:
  Scheme_Deferred_Expr *clos = (Scheme_Deferred_Expr *)p;
  
  gcMARK2(clos->expr, gc);
  gcMARK2(clos->frame, gc);
  gcMARK2(clos->uvars, gc);
  gcMARK2(clos->pvars, gc);
  gcMARK2(clos->subexpr_ls, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Deferred_Expr));
}

END letrec_check;

/**********************************************************************/

START optimize;

mark_optimize_info {
 mark:
  Optimize_Info *i = (Optimize_Info *)p;
  
  gcMARK2(i->stat_dists, gc);
  gcMARK2(i->sd_depths, gc);
  gcMARK2(i->next, gc);
  gcMARK2(i->use, gc);
  gcMARK2(i->consts, gc);
  gcMARK2(i->cp, gc);
  gcMARK2(i->top_level_consts, gc);
  gcMARK2(i->transitive_use, gc);
  gcMARK2(i->transitive_use_len, gc);
  gcMARK2(i->context, gc);
  gcMARK2(i->logger, gc);
  gcMARK2(i->types, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Optimize_Info));
}

mark_once_used {
 mark:
  Scheme_Once_Used *o = (Scheme_Once_Used *)p;
  gcMARK2(o->expr, gc);
  gcMARK2(o->info, gc);
  gcMARK2(o->next, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Once_Used));
}

END optimize;

/**********************************************************************/

START eval;

mark_comp_info {
 mark:
  Scheme_Compile_Info *i = (Scheme_Compile_Info *)p;
  
  gcMARK2(i->value_name, gc);
  gcMARK2(i->observer, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compile_Info));
}

mark_saved_stack {
 mark:
  Scheme_Saved_Stack *saved = (Scheme_Saved_Stack *)p;
  
  gcMARK2(saved->prev, gc);
  gcMARK2(saved->runstack_start, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Saved_Stack));
}

END eval;

/**********************************************************************/

START validate;

mark_validate_clearing {
 mark:
  Validate_Clearing *vc = (Validate_Clearing *)p;
  
  gcMARK2(vc->stack, gc);
  gcMARK2(vc->ncstack, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Validate_Clearing));
}

END validate;

/**********************************************************************/

START fun;

mark_closure_info {
 mark:
  Closure_Info *i = (Closure_Info *)p;
  
  gcMARK2(i->local_flags, gc);
  gcMARK2(i->base_closure_map, gc);
  gcMARK2(i->local_type_map, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Closure_Info));
}

mark_dyn_wind_cell {
 mark:
  Scheme_Dynamic_Wind_List *l = (Scheme_Dynamic_Wind_List *)p;
  
  gcMARK2(l->dw, gc);
  gcMARK2(l->next, gc);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Dynamic_Wind_List));
}

mark_dyn_wind_info {
 mark:
  Dyn_Wind *d = (Dyn_Wind *)p;
  
  gcMARK2(d->pre, gc);
  gcMARK2(d->act, gc);
  gcMARK2(d->post, gc);

 size:
   gcBYTES_TO_WORDS(sizeof(Dyn_Wind));
}

mark_cont_mark_chain {
 mark:
  Scheme_Cont_Mark_Chain *c = (Scheme_Cont_Mark_Chain *)p;
  
  gcMARK2(c->key, gc);
  gcMARK2(c->val, gc);
  gcMARK2(c->next, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont_Mark_Chain));
}

#ifdef MZ_USE_JIT

mark_lightweight_cont {
 mark:
  Scheme_Lightweight_Continuation *lw = (Scheme_Lightweight_Continuation *)p;

  gcMARK2(lw->saved_lwc, gc);
  gcMARK2(lw->stack_slice, gc);
  gcMARK2(lw->runstack_slice, gc);
  gcMARK2(lw->cont_mark_stack_slice, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Lightweight_Continuation));
}

#endif

END fun;

/**********************************************************************/

START hash;

hash_tree_val {
 mark:
  Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)p;

  gcMARK2(ht->root, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Hash_Tree));
}

mark_avl_node {
 mark:
  AVLNode *avl = (AVLNode *)p;

  /* Short-circuit on NULL pointers, which are especially likely */
  if (avl->left) {
    gcMARK2(avl->left, gc);
  }
  if (avl->right) {
    gcMARK2(avl->right, gc);
  }
  gcMARK2(avl->key, gc);
  gcMARK2(avl->val, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(AVLNode));
}

END hash;

/**********************************************************************/

START place;

place_bi_channel_val {
 mark:
  Scheme_Place_Bi_Channel *pbc = (Scheme_Place_Bi_Channel *)p;
  gcMARK2(pbc->link, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Place_Bi_Channel));
}

place_object_val {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Place_Object));
}

place_val {
 mark:
  Scheme_Place *pr = (Scheme_Place *)p;
  gcMARK2(pr->channel, gc);
  gcMARK2(pr->mref, gc);
  gcMARK2(pr->pumper_threads, gc);
  gcMARK2(pr->place_obj, gc);
  gcMARK2(pr->prev, gc);
  gcMARK2(pr->next, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Place));
}

place_async_channel_val {
 mark:
  Scheme_Place_Async_Channel *pac = (Scheme_Place_Async_Channel *)p;
  Scheme_Object *pr;
  int i, j, sz;
  gcMARK2(pac->msgs, gc);
  gcMARK2(pac->msg_memory, gc);
  gcMARK2(pac->msg_chains, gc);
  gcMARK2(pac->wakeup_signal, gc);

  /* mark master-allocated objects within each messages; the
     raw pairs that form the list are embedded in each message block */
  j = pac->out;
  sz = pac->size;
  for (i = pac->count; i--; ) {
    pr = pac->msg_chains[j];
    while (pr) {
      gcMARK2(SCHEME_CAR(pr), gc);
      pr = SCHEME_CDR(pr);
    }
    j = ((j + 1) % sz);
  }

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Place_Async_Channel));
}

serialized_file_fd_val {
 mark:
  Scheme_Serialized_File_FD *ffd = (Scheme_Serialized_File_FD *) p;
  gcMARK2(ffd->name, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Serialized_File_FD));
}

serialized_socket_fd_val {
 mark:
  Scheme_Serialized_Socket_FD *sfd = (Scheme_Serialized_Socket_FD *) p;
  gcMARK2(sfd->name, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Serialized_Socket_FD));
}

END place;

/**********************************************************************/

START portfun;

mark_load_handler_data {
 mark:
  LoadHandlerData *d = (LoadHandlerData *)p;
    
  gcMARK2(d->config, gc);
  gcMARK2(d->port, gc);
  gcMARK2(d->p, gc);
  gcMARK2(d->stxsrc, gc);
  gcMARK2(d->expected_module, gc);
  
 size:
  gcBYTES_TO_WORDS(sizeof(LoadHandlerData));
}

mark_indexed_string {
 mark:
  Scheme_Indexed_String *is = (Scheme_Indexed_String *)p;
    
  gcMARK2(is->string, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Indexed_String));
}

mark_user_input {
 mark:
  User_Input_Port *uip = (User_Input_Port *)p;

  gcMARK2(uip->read_proc, gc);
  gcMARK2(uip->peek_proc, gc);
  gcMARK2(uip->progress_evt_proc, gc);
  gcMARK2(uip->peeked_read_proc, gc);
  gcMARK2(uip->location_proc, gc);
  gcMARK2(uip->count_lines_proc, gc);
  gcMARK2(uip->buffer_mode_proc, gc);
  gcMARK2(uip->close_proc, gc);
  gcMARK2(uip->reuse_str, gc);
  gcMARK2(uip->peeked, gc);
  gcMARK2(uip->prefix_pipe, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(User_Input_Port));
}

mark_user_output {
 mark:
  User_Output_Port *uop = (User_Output_Port *)p;

  gcMARK2(uop->evt, gc);
  gcMARK2(uop->write_evt_proc, gc);
  gcMARK2(uop->write_proc, gc);
  gcMARK2(uop->write_special_evt_proc, gc);
  gcMARK2(uop->write_special_proc, gc);
  gcMARK2(uop->location_proc, gc);
  gcMARK2(uop->count_lines_proc, gc);
  gcMARK2(uop->buffer_mode_proc, gc);
  gcMARK2(uop->close_proc, gc);
  gcMARK2(uop->buffer_pipe, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(User_Output_Port));
}

END portfun;

/**********************************************************************/

START port;

mark_input_file {
 mark:
  Scheme_Input_File *i = (Scheme_Input_File *)p;

  gcMARK2(i->f, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Input_File));
}

mark_output_file {
 mark:
  Scheme_Output_File *o = (Scheme_Output_File *)p;

  gcMARK2(o->f, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Output_File));
}

#ifdef MZ_FDS
mark_input_fd {
 mark:
  Scheme_FD *fd = (Scheme_FD *)p;

  gcMARK2(fd->buffer, gc);
  gcMARK2(fd->refcount, gc);
  gcMARK2(fd->flush_handle, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_FD));
}
#endif

#if defined(UNIX_PROCESSES) && !(defined(MZ_USE_PLACES) && defined(MZ_PRECISE_GC))
mark_system_child {
 mark:
  System_Child *sc = (System_Child *)p;

  gcMARK2(sc->next, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(System_Child));
}
#endif

#ifdef USE_OSKIT_CONSOLE
mark_oskit_console_input {
 mark:
  osk_console_input *c = (osk_console_input *)p;
    
  gcMARK2(c->buffer, gc);
  gcMARK2(c->next, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(osk_console_input));
}
#endif

mark_subprocess {
 mark:
#ifndef WINDOWS_PROCESSES
  Scheme_Subprocess *sp = (Scheme_Subprocess *)p;
  gcMARK2(sp->handle, gc);
  gcMARK2(sp->mref, gc);
#endif
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Subprocess));
}

mark_read_write_evt {
 mark:
  Scheme_Read_Write_Evt *rww = (Scheme_Read_Write_Evt *)p;
  gcMARK2(rww->port, gc);
  gcMARK2(rww->v, gc);
  gcMARK2(rww->str, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Read_Write_Evt));
}

mark_filesystem_change_evt {
 mark:
  Scheme_Filesystem_Change_Evt *fc = (Scheme_Filesystem_Change_Evt *)p;
  gcMARK2(fc->sema, gc);
  gcMARK2(fc->mref, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Filesystem_Change_Evt));
}

END port;

/**********************************************************************/

START print;

mark_print_params {
 mark:
  PrintParams *pp = (PrintParams *)p;
  gcMARK2(pp->inspector, gc);
  gcMARK2(pp->print_port, gc);
  gcMARK2(pp->print_buffer, gc);
  gcMARK2(pp->depth_delta, gc);
  gcMARK2(pp->uq_ht, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(PrintParams));
}

mark_marshal_tables {
 mark:
  Scheme_Marshal_Tables *mt = (Scheme_Marshal_Tables *)p;
  gcMARK2(mt->symtab, gc);
  gcMARK2(mt->rns, gc);
  gcMARK2(mt->rn_refs, gc);
  gcMARK2(mt->st_refs, gc);
  gcMARK2(mt->st_ref_stack, gc);
  gcMARK2(mt->reverse_map, gc);
  gcMARK2(mt->same_map, gc);
  gcMARK2(mt->shift_map, gc);
  gcMARK2(mt->top_map, gc);
  gcMARK2(mt->key_map, gc);
  gcMARK2(mt->delay_map, gc);
  gcMARK2(mt->cdata_map, gc);
  gcMARK2(mt->rn_saved, gc);
  gcMARK2(mt->shared_offsets, gc);
  gcMARK2(mt->sorted_keys, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Marshal_Tables));
}

END print;

/**********************************************************************/

START network;

#ifdef USE_TCP
mark_listener {
  listener_t *l = (listener_t *)p;

 mark:

  gcMARK2(l->mref, gc);
# ifdef HAVE_POLL_SYSCALL
  gcMARK2(l->pfd, gc);
# endif

 size:
  gcBYTES_TO_WORDS(sizeof(listener_t) + ((l->count - mzFLEX_DELTA) * sizeof(tcp_t)));
}

mark_tcp {
 mark:
  Scheme_Tcp *tcp = (Scheme_Tcp *)p;

  gcMARK2(tcp->b.buffer, gc);
  gcMARK2(tcp->b.out_buffer, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Tcp));
}

# ifdef UDP_IS_SUPPORTED
mark_udp {
 mark:
  Scheme_UDP *udp = (Scheme_UDP *)p;

  gcMARK2(udp->previous_from_addr, gc);
  gcMARK2(udp->mref, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_UDP));
}

mark_udp_evt {
 mark:
  Scheme_UDP_Evt *uw = (Scheme_UDP_Evt *)p;

  gcMARK2(uw->udp, gc);
  gcMARK2(uw->str, gc);
  gcMARK2(uw->dest_addr, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_UDP_Evt));
}
# endif
#endif

END network;

/**********************************************************************/

START thread;

mark_parameterization {
 mark:
  Scheme_Parameterization *c = (Scheme_Parameterization *)p;
  int i;
    
  for (i = max_configs; i--; ) {
    gcMARK2(c->prims[i], gc);
  }
  gcMARK2(c->extensions, gc);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Parameterization)
		    + ((max_configs - mzFLEX_DELTA) * sizeof(Scheme_Object*))));
}

mark_config {
 mark:
  Scheme_Config *config = (Scheme_Config *)p;
  gcMARK2(config->ht, gc);
  gcMARK2(config->root, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Config));
}

mark_will_executor_val {
 mark:
  WillExecutor *e = (WillExecutor *)p;
  
  gcMARK2(e->sema, gc);
  gcMARK2(e->first, gc);
  gcMARK2(e->last, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(WillExecutor));
}

mark_custodian_val {
 mark:
  Scheme_Custodian *m = (Scheme_Custodian *)p;
  
  gcMARK2(m->boxes, gc);
  gcMARK2(m->mrefs, gc);
  gcMARK2(m->closers, gc);
  gcMARK2(m->data, gc);
  gcMARK2(m->data_ptr, gc);

  gcMARK2(m->parent, gc);
  gcMARK2(m->sibling, gc);
  gcMARK2(m->children, gc);

  gcMARK2(m->global_next, gc);
  gcMARK2(m->global_prev, gc);

  gcMARK2(m->cust_boxes, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Custodian));
}

mark_custodian_box_val {
 mark:
  Scheme_Custodian_Box *b = (Scheme_Custodian_Box *)p;
  int sd = ((Scheme_Custodian *)GC_resolve2(b->cust, gc))->shut_down;

  gcMARK2(b->cust, gc);
  if (!sd) {
    gcMARK2(b->v, gc);
  }

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Custodian_Box));
}

mark_thread_hop {
 mark:
  Scheme_Thread_Custodian_Hop *hop = (Scheme_Thread_Custodian_Hop *)p;

  gcMARK2(hop->p, gc);

 size:
   gcBYTES_TO_WORDS(sizeof(Scheme_Thread_Custodian_Hop));
}

mark_param_data {
 mark:
  ParamData *d = (ParamData *)p;

  gcMARK2(d->key, gc);
  gcMARK2(d->guard, gc);
  gcMARK2(d->extract_guard, gc);
  gcMARK2(d->defcell, gc);

 size:
   gcBYTES_TO_WORDS(sizeof(ParamData));
}

mark_will {
 mark:
  ActiveWill *w = (ActiveWill *)p;
  
  gcMARK2(w->o, gc);
  gcMARK2(w->proc, gc);
  gcMARK2(w->w, gc);
  gcMARK2(w->next, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(ActiveWill));
}

mark_evt {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Evt));
}

mark_syncing {
 mark:
  Syncing *w = (Syncing *)p;
 
  gcMARK2(w->set, gc);
  gcMARK2(w->wrapss, gc);
  gcMARK2(w->nackss, gc);
  gcMARK2(w->reposts, gc);
  gcMARK2(w->accepts, gc);
  gcMARK2(w->disable_break, gc);
  gcMARK2(w->thread, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Syncing));
}

mark_evt_set {
 mark:
  Evt_Set *w = (Evt_Set *)p;
 
  gcMARK2(w->ws, gc);
  gcMARK2(w->argv, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Evt_Set));
}

mark_thread_set {
 mark:
  Scheme_Thread_Set *ts = (Scheme_Thread_Set *)p;
 
  gcMARK2(ts->parent, gc);
  gcMARK2(ts->first, gc);
  gcMARK2(ts->next, gc);
  gcMARK2(ts->prev, gc);
  gcMARK2(ts->search_start, gc);
  gcMARK2(ts->current, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Thread_Set));
}

mark_thread_cell {
 mark:
  Thread_Cell *c = (Thread_Cell *)p;
 
  gcMARK2(c->def_val, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Thread_Cell));
}

mark_plumber {
 mark:
  Scheme_Plumber *pl = (Scheme_Plumber *)p;
 
  gcMARK2(pl->handles, gc);
  gcMARK2(pl->weak_handles, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Plumber));
}

END thread;

/**********************************************************************/

START salloc;

mark_finalization {
 mark:
  Finalization *f = (Finalization *)p;
  
  gcMARK2(f->data, gc);
  gcMARK2(f->next, gc);
  gcMARK2(f->prev, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Finalization));
}

mark_finalizations {
 mark:
  Finalizations *f = (Finalizations *)p;

  gcMARK2(f->scheme_first, gc);
  gcMARK2(f->scheme_last, gc);
  gcMARK2(f->prim_first, gc);
  gcMARK2(f->prim_last, gc);
  gcMARK2(f->ext_data, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Finalizations));
}

END salloc;

/**********************************************************************/

START sema;

mark_channel_syncer {
 mark:
  Scheme_Channel_Syncer *w = (Scheme_Channel_Syncer *)p;

  gcMARK2(w->p, gc);
  gcMARK2(w->prev, gc);
  gcMARK2(w->next, gc);
  gcMARK2(w->syncing, gc);
  gcMARK2(w->obj, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Channel_Syncer));
}

mark_alarm {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Alarm));
}

END sema;

/**********************************************************************/

START struct;

#ifdef MZ_USE_PLACES
mark_serialized_struct_val {
  Scheme_Serialized_Structure *s = (Scheme_Serialized_Structure *)p;
  int num_slots = s->num_slots;

 mark:
  int i;

  gcMARK2(s->prefab_key, gc);
  
  for(i = num_slots; i--; )
    gcMARK2(s->slots[i], gc);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Serialized_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
}
#endif

mark_struct_val {
  Scheme_Structure *s = (Scheme_Structure *)p;
  int num_slots = ((Scheme_Struct_Type *)GC_resolve2(s->stype, gc))->num_slots;

 mark:
  int i;

  gcFIXUP2_TYPED_NOW(Scheme_Struct_Type *, s->stype, gc);
  
  for(i = num_slots; i--; )
    gcMARK2(s->slots[i], gc);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Structure) 
		    + ((num_slots - mzFLEX_DELTA) * sizeof(Scheme_Object *))));
}

mark_struct_type_val {
  Scheme_Struct_Type *t = (Scheme_Struct_Type *)p;

 mark:
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

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Struct_Type)
		    + ((t->name_pos + 1 - mzFLEX_DELTA) 
                       * sizeof(Scheme_Struct_Type *))));
}

mark_struct_property {
 mark:
  Scheme_Struct_Property *i = (Scheme_Struct_Property *)p;
  gcMARK2(i->name, gc);
  gcMARK2(i->guard, gc);
  gcMARK2(i->supers, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Struct_Property));
}

mark_wrapped_evt {
 mark:
  Wrapped_Evt *ww = (Wrapped_Evt *)p;

  gcMARK2(ww->evt, gc);
  gcMARK2(ww->wrapper, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Wrapped_Evt));
}

mark_nack_guard_evt {
 mark:
  Nack_Guard_Evt *nw = (Nack_Guard_Evt *)p;

  gcMARK2(nw->maker, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Nack_Guard_Evt));
}

mark_chaperone {
 mark:
  Scheme_Chaperone *px = (Scheme_Chaperone *)p;

  gcMARK2(px->val, gc);
  gcMARK2(px->prev, gc);
  gcMARK2(px->props, gc);
  gcMARK2(px->redirects, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Chaperone));
}

END struct;

/**********************************************************************/

START compile;

END compile;

/**********************************************************************/

START read;

mark_indent {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Indent));
}

mark_cport {
 mark:
  CPort *cp = (CPort *)p;
  gcMARK2(cp->start, gc);
  gcMARK2(cp->orig_port, gc);
  gcMARK2(cp->ht, gc);
  gcMARK2(cp->ut, gc);
  gcMARK2(cp->symtab, gc);
  gcMARK2(cp->relto, gc);
  gcMARK2(cp->magic_sym, gc);
  gcMARK2(cp->magic_val, gc);
  gcMARK2(cp->shared_offsets, gc);
  gcMARK2(cp->delay_info, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(CPort));
}

mark_readtable {
 mark:
  Readtable *t = (Readtable *)p;
  gcMARK2(t->mapping, gc);
  gcMARK2(t->fast_mapping, gc);
  gcMARK2(t->symbol_parser, gc);
  gcMARK2(t->names, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Readtable));
}

mark_read_params {
 mark:
  ReadParams *rp = (ReadParams *)p;
  gcMARK2(rp->table, gc);
  gcMARK2(rp->magic_sym, gc);
  gcMARK2(rp->magic_val, gc);
  gcMARK2(rp->delay_load_info, gc);
  gcMARK2(rp->read_relative_path, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(ReadParams));
}

mark_delay_load {
 mark:
  Scheme_Load_Delay *ld = (Scheme_Load_Delay *)p;
  gcMARK2(ld->path, gc);
  gcMARK2(ld->symtab, gc);
  gcMARK2(ld->shared_offsets, gc);
  gcMARK2(ld->relto, gc);
  gcMARK2(ld->ut, gc);
  gcMARK2(ld->current_rp, gc);
  gcMARK2(ld->cached, gc);
  gcMARK2(ld->cached_port, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Load_Delay));
}

mark_unmarshal_tables {
 mark:
  Scheme_Unmarshal_Tables *ut = (Scheme_Unmarshal_Tables *)p;
  gcMARK2(ut->rns, gc);
  gcMARK2(ut->rp, gc);
  gcMARK2(ut->decoded, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Unmarshal_Tables));
}

END read;

/**********************************************************************/

START regexp;

mark_regexp {
  regexp *r = (regexp *)p;
 mark:
  gcMARK2(r->source, gc);
  gcMARK2(r->regstart, gc);
 size:
  gcBYTES_TO_WORDS((sizeof(regexp) + r->regsize));
}

mark_regwork {
 mark:
  Regwork *r = (Regwork *)p;
  gcMARK2(r->str, gc);
  gcMARK2(r->instr, gc);
  gcMARK2(r->port, gc);
  gcMARK2(r->unless_evt, gc);
  gcMARK2(r->startp, gc);
  gcMARK2(r->maybep, gc);
  gcMARK2(r->endp, gc);
  gcMARK2(r->counters, gc);
  gcMARK2(r->peekskip, gc);
  gcMARK2(r->prefix, gc);
  gcMARK2(r->lazy_string, gc);
  gcMARK2(r->rewind_stack, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Regwork));
}

mark_lazy_string {
 mark:
  rx_lazy_str_t *ls = (rx_lazy_str_t *)p;
  gcMARK2(ls->s, gc);
  gcMARK2(ls->chars, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(rx_lazy_str_t));
}

END regexp;

/**********************************************************************/

START string;

mark_string_convert {
 mark:
  Scheme_Converter *c = (Scheme_Converter *)p;
  gcMARK2(c->mref, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Converter));
}

END string;

/**********************************************************************/

START syntax;

mark_rename_table {
 mark:
  Module_Renames *rn = (Module_Renames *)p;
  gcMARK2(rn->phase, gc);
  gcMARK2(rn->ht, gc);
  gcMARK2(rn->nomarshal_ht, gc);
  gcMARK2(rn->unmarshal_info, gc);
  gcMARK2(rn->shared_pes, gc);
  gcMARK2(rn->set_identity, gc);
  gcMARK2(rn->marked_names, gc);
  gcMARK2(rn->free_id_renames, gc);
  gcMARK2(rn->insp, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Module_Renames));
}

mark_rename_table_set {
 mark:
  Module_Renames_Set *rns = (Module_Renames_Set *)p;
  gcMARK2(rns->et, gc);
  gcMARK2(rns->rt, gc);
  gcMARK2(rns->other_phases, gc);
  gcMARK2(rns->share_marked_names, gc);
  gcMARK2(rns->set_identity, gc);
  gcMARK2(rns->insp, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Module_Renames_Set));
}

mark_srcloc {
 mark:
  Scheme_Stx_Srcloc *s = (Scheme_Stx_Srcloc *)p;
  gcMARK2(s->src, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Stx_Srcloc));
}

mark_wrapchunk {
  Wrap_Chunk *wc = (Wrap_Chunk *)p;
 mark:
  int i;
  for (i = wc->len; i--; ) {
    gcMARK2(wc->a[i], gc);
  }
 size:
  gcBYTES_TO_WORDS(sizeof(Wrap_Chunk) + ((wc->len - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
}

lex_rib {
 mark:
  Scheme_Lexical_Rib *rib = (Scheme_Lexical_Rib *)p;
  gcMARK2(rib->rename, gc);
  gcMARK2(rib->timestamp, gc);
  gcMARK2(rib->sealed, gc);
  gcMARK2(rib->mapped_names, gc);
  gcMARK2(rib->next, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Lexical_Rib));
}

END syntax;

/**********************************************************************/

START jit;

native_closure {
  Scheme_Native_Closure *c = (Scheme_Native_Closure *)p;
  int closure_size = ((Scheme_Native_Closure_Data *)GC_resolve2(c->code, gc))->closure_size;

  if (closure_size < 0) {
    closure_size = -(closure_size + 1);
  }

 mark:
  {
  int i = closure_size;
  START_MARK_ONLY;
# define CLOSURE_DATA_TYPE Scheme_Native_Closure_Data
# include "mzclpf_decl.inc"
  END_MARK_ONLY;

  gcMARK2(c->code, gc);

  START_MARK_ONLY;
# include "mzclpf_pre.inc"
  END_MARK_ONLY;

  while (i--)
    gcMARK2(c->vals[i], gc);

  START_MARK_ONLY;
# include "mzclpf_post.inc"
# undef CLOSURE_DATA_TYPE
  END_MARK_ONLY;
  }

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Native_Closure)
		    + (closure_size - mzFLEX_DELTA) * sizeof(Scheme_Object *)));
}

mark_jit_state {
 mark:
  mz_jit_state *j = (mz_jit_state *)p;
  gcMARK2(j->mappings, gc);
  gcMARK2(j->self_data, gc);
  gcMARK2(j->example_argv, gc);
  gcMARK2(j->nc, gc);
  gcMARK2(j->retaining_data, gc);
  gcMARK2(j->patch_depth, gc);
  
 size:
  gcBYTES_TO_WORDS(sizeof(mz_jit_state));
}

native_unclosed_proc {
 mark:
  Scheme_Native_Closure_Data *d = (Scheme_Native_Closure_Data *)p;
  int i;

  gcMARK2(d->u2.name, gc);
  if (d->retained) {
    for (i = SCHEME_INT_VAL(d->retained[0]); i--; ) {
      gcMARK2(d->retained[i], gc);
    }
  }
  if (d->closure_size < 0) {
    gcMARK2(d->u.arities, gc);
  }
  gcMARK2(d->tl_map, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Native_Closure_Data));
}

native_unclosed_proc_plus_case {
 mark:
  Scheme_Native_Closure_Data_Plus_Case *d = (Scheme_Native_Closure_Data_Plus_Case *)p;

  native_unclosed_proc_MARK(p, gc);
  gcMARK2(d->case_lam, gc);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Native_Closure_Data_Plus_Case));
}

END jit;

/**********************************************************************/

START future;

#ifdef MZ_USE_FUTURES

future {
 mark:
  future_t *f = (future_t *)p;
  gcMARK2(f->orig_lambda, gc);
  gcMARK2(f->cust, gc);
  gcMARK2(f->arg_s0, gc);
  gcMARK2(f->arg_t0, gc);
  gcMARK2(f->arg_S0, gc);
  gcMARK2(f->arg_b0, gc);
  gcMARK2(f->arg_n0, gc);
  gcMARK2(f->arg_s1, gc);
  gcMARK2(f->arg_t1, gc);
  gcMARK2(f->arg_S1, gc);
  gcMARK2(f->arg_s2, gc);
  gcMARK2(f->arg_S2, gc);
  gcMARK2(f->arg_S4, gc);
  gcMARK2(f->retval_s, gc);
  gcMARK2(f->retval, gc);
  gcMARK2(f->multiple_array, gc);
  gcMARK2(f->tail_rator, gc);
  gcMARK2(f->tail_rands, gc);
  gcMARK2(f->prev, gc);
  gcMARK2(f->next, gc);
  gcMARK2(f->next_waiting_atomic, gc);
  gcMARK2(f->next_waiting_lwc, gc);
  gcMARK2(f->next_waiting_touch, gc);
  gcMARK2(f->suspended_lw, gc);
  gcMARK2(f->suspended_lw_stack, gc);
  gcMARK2(f->prev_in_fsema_queue, gc);
  gcMARK2(f->next_in_fsema_queue, gc);
  gcMARK2(f->touching, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(future_t));
}

fsemaphore {
 mark:
    fsemaphore_t *s = (fsemaphore_t*)p;
    gcMARK2(s->queue_front, gc);
    gcMARK2(s->queue_end, gc);
 size:
    gcBYTES_TO_WORDS(sizeof(fsemaphore_t));
}

#else

sequential_future {
 mark:
  future_t *f = (future_t *)p;
  gcMARK2(f->orig_lambda, gc);
  gcMARK2(f->running_sema, gc);
  gcMARK2(f->retval, gc);
  gcMARK2(f->multiple_array, gc);
 size:
  gcBYTES_TO_WORDS(sizeof(future_t));
}

sequential_fsemaphore {
 mark:
    fsemaphore_t *s = (fsemaphore_t*)p;
    gcMARK2(s->sema, gc);
 size:
    gcBYTES_TO_WORDS(sizeof(fsemaphore_t));
}

#endif



END future;

/**********************************************************************/
