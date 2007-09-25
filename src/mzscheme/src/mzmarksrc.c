
START type;

variable_obj {
 mark:
  Scheme_Bucket *b = (Scheme_Bucket *)p;

  gcMARK(b->key);
  gcMARK(b->val);
  gcMARK(((Scheme_Bucket_With_Home *)b)->home);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Bucket_With_Home));
}

module_var {
 mark:
  Module_Variable *mv = (Module_Variable *)p;

  gcMARK(mv->modidx);
  gcMARK(mv->sym);
  gcMARK(mv->insp);

 size:
  gcBYTES_TO_WORDS(sizeof(Module_Variable));
}

bucket_obj {
 mark:
  Scheme_Bucket *b = (Scheme_Bucket *)p;

  gcMARK(b->key);
  gcMARK(b->val);

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
  gcMARK(SCHEME_CPTR_VAL(p));
  gcMARK(SCHEME_CPTR_TYPE(p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cptr));
}

offset_cpointer_obj {
 mark:
  gcMARK(SCHEME_CPTR_VAL(p));
  gcMARK(SCHEME_CPTR_TYPE(p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Offset_Cptr));
}

second_of_cons {
 mark:
  gcMARK(SCHEME_PTR2_VAL((Scheme_Object *)p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

twoptr_obj {
 mark:
  gcMARK(SCHEME_PTR1_VAL((Scheme_Object *)p));
  gcMARK(SCHEME_PTR2_VAL((Scheme_Object *)p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

iptr_obj {
 mark:
  gcMARK(SCHEME_IPTR_VAL((Scheme_Object *)p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

small_object {
 mark:
  gcMARK(((Scheme_Small_Object *)p)->u.ptr_value);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Small_Object));
}

app_rec {
  Scheme_App_Rec *r = (Scheme_App_Rec *)p;

 mark:
  int i = r->num_args + 1;
  while (i--) 
    gcMARK(r->args[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_App_Rec) 
		    + (r->num_args * sizeof(Scheme_Object *))
		    + ((r->num_args + 1) * sizeof(char))));
}

app2_rec {
 mark:
  Scheme_App2_Rec *r = (Scheme_App2_Rec *)p;
  gcMARK(r->rator);
  gcMARK(r->rand);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_App2_Rec));
}

app3_rec {
 mark:
  Scheme_App3_Rec *r = (Scheme_App3_Rec *)p;
  gcMARK(r->rator);
  gcMARK(r->rand1);
  gcMARK(r->rand2);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_App3_Rec));
}

seq_rec {
  Scheme_Sequence *s = (Scheme_Sequence *)p;

 mark:
  int i = s->count;
  while (i--)
    gcMARK(s->array[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Sequence)
		    + ((s->count - 1) * sizeof(Scheme_Object *))));
}

branch_rec {
 mark:
  Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)p;
  
  gcMARK(b->test);
  gcMARK(b->tbranch);
  gcMARK(b->fbranch);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Branch_Rec));
}

unclosed_proc {
 mark:
  Scheme_Closure_Data *d = (Scheme_Closure_Data *)p;

  gcMARK(d->name);
  gcMARK(d->code);
  gcMARK(d->closure_map);
#ifdef MZ_USE_JIT
  gcMARK(d->u.native_code);
  gcMARK(d->context);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Closure_Data));
}

let_value {
 mark:
  Scheme_Let_Value *l = (Scheme_Let_Value *)p;
  
  gcMARK(l->value);
  gcMARK(l->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Value));
}

let_void {
 mark:
  Scheme_Let_Void *l = (Scheme_Let_Void *)p;

  gcMARK(l->body);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Void));
}

letrec {
 mark:
  Scheme_Letrec *l = (Scheme_Letrec *)p;
  
  gcMARK(l->procs);
  gcMARK(l->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Letrec));
}

let_one {
 mark:
  Scheme_Let_One *l = (Scheme_Let_One *)p;
  
  gcMARK(l->value);
  gcMARK(l->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_One));
}

with_cont_mark {
 mark:
  Scheme_With_Continuation_Mark *w = (Scheme_With_Continuation_Mark *)p;

  gcMARK(w->key);
  gcMARK(w->val);
  gcMARK(w->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_With_Continuation_Mark));
}

comp_let_value {
 mark:
  Scheme_Compiled_Let_Value *c = (Scheme_Compiled_Let_Value *)p;

  gcMARK(c->flags);
  gcMARK(c->value);
  gcMARK(c->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compiled_Let_Value));
}

let_header {
 mark:
  Scheme_Let_Header *h = (Scheme_Let_Header *)p;
  
  gcMARK(h->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Header));
}

prim_proc {
  Scheme_Primitive_Proc *prim = (Scheme_Primitive_Proc *)p;

 mark:
  gcMARK(prim->name);
  if (prim->mina < 0) {
    gcMARK(prim->mu.cases);
  }
  if (prim->pp.flags & SCHEME_PRIM_IS_CLOSURE) {
    Scheme_Primitive_Closure *cc = (Scheme_Primitive_Closure *)prim;
    int i;
    for (i = cc->count; i--; ) {
      gcMARK(cc->val[i]);
    }
  }  
  
 size:
  ((prim->pp.flags & SCHEME_PRIM_IS_CLOSURE)
   ? (gcBYTES_TO_WORDS(sizeof(Scheme_Primitive_Closure))
      + ((Scheme_Primitive_Closure *)prim)->count - 1)
   : ((prim->pp.flags & SCHEME_PRIM_IS_MULTI_RESULT)
      ? gcBYTES_TO_WORDS(sizeof(Scheme_Prim_W_Result_Arity))
      : gcBYTES_TO_WORDS(sizeof(Scheme_Primitive_Proc))));
}

closed_prim_proc {
  Scheme_Closed_Primitive_Proc *c = (Scheme_Closed_Primitive_Proc *)p;

 mark:
  gcMARK(c->name);
  gcMARK(SCHEME_CLSD_PRIM_DATA(c));
  if (c->mina == -2) {
    gcMARK(((Scheme_Closed_Case_Primitive_Proc *)c)->cases);
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
                      ? ((Scheme_Closure_Data *)GC_resolve(c->code))->closure_size
                      : 0);

 mark:

  int i = closure_size;
  while (i--)
    gcMARK(c->vals[i]);
  gcMARK(c->code);
  
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Closure)
		    + (closure_size - 1) * sizeof(Scheme_Object *)));
}

case_closure {
  Scheme_Case_Lambda *c = (Scheme_Case_Lambda *)p;

 mark:
  int i;
  
  for (i = c->count; i--; )
    gcMARK(c->array[i]);
  gcMARK(c->name);
#ifdef MZ_USE_JIT
  gcMARK(c->native_code);
#endif

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Case_Lambda)
		    + ((c->count - 1) * sizeof(Scheme_Object *))));
}

cont_proc {
 mark:
  Scheme_Cont *c = (Scheme_Cont *)p;
  
  gcMARK(c->dw);
  gcMARK(c->prompt_tag);
  gcMARK(c->meta_continuation);
  gcMARK(c->common_dw);
  gcMARK(c->save_overflow);
  gcMARK(c->runstack_copied);
  gcMARK(c->runstack_owner);
  gcMARK(c->cont_mark_stack_copied);
  gcMARK(c->cont_mark_stack_owner);
  gcMARK(c->init_config);
  gcMARK(c->init_break_cell);
#ifdef MZ_USE_JIT
  gcMARK(c->native_trace);
#endif

  MARK_jmpup(&c->buf);
  MARK_cjs(&c->cjs);
  MARK_stack_state(&c->ss);
  gcMARK(c->barrier_prompt);
  gcMARK(c->runstack_start);
  gcMARK(c->runstack_saved);

  gcMARK(c->prompt_id);
  gcMARK(c->prompt_buf);

  /* These shouldn't actually persist across a GC, but
     just in case... */
  gcMARK(c->value);
  gcMARK(c->resume_to);
  gcMARK(c->use_next_cont);
  gcMARK(c->extra_marks);
  gcMARK(c->shortcut_prompt);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont));
}

meta_cont_proc {
 mark:
  Scheme_Meta_Continuation *c = (Scheme_Meta_Continuation *)p;
  
  gcMARK(c->prompt_tag);
  gcMARK(c->overflow);
  gcMARK(c->next);
  gcMARK(c->cont_mark_stack_copied);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Meta_Continuation));
}

mark_dyn_wind {
 mark:
  Scheme_Dynamic_Wind *dw = (Scheme_Dynamic_Wind *)p;
  
  gcMARK(dw->id);
  gcMARK(dw->data);
  gcMARK(dw->prompt_tag);
  gcMARK(dw->prev);
    
  MARK_stack_state(&dw->envss);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Dynamic_Wind));
}

mark_overflow {
 mark:
  Scheme_Overflow *o = (Scheme_Overflow *)p;

  gcMARK(o->prev);
  gcMARK(o->jmp);
  gcMARK(o->id);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Overflow));
}

mark_overflow_jmp {
 mark:
  Scheme_Overflow_Jmp *o = (Scheme_Overflow_Jmp *)p;

  MARK_jmpup(&o->cont);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Overflow_Jmp));
}

escaping_cont_proc {
 mark:
  Scheme_Escaping_Cont *c = (Scheme_Escaping_Cont *)p;

#ifdef MZ_USE_JIT
  gcMARK(c->native_trace);
#endif

  gcMARK(c->barrier_prompt);
  MARK_stack_state(&c->envss);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Escaping_Cont));
}

char_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Small_Object));
}

bignum_obj {
  Scheme_Bignum *b = (Scheme_Bignum *)p;

 mark:
  if (!SCHEME_BIGINLINE(b)) {
    gcMARK(b->digits);
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
  
  gcMARK(r->num);
  gcMARK(r->denom);

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

complex_obj {
 mark:
  Scheme_Complex *c = (Scheme_Complex *)p;
  
  gcMARK(c->r);
  gcMARK(c->i);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Complex));
}

string_obj {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  gcMARK(SCHEME_CHAR_STR_VAL(o));

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

bstring_obj {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  gcMARK(SCHEME_BYTE_STR_VAL(o));

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

symbol_obj {
  Scheme_Symbol *s = (Scheme_Symbol *)p;

 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Symbol) + s->len - 3);
}

cons_cell {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  
  gcMARK(SCHEME_CAR(o));
  gcMARK(SCHEME_CDR(o));

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

vector_obj {
  Scheme_Vector *vec = (Scheme_Vector *)p;

 mark:
  int i;
  for (i = vec->size; i--; )
    gcMARK(vec->els[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Vector) 
		    + ((vec->size - 1) * sizeof(Scheme_Object *))));
}

input_port {
 mark:
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
  
  gcMARK(ip->sub_type);
  gcMARK(ip->port_data);
  gcMARK(ip->name);
  gcMARK(ip->peeked_read);
  gcMARK(ip->peeked_write);
  gcMARK(ip->read_handler);
  gcMARK(ip->mref);
  gcMARK(ip->output_half);
  gcMARK(ip->special);
  gcMARK(ip->ungotten_special);
  gcMARK(ip->progress_evt);
  gcMARK(ip->input_lock);
  gcMARK(ip->input_giveup);
  gcMARK(ip->input_extras);
  gcMARK(ip->input_extras_ready);
  gcMARK(ip->unless);
  gcMARK(ip->unless_cache);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Input_Port));
}

output_port {
 mark:
  Scheme_Output_Port *op = (Scheme_Output_Port *)p;

  gcMARK(op->sub_type);
  gcMARK(op->port_data);
  gcMARK(op->name);
  gcMARK(op->display_handler);
  gcMARK(op->write_handler);
  gcMARK(op->print_handler);
  gcMARK(op->mref);
  gcMARK(op->input_half);

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
  
  gcMARK(pr->next);
  gcMARK(pr->prev);

  gcMARK(pr->t_set_parent);
  gcMARK(pr->t_set_next);
  gcMARK(pr->t_set_prev);

  MARK_cjs(&pr->cjs);

  gcMARK(pr->cell_values);
  gcMARK(pr->init_config);
  gcMARK(pr->init_break_cell);

  {
    Scheme_Object **rs = pr->runstack_start;
    gcFIXUP_TYPED_NOW(Scheme_Object **, pr->runstack_start);
    pr->runstack = pr->runstack_start + (pr->runstack - rs);
  }
  gcMARK(pr->runstack_saved);
  gcMARK(pr->runstack_owner);
  gcMARK(pr->runstack_swapped);
  pr->spare_runstack = NULL; /* just in case */

  gcMARK(pr->meta_prompt);
  gcMARK(pr->meta_continuation);
  
  gcMARK(pr->cont_mark_stack_segments);
  gcMARK(pr->cont_mark_stack_owner);
  gcMARK(pr->cont_mark_stack_swapped);

  MARK_jmpup(&pr->jmpup_buf);
  
  gcMARK(pr->dw);
  
  gcMARK(pr->nester);
  gcMARK(pr->nestee);
  
  gcMARK(pr->blocker);
  gcMARK(pr->overflow);
  
  gcMARK(pr->current_local_env);
  gcMARK(pr->current_local_mark);
  gcMARK(pr->current_local_name);
  gcMARK(pr->current_local_certs);
  gcMARK(pr->current_local_modidx);
  gcMARK(pr->current_local_menv);
  
  gcMARK(pr->overflow_reply);

  gcMARK(pr->values_buffer);

  gcMARK(pr->tail_buffer);
  
  gcMARK(pr->ku.eval.wait_expr);

  gcMARK(pr->ku.apply.tail_rator);
  gcMARK(pr->ku.apply.tail_rands);

  gcMARK(pr->ku.multiple.array);

  gcMARK(pr->ku.k.p1);
  gcMARK(pr->ku.k.p2);
  gcMARK(pr->ku.k.p3);
  gcMARK(pr->ku.k.p4);
  gcMARK(pr->ku.k.p5);
  
  gcMARK(pr->list_stack);
  
  gcMARK(pr->kill_data);
  gcMARK(pr->private_kill_data);
  gcMARK(pr->private_kill_next);
  
  gcMARK(pr->user_tls);
  
  gcMARK(pr->mr_hop);
  gcMARK(pr->mref);
  gcMARK(pr->extra_mrefs);

  gcMARK(pr->name);

  gcMARK(pr->transitive_resumes);

  gcMARK(pr->suspended_box);
  gcMARK(pr->resumed_box);
  gcMARK(pr->dead_box);
  gcMARK(pr->running_box);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Thread));
}

runstack_val {
  long *s = (long *)p;
 mark:
  void **a, **b;
  a = (void **)s + 4 + s[2];
  b = (void **)s + 4 + s[3];
  while (a < b) {
    gcMARK(*a);
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
  gcMARK(pr->boundary_overflow_id);
  gcMARK(pr->runstack_boundary_start);
  gcMARK(pr->tag);
  gcMARK(pr->id);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Prompt));
}

cont_mark_set_val {
 mark:
  Scheme_Cont_Mark_Set *s = (Scheme_Cont_Mark_Set *)p;
  gcMARK(s->chain);
  gcMARK(s->native_stack_trace);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont_Mark_Set));
}

sema_val {
 mark:
  Scheme_Sema *s = (Scheme_Sema *)p;

  gcMARK(s->first);
  gcMARK(s->last);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Sema));
}

channel_val {
 mark:
  Scheme_Channel *s = (Scheme_Channel *)p;

  gcMARK(s->get_first);
  gcMARK(s->get_last);
  gcMARK(s->put_first);
  gcMARK(s->put_last);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Channel));
}

channel_put_val {
 mark:
  Scheme_Channel_Put *s = (Scheme_Channel_Put *)p;

  gcMARK(s->ch);
  gcMARK(s->val);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Channel_Put));
}

hash_table_val {
 mark:
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p;

  gcMARK(ht->keys);
  gcMARK(ht->vals);
  gcMARK(ht->mutex);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Hash_Table));
}

bucket_table_val {
 mark:
  Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)p;

  gcMARK(ht->buckets);
  gcMARK(ht->mutex);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Bucket_Table));
}

namespace_val {
 mark:
  Scheme_Env *e = (Scheme_Env *)p;

  gcMARK(e->module);
  gcMARK(e->module_registry);
  gcMARK(e->export_registry);
  gcMARK(e->insp);

  gcMARK(e->rename);
  gcMARK(e->et_rename);
  gcMARK(e->tt_rename);
  gcMARK(e->dt_rename);

  gcMARK(e->syntax);
  gcMARK(e->exp_env);
  gcMARK(e->template_env);

  gcMARK(e->shadowed_syntax);

  gcMARK(e->link_midx);
  gcMARK(e->require_names);
  gcMARK(e->et_require_names);
  gcMARK(e->tt_require_names);
  gcMARK(e->dt_require_names);

  gcMARK(e->toplevel);
  gcMARK(e->modchain);

  gcMARK(e->modvars);

  gcMARK(e->marked_names);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Env));
}

random_state_val {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Random_State));
}

compilation_top_val {
 mark:
  Scheme_Compilation_Top *t = (Scheme_Compilation_Top *)p;
  gcMARK(t->code);
  gcMARK(t->prefix);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compilation_Top));
}

resolve_prefix_val {
 mark:
  Resolve_Prefix *rp = (Resolve_Prefix *)p;
  gcMARK(rp->toplevels);
  gcMARK(rp->stxes);
  gcMARK(rp->delay_info);

 size:
  gcBYTES_TO_WORDS(sizeof(Resolve_Prefix));
}

comp_prefix_val {
 mark:
  Comp_Prefix *cp = (Comp_Prefix *)p;
  gcMARK(cp->toplevels);
  gcMARK(cp->stxes);

 size:
  gcBYTES_TO_WORDS(sizeof(Comp_Prefix));
}

svector_val {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;

  gcMARK(SCHEME_SVEC_VEC(o));

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object));
}

stx_val {
 mark:
  Scheme_Stx *stx = (Scheme_Stx *)p;
  gcMARK(stx->val);
  gcMARK(stx->srcloc);
  gcMARK(stx->wraps);
  gcMARK(stx->certs);
  gcMARK(stx->props);
  if (!(MZ_OPT_HASH_KEY(&(stx)->iso) & STX_SUBSTX_FLAG))
    gcMARK(stx->u.modinfo_cache);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Stx));
}

stx_off_val {
 mark:
  Scheme_Stx_Offset *o = (Scheme_Stx_Offset *)p;
  gcMARK(o->src);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Stx_Offset));
}

module_val {
 mark:
  Scheme_Module *m = (Scheme_Module *)p;
  gcMARK(m->modname);

  gcMARK(m->et_requires);
  gcMARK(m->requires);
  gcMARK(m->tt_requires);
  gcMARK(m->dt_requires);

  gcMARK(m->body);
  gcMARK(m->et_body);

  gcMARK(m->me);

  gcMARK(m->provide_protects);
  gcMARK(m->indirect_provides);

  gcMARK(m->et_provide_protects);
  gcMARK(m->et_indirect_provides);

  gcMARK(m->self_modidx);

  gcMARK(m->accessible);
  gcMARK(m->et_accessible);

  gcMARK(m->insp);

  gcMARK(m->hints);
  gcMARK(m->ii_src);

  gcMARK(m->comp_prefix);
  gcMARK(m->prefix);
  gcMARK(m->dummy);

  gcMARK(m->rn_stx);
  gcMARK(m->et_rn_stx);
  gcMARK(m->tt_rn_stx);
  gcMARK(m->dt_rn_stx);

  gcMARK(m->primitive);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module));
}

module_phase_exports_val {
 mark:
  Scheme_Module_Phase_Exports *m = (Scheme_Module_Phase_Exports *)p;

  gcMARK(m->provides);
  gcMARK(m->provide_srcs);
  gcMARK(m->provide_src_names);
  gcMARK(m->provide_src_phases);

  gcMARK(m->kernel_exclusion);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module_Phase_Exports));
}

module_exports_val {
 mark:
  Scheme_Module_Exports *m = (Scheme_Module_Exports *)p;

  gcMARK(m->rt);
  gcMARK(m->et);
  gcMARK(m->dt);

  gcMARK(m->src_modidx);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module_Exports));
}

modidx_val {
 mark:
  Scheme_Modidx *modidx = (Scheme_Modidx *)p;

  gcMARK(modidx->path);
  gcMARK(modidx->base);
  gcMARK(modidx->resolved);
  gcMARK(modidx->shift_cache);
  gcMARK(modidx->cache_next);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Modidx));
}

guard_val {
 mark:
  Scheme_Security_Guard *g = (Scheme_Security_Guard *)p;

  gcMARK(g->parent);
  gcMARK(g->file_proc);
  gcMARK(g->network_proc);
  gcMARK(g->link_proc);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Security_Guard));
}

buf_holder {
 mark:
  Scheme_Jumpup_Buf_Holder *h = (Scheme_Jumpup_Buf_Holder *)p;
 
  MARK_jmpup(&h->buf);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Jumpup_Buf_Holder));
}

mark_inspector {
 mark:
  Scheme_Inspector *i = (Scheme_Inspector *)p;
  gcMARK(i->superior);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Inspector));
}

mark_pipe {
 mark:
  Scheme_Pipe *pp = (Scheme_Pipe *)p;
    
  gcMARK(pp->buf);
  gcMARK(pp->wakeup_on_read);
  gcMARK(pp->wakeup_on_write);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Pipe));
}

END type;

/**********************************************************************/

START env;

mark_comp_env {
 mark:
  Scheme_Full_Comp_Env *e = (Scheme_Full_Comp_Env *)p;

  gcMARK(e->base.genv);
  gcMARK(e->base.insp);
  gcMARK(e->base.prefix);
  gcMARK(e->base.next);
  gcMARK(e->base.values);
  gcMARK(e->base.certs);
  gcMARK(e->base.renames);
  gcMARK(e->base.uid);
  gcMARK(e->base.uids);
  gcMARK(e->base.dup_check);
  gcMARK(e->base.intdef_name);
  gcMARK(e->base.in_modidx);
  gcMARK(e->base.skip_table);
  
  gcMARK(e->data.const_names);
  gcMARK(e->data.const_vals);
  gcMARK(e->data.const_uids);
  gcMARK(e->data.use);
  gcMARK(e->data.lifts);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Full_Comp_Env));
}

mark_resolve_info {
 mark:
  Resolve_Info *i = (Resolve_Info *)p;
  
  gcMARK(i->prefix);
  gcMARK(i->stx_map);
  gcMARK(i->old_pos);
  gcMARK(i->new_pos);
  gcMARK(i->old_stx_pos);
  gcMARK(i->flags);
  gcMARK(i->lifts);
  gcMARK(i->lifted);
  gcMARK(i->next);

 size:
  gcBYTES_TO_WORDS(sizeof(Resolve_Info));
}

mark_optimize_info {
 mark:
  Optimize_Info *i = (Optimize_Info *)p;
  
  gcMARK(i->stat_dists);
  gcMARK(i->sd_depths);
  gcMARK(i->next);
  gcMARK(i->use);
  gcMARK(i->consts);
  gcMARK(i->top_level_consts);
  gcMARK(i->transitive_use);
  gcMARK(i->transitive_use_len);

 size:
  gcBYTES_TO_WORDS(sizeof(Optimize_Info));
}


END env;

/**********************************************************************/

START eval;

mark_comp_info {
 mark:
  Scheme_Compile_Info *i = (Scheme_Compile_Info *)p;
  
  gcMARK(i->value_name);
  gcMARK(i->certs);
  gcMARK(i->observer);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compile_Info));
}

mark_saved_stack {
 mark:
  Scheme_Saved_Stack *saved = (Scheme_Saved_Stack *)p;
  
  gcMARK(saved->prev);
  gcMARK(saved->runstack_start);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Saved_Stack));
}

END eval;

/**********************************************************************/

START file;

mark_reply_item {
 mark:
  ReplyItem *r = (ReplyItem *)p;
  
  gcMARK(r->next);

 size:
  gcBYTES_TO_WORDS(sizeof(ReplyItem));
}

END file;

/**********************************************************************/

START fun;

mark_closure_info {
 mark:
  Closure_Info *i = (Closure_Info *)p;
  
  gcMARK(i->local_flags);
  gcMARK(i->base_closure_map);

 size:
  gcBYTES_TO_WORDS(sizeof(Closure_Info));
}

mark_dyn_wind_cell {
 mark:
  Scheme_Dynamic_Wind_List *l = (Scheme_Dynamic_Wind_List *)p;
  
  gcMARK(l->dw);
  gcMARK(l->next);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Dynamic_Wind_List));
}

mark_dyn_wind_info {
 mark:
  Dyn_Wind *d = (Dyn_Wind *)p;
  
  gcMARK(d->pre);
  gcMARK(d->act);
  gcMARK(d->post);

 size:
   gcBYTES_TO_WORDS(sizeof(Dyn_Wind));
}

mark_cont_mark_chain {
 mark:
  Scheme_Cont_Mark_Chain *c = (Scheme_Cont_Mark_Chain *)p;
  
  gcMARK(c->key);
  gcMARK(c->val);
  gcMARK(c->next);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont_Mark_Chain));
}

END fun;

/**********************************************************************/

START portfun;

mark_load_handler_data {
 mark:
  LoadHandlerData *d = (LoadHandlerData *)p;
    
  gcMARK(d->config);
  gcMARK(d->port);
  gcMARK(d->p);
  gcMARK(d->stxsrc);
  gcMARK(d->expected_module);
  gcMARK(d->delay_load_info);
  
 size:
  gcBYTES_TO_WORDS(sizeof(LoadHandlerData));
}

mark_indexed_string {
 mark:
  Scheme_Indexed_String *is = (Scheme_Indexed_String *)p;
    
  gcMARK(is->string);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Indexed_String));
}

mark_user_input {
 mark:
  User_Input_Port *uip = (User_Input_Port *)p;

  gcMARK(uip->read_proc);
  gcMARK(uip->peek_proc);
  gcMARK(uip->progress_evt_proc);
  gcMARK(uip->peeked_read_proc);
  gcMARK(uip->location_proc);
  gcMARK(uip->count_lines_proc);
  gcMARK(uip->buffer_mode_proc);
  gcMARK(uip->close_proc);
  gcMARK(uip->reuse_str);
  gcMARK(uip->peeked);
 size:
  gcBYTES_TO_WORDS(sizeof(User_Input_Port));
}

mark_user_output {
 mark:
  User_Output_Port *uop = (User_Output_Port *)p;

  gcMARK(uop->evt);
  gcMARK(uop->write_evt_proc);
  gcMARK(uop->write_proc);
  gcMARK(uop->write_special_evt_proc);
  gcMARK(uop->write_special_proc);
  gcMARK(uop->location_proc);
  gcMARK(uop->count_lines_proc);
  gcMARK(uop->buffer_mode_proc);
  gcMARK(uop->close_proc);
 size:
  gcBYTES_TO_WORDS(sizeof(User_Output_Port));
}

END portfun;

/**********************************************************************/

START port;

#ifdef WINDOWS_PROCESSES
mark_thread_memory {
 mark:
  Scheme_Thread_Memory *tm = (Scheme_Thread_Memory *)p;
  gcMARK(tm->prev);
  gcMARK(tm->next);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Thread_Memory));
}
#endif

mark_input_file {
 mark:
  Scheme_Input_File *i = (Scheme_Input_File *)p;

  gcMARK(i->f);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Input_File));
}

#if defined(WIN32_FD_HANDLES)
mark_tcp_select_info {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Tcp_Select_Info));
}
#endif

mark_output_file {
 mark:
  Scheme_Output_File *o = (Scheme_Output_File *)p;

  gcMARK(o->f);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Output_File));
}

#ifdef MZ_FDS
mark_input_fd {
 mark:
  Scheme_FD *fd = (Scheme_FD *)p;

  gcMARK(fd->buffer);
  gcMARK(fd->refcount);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_FD));
}
#endif

#if defined(UNIX_PROCESSES)
mark_system_child {
 mark:
  System_Child *sc = (System_Child *)p;

  gcMARK(sc->next);

 size:
  gcBYTES_TO_WORDS(sizeof(System_Child));
}
#endif

#ifdef USE_OSKIT_CONSOLE
mark_oskit_console_input {
 mark:
  osk_console_input *c = (osk_console_input *)p;
    
  gcMARK(c->buffer);
  gcMARK(c->next);

 size:
  gcBYTES_TO_WORDS(sizeof(osk_console_input));
}
#endif

mark_subprocess {
 mark:
#ifndef WINDOWS_PROCESSES
  Scheme_Subprocess *sp = (Scheme_Subprocess *)p;
  gcMARK(sp->handle);
#endif
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Subprocess));
}

mark_read_write_evt {
 mark:
  Scheme_Read_Write_Evt *rww = (Scheme_Read_Write_Evt *)p;
  gcMARK(rww->port);
  gcMARK(rww->v);
  gcMARK(rww->str);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Read_Write_Evt));
}

END port;

/**********************************************************************/

START print;

mark_print_params {
 mark:
  PrintParams *pp = (PrintParams *)p;
  gcMARK(pp->inspector);
  gcMARK(pp->print_port);
  gcMARK(pp->print_buffer);
 size:
  gcBYTES_TO_WORDS(sizeof(PrintParams));
}

mark_marshal_tables {
 mark:
  Scheme_Marshal_Tables *mt = (Scheme_Marshal_Tables *)p;
  gcMARK(mt->symtab);
  gcMARK(mt->rns);
  gcMARK(mt->rn_refs);
  gcMARK(mt->st_refs);
  gcMARK(mt->st_ref_stack);
  gcMARK(mt->reverse_map);
  gcMARK(mt->same_map);
  gcMARK(mt->top_map);
  gcMARK(mt->key_map);
  gcMARK(mt->delay_map);
  gcMARK(mt->rn_saved);
  gcMARK(mt->shared_offsets);
  gcMARK(mt->sorted_keys);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Marshal_Tables));
}

END print;

/**********************************************************************/

START network;

mark_listener {
  listener_t *l = (listener_t *)p;

 mark:

  gcMARK(l->mref);

 size:
  gcBYTES_TO_WORDS(sizeof(listener_t) + ((l->count - 1) * sizeof(tcp_t)));
}

#ifdef USE_TCP
mark_tcp {
 mark:
  Scheme_Tcp *tcp = (Scheme_Tcp *)p;

  gcMARK(tcp->b.buffer);
  gcMARK(tcp->b.out_buffer);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Tcp));
}

# ifdef UDP_IS_SUPPORTED
mark_udp {
 mark:
  Scheme_UDP *udp = (Scheme_UDP *)p;

  gcMARK(udp->previous_from_addr);
  gcMARK(udp->mref);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_UDP));
}

mark_udp_evt {
 mark:
  Scheme_UDP_Evt *uw = (Scheme_UDP_Evt *)p;

  gcMARK(uw->udp);
  gcMARK(uw->str);
  gcMARK(uw->dest_addr);

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
    gcMARK(c->prims[i]);
  }
  gcMARK(c->extensions);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Parameterization)
		    + ((max_configs - 1) * sizeof(Scheme_Object*))));
}

mark_config {
 mark:
  Scheme_Config *config = (Scheme_Config *)p;
  gcMARK(config->key);
  gcMARK(config->cell);
  gcMARK(config->next);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Config));
}

mark_will_executor_val {
 mark:
  WillExecutor *e = (WillExecutor *)p;
  
  gcMARK(e->sema);
  gcMARK(e->first);
  gcMARK(e->last);

 size:
  gcBYTES_TO_WORDS(sizeof(WillExecutor));
}

mark_custodian_val {
 mark:
  Scheme_Custodian *m = (Scheme_Custodian *)p;
  
  gcMARK(m->boxes);
  gcMARK(m->mrefs);
  gcMARK(m->closers);
  gcMARK(m->data);

  gcMARK(m->parent);
  gcMARK(m->sibling);
  gcMARK(m->children);

  gcMARK(m->global_next);
  gcMARK(m->global_prev);

  gcMARK(m->cust_boxes);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Custodian));
}

mark_custodian_box_val {
 mark:
  Scheme_Custodian_Box *b = (Scheme_Custodian_Box *)p;
  int sd = ((Scheme_Custodian *)GC_resolve(b->cust))->shut_down;

  gcMARK(b->cust);
  if (!sd) {
    gcMARK(b->v);
  }

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Custodian_Box));
}

mark_thread_hop {
 mark:
  Scheme_Thread_Custodian_Hop *hop = (Scheme_Thread_Custodian_Hop *)p;

  gcMARK(hop->p);

 size:
   gcBYTES_TO_WORDS(sizeof(Scheme_Thread_Custodian_Hop));
}

mark_namespace_option {
 mark:
  Scheme_NSO *o = (Scheme_NSO *)p;

  gcMARK(o->key);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_NSO));
}

mark_param_data {
 mark:
  ParamData *d = (ParamData *)p;

  gcMARK(d->key);
  gcMARK(d->guard);
  gcMARK(d->extract_guard);
  gcMARK(d->defcell);

 size:
   gcBYTES_TO_WORDS(sizeof(ParamData));
}

mark_will {
 mark:
  ActiveWill *w = (ActiveWill *)p;
  
  gcMARK(w->o);
  gcMARK(w->proc);
  gcMARK(w->w);
  gcMARK(w->next);

 size:
  gcBYTES_TO_WORDS(sizeof(ActiveWill));
}

mark_will_registration {
 mark:
  WillRegistration *r = (WillRegistration *)p;
 
  gcMARK(r->proc);
  gcMARK(r->w);

 size:
  gcBYTES_TO_WORDS(sizeof(WillRegistration));
}

mark_evt {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Evt));
}

mark_syncing {
 mark:
  Syncing *w = (Syncing *)p;
 
  gcMARK(w->set);
  gcMARK(w->wrapss);
  gcMARK(w->nackss);
  gcMARK(w->reposts);
  gcMARK(w->disable_break);

 size:
  gcBYTES_TO_WORDS(sizeof(Syncing));
}

mark_evt_set {
 mark:
  Evt_Set *w = (Evt_Set *)p;
 
  gcMARK(w->ws);
  gcMARK(w->argv);

 size:
  gcBYTES_TO_WORDS(sizeof(Evt_Set));
}

mark_thread_set {
 mark:
  Scheme_Thread_Set *ts = (Scheme_Thread_Set *)p;
 
  gcMARK(ts->parent);
  gcMARK(ts->first);
  gcMARK(ts->next);
  gcMARK(ts->prev);
  gcMARK(ts->search_start);
  gcMARK(ts->current);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Thread_Set));
}

mark_thread_cell {
 mark:
  Thread_Cell *c = (Thread_Cell *)p;
 
  gcMARK(c->def_val);

 size:
  gcBYTES_TO_WORDS(sizeof(Thread_Cell));
}

END thread;

/**********************************************************************/

START salloc;

mark_finalization {
 mark:
  Finalization *f = (Finalization *)p;
  
  gcMARK(f->data);
  gcMARK(f->next);
  gcMARK(f->prev);

 size:
  gcBYTES_TO_WORDS(sizeof(Finalization));
}

mark_finalizations {
 mark:
  Finalizations *f = (Finalizations *)p;

  gcMARK(f->scheme_first);
  gcMARK(f->scheme_last);
  gcMARK(f->prim_first);
  gcMARK(f->prim_last);
  gcMARK(f->ext_data);

 size:
  gcBYTES_TO_WORDS(sizeof(Finalizations));
}

END salloc;

/**********************************************************************/

START sema;

mark_channel_syncer {
 mark:
  Scheme_Channel_Syncer *w = (Scheme_Channel_Syncer *)p;

  gcMARK(w->p);
  gcMARK(w->prev);
  gcMARK(w->next);
  gcMARK(w->syncing);
  gcMARK(w->obj);

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

mark_struct_val {
  Scheme_Structure *s = (Scheme_Structure *)p;
  int num_slots = ((Scheme_Struct_Type *)GC_resolve(s->stype))->num_slots;

 mark:
  int i;

  gcFIXUP_TYPED_NOW(Scheme_Struct_Type *, s->stype);
  
  for(i = num_slots; i--; )
    gcMARK(s->slots[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Structure) 
		    + ((num_slots - 1) * sizeof(Scheme_Object *))));
}

mark_struct_type_val {
  Scheme_Struct_Type *t = (Scheme_Struct_Type *)p;

 mark:
  int i;
  for (i = t->name_pos + 1; i--; ) {
    gcMARK(t->parent_types[i]);
  }
  gcMARK(t->name);
  gcMARK(t->inspector);
  gcMARK(t->accessor);
  gcMARK(t->mutator);
  gcMARK(t->uninit_val);
  gcMARK(t->props);
  gcMARK(t->proc_attr);
  gcMARK(t->guard);
  gcMARK(t->immutables);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Struct_Type)
		    + (t->name_pos * sizeof(Scheme_Struct_Type *))));
}

mark_struct_proc_info {
 mark:
  Struct_Proc_Info *i = (Struct_Proc_Info *)p;

  gcMARK(i->struct_type);
  gcMARK(i->func_name);

 size:
  gcBYTES_TO_WORDS(sizeof(Struct_Proc_Info));
}

mark_struct_property {
 mark:
  Scheme_Struct_Property *i = (Scheme_Struct_Property *)p;
  gcMARK(i->name);
  gcMARK(i->guard);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Struct_Property));
}

mark_wrapped_evt {
 mark:
  Wrapped_Evt *ww = (Wrapped_Evt *)p;

  gcMARK(ww->evt);
  gcMARK(ww->wrapper);

 size:
  gcBYTES_TO_WORDS(sizeof(Wrapped_Evt));
}

mark_nack_guard_evt {
 mark:
  Nack_Guard_Evt *nw = (Nack_Guard_Evt *)p;

  gcMARK(nw->maker);

 size:
  gcBYTES_TO_WORDS(sizeof(Nack_Guard_Evt));
}

END struct;

/**********************************************************************/

START syntax;

END syntax;

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
  gcMARK(cp->start);
  gcMARK(cp->orig_port);
  gcMARK(cp->ht);
  gcMARK(cp->ut);
  gcMARK(cp->symtab);
  gcMARK(cp->insp);
  gcMARK(cp->magic_sym);
  gcMARK(cp->magic_val);
  gcMARK(cp->shared_offsets);
  gcMARK(cp->delay_info);
 size:
  gcBYTES_TO_WORDS(sizeof(CPort));
}

mark_readtable {
 mark:
  Readtable *t = (Readtable *)p;
  gcMARK(t->mapping);
  gcMARK(t->fast_mapping);
  gcMARK(t->symbol_parser);
  gcMARK(t->names);
 size:
  gcBYTES_TO_WORDS(sizeof(Readtable));
}

mark_read_params {
 mark:
  ReadParams *rp = (ReadParams *)p;
  gcMARK(rp->table);
  gcMARK(rp->magic_sym);
  gcMARK(rp->magic_val);
  gcMARK(rp->delay_load_info);
 size:
  gcBYTES_TO_WORDS(sizeof(ReadParams));
}

mark_delay_load {
 mark:
  Scheme_Load_Delay *ld = (Scheme_Load_Delay *)p;
  gcMARK(ld->path);
  gcMARK(ld->symtab);
  gcMARK(ld->shared_offsets);
  gcMARK(ld->insp);
  gcMARK(ld->rn_memory);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Load_Delay));
}

mark_unmarshal_tables {
 mark:
  Scheme_Unmarshal_Tables *ut = (Scheme_Unmarshal_Tables *)p;
  gcMARK(ut->rns);
  gcMARK(ut->rp);
  gcMARK(ut->decoded);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Unmarshal_Tables));
}

END read;

/**********************************************************************/

START regexp;

mark_regexp {
  regexp *r = (regexp *)p;
 mark:
  gcMARK(r->source);
  gcMARK(r->regstart);
 size:
  gcBYTES_TO_WORDS((sizeof(regexp) + r->regsize));
}

mark_regwork {
 mark:
  Regwork *r = (Regwork *)p;
  gcMARK(r->str);
  gcMARK(r->instr);
  gcMARK(r->port);
  gcMARK(r->unless_evt);
  gcMARK(r->startp);
  gcMARK(r->maybep);
  gcMARK(r->endp);
  gcMARK(r->counters);
  gcMARK(r->peekskip);
 size:
  gcBYTES_TO_WORDS(sizeof(Regwork));
}

END regexp;

/**********************************************************************/

START string;

mark_string_convert {
 mark:
  Scheme_Converter *c = (Scheme_Converter *)p;
  gcMARK(c->mref);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Converter));
}

END string;

/**********************************************************************/

START stxobj;

mark_rename_table {
 mark:
  Module_Renames *rn = (Module_Renames *)p;
  gcMARK(rn->ht);
  gcMARK(rn->nomarshal_ht);
  gcMARK(rn->unmarshal_info);
  gcMARK(rn->plus_kernel_nominal_source);
  gcMARK(rn->marked_names);
 size:
  gcBYTES_TO_WORDS(sizeof(Module_Renames));
}

mark_srcloc {
 mark:
  Scheme_Stx_Srcloc *s = (Scheme_Stx_Srcloc *)p;
  gcMARK(s->src);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Stx_Srcloc));
}

mark_wrapchunk {
  Wrap_Chunk *wc = (Wrap_Chunk *)p;
 mark:
  int i;
  for (i = wc->len; i--; ) {
    gcMARK(wc->a[i]);
  }
 size:
  gcBYTES_TO_WORDS(sizeof(Wrap_Chunk) + ((wc->len - 1) * sizeof(Scheme_Object *)));
}

mark_cert {
 mark:
  Scheme_Cert *c = (Scheme_Cert *)p;
  gcMARK(c->mark);
  gcMARK(c->modidx);
  gcMARK(c->insp);
  gcMARK(c->key);
  gcMARK(c->mapped);
  gcMARK(c->next);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cert));
}

lex_rib {
 mark:
  Scheme_Lexical_Rib *rib = (Scheme_Lexical_Rib *)p;
  gcMARK(rib->rename);
  gcMARK(rib->next);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Lexical_Rib));
}

END stxobj;

/**********************************************************************/

START jit;

native_closure {
  Scheme_Native_Closure *c = (Scheme_Native_Closure *)p;
  int closure_size = ((Scheme_Native_Closure_Data *)GC_resolve(c->code))->closure_size;

  if (closure_size < 0) {
    closure_size = -(closure_size + 1);
  }

 mark:

  {
    int i = closure_size;
    while (i--)
      gcMARK(c->vals[i]);
  }
  gcMARK(c->code);
  
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Native_Closure)
		    + (closure_size - 1) * sizeof(Scheme_Object *)));
}

mark_jit_state {
 mark:
  mz_jit_state *j = (mz_jit_state *)p;
  gcMARK(j->mappings);
  gcMARK(j->self_data);
 size:
  gcBYTES_TO_WORDS(sizeof(mz_jit_state));
}

native_unclosed_proc {
 mark:
  Scheme_Native_Closure_Data *d = (Scheme_Native_Closure_Data *)p;
  int i;

  gcMARK(d->u2.name);
  for (i = d->retain_count; i--; ) {
    gcMARK(d->retained[i]);
  }
  if (d->closure_size < 0) {
    gcMARK(d->u.arities);
  }

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Native_Closure_Data));
}

native_unclosed_proc_plus_case {
 mark:
  Scheme_Native_Closure_Data_Plus_Case *d = (Scheme_Native_Closure_Data_Plus_Case *)p;

  native_unclosed_proc_MARK(p);
  gcMARK(d->case_lam);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Native_Closure_Data_Plus_Case));
}

END jit;

/**********************************************************************/

#define GC_REG_TRAV(type, base) GC_register_traversers(type, base ## _SIZE, base ## _MARK, base ## _FIXUP, base ## _IS_CONST_SIZE, base ## _IS_ATOMIC)
