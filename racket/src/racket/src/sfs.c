/*
  Racket
  Copyright (c) 2004-2014 PLT Design Inc.
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

/* This file implements the bytecode safe-for-space pass.

   See "eval.c" for an overview of compilation passes. */

#include "schpriv.h"
#include "schrunst.h"
#include "schexpobs.h"

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define FAR_VALUE_FOR_MAX_USED 0x3FFFFFFe

void scheme_init_sfs()
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

/* For debugging and measuring the worst-case cost of sfs clears: */
#define MAX_SFS_CLEARING 0

#define SFS_LOG(x) /* nothing */

Scheme_Object *scheme_sfs(Scheme_Object *o, SFS_Info *info, int max_let_depth)
{
  int init, i;

  SFS_LOG(printf("sfs %d\n", SCHEME_TYPE(o)));

  if (!info) {
    info = scheme_new_sfs_info(max_let_depth);
  }

  info->pass = 0;
  info->ip = 1;
  info->saved = scheme_null;
  info->min_touch = -1;
  info->max_touch = -1;
  info->tail_pos = 1;
  init = info->stackpos;
  o = scheme_sfs_expr(o, info, -1);

  if (info->seqn)
    scheme_signal_error("ended in the middle of an expression?");

# if MAX_SFS_CLEARING
  info->max_nontail = info->ip;
# endif

  for (i = info->depth; i-- > init; ) {
    info->max_calls[i] = info->max_nontail;
  }

  {
    Scheme_Object *v;
    v = scheme_reverse(info->saved);
    info->saved = v;
  }

  info->pass = 1;
  info->seqn = 0;
  info->ip = 1;
  info->tail_pos = 1;
  info->stackpos = init;
  o = scheme_sfs_expr(o, info, -1);

  return o;
}

SFS_Info *scheme_new_sfs_info(int depth)
{
  SFS_Info *info;
  int *max_used, *max_calls;

  info = MALLOC_ONE_RT(SFS_Info);
  SET_REQUIRED_TAG(info->type = scheme_rt_sfs_info);

  info->depth = depth;
  info->stackpos = depth;
  info->tlpos = depth;

  max_used = (int *)scheme_malloc_atomic(sizeof(int) * depth);
  max_calls = (int *)scheme_malloc_atomic(sizeof(int) * depth);

  memset(max_used, 0, sizeof(int) * depth);
  memset(max_calls, 0, sizeof(int) * depth);

  info->max_used = max_used;
  info->max_calls = max_calls;

  return info;
}

static void scheme_sfs_save(SFS_Info *info, Scheme_Object *v)
{
  if (info->pass)
    scheme_signal_error("internal error: wrong pass to save info");
  v = scheme_make_pair(v, info->saved);
  info->saved = v;
}

static Scheme_Object *scheme_sfs_next_saved(SFS_Info *info)
{
  Scheme_Object *v;

  if (!info->pass)
    scheme_signal_error("internal error: wrong pass to get saved info");
  if (!SCHEME_PAIRP(info->saved))
    scheme_signal_error("internal error: no saved info");

  v = SCHEME_CAR(info->saved);
  info->saved = SCHEME_CDR(info->saved);
  return v;
}

void scheme_sfs_start_sequence(SFS_Info *info, int cnt, int last_is_tail)
{
  info->seqn += (cnt - (last_is_tail ? 1 : 0));
}

void scheme_sfs_push(SFS_Info *info, int cnt, int track)
{
  info->stackpos -= cnt;

  if (info->stackpos < 0)
    scheme_signal_error("internal error: pushed too deep");

  if (track) {
    while (cnt--) {
      scheme_sfs_used(info, cnt);
    }
  }
}

void scheme_sfs_used(SFS_Info *info, int pos)
{
  if (info->pass)
    return;

  pos += info->stackpos;

  if ((pos < 0) || (pos >= info->depth)) {
    scheme_signal_error("internal error: stack use out of bounds");
  }
  if (pos == info->tlpos)
    scheme_signal_error("internal error: misuse of toplevel pointer");

  SFS_LOG(printf("touch %d %d\n", pos, info->ip));

  if (info->max_used[pos] >= FAR_VALUE_FOR_MAX_USED) {
    info->max_used[pos] = (FAR_VALUE_FOR_MAX_USED + 1);
    return;
  }
  
  if ((info->min_touch == -1)
      || (pos < info->min_touch))
    info->min_touch = pos;
  if (pos > info->max_touch)
    info->max_touch = pos;

  info->max_used[pos] = info->ip;
}

Scheme_Object *scheme_sfs_add_clears(Scheme_Object *expr, Scheme_Object *clears, int pre)
{
  int len, i;
  Scheme_Object *loc;
  Scheme_Sequence *s;

  if (SCHEME_NULLP(clears))
    return expr;

  len = scheme_list_length(clears);

  s = scheme_malloc_sequence(len + 1);
  s->so.type = (pre ? scheme_sequence_type : scheme_begin0_sequence_type);
  s->count = len + 1;
  s->array[pre ? len : 0] = expr;

  for (i = 0; i < len; i++) {
    loc = scheme_make_local(scheme_local_type,
                            SCHEME_INT_VAL(SCHEME_CAR(clears)),
                            SCHEME_LOCAL_CLEAR_ON_READ);
    s->array[i + (pre ? 0 : 1)] = loc;
    clears = SCHEME_CDR(clears);    
  }

  return (Scheme_Object *)s;
}

static void sfs_note_app(SFS_Info *info, Scheme_Object *rator, int flags)
{
  if (!info->pass) {
    if (!info->tail_pos) {
      if (flags & APPN_FLAG_IMMED)
        return;
      if (SAME_OBJ(scheme_values_func, rator))
        /* no need to clear for app of `values' */
        return;
      if (SCHEME_PRIMP(rator)) {
        /* Double-check for immediate primitives: */
        int opt;
        opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
        if (opt >= SCHEME_PRIM_OPT_IMMEDIATE)
          /* Don't need to clear stack before an immediate/folding call */
          return;
      }
      info->max_nontail = info->ip;
    } else {
      int tail_ok = (flags & APPN_FLAG_SFS_TAIL);      
      if (!MAX_SFS_CLEARING && (info->selfpos >= 0)) {
        if (SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type)
            && (SCHEME_LOCAL_POS(rator) + info->stackpos) == info->selfpos) {
          /* No point in clearing out any of the closure before the
             tail call. */
          int i;
          for (i = info->selflen; i--; ) {
            if ((info->selfstart + i) != info->tlpos)
              scheme_sfs_used(info, (info->selfstart - info->stackpos) + i);
          }
          tail_ok = 1;
        }
      }
      if (!tail_ok)
        info->max_nontail = info->ip;
    }
  }
}

static Scheme_Object *sfs_application(Scheme_Object *o, SFS_Info *info)
{
  Scheme_Object *orig, *naya = NULL;
  Scheme_App_Rec *app;
  int i, n;

  app = (Scheme_App_Rec *)o;
  n = app->num_args + 1;

  scheme_sfs_start_sequence(info, n, 0);
  scheme_sfs_push(info, n-1, 0);

  for (i = 0; i < n; i++) {
    orig = app->args[i];
    naya = scheme_sfs_expr(orig, info, -1);
    app->args[i] = naya;
  }

  sfs_note_app(info, app->args[0], SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);

  scheme_finish_application(app);

  return o;
}

static Scheme_Object *sfs_application2(Scheme_Object *o, SFS_Info *info)
{
  Scheme_App2_Rec *app;
  Scheme_Object *nrator, *nrand;

  app = (Scheme_App2_Rec *)o;

  scheme_sfs_start_sequence(info, 2, 0);
  scheme_sfs_push(info, 1, 0);

  nrator = scheme_sfs_expr(app->rator, info, -1);
  nrand = scheme_sfs_expr(app->rand, info, -1);
  app->rator = nrator;
  app->rand = nrand;

  sfs_note_app(info, app->rator, SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);

  scheme_reset_app2_eval_type(app);
  
  return o;
}

static Scheme_Object *sfs_application3(Scheme_Object *o, SFS_Info *info)
{
  Scheme_App3_Rec *app;
  Scheme_Object *nrator, *nrand1, *nrand2;

  app = (Scheme_App3_Rec *)o;

  scheme_sfs_start_sequence(info, 3, 0);
  scheme_sfs_push(info, 2, 0);

  nrator = scheme_sfs_expr(app->rator, info, -1);
  nrand1 = scheme_sfs_expr(app->rand1, info, -1);
  nrand2 = scheme_sfs_expr(app->rand2, info, -1);
  
  app->rator = nrator;
  app->rand1 = nrand1;
  app->rand2 = nrand2;

  sfs_note_app(info, app->rator, SCHEME_APPN_FLAGS(app) & APPN_FLAG_MASK);

  scheme_reset_app3_eval_type(app);

  return o;
}

static Scheme_Object *flatten_sequence(Scheme_Object *o)
{
  /* At this point, we sometimes have (begin ... (begin ... (begin ...))).
     Flatten those out. */
  Scheme_Sequence *s = (Scheme_Sequence *)o, *s2;
  int i, extra = 0;

  o = s->array[s->count - 1];

  while (SAME_TYPE(SCHEME_TYPE(o), scheme_sequence_type)) {
    s2 = (Scheme_Sequence *)o;
    extra += s2->count - 1;
    o = s2->array[s2->count - 1];
  }

  if (extra) {
    s2 = scheme_malloc_sequence(s->count + extra);
    s2->so.type = scheme_sequence_type;
    s2->count = s->count + extra;

    extra = 0;
    o = (Scheme_Object *)s;
    while (SAME_TYPE(SCHEME_TYPE(o), scheme_sequence_type)) {
      s = (Scheme_Sequence *)o;
      for (i = 0; i < s->count - 1; i++) {
        s2->array[extra++] = s->array[i];
      }
      o = s->array[i];
    }
    s2->array[extra++] = o;

    if (extra != s2->count) scheme_signal_error("internal error: flatten failed");

    return (Scheme_Object *)s2;
  } else
    return (Scheme_Object *)s;
}

static Scheme_Object *sfs_sequence(Scheme_Object *o, SFS_Info *info, int can_flatten)
{
  Scheme_Object *orig, *naya;
  Scheme_Sequence *seq;
  int i, n;

  seq = (Scheme_Sequence *)o;
  n = seq->count;

  scheme_sfs_start_sequence(info, n, 1);

  for (i = 0; i < n; i++) {
    orig = seq->array[i];
    naya = scheme_sfs_expr(orig, info, -2);
    seq->array[i] = naya;
  }

  if (can_flatten && info->pass)
    o = flatten_sequence(o);

  return o;
}

#define SFS_BRANCH_W 4

static Scheme_Object *sfs_one_branch(SFS_Info *info, int ip, 
                                     Scheme_Object *vec, int delta,
                                     Scheme_Object *tbranch)
{
  int t_min_t, t_max_t, t_cnt, n, stackpos, i, save_nt, b_end, nt;
  Scheme_Object *t_vec, *o;
  Scheme_Object *clears = scheme_null;

  info->min_touch = -1;
  info->max_touch = -1;
  save_nt = info->max_nontail;

  SFS_LOG(printf("%d %d %s %d\n", info->pass, ip, (delta ? "else" : "then"), ip));

  if (info->pass) {
    /* Re-install max_used entries that refer to the branch */
    o = SCHEME_VEC_ELS(vec)[delta * SFS_BRANCH_W];
    t_min_t = SCHEME_INT_VAL(o);
    o = SCHEME_VEC_ELS(vec)[(delta * SFS_BRANCH_W) + 2];
    nt = SCHEME_INT_VAL(o);
    if (nt > info->max_nontail)
      info->max_nontail = nt;
    if (t_min_t > -1) {
      t_vec = SCHEME_VEC_ELS(vec)[(delta * SFS_BRANCH_W) + 1];
      t_cnt = SCHEME_VEC_SIZE(t_vec);
      for (i = 0; i < t_cnt; i++) {
        o = SCHEME_VEC_ELS(t_vec)[i];
        if (SCHEME_INTP(o)) {
          n = SCHEME_INT_VAL(o);
          SFS_LOG(printf(" @%d %d\n", i + t_min_t, n));
          if (info->max_used[i + t_min_t] < n) {
            SFS_LOG(printf(" |%d %d %d\n", i + t_min_t, n, info->max_nontail));
            info->max_used[i + t_min_t] = n;
            info->max_calls[i + t_min_t] = info->max_nontail;
          }
        }
      }
    }
    /* If the other branch has last use for something not used in this
       branch, and if there's a non-tail call in this branch
       of later, then we'll have to start with explicit clears. 
       Note that it doesn't matter whether the other branch actually
       clears them (i.e., the relevant non-tail call might be only
       in this branch). */
    o = SCHEME_VEC_ELS(vec)[(delta * SFS_BRANCH_W) + 3];
    b_end = SCHEME_INT_VAL(o);
    SFS_LOG(printf(" %d %d %d %d\n", nt, ip, b_end, save_nt));
    if (((nt > (ip + 1)) && (nt < b_end)) /* => non-tail call in branch */
        || ((ip + 1) < save_nt)) { /* => non-tail call after branches */
      SFS_LOG(printf(" other\n"));
      o = SCHEME_VEC_ELS(vec)[(1 - delta) * SFS_BRANCH_W];
      t_min_t = SCHEME_INT_VAL(o);
      if (t_min_t > -1) {
        int at_ip, pos;
        t_vec = SCHEME_VEC_ELS(vec)[((1 - delta) * SFS_BRANCH_W) + 1];
        t_cnt = SCHEME_VEC_SIZE(t_vec);
        o = SCHEME_VEC_ELS(vec)[((1 - delta) * SFS_BRANCH_W) + 2];
        nt = SCHEME_INT_VAL(o);
        o = SCHEME_VEC_ELS(vec)[((1 - delta) * SFS_BRANCH_W) + 3];
        b_end = SCHEME_INT_VAL(o);
        for (i = 0; i < t_cnt; i++) {
          o = SCHEME_VEC_ELS(t_vec)[i];
          if (SCHEME_INTP(o)) {
            n = SCHEME_INT_VAL(o);
            pos = i + t_min_t;
            at_ip = info->max_used[pos];
            SFS_LOG(printf(" ?%d %d %d\n", pos, n, at_ip));
            /* is last use in other branch? */
            if (((!delta && (at_ip == ip))
                 || (delta && (at_ip == n)))) {
              /* Yes, so add clear */
              SFS_LOG(printf(" !%d %d %d\n", pos, n, at_ip));
              pos -= info->stackpos;
              clears = scheme_make_pair(scheme_make_integer(pos), 
                                        clears);
            }
          }
        }
      }
    }
  }

  stackpos = info->stackpos;

  tbranch = scheme_sfs_expr(tbranch, info, -1);

  if (info->pass)
    info->max_nontail = save_nt;
# if MAX_SFS_CLEARING
  else
    info->max_nontail = info->ip;
# endif

  tbranch = scheme_sfs_add_clears(tbranch, clears, 1);

  if (!info->pass) {
    t_min_t = info->min_touch;
    t_max_t = info->max_touch;
    if (t_min_t < stackpos)
      t_min_t = stackpos;
    if (t_max_t < stackpos)
      t_max_t = -1;
    SFS_LOG(printf("%d %s %d [%d,%d] /%d\n", info->pass, (delta ? "else" : "then"), ip, 
                   t_min_t, t_max_t, stackpos));
    if (t_max_t < 0) {
      t_min_t = -1;
      t_vec = scheme_false;
    } else {
      t_cnt = t_max_t - t_min_t + 1;
      t_vec = scheme_make_vector(t_cnt, NULL);
      for (i = 0; i < t_cnt; i++) {
        n = info->max_used[i + t_min_t];
        SFS_LOG(printf("%d %s %d %d -> %d/%d\n", info->pass, (delta ? "else" : "then"), ip, 
                       i + t_min_t, n, info->max_calls[i+ t_min_t]));
        if ((n > ip) && (n < FAR_VALUE_FOR_MAX_USED)) {
          SCHEME_VEC_ELS(t_vec)[i] = scheme_make_integer(n);
          info->max_used[i + t_min_t] = ip;
        } else {
          SCHEME_VEC_ELS(t_vec)[i] = scheme_false;
        }
      }
    }
    SCHEME_VEC_ELS(vec)[delta * SFS_BRANCH_W] = scheme_make_integer(t_min_t);
    SCHEME_VEC_ELS(vec)[(delta * SFS_BRANCH_W) + 1] = t_vec;
    SCHEME_VEC_ELS(vec)[(delta * SFS_BRANCH_W) + 2] = scheme_make_integer(info->max_nontail);
    SCHEME_VEC_ELS(vec)[(delta * SFS_BRANCH_W) + 3] = scheme_make_integer(info->ip);
  }

  memset(info->max_used + info->stackpos, 0, (stackpos - info->stackpos) * sizeof(int));
  memset(info->max_calls + info->stackpos, 0, (stackpos - info->stackpos) * sizeof(int));

  info->stackpos = stackpos;

  return tbranch;
}

static Scheme_Object *sfs_branch(Scheme_Object *o, SFS_Info *info)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb, *vec;
  int ip, min_t, max_t;

  b = (Scheme_Branch_Rec *)o;

  scheme_sfs_start_sequence(info, 1, 0);

  t = scheme_sfs_expr(b->test, info, -1);

  ip = info->ip;
  info->ip++;
  /* Use ip to represent all uses in the two branches.
     Use ip+1 to represent all non-tail calls in the two branches. */

  min_t = info->min_touch;
  max_t = info->max_touch;

  SFS_LOG(printf(" after test: %d %d\n", min_t, max_t));

  if (!info->pass) {
    vec = scheme_make_vector(SFS_BRANCH_W * 2, NULL);
    scheme_sfs_save(info, vec);
  } else {
    vec = scheme_sfs_next_saved(info);
  }

  tb = sfs_one_branch(info, ip, vec, 0, b->tbranch);

  if (!info->pass) {
    if ((min_t == -1)
        || ((info->min_touch > -1) && (info->min_touch < min_t)))
      min_t = info->min_touch;
    if (info->max_touch > max_t)
      max_t = info->max_touch;
    if (info->max_nontail > ip + 1)
      info->max_nontail = ip + 1;
  }

  fb = sfs_one_branch(info, ip, vec, 1, b->fbranch);

  if (!info->pass) {
    if ((min_t == -1)
        || ((info->min_touch > -1) && (info->min_touch < min_t)))
      min_t = info->min_touch;
    if (info->max_touch > max_t)
      max_t = info->max_touch;
    if (info->max_nontail > ip + 1)
      info->max_nontail = ip + 1;
  }

  SFS_LOG(printf(" done if: %d %d\n", min_t, max_t));
  
  info->min_touch = min_t;
  info->max_touch = max_t;
  
  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  return o;
}

static Scheme_Object *sfs_let_value(Scheme_Object *o, SFS_Info *info)
{
  Scheme_Let_Value *lv = (Scheme_Let_Value *)o;
  Scheme_Object *body, *rhs, *clears = scheme_null;
  int i, pos;

  scheme_sfs_start_sequence(info, 2, 1);

  rhs = scheme_sfs_expr(lv->value, info, -1);

  if (!info->pass
      || (info->ip < info->max_nontail)) {
    for (i = 0; i < lv->count; i++) {
      pos = lv->position + i;
      if (!info->pass)
        scheme_sfs_used(info, pos);
      else {
        int spos;
        spos = pos + info->stackpos;
        if ((info->max_used[spos] == info->ip)
            && (info->max_calls[spos] > info->ip)) {
          /* No one is using the id after we set it.
             We still need to set it, in case it's boxed and shared,
             but then remove the binding or box. */
          clears = scheme_make_pair(scheme_make_integer(pos),
                                    clears);
        }
      }
    }
  }

  body = scheme_sfs_expr(lv->body, info, -1);

  body = scheme_sfs_add_clears(body, clears, 1);

  lv->value = rhs;
  lv->body = body;
  
  return o;
}

static Scheme_Object *sfs_let_one(Scheme_Object *o, SFS_Info *info)
{
  Scheme_Let_One *lo = (Scheme_Let_One *)o;
  Scheme_Object *body, *rhs, *vec;
  int pos, save_mnt, ip, et;
  int unused = 0;

  scheme_sfs_start_sequence(info, 2, 1);

  scheme_sfs_push(info, 1, 1);
  ip = info->ip;
  pos = info->stackpos;
  save_mnt = info->max_nontail;

  if (!info->pass) {
    if (SCHEME_LET_ONE_TYPE(lo)) {
      /* never clear a typed slot */
      info->max_used[pos] = FAR_VALUE_FOR_MAX_USED;
    }
    vec = scheme_make_vector(3, NULL);
    scheme_sfs_save(info, vec);
  } else {
    vec = scheme_sfs_next_saved(info);
    if (SCHEME_VEC_SIZE(vec) != 3)
      scheme_signal_error("internal error: bad vector length");
    info->max_used[pos] = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[0]);
    info->max_calls[pos] = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[1]);
    info->max_nontail = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[2]);
  }

  rhs = scheme_sfs_expr(lo->value, info, -1);
  body = scheme_sfs_expr(lo->body, info, -1);

# if MAX_SFS_CLEARING
  if (!info->pass)
    info->max_nontail = info->ip;
# endif

  if (!info->pass) {
    int n;
    info->max_calls[pos] = info->max_nontail;
    n = info->max_used[pos];
    SCHEME_VEC_ELS(vec)[0] = scheme_make_integer(n);
    n = info->max_calls[pos];
    SCHEME_VEC_ELS(vec)[1] = scheme_make_integer(n);
    SCHEME_VEC_ELS(vec)[2] = scheme_make_integer(info->max_nontail);
  } else {
    info->max_nontail = save_mnt;

    if ((info->max_used[pos] <= ip) 
        || (info->max_used[pos] == FAR_VALUE_FOR_MAX_USED)) {
      /* No one is using it, so don't actually push the value at run time
         (but keep the check that the result is single-valued).
         The optimizer normally would have converted away the binding, but
         it might not because (1) it was introduced late by inlining,
         or (2) the rhs expression doesn't always produce a single
         value. */
      if (scheme_omittable_expr(rhs, 1, -1, 1, NULL, NULL, 0, 0, 0)) {
        rhs = scheme_false;
      } else if ((ip < info->max_calls[pos])
                 && SAME_TYPE(SCHEME_TYPE(rhs), scheme_toplevel_type)) {
        /* Unusual case: we can't just drop the global-variable access,
           because it might be undefined, but we don't need the value,
           and we want to avoid an SFS clear in the interpreter loop.
           So, bind #f and then access in the global in a `begin'. */
        Scheme_Sequence *s;
        s = scheme_malloc_sequence(2);
        s->so.type = scheme_sequence_type;
        s->count = 2;
        s->array[0] = rhs;
        s->array[1] = body;
        body = (Scheme_Object *)s;
        rhs = scheme_false;
      }
      unused = 1;
    }
  }

  lo->value = rhs;
  lo->body = body;

  et = scheme_get_eval_type(lo->value);
  SCHEME_LET_EVAL_TYPE(lo) = (et 
                              | (unused ? 0 : (SCHEME_LET_EVAL_TYPE(lo) & LET_ONE_TYPE_MASK))
                              | (unused ? LET_ONE_UNUSED : 0));

  return o;
}

static Scheme_Object *sfs_let_void(Scheme_Object *o, SFS_Info *info)
{
  Scheme_Let_Void *lv = (Scheme_Let_Void *)o;
  Scheme_Object *body;
  int i, pos, save_mnt;
  Scheme_Object *vec;
    
  scheme_sfs_push(info, lv->count, 1);
  pos = info->stackpos;
  save_mnt = info->max_nontail;

  if (!info->pass) {
    vec = scheme_make_vector(lv->count + 1, NULL);
    scheme_sfs_save(info, vec);
  } else {
    vec = scheme_sfs_next_saved(info);
    if (!SCHEME_VECTORP(vec))
      scheme_signal_error("internal error: not a vector");
    for (i = 0; i < lv->count; i++) {
      info->max_used[pos + i] = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[i]);
      info->max_calls[pos + i] = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[lv->count]);
    }
    info->max_nontail = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[lv->count]);
  }

  body = scheme_sfs_expr(lv->body, info, -1);

# if MAX_SFS_CLEARING
  if (!info->pass)
    info->max_nontail = info->ip;
# endif

  if (!info->pass) {
    int n;
    SCHEME_VEC_ELS(vec)[lv->count] = scheme_make_integer(info->max_nontail);
    for (i = 0; i < lv->count; i++) {
      n = info->max_used[pos + i];
      SCHEME_VEC_ELS(vec)[i] = scheme_make_integer(n);
    }
  } else {
    info->max_nontail = save_mnt;
  }

  lv->body = body;

  return o;
}

static Scheme_Object *sfs_letrec(Scheme_Object *o, SFS_Info *info)
{
  Scheme_Letrec *lr = (Scheme_Letrec *)o;
  Scheme_Object **procs, *v, *clears = scheme_null;
  int i, count;

  count = lr->count;

  scheme_sfs_start_sequence(info, count + 1, 1);

  procs = lr->procs;

  for (i = 0; i < count; i++) { 
    v = scheme_sfs_expr(procs[i], info, i);

    if (SAME_TYPE(SCHEME_TYPE(v), scheme_begin0_sequence_type)) {
      /* Some clearing actions were added to the closure.
         Lift them out. */
      int j;
      Scheme_Sequence *cseq = (Scheme_Sequence *)v;
      for (j = 1; j < cseq->count; j++) {
        int pos;
        pos = SCHEME_LOCAL_POS(cseq->array[j]);
        clears = scheme_make_pair(scheme_make_integer(pos), clears);
      }
      v = cseq->array[0];
    }
    procs[i] = v;
  }

  v = scheme_sfs_expr(lr->body, info, -1);

  v = scheme_sfs_add_clears(v, clears, 1);

  lr->body = v;

  return o;
}

static Scheme_Object *sfs_wcm(Scheme_Object *o, SFS_Info *info)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  scheme_sfs_start_sequence(info, 3, 1);

  k = scheme_sfs_expr(wcm->key, info, -1);
  v = scheme_sfs_expr(wcm->val, info, -1);
  b = scheme_sfs_expr(wcm->body, info, -1);
  
  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  return o;
}

/*========================================================================*/
/*                            other syntax                                */
/*========================================================================*/

static Scheme_Object *
define_values_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Object *e;
  scheme_sfs_start_sequence(info, 1, 0);
  e = scheme_sfs_expr(SCHEME_VEC_ELS(data)[0], info, -1);
  SCHEME_VEC_ELS(data)[0] = e;
  return data;
}

static Scheme_Object *
inline_variant_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Object *e;
  scheme_sfs_start_sequence(info, 1, 0);
  e = scheme_sfs_expr(SCHEME_VEC_ELS(data)[0], info, -1);
  SCHEME_VEC_ELS(data)[0] = e;
  /* we don't bother with inlinable variant, since it isn't called directly */
  return data;
}

static Scheme_Object *
set_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *var, *val;

  var = sb->var;
  val = sb->val;
  
  scheme_sfs_start_sequence(info, 2, 0);

  val = scheme_sfs_expr(val, info, -1);
  var = scheme_sfs_expr(var, info, -1);

  sb->var = var;
  sb->val = val;

  return (Scheme_Object *)sb;
}

static Scheme_Object *
ref_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Object *a_naya;
  Scheme_Object *b_naya;

  scheme_sfs_start_sequence(info, 1, 0);
  a_naya = scheme_sfs_expr(SCHEME_PTR1_VAL(data), info, -1);
  b_naya = scheme_sfs_expr(SCHEME_PTR2_VAL(data), info, -1);
  SCHEME_PTR1_VAL(data) = a_naya;
  SCHEME_PTR2_VAL(data) = b_naya;

  return data;
}

static Scheme_Object *
apply_values_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Object *f, *e;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  scheme_sfs_start_sequence(info, 2, 0);

  f = scheme_sfs_expr(f, info, -1);
  e = scheme_sfs_expr(e, info, -1);

  SCHEME_PTR1_VAL(data) = f;
  SCHEME_PTR2_VAL(data) = e;

  return data;
}

static Scheme_Object *
case_lambda_sfs(Scheme_Object *expr, SFS_Info *info)
{
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;
  Scheme_Object *le, *clears = scheme_null;
  int i;

  scheme_sfs_start_sequence(info, seq->count, 0);

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = scheme_sfs_expr(le, info, -1);
    if (SAME_TYPE(SCHEME_TYPE(le), scheme_begin0_sequence_type)) {
      /* Some clearing actions were added to the closure.
         Lift them out. */
      int j;
      Scheme_Sequence *cseq = (Scheme_Sequence *)le;
      if (!cseq->count)
        scheme_signal_error("internal error: empty sequence");
      for (j = 1; j < cseq->count; j++) {
        int pos;
        pos = SCHEME_LOCAL_POS(cseq->array[j]);
        clears = scheme_make_pair(scheme_make_integer(pos), clears);
      }
      le = cseq->array[0];
    }
    if (!SAME_TYPE(SCHEME_TYPE(le), scheme_unclosed_procedure_type)
        && !SAME_TYPE(SCHEME_TYPE(le), scheme_closure_type)) {
      scheme_signal_error("internal error: not a lambda for case-lambda: %d",
                          SCHEME_TYPE(le));
    }
    seq->array[i] = le;
  }

  if (!SCHEME_NULLP(clears)) {
    return scheme_sfs_add_clears(expr, clears, 0);
  } else
    return expr;
}

static Scheme_Object *bangboxenv_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Object *e;
  int spos, drop;

  spos = SCHEME_INT_VAL(SCHEME_PTR1_VAL(data)) + info->stackpos;
  if (info->pass 
      && (info->max_used[spos] < info->ip))
    /* Not used, so don't bother boxing. In fact, the original value
       might be cleared already, so we wan't legally box anymore. */
    drop = 1;
  else
    drop = 0;

  e = scheme_sfs_expr(SCHEME_PTR2_VAL(data), info, -1);

  if (drop)
    return e;
  else {
    SCHEME_PTR2_VAL(data) = e;
    return data;
  }
}

static Scheme_Object *
begin0_sfs(Scheme_Object *obj, SFS_Info *info)
{
  int i, cnt;
  
  cnt = ((Scheme_Sequence *)obj)->count;

  scheme_sfs_start_sequence(info, cnt, 0);

  for (i = 0; i < cnt; i++) {
    Scheme_Object *le;
    le = scheme_sfs_expr(((Scheme_Sequence *)obj)->array[i], info, -1);
    ((Scheme_Sequence *)obj)->array[i] = le;
  }

  return obj;
}

static Scheme_Object *do_define_syntaxes_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Object *e;

  if (!info->pass) {
    int depth;
    depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(data)[2]);
    info = scheme_new_sfs_info(depth);
    e = scheme_sfs(SCHEME_VEC_ELS(data)[0], info, depth);
    SCHEME_VEC_ELS(data)[0] = e;
  }

  return data;
}

static Scheme_Object *define_syntaxes_sfs(Scheme_Object *data, SFS_Info *info)
{
  return do_define_syntaxes_sfs(data, info);
}

static Scheme_Object *begin_for_syntax_sfs(Scheme_Object *data, SFS_Info *info)
{
  Scheme_Object *l, *a;

  if (!info->pass) {
    int depth;
    depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(data)[2]);

    for (l = SCHEME_VEC_ELS(data)[0]; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      info = scheme_new_sfs_info(depth);
      a = scheme_sfs(a, info, depth);
      SCHEME_CAR(l) = a;
    }
  }

  return data;
}

/*========================================================================*/
/*                             closures                                   */
/*========================================================================*/

static Scheme_Object *sfs_closure(Scheme_Object *expr, SFS_Info *info, int self_pos)
{
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)expr;
  Scheme_Object *code;
  int i, size, has_tl = 0;

  size = data->closure_size;
  if (size) {
    if (info->stackpos + data->closure_map[size - 1] == info->tlpos) {
      has_tl = 1;
      --size;
    }
  }

  if (!info->pass) {
    for (i = size; i--; ) {
      scheme_sfs_used(info, data->closure_map[i]);
    }
  } else {
    /* Check whether we need to zero out any stack positions
       after capturing them in a closure: */
    Scheme_Object *clears = scheme_null;

    if (info->ip < info->max_nontail) {
      int pos, ip;
      for (i = size; i--; ) {
        pos = data->closure_map[i] + info->stackpos;
        if (pos < info->depth) {
          ip = info->max_used[pos];
          if ((ip == info->ip)
              && (ip < info->max_calls[pos])) {
            pos -= info->stackpos;
            clears = scheme_make_pair(scheme_make_integer(pos),
                                      clears);
          }
        }
      }
    }

    return scheme_sfs_add_clears(expr, clears, 0);
  }

  if (!(SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SFS)) {
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_SFS;
    info = scheme_new_sfs_info(data->max_let_depth);
    scheme_sfs_push(info, data->closure_size + data->num_params, 1);

    if (has_tl)
      info->tlpos = info->stackpos + data->closure_size - 1;

    if (self_pos >= 0) {
      for (i = size; i--; ) {
        if (data->closure_map[i] == self_pos) {
          info->selfpos = info->stackpos + i;
          info->selfstart = info->stackpos;
          info->selflen = data->closure_size;
          break;
        }
      }
    }

    /* Never clear typed arguments or typed closure elements: */
    if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_TYPED_ARGS) {
      int delta, size, ct, j, pos;
      mzshort *map;
      delta = data->closure_size;
      size = data->closure_size + data->num_params;
      map = data->closure_map;
      for (j = 0; j < size; j++) {
        ct = scheme_boxmap_get(map, j, delta);
        if (ct > CLOS_TYPE_TYPE_OFFSET) {
          if (j < data->num_params)
            pos = info->stackpos + delta + j;
          else
            pos = info->stackpos + (j - data->num_params);
          info->max_used[pos] = FAR_VALUE_FOR_MAX_USED;
        }
      }
    }

    code = scheme_sfs(data->code, info, data->max_let_depth);

    /* If any arguments go unused, and if there's a non-tail,
       non-immediate call in the body, then we flush the
       unused arguments at the start of the body. We assume that
       the closure values are used (otherwise they wouldn't
       be in the closure). */
    if (info->max_nontail) {
      int i, pos, cnt;
      Scheme_Object *clears = scheme_null;

      cnt = data->num_params;
      for (i = 0; i < cnt; i++) {
        pos = data->max_let_depth - (cnt - i);
        if (!info->max_used[pos]) {
          pos = i + data->closure_size;
          clears = scheme_make_pair(scheme_make_integer(pos),
                                    clears);
        }
      }
      
      if (SCHEME_PAIRP(clears))
        code = scheme_sfs_add_clears(code, clears, 1);

      if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
        SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_NEED_REST_CLEAR;
    }

    data->code = code;
  }

  return expr;
}

/*========================================================================*/
/*                              module                                    */
/*========================================================================*/

static Scheme_Object *
module_sfs(Scheme_Object *data, SFS_Info *old_info)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *e, *ex;
  SFS_Info *info;
  int i, j, cnt, let_depth;

  if (!old_info->for_mod) {
    if (old_info->pass)
      return data;

    info = scheme_new_sfs_info(m->max_let_depth);
    info->for_mod = 1;
    scheme_sfs(data, info, m->max_let_depth);
    return data;
  }

  info = old_info;

  cnt = SCHEME_VEC_SIZE(m->bodies[0]);
  scheme_sfs_start_sequence(info, cnt, 0);

  for (i = 0; i < cnt; i++) {
    e = scheme_sfs_expr(SCHEME_VEC_ELS(m->bodies[0])[i], info, -1);
    SCHEME_VEC_ELS(m->bodies[0])[i] = e;
  }

  if (!info->pass) {
    for (j = m->num_phases; j-- > 1; ) {
      cnt = SCHEME_VEC_SIZE(m->bodies[j]);
      for (i = 0; i < cnt; i++) {
        e = SCHEME_VEC_ELS(m->bodies[j])[i];
        
        let_depth = SCHEME_INT_VAL(SCHEME_VEC_ELS(e)[2]);
        ex = SCHEME_VEC_ELS(e)[1];
        
        info = scheme_new_sfs_info(let_depth);
        ex = scheme_sfs(ex, info, let_depth);
        SCHEME_VEC_ELS(e)[1] = ex;
      }
    }
  }

  return data;
}

static Scheme_Object *
top_level_require_sfs(Scheme_Object *data, SFS_Info *rslv)
{
  return data;
}

/*========================================================================*/
/*                            expressions                                 */
/*========================================================================*/

Scheme_Object *scheme_sfs_expr(Scheme_Object *expr, SFS_Info *info, int closure_self_pos)
/* closure_self_pos == -2 => immediately in sequence */
{
  Scheme_Type type = SCHEME_TYPE(expr);
  int seqn, stackpos, tp;

  seqn = info->seqn;
  stackpos = info->stackpos;
  tp = info->tail_pos;
  if (seqn) {
    info->seqn = 0;
    info->tail_pos = 0;
  }
  info->ip++;

  switch (type) {
  case scheme_local_type:
  case scheme_local_unbox_type:
    if (!info->pass)
      scheme_sfs_used(info, SCHEME_LOCAL_POS(expr));
    else if (!SCHEME_GET_LOCAL_TYPE(expr)) {
      int pos, at_ip;
      pos = SCHEME_LOCAL_POS(expr);
      at_ip = info->max_used[info->stackpos + pos];
      if (at_ip < info->max_calls[info->stackpos + pos]) {
        if (at_ip == info->ip) {
          /* Clear on read: */
          expr = scheme_make_local(type, pos, SCHEME_LOCAL_CLEAR_ON_READ);
        } else {
          /* Someone else clears it: */
          expr = scheme_make_local(type, pos, SCHEME_LOCAL_OTHER_CLEARS);
        }
      } else {
# if MAX_SFS_CLEARING
        scheme_signal_error("should have been cleared somewhere");
# endif
      }
    }
    break;
  case scheme_application_type:
    expr = sfs_application(expr, info);
    break;
  case scheme_application2_type:
    expr = sfs_application2(expr, info);
    break;
  case scheme_application3_type:
    expr = sfs_application3(expr, info);
    break;
  case scheme_sequence_type:
    expr = sfs_sequence(expr, info, closure_self_pos != -2);
    break;
  case scheme_splice_sequence_type:
    expr = sfs_sequence(expr, info, 0);
    break;
  case scheme_branch_type:
    expr = sfs_branch(expr, info);
    break;
  case scheme_with_cont_mark_type:
    expr = sfs_wcm(expr, info);
    break;
  case scheme_unclosed_procedure_type:
    expr = sfs_closure(expr, info, closure_self_pos);
    break;
  case scheme_let_value_type:
    expr = sfs_let_value(expr, info);
    break;
  case scheme_let_void_type:
    expr = sfs_let_void(expr, info);
    break;
  case scheme_letrec_type:
    expr = sfs_letrec(expr, info);
    break;
  case scheme_let_one_type:
    expr = sfs_let_one(expr, info);
    break;
  case scheme_closure_type:
    {
      Scheme_Closure *c = (Scheme_Closure *)expr;
      if (ZERO_SIZED_CLOSUREP(c)) {
        Scheme_Object *code;
	code = sfs_closure((Scheme_Object *)c->code, info, closure_self_pos);
        if (SAME_TYPE(SCHEME_TYPE(code), scheme_begin0_sequence_type))  {
          Scheme_Sequence *seq = (Scheme_Sequence *)code;
          c->code = (Scheme_Closure_Data *)seq->array[0];
          seq->array[0] = expr;
          expr = code;
        } else {
          c->code = (Scheme_Closure_Data *)code;
        }
      }
    }
    break;
  case scheme_toplevel_type:
    {
      int c = SCHEME_TOPLEVEL_DEPTH(expr);
      if (info->stackpos + c != info->tlpos)
        scheme_signal_error("toplevel access not at expected place");
    }
    break;
  case scheme_case_closure_type:
    {
      /* FIXME: maybe need to handle eagerly created closure */
    }
    break;
  case scheme_define_values_type:
    expr = define_values_sfs(expr, info);
    break;
  case scheme_define_syntaxes_type:
    expr = define_syntaxes_sfs(expr, info);
    break;
  case scheme_begin_for_syntax_type:
    expr = begin_for_syntax_sfs(expr, info);
    break;
  case scheme_set_bang_type:
    expr = set_sfs(expr, info);
    break;
  case scheme_boxenv_type:
    expr = bangboxenv_sfs(expr, info);
    break;
  case scheme_begin0_sequence_type:
    expr = begin0_sfs(expr, info);
    break;
  case scheme_require_form_type:
    expr = top_level_require_sfs(expr, info);
    break;
  case scheme_varref_form_type:
    expr = ref_sfs(expr, info);
    break;
  case scheme_apply_values_type:
    expr = apply_values_sfs(expr, info);
    break;
  case scheme_case_lambda_sequence_type:
    expr = case_lambda_sfs(expr, info);
    break;
  case scheme_module_type:
    expr = module_sfs(expr, info);
    break;
  case scheme_inline_variant_type:
    expr = inline_variant_sfs(expr, info);
    break;
  default:
    break;
  }

  info->ip++;

  if (seqn) {
    info->seqn = seqn - 1;
    memset(info->max_used + info->stackpos, 0, (stackpos - info->stackpos) * sizeof(int));
    memset(info->max_calls + info->stackpos, 0, (stackpos - info->stackpos) * sizeof(int));
    info->stackpos = stackpos;
    info->tail_pos = tp;
  }

  return expr;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_sfs.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_sfs_info, mark_sfs_info);
}

END_XFORM_SKIP;

#endif
