 Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g53, int g54, Scheme_Object** g55)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_siS_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g53;
    future->arg_i1 = g54;
    future->arg_S2 = g55;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g56, Scheme_Object** g57, Scheme_Object* g58)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iSs_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g56;
    future->arg_S1 = g57;
    future->arg_s2 = g58;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g59)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_s_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g59;
  send_special_result(future, g59);
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Closure_Data* g60)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_n_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_n0 = g60;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall__s(const char *who, int src_type, prim__s f )
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG__s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
  
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g61, Scheme_Object* g62)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g61;
    future->arg_s1 = g62;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_ssi_s(const char *who, int src_type, prim_ssi_s f, Scheme_Object* g63, Scheme_Object* g64, int g65)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ssi_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g63;
    future->arg_s1 = g64;
    future->arg_i2 = g65;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_tt_s(const char *who, int src_type, prim_tt_s f, const Scheme_Object* g66, const Scheme_Object* g67)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_tt_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_t0 = g66;
    future->arg_t1 = g67;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g68, Scheme_Object* g69)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  MZ_MARK_STACK_TYPE retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_m;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g68;
    future->arg_s1 = g69;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_m;
  future->retval_m = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g70, intptr_t g71)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_Sl_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_S0 = g70;
    future->arg_l1 = g71;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, intptr_t g72)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_l_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_l0 = g72;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g73, Scheme_Object* g74, int g75)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_bsi_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_b0 = g73;
    future->arg_s1 = g74;
    future->arg_i2 = g75;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g76, int g77, Scheme_Object** g78)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iiS_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g76;
    future->arg_i1 = g77;
    future->arg_S2 = g78;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g79, Scheme_Object* g80)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g79;
    future->arg_s1 = g80;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g81)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_b_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_b0 = g81;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g82, intptr_t g83)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_sl_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g82;
    future->arg_l1 = g83;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g84, Scheme_Object** g85)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iS_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g84;
    future->arg_S1 = g85;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g86)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_S_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_S0 = g86;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g87)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_s_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g87;
  send_special_result(future, g87);
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g88, Scheme_Object** g89, int g90)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_iSi_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_i0 = g88;
    future->arg_S1 = g89;
    future->arg_i2 = g90;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g91, int g92, Scheme_Object** g93)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_siS_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g91;
    future->arg_i1 = g92;
    future->arg_S2 = g93;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g94)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  void* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_z_p;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_z0 = g94;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_p;
  future->retval_p = 0;
  
  return retval;
}
 Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g95, int g96)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  Scheme_Object* retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_si_s;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g95;
    future->arg_i1 = g96;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_s;
  future->retval_s = 0;
  receive_special_result(future, retval, 1);
  return retval;
}
 void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g97, int g98, Scheme_Object* g99)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_sis_v;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g97;
    future->arg_i1 = g98;
    future->arg_s2 = g99;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  
  
  
  
}
 int scheme_rtcall_ss_i(const char *who, int src_type, prim_ss_i f, Scheme_Object* g100, Scheme_Object* g101)
   XFORM_SKIP_PROC
{
  Scheme_Future_Thread_State *fts = scheme_future_thread_state;
  future_t *future;
  double tm;
  int retval;

  future = fts->thread->current_ft;
  future->prim_protocol = SIG_ss_i;
  future->prim_func = f;
  tm = get_future_timestamp();
  future->time_of_request = tm;
  future->source_of_request = who;
  future->source_type = src_type;
      future->arg_s0 = g100;
    future->arg_s1 = g101;
  
  future_do_runtimecall(fts, (void*)f, 0, 1);
  future = fts->thread->current_ft;
  retval = future->retval_i;
  future->retval_i = 0;
  
  return retval;
}
